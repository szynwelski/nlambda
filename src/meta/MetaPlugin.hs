module MetaPlugin where
import GhcPlugins
import PprCore
import Data.IORef
import System.IO.Unsafe
import Unique
import Avail
import Serialized
import Annotations
import GHC hiding (exprType)
import Control.Monad (unless)
import Data.Data (Data)
import Data.List (find, isInfixOf, isPrefixOf, isSuffixOf, intersperse)
import Data.Maybe (fromJust)
import TypeRep
import Maybes
import TcType (tcSplitSigmaTy)
import TyCon
import Unify
import CoreSubst

import Data.Map (Map)
import qualified Data.Map as Map
import Meta

import Debug.Trace (trace) --pprTrace


plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  reinitializeGlobals
  env <- getHscEnv
  return (CoreDoPluginPass "MetaPlugin" (pass $ getMetaModule env) : todo)


modInfo label fun guts = putMsg $ text label <> text ": " <> (ppr $ fun guts)

pass :: HomeModInfo -> ModGuts -> CoreM ModGuts
pass mod guts = do putMsg $ (text ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> start:") <+> (ppr $ mg_module guts)
                   -- vars maps
                   varMap <- mkVarMap guts mod

                   -- binds
                   binds <- newBinds mod varMap (getDataCons guts) (mg_binds guts)

                   -- exports
                   let exps = newExports varMap

                   -- new guts
                   let guts' = guts {mg_binds = mg_binds guts ++ binds, mg_exports = mg_exports guts ++ exps}

                   -- show info
                   putMsg $ text "binds:\n" <+> (foldr (<+>) (text "") $ map showBind $ mg_binds guts')

--                   modInfo "binds" mg_binds guts'
                   modInfo "exports" mg_exports guts'
--                   modInfo "type constructors" mg_tcs guts
--                   modInfo "used names" mg_used_names guts
--                   modInfo "global rdr env" mg_rdr_env guts
--                   modInfo "fixities" mg_fix_env guts
--                   modInfo "class instances" mg_insts guts
--                   modInfo "family instances" mg_fam_insts guts
--                   modInfo "pattern synonyms" mg_patsyns guts
--                   modInfo "core rules" mg_rules guts
--                   modInfo "vect decls" mg_vect_decls guts
--                   modInfo "vect info" mg_vect_info guts
--                   modInfo "files" mg_dependent_files guts

                   putMsg $ (text ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> end:") <+> (ppr $ mg_module guts)
                   return guts'


----------------------------------------------------------------------------------------
-- Type constructors
----------------------------------------------------------------------------------------

getDataCons :: ModGuts -> [DataCon]
getDataCons = concatMap tyConDataCons . filter isAlgTyCon . mg_tcs

----------------------------------------------------------------------------------------
-- Variables / names map
----------------------------------------------------------------------------------------

mkVarMap :: ModGuts -> HomeModInfo -> CoreM (Map Var Var)
mkVarMap guts mod = do bindMap <- mkBindVarMap guts mod
                       tyConMap <- mkTyConMap guts mod
                       return $ Map.union bindMap tyConMap

mkMapWithVars :: ModGuts -> HomeModInfo -> [Var] -> CoreM (Map Var Var)
mkMapWithVars guts mod vars = do newVars <- mapM (newBindVar mod) vars
                                 return $ Map.fromList $ zip vars newVars

mkTyConMap :: ModGuts -> HomeModInfo -> CoreM (Map Id Id)
mkTyConMap guts mod = mkMapWithVars guts mod vars
    where vars = fmap dataConWorkId $ getDataCons guts

mkBindVarMap :: ModGuts -> HomeModInfo -> CoreM (Map CoreBndr CoreBndr)
mkBindVarMap guts mod = mkMapWithVars guts mod vars
    where vars = concatMap toVars (mg_binds guts)
          toVars (NonRec v _) = [v]
          toVars (Rec bs) = fmap fst bs

newBindVar :: HomeModInfo -> Var -> CoreM Var
newBindVar mod v = let var = mkLocalId (varName v) (newBindType mod v)
                   in changeVarName "nlambda_" "" (if isExportedId v then setIdExported var else setIdNotExported var)

changeVarName :: String -> String -> Var -> CoreM Var
changeVarName prefix suffix v = do name <- newName $ varName v
                                   return $ setVarName v name
    where newName name = do let occName = nameOccName name
                            let newOccName = mkOccName (occNameSpace occName) (prefix ++ occNameString occName ++ suffix)
                            uniq <- getUniqueM
                            return $ setNameLoc (setNameUnique (tidyNameOcc name newOccName) uniq) noSrcSpan

----------------------------------------------------------------------------------------
-- Exports
----------------------------------------------------------------------------------------

newExports :: Map Var Var -> Avails
newExports = fmap Avail . fmap varName . Map.elems

----------------------------------------------------------------------------------------
-- Binds
----------------------------------------------------------------------------------------

newBinds :: HomeModInfo -> Map Var Var -> [DataCon] -> CoreProgram -> CoreM CoreProgram
newBinds mod varMap dcs bs = do bs' <- mapM (changeBind mod varMap) bs
                                bs'' <- mapM (dataBind mod varMap) dcs
                                return $ bs' ++ bs''

changeBind :: HomeModInfo -> Map Var Var -> CoreBind -> CoreM CoreBind
changeBind mod varMap (NonRec b e) =
    do newExpr <- changeExpr mod varMap e
       return $ NonRec (varMap Map.! b) newExpr
changeBind mod varMap b = return b

dataBind :: HomeModInfo -> Map Var Var -> DataCon -> CoreM CoreBind
dataBind mod varMap dc = do expr <- dataConExpr mod dc [] 0
                            return $ NonRec (varMap Map.! dataConWorkId dc) expr

----------------------------------------------------------------------------------------
-- Type
----------------------------------------------------------------------------------------

newBindType :: HomeModInfo -> CoreBndr -> Type
newBindType mod = changeType mod . varType

changeType :: HomeModInfo -> Type -> Type
changeType mod t | (Just (tv, t')) <- splitForAllTy_maybe t
                 = mkForAllTy tv (changeType mod t')
changeType mod t | (Just (funArg, funRes)) <- splitFunTy_maybe t
                 , isPredTy funArg
                 = mkFunTy funArg (changeType mod funRes)
changeType mod t = withMetaType mod $ changeTypeRec mod t

changeTypeRec :: HomeModInfo -> Type -> Type
changeTypeRec mod t | (Just (funArg, funRes)) <- splitFunTy_maybe t
                    = mkFunTy (changeType mod funArg) (changeType mod funRes)
changeTypeRec mod t | (Just (t1, t2)) <- splitAppTy_maybe t
                    = mkAppTy t1 (changeTypeRec mod t2)
changeTypeRec mod t = t

----------------------------------------------------------------------------------------
-- Expr
----------------------------------------------------------------------------------------

getVarNameStr :: Var -> String
getVarNameStr = occNameString . nameOccName . varName

isInternalVar :: Var -> Bool
isInternalVar = isSuffixOf "#" . getVarNameStr

isDictVar :: Var -> Bool
isDictVar = isPrefixOf "$" . getVarNameStr

getExprFromBind :: CoreBind -> CoreExpr
getExprFromBind (NonRec b e) = e
getExprFromBind (Rec bs) = head $ fmap snd bs -- FIXME

changeExpr :: HomeModInfo -> Map Var Var -> CoreExpr -> CoreM CoreExpr
changeExpr mod varMap e = newExpr e
    where newExpr (Var v) | Map.member v varMap = return $ Var (varMap Map.! v)
          newExpr (Var v) | isLocalVar v = return $ Var v
          newExpr (Lit l) = emptyExpr mod (Lit l)
          newExpr (App f (Type t)) = do f' <- newExpr f
                                        return $ App f' (Type t)
          newExpr (App f x) = do f' <- newExpr f
                                 f'' <- valueExpr mod f'
                                 x' <- newExpr x
                                 return $ mkCoreApp f'' x'
          newExpr (Lam x e) = do e' <- newExpr e
                                 emptyExpr mod (Lam x e')
          newExpr e = return e

--    where newExpr (Var v) | Map.member v varMap = Var (varMap Map.! v)
--          newExpr (Var v) = emptyV mod
--          newExpr (Lit l) = emptyV mod
--          newExpr (App f (Type t)) = newExpr f
--          newExpr (App f (Var v)) | isDictVar v = newExpr f
--          newExpr (App (Var v) x) | isDictVar v = newExpr x
--          newExpr (App f x) = unionExpr mod (newExpr f) (newExpr x)
--          newExpr (Lam x e) = newExpr e
--          newExpr (Let b e) = unionExpr mod (getExprFromBind b) e
--          newExpr (Case e b t as) = emptyV mod
--          newExpr (Cast e c) = emptyV mod
--          newExpr (Tick t e) = emptyV mod
--          newExpr (Type t) = emptyV mod
--          newExpr (Coercion c) = emptyV mod

dataConExpr :: HomeModInfo -> DataCon -> [Var] -> Int -> CoreM (CoreExpr)
dataConExpr mod dc xs argNumber =
    if argNumber == dataConSourceArity dc
    then do let revXs = reverse xs
            xs' <- mapM (changeVarName "" "'") revXs
            xValues <- mapM (valueExpr mod) (fmap Var $ xs')
            mkLetUnionExpr (emptyMetaV mod) revXs xs' $ mkCoreConApps dc xValues
    else do uniq <- getUniqueM
            let xnm = mkInternalName uniq (mkVarOcc $ "x" ++ show argNumber) noSrcSpan
            let ty = withMetaType mod $ dataConOrigArgTys dc !! argNumber
            let x = mkLocalId xnm ty
            expr <- dataConExpr mod dc (x : xs) (succ argNumber)
            emptyExpr mod $ Lam x expr
    where mkLetUnionExpr :: (CoreExpr) -> [Var] -> [Var] -> CoreExpr -> CoreM (CoreExpr)
          mkLetUnionExpr meta (x:xs) (x':xs') expr = do union <- unionExpr mod (Var x) meta
                                                        meta' <- metaExpr mod (Var x')
                                                        expr' <- mkLetUnionExpr meta' xs xs' expr
                                                        return $ bindNonRec x' union expr'
          mkLetUnionExpr meta [] [] expr = createExpr mod expr meta

----------------------------------------------------------------------------------------
-- Meta
----------------------------------------------------------------------------------------

getMetaModule :: HscEnv -> HomeModInfo
getMetaModule = fromJust . find ((== "Meta") . moduleNameString . moduleName . mi_module . hm_iface) . eltsUFM . hsc_HPT

hasName :: String -> Name -> Bool
hasName nmStr nm = occNameString (nameOccName nm) == nmStr

getTyThing :: HomeModInfo -> String -> (TyThing -> Bool) -> (TyThing -> a) -> (a -> Name) -> a
getTyThing mod nm cond fromThing getName = fromThing $ head $ nameEnvElts $ filterNameEnv
                                                                                    (\t -> cond t && hasName nm (getName $ fromThing t))
                                                                                    (md_types $ hm_details mod)

isTyThingId :: TyThing -> Bool
isTyThingId (AnId _) = True
isTyThingId _        = False

getVar :: HomeModInfo -> String -> Var
getVar mod nm = getTyThing mod nm isTyThingId tyThingId varName

isTyThingTyCon :: TyThing -> Bool
isTyThingTyCon (ATyCon _) = True
isTyThingTyCon _          = False

getTyCon :: HomeModInfo -> String -> GHC.TyCon
getTyCon mod nm = getTyThing mod nm isTyThingTyCon tyThingTyCon tyConName

emptyV mod = Var $ getVar mod "empty"
emptyMetaV mod = Var $ getVar mod "emptyMeta"
unionV mod = Var $ getVar mod $ "union"
metaV mod = Var $ getVar mod "meta"
valueV mod = Var $ getVar mod "value"
createV mod = Var $ getVar mod "create"
withMetaC mod = getTyCon mod "WithMeta"

withMetaType :: HomeModInfo -> Type -> Type
withMetaType mod ty = mkTyConApp (withMetaC mod) [ty]

mkPredVar :: (Class, [Type]) -> CoreM DictId
mkPredVar (cls, tys) = do uniq <- getUniqueM
                          let name = mkSystemName uniq (mkDictOcc (getOccName cls))
                          return (mkLocalId name (mkClassPred cls tys))

makeTyVarUnique :: TyVar -> CoreM TyVar
makeTyVarUnique v = do uniq <- getUniqueM
                       return $ mkTyVar (setNameUnique (tyVarName v) uniq) (tyVarKind v)

splitType :: CoreExpr -> CoreM ([TyVar], [DictId], Type)
splitType e =
    do let ty = exprType e
       let (tyVars, preds, ty') = tcSplitSigmaTy ty
       tyVars' <- mapM makeTyVarUnique tyVars
       let preds' = filter isClassPred preds
       let classTys = map getClassPredTys preds'
       predVars <- mapM mkPredVar classTys
       return (tyVars', predVars, ty')

applyExpr :: CoreExpr -> CoreExpr -> CoreM CoreExpr
applyExpr fun e =
    do (tyVars, predVars, ty) <- splitType e
       let (funTyVars, _, funTy) = tcSplitSigmaTy $ exprType fun
       let subst = fromJust $ tcUnifyTy (funArgTy funTy) ty
       let funTyVarSubstExprs = fmap (Type . substTyVar subst) funTyVars
       return $
         mkCoreLams tyVars $ mkCoreLams predVars $
            mkCoreApp
                (mkCoreApps fun $ funTyVarSubstExprs)
                (mkCoreApps
                    (mkCoreApps e $ fmap Type $ mkTyVarTys tyVars)
                    (fmap Var predVars))

emptyExpr :: HomeModInfo -> CoreExpr -> CoreM CoreExpr
emptyExpr mod e = applyExpr (emptyV mod) e

valueExpr :: HomeModInfo -> CoreExpr -> CoreM CoreExpr
valueExpr mod e = applyExpr (valueV mod) e

metaExpr :: HomeModInfo -> CoreExpr -> CoreM CoreExpr
metaExpr mod e = applyExpr (metaV mod) e

unionExpr :: HomeModInfo -> CoreExpr -> CoreExpr -> CoreM CoreExpr
unionExpr mod e1 e2 = do e <- applyExpr (unionV mod) e1
                         return $ mkCoreApp e e2

createExpr :: HomeModInfo -> CoreExpr -> CoreExpr -> CoreM CoreExpr
createExpr mod e1 e2 = do e <- applyExpr (createV mod) e1
                          return $ mkCoreApp e e2


----------------------------------------------------------------------------------------
-- Show
----------------------------------------------------------------------------------------

when c v = if c then text " " <> ppr v else text ""
whenT c v = if c then text " " <> text v else text ""

showBind :: CoreBind -> SDoc
showBind (NonRec b e) = text "===> "
                        <+> showVar b
                        <+> text "::"
                        <+> showType (varType b)
                        <> text "\n"
                        <+> showExpr e
                        <> text "\n"
showBind b@(Rec _) = text "Rec [" <+> ppr b <+> text "]"

showType :: Type -> SDoc
--showType = ppr
showType (TyVarTy v) = text "TyVarTy(" <> showVar v <> text ")"
showType (AppTy t1 t2) = text "AppTy(" <> showType t1 <+> showType t2 <> text ")"
showType (TyConApp tc ts) = text "TyConApp(" <> showTyCon tc <+> hsep (fmap showType ts) <> text ")"
showType (FunTy t1 t2) = text "FunTy(" <> showType t1 <+> showType t2 <> text ")"
showType (ForAllTy v t) = text "ForAllTy(" <> showVar v <+> showType t <> text ")"
showType (LitTy tl) = text "LitTy(" <> ppr tl <> text ")"

showTypeStr :: Type -> String
showTypeStr (TyVarTy v) = showVarStr v
showTypeStr (AppTy t1 t2) = showTypeStr t1 ++ "<" ++ showTypeStr t2 ++ ">"
showTypeStr (TyConApp tc ts) = showTyConStr tc ++ (if null ts then "" else ("{" ++ concatMap showTypeStr ts) ++ "}")
showTypeStr (FunTy t1 t2) = showTypeStr t1 ++ " -> " ++ showTypeStr t2
showTypeStr (ForAllTy v t) = "forall " ++ showVarStr v ++ ". " ++ showTypeStr t
showTypeStr (LitTy tl) = "LitTy"


showTyCon :: TyCon -> SDoc
showTyCon tc = text "'" <> text (occNameString $ nameOccName $ tyConName tc) <> text "'"
--    <> text "{"
--    <> (whenT (isAlgTyCon tc) "Alg,")
--    <> (whenT (isClassTyCon tc) "Class,")
--    <> (whenT (isFamInstTyCon tc) "FamInst,")
--    <> (whenT (isFunTyCon tc) "Fun,")
--    <> (whenT (isPrimTyCon tc) "Prim,")
--    <> (whenT (isTupleTyCon tc) "Tuple,")
--    <> (whenT (isUnboxedTupleTyCon tc) "UnboxedTyple,")
--    <> (whenT (isBoxedTupleTyCon tc) "BoxedTyple,")
--    <> (whenT (isTypeSynonymTyCon tc) "TypeSynonym,")
--    <> (whenT (isDecomposableTyCon tc) "Decomposable,")
--    <> (whenT (isPromotedDataCon tc) "PromotedDataCon,")
--    <> (whenT (isPromotedTyCon tc) "Promoted")
--    <> text "}"
--    <> (vcat $ fmap showName $ fmap dataConName $ tyConDataCons tc)
--    <> (ppr $ fmap dataConWorkId $ tyConDataCons tc)

showTyConStr :: TyCon -> String
showTyConStr tc = "'" ++ (occNameString $ nameOccName $ tyConName tc) ++ "'"
--    ++ "{"
--    ++ (if isAlgTyCon tc then "Alg," else "")
--    ++ (if isClassTyCon tc then "Class," else "")
--    ++ (if isFamInstTyCon tc then "FamInst," else "")
--    ++ (if isFunTyCon tc then "Fun," else "")
--    ++ (if isPrimTyCon tc then "Prim," else "")
--    ++ (if isTupleTyCon tc then "Tuple," else "")
--    ++ (if isUnboxedTupleTyCon tc then "UnboxedTyple," else "")
--    ++ (if isBoxedTupleTyCon tc then "BoxedTyple," else "")
--    ++ (if isTypeSynonymTyCon tc then "TypeSynonym," else "")
--    ++ (if isDecomposableTyCon tc then "Decomposable," else "")
--    ++ (if isPromotedDataCon tc then "PromotedDataCon," else "")
--    ++ (if isPromotedTyCon tc then "Promoted" else "")
--    ++ "}"


showName :: Name -> SDoc
showName = ppr
--showName n = text "<"
--             <> ppr (nameOccName n)
--             <+> ppr (nameUnique n)
--             <+> text "("
--             <> ppr (nameModule_maybe n)
--             <> text ")"
--             <+> ppr (nameSrcLoc n)
--             <+> ppr (nameSrcSpan n)
--             <> text ">"

showVar :: Var -> SDoc
showVar = ppr
--showVar v = text "["
--            <> showName (varName v)
--            <+> ppr (varUnique v)
--            <+> showType (varType v)
--            <> (when (isId v) (idDetails v))
--            <> (when (isId v) (cafInfo $ idInfo v))
--            <> (when (isId v) (arityInfo $ idInfo v))
----            <> (when (isId v) (specInfo $ idInfo v))
--            <> (when (isId v) (unfoldingInfo $ idInfo v))
--            <> (when (isId v) (oneShotInfo $ idInfo v))
--            <> (when (isId v) (inlinePragInfo $ idInfo v))
--            <> (when (isId v) (occInfo $ idInfo v))
--            <> (when (isId v) (demandInfo $ idInfo v))
--            <> (when (isId v) (strictnessInfo $ idInfo v))
--            <> (when (isId v) (callArityInfo $ idInfo v))
--            <> (whenT (isId v) "Id")
--            <> (whenT (isTKVar v) "TKVar")
--            <> (whenT (isTyVar v) "TyVar")
--            <> (whenT (isTcTyVar v) "TcTyVar")
--            <> (whenT (isLocalVar v) "LocalVar")
--            <> (whenT (isLocalId v) "LocalId")
--            <> (whenT (isGlobalId v) "GlobalId")
--            <> (whenT (isExportedId v) "ExportedId")
--            <> text "]"

showVarStr :: Var -> String
showVarStr v =
             (occNameString $ nameOccName $ varName v)
--             ++ "[" ++ (show $ varUnique v) ++ "]"
--             ++ "{" ++ (showTypeStr $ varType v) ++ "}"

showExpr :: CoreExpr -> SDoc
showExpr (Var i) = text "<" <> showVar i <> text ">"
showExpr (Lit l) = text "Lit" <+> pprLiteral id l
showExpr (App e (Type t)) = showExpr e <+> text "@{" <+> showType t <> text "}"
showExpr (App e a) = text "(" <> showExpr e <> text " $ " <> showExpr a <> text ")"
showExpr (Lam b e) = text "(" <> showVar b <> text " -> " <> showExpr e <> text ")"
showExpr (Let b e) = text "Let" <+> ppr b <+> ppr e
showExpr (Case e b t as) = text "Case" <+> ppr e {-<+> ppr b <+> ppr t-} <+> vcat (showAlt <$> as)
showExpr (Cast e c) = text "Cast" <+> ppr e <+> ppr c
showExpr (Tick t e) = text "Tick" <+> ppr t <+> ppr e
showExpr (Type t) = text "Type" <+> ppr t
showExpr (Coercion c) = text "Coercion" <+> ppr c

showAlt (_, bs, e) = ppr bs <+> showExpr e

showExprStr :: CoreExpr -> String
showExprStr (Var i) = showVarStr i
showExprStr (Lit l) = "Lit"
showExprStr (App e (Type t)) = showExprStr e ++ "@{" ++ showTypeStr t ++ "}"
showExprStr (App e a) = "(" ++ showExprStr e ++  " $ " ++ showExprStr a ++ ")"
showExprStr (Lam b e) = "(" ++ showVarStr b ++ "-> " ++ showExprStr e ++ ")"
showExprStr (Let b e) = "Let"
showExprStr (Case e b t as) = "Case"
showExprStr (Cast e c) = "Cast"
showExprStr (Tick t e) = "Tick"
showExprStr (Type t) = "Type"
showExprStr (Coercion c) = "Coercion"

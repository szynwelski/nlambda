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
import Data.Foldable
import InstEnv
import Class
import MkId

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

showMap map showElem = putMsg $ doubleQuotes $ vcat (concatMap (\(x,y) -> [showElem x <+> text "->" <+> showElem y]) $ Map.toList map)

pass :: HomeModInfo -> ModGuts -> CoreM ModGuts
pass mod guts = do putMsg $ (text ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> start:") <+> (ppr $ mg_module guts)

                   -- classes
                   clsNameMap <- mkClassNameMap guts mod
--                   showMap clsNameMap showName
                   tcMap <- mkTyConMap mod clsNameMap (mg_tcs guts)
--                   showMap tcMap showTyCon

                   -- vars maps
                   varMap <- mkVarMap guts mod tcMap
                   let tcMap' = updateDefaultMethods varMap tcMap
--                   showMap varMap showVar

                   -- binds
                   binds <- newBinds mod varMap tcMap' (getDataCons guts) (mg_binds guts)

                   -- exports
                   let exps = newExports (mg_exports guts) (getNameMap guts varMap)

                   -- new guts
                   let guts' = guts {mg_tcs = mg_tcs guts ++ Map.elems tcMap', mg_binds = mg_binds guts ++ binds, mg_exports = mg_exports guts ++ exps}

                   -- show info
                   putMsg $ text "binds:\n" <+> (foldr (<+>) (text "") $ map showBind $ mg_binds guts' ++ getImplicitBinds guts')
                   putMsg $ text "classes:\n" <+> (vcat $ fmap showClass $ getClasses guts')

--                   modInfo "module" mg_module guts'
--                   modInfo "binds" mg_binds guts'
--                   modInfo "exports" mg_exports guts'
--                   modInfo "type constructors" mg_tcs guts'
--                   modInfo "used names" mg_used_names guts'
--                   modInfo "global rdr env" mg_rdr_env guts'
--                   modInfo "fixities" mg_fix_env guts'
--                   modInfo "class instances" mg_insts guts'
--                   modInfo "family instances" mg_fam_insts guts'
--                   modInfo "pattern synonyms" mg_patsyns guts'
--                   modInfo "core rules" mg_rules guts'
--                   modInfo "vect decls" mg_vect_decls guts'
--                   modInfo "vect info" mg_vect_info guts'
--                   modInfo "files" mg_dependent_files guts'
--                   modInfo "classes" getClasses guts'
--                   modInfo "implicit binds" getImplicitBinds guts'
                   putMsg $ (text ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> end:") <+> (ppr $ mg_module guts')
                   return guts'

----------------------------------------------------------------------------------------
-- Implicit Binds - copy from compiler/main/TidyPgm.hs
----------------------------------------------------------------------------------------

getImplicitBinds :: ModGuts -> [CoreBind]
getImplicitBinds guts = (concatMap getClassImplicitBinds $ getClasses guts) ++ (concatMap getTyConImplicitBinds $ mg_tcs guts)

getClassImplicitBinds :: Class -> [CoreBind]
getClassImplicitBinds cls
  = [ NonRec op (mkDictSelRhs cls val_index)
    | (op, val_index) <- classAllSelIds cls `zip` [0..] ]

getTyConImplicitBinds :: TyCon -> [CoreBind]
getTyConImplicitBinds tc = map get_defn (mapMaybe dataConWrapId_maybe (tyConDataCons tc))

get_defn :: Id -> CoreBind
get_defn id = NonRec id (unfoldingTemplate (realIdUnfolding id))


--mkDictSelRhs' :: Class
--             -> Int         -- 0-indexed selector among (superclasses ++ methods)
--             -> CoreExpr
--mkDictSelRhs' clas val_index
--  = mkLams tyvars (Lam dict_id rhs_body)
--  where
--    tycon          = pprTrace "tycon" (ppr $ classTyCon clas) classTyCon clas
--    new_tycon      = pprTrace "new_tycon" (ppr $ isNewTyCon tycon) isNewTyCon tycon
--    [data_con]     = pprTrace "[data_con]" (ppr $ tyConDataCons tycon) tyConDataCons tycon
--    tyvars         = pprTrace "tyvars" (ppr $ dataConUnivTyVars data_con) dataConUnivTyVars data_con
--    arg_tys        = pprTrace "arg_tys" (ppr $ dataConRepArgTys data_con) dataConRepArgTys data_con
--
--    the_arg_id     = pprTrace "the_arg_id" (ppr $ getNth arg_ids val_index) getNth arg_ids val_index
--    pred           = pprTrace "pred" (ppr $ mkClassPred clas (mkTyVarTys tyvars)) mkClassPred clas (mkTyVarTys tyvars)
--    dict_id        = pprTrace "dict_id" (ppr $ mkTemplateLocal 1 pred) mkTemplateLocal 1 pred
--    arg_ids        = mkTemplateLocalsNum 2 arg_tys
--
--    rhs_body | new_tycon = unwrapNewTypeBody tycon (map mkTyVarTy tyvars) (Var dict_id)
--             | otherwise = Case (Var dict_id) dict_id (idType the_arg_id)
--                                [(DataAlt data_con, arg_ids, varToCoreExpr the_arg_id)]
--                                -- varToCoreExpr needed for equality superclass selectors
--                                --   sel a b d = case x of { MkC _ (g:a~b) _ -> CO g }
--
--getNth :: Outputable a => [a] -> Int -> a
--getNth xs n = xs !! n

----------------------------------------------------------------------------------------
-- Data constructors
----------------------------------------------------------------------------------------

getDataCons :: ModGuts -> [DataCon]
getDataCons = concatMap tyConDataCons . filter (not . isClassTyCon) .filter isAlgTyCon . mg_tcs

----------------------------------------------------------------------------------------
-- Variables / names map
----------------------------------------------------------------------------------------

type VarMap = Map Var Var

newVar :: VarMap -> Var -> Var
newVar = (Map.!)

mkVarMap :: ModGuts -> HomeModInfo -> TyConMap -> CoreM VarMap
mkVarMap guts mod tcMap = do let classMap = mkClassVarMap tcMap
                             dataMap <- mkDataConVarMap guts mod tcMap
                             bindMap <- mkBindVarMap guts mod tcMap
                             return $ Map.unions [classMap, dataMap, bindMap]

mkMapWithVars :: ModGuts -> HomeModInfo -> TyConMap -> [Var] -> CoreM VarMap
mkMapWithVars guts mod tcMap vars = do newVars <- mapM (newBindVar mod tcMap) vars
                                       return $ Map.fromList $ zip vars newVars

mkClassVarMap :: TyConMap -> VarMap
mkClassVarMap tcMap = Map.fromList $ concatMap classVarMap $ Map.toList tcMap
    where classVarMap (tc1, tc2) = zip (classMethods $ tyConClass tc1) (classMethods $ tyConClass tc2)

mkDataConVarMap :: ModGuts -> HomeModInfo -> TyConMap -> CoreM VarMap
mkDataConVarMap guts mod tcMap = mkMapWithVars guts mod tcMap $ fmap dataConWorkId $ getDataCons guts

mkBindVarMap :: ModGuts -> HomeModInfo -> TyConMap -> CoreM VarMap
mkBindVarMap guts mod tcMap = mkMapWithVars guts mod tcMap vars
    where vars = concatMap toVars (mg_binds guts)
          toVars (NonRec v _) = [v]
          toVars (Rec bs) = fmap fst bs

newBindVar :: HomeModInfo -> TyConMap -> Var -> CoreM Var
newBindVar mod tcMap v = let var = mkLocalId (varName v) (newBindType mod tcMap v)
                         in changeVarName "nlambda_" "" (if isExportedId v then setIdExported var else setIdNotExported var)

changeVarName :: String -> String -> Var -> CoreM Var
changeVarName prefix suffix v = do name <- newName prefix suffix $ varName v
                                   return $ setVarName v name

newName :: String -> String -> Name -> CoreM Name
newName prefix suffix name = do let occName = nameOccName name
                                let newOccName = mkOccName (occNameSpace occName) (prefix ++ occNameString occName ++ suffix)
                                uniq <- getUniqueM
                                return $ setNameLoc (setNameUnique (tidyNameOcc name newOccName) uniq) noSrcSpan

type NameMap = Map Name Name

newVarName :: NameMap -> Name -> Name
newVarName = (Map.!)

getNameMap :: ModGuts -> VarMap -> NameMap
getNameMap guts varMap = Map.union varNameMap dataConNameMap
    where varNameMap = Map.mapKeys varName $ Map.map varName varMap
          dataConNameMap = Map.fromList $ fmap (\dc -> (dataConName dc, newVarName varNameMap (idName $ dataConWorkId dc))) (getDataCons guts)

----------------------------------------------------------------------------------------
-- Exports
----------------------------------------------------------------------------------------

newExports :: Avails -> NameMap -> Avails
newExports avls nameMap = concatMap go avls
    where go (Avail n) = [Avail $ getNewName n]
          go (AvailTC nm nms) = fmap (Avail . getNewName) (drop 1 nms)
          getNewName n | isJust $ Map.lookup n nameMap = newVarName nameMap n
          getNewName n = pprPanic "can't find export variable" (showName n)

----------------------------------------------------------------------------------------
-- Classes
----------------------------------------------------------------------------------------

getClasses :: ModGuts -> [Class]
getClasses = fmap fromJust . filter isJust . fmap tyConClass_maybe . mg_tcs

type ClassNameMap = Map Name Name

newClassName :: ClassNameMap -> Name -> Name
newClassName = (Map.!)

mkClassNameMap :: ModGuts -> HomeModInfo -> CoreM ClassNameMap
mkClassNameMap guts mod = do let cls = fmap className $ getClasses guts
                             newCls <- mapM (newName "NLambda" "") cls
                             return $ Map.fromList $ zip cls newCls

type TyConMap = Map TyCon TyCon

newTyCon :: TyConMap -> TyCon -> TyCon
newTyCon = (Map.!)

mkTyConMap :: HomeModInfo -> ClassNameMap -> [TyCon] -> CoreM TyConMap
mkTyConMap mod clsNameMap tcs =
    do let ctcs = filter isClassTyCon tcs
       ctcs' <- mapM (newTyConClass mod clsNameMap Map.empty) ctcs
       let tcMap = Map.fromList $ zip ctcs ctcs'
       ctcs'' <- mapM (newTyConClass mod clsNameMap tcMap) ctcs
       return $ Map.fromList $ zip ctcs ctcs''

tyConClass :: TyCon -> Class
tyConClass = fromJust . tyConClass_maybe

newTyConClass :: HomeModInfo -> ClassNameMap -> TyConMap-> TyCon -> CoreM TyCon
newTyConClass mod clsNameMap tcMap tc =
    do let name = newClassName clsNameMap $ tyConName tc
       cls' <- createClass mod tcMap $ updateTyConClass name (tyConClass tc) tc
       return $ updateTyConClass name cls' tc

updateTyConClass :: Name -> Class -> TyCon -> TyCon
updateTyConClass nm cls tc = mkClassTyCon
                               nm
                               (tyConKind tc)
                               (tyConTyVars tc)
                               (tyConRoles tc)
                               (algTyConRhs tc) -- TODO update data constructor!
                               cls
                               (if isRecursiveTyCon tc then Recursive else NonRecursive)

createClass :: HomeModInfo -> TyConMap -> TyCon -> CoreM Class
createClass mod tcMap tc =
    do scSels' <- mapM (newBindVar mod tcMap) scSels
       opStuff' <- mapM (\(v, dm) -> do {v' <- newBindVar mod tcMap v; return (v', dm)}) opStuff
       return $ mkClass tyVars funDeps scTheta scSels' ats opStuff' (classMinimalDef cls) tc
    where cls = tyConClass tc
          (tyVars, funDeps, scTheta, scSels, ats, opStuff) = classExtraBigSig cls

updateDefaultMethods :: VarMap -> TyConMap -> TyConMap
updateDefaultMethods varMap = Map.map updateTc
    where updateTc tc = updateTyConClass (tyConName tc) (updateCl $ tyConClass tc) tc
          updateCl cl = let (tyVars, funDeps, scTheta, scSels, ats, opStuff) = classExtraBigSig cl
                            opStuff' = fmap (\(v, dm) -> (v, updateDm dm)) opStuff
                        in mkClass tyVars funDeps scTheta scSels ats opStuff' (classMinimalDef cl) (classTyCon cl)
          varNameMap = Map.mapKeys varName $ Map.map varName varMap
          updateDm NoDefMeth = NoDefMeth
          updateDm (DefMeth nm) = DefMeth $ newVarName varNameMap nm
          updateDm (GenDefMeth nm) = GenDefMeth $ newVarName varNameMap nm

----------------------------------------------------------------------------------------
-- Binds
----------------------------------------------------------------------------------------

newBinds :: HomeModInfo -> VarMap -> TyConMap -> [DataCon] -> CoreProgram -> CoreM CoreProgram
newBinds mod varMap tcMap dcs bs = do bs' <- mapM (changeBind mod varMap tcMap) bs
                                      bs'' <- mapM (dataBind mod varMap) dcs
                                      return $ bs' ++ bs''

changeBind :: HomeModInfo -> VarMap -> TyConMap -> CoreBind -> CoreM CoreBind
changeBind mod varMap tcMap (NonRec b e) = do (b',e') <- changeBindExpr mod varMap tcMap (b, e)
                                              return (NonRec b' e')
changeBind mod varMap tcMap (Rec bs) = do bs' <- mapM (changeBindExpr mod varMap tcMap) bs
                                          return (Rec bs')

changeBindExpr :: HomeModInfo -> VarMap -> TyConMap -> (CoreBndr, CoreExpr) -> CoreM (CoreBndr, CoreExpr)
changeBindExpr mod varMap tcMap (b, e) = do newExpr <- changeExpr mod varMap tcMap e
                                            return (newVar varMap b, newExpr)

dataBind :: HomeModInfo -> VarMap -> DataCon -> CoreM CoreBind
dataBind mod varMap dc = do expr <- dataConExpr mod dc [] 0
                            return $ NonRec (newVar varMap $ dataConWorkId dc) expr

----------------------------------------------------------------------------------------
-- Type
----------------------------------------------------------------------------------------

newBindType :: HomeModInfo -> TyConMap -> CoreBndr -> Type
newBindType mod tcMap = changeType mod tcMap . varType

changeBindType :: HomeModInfo -> TyConMap -> CoreBndr -> CoreBndr
changeBindType mod tcMap x = setVarType x $ newBindType mod tcMap x

changeType :: HomeModInfo -> TyConMap -> Type -> Type
changeType mod tcMap t | isPredTy t = changePredType tcMap t -- probably without meta
changeType mod tcMap t | (Just (tv, t')) <- splitForAllTy_maybe t
                       = mkForAllTy tv (changeType mod tcMap t')
changeType mod tcMap t | (Just (t1, t2)) <- splitFunTy_maybe t, isPredTy t1
                       = mkFunTy (changePredType tcMap t1) (changeType mod tcMap t2)
changeType mod tcMap t | (Just (t1, t2)) <- splitFunTy_maybe t
                       = withMetaType mod $ mkFunTy (changeType mod tcMap t1) (changeType mod tcMap t2)
changeType mod tcMap t = withMetaType mod t -- FIXME other cases?, maybe use makeTyVarUnique?

changePredType :: TyConMap -> PredType -> PredType
changePredType tcMap t | (Just (tc, ts)) <- splitTyConApp_maybe t, isClassTyCon tc
                       = mkTyConApp (newTyCon tcMap tc) ts
changePredType _ t = t

----------------------------------------------------------------------------------------
-- Expr
----------------------------------------------------------------------------------------

getVarNameStr :: Var -> String
getVarNameStr = occNameString . nameOccName . varName

isInternalVar :: Var -> Bool
isInternalVar = isSuffixOf "#" . getVarNameStr -- FIXME use isPrimOpId ??

isValueVar :: Var -> Bool
isValueVar v = let (_,_,ty) = tcSplitSigmaTy (varType v) in not $ isFunTy ty

changeExpr :: HomeModInfo -> VarMap -> TyConMap -> CoreExpr -> CoreM CoreExpr
changeExpr mod varMap tcMap e = newExpr varMap e
    where -- newExpr varMap e | pprTrace "newExpr" (showExpr e) False = undefined
          newExpr varMap (Var v) | Map.member v varMap = return $ Var (newVar varMap v)
          newExpr varMap (Var v) | isValueVar v = emptyExpr mod (Var v)
          newExpr varMap (Lit l) = emptyExpr mod (Lit l)
          newExpr varMap a@(App (Var v) _) | isInternalVar v = emptyExpr mod a
          newExpr varMap (App f (Type t)) = do f' <- newExpr varMap f
                                               return $ App f' (Type t) -- without changing type
          newExpr varMap (App f x) = do f' <- newExpr varMap f
                                        f'' <- if isPredTy (exprType x)
                                               then return f'
                                               else valueExpr mod f'
                                        x' <- newExpr varMap x
                                        return $ mkCoreApp f'' x'
          newExpr varMap (Lam x e) | isTKVar x = do e' <- newExpr varMap e
                                                    return $ Lam x e'
          newExpr varMap (Lam x e) | isEvVar x = do let x' = changeBindType mod tcMap x
                                                    e' <- newExpr (Map.insert x x' varMap) e
                                                    return $ Lam x e'
          newExpr varMap (Lam x e) = do let x' = changeBindType mod tcMap x
                                        e' <- newExpr (Map.insert x x' varMap) e
                                        emptyExpr mod (Lam x' e')
          newExpr varMap (Let b e) = do (b', varMap') <- changeLetBind b varMap
                                        e' <- newExpr varMap' e
                                        return $ Let b' e'
          newExpr varMap (Case e b t as) = do e' <- newExpr varMap e
                                              e'' <- valueExpr mod e'
                                              m <- metaExpr mod e'
                                              as' <- mapM (changeAlternative varMap m) as
                                              return $ Case e'' b t as'
          newExpr varMap (Cast e c) = do e' <- newExpr varMap e
                                         return $ Cast e' (changeCoercion mod tcMap c)
          newExpr varMap (Tick t e) = do e' <- newExpr varMap e
                                         return $ Tick t e'
          newExpr varMap (Type t) = return $ Type t -- without changing type
          newExpr varMap (Coercion c) = return $ Coercion $ changeCoercion mod tcMap c
          newExpr varMap e = pprPanic "unknown variable: " (case e of
                                                              (Var v) -> showVar v
                                                              _       -> ppr e)
          changeLetBind (NonRec b e) varMap = do let b' = changeBindType mod tcMap b
                                                 let varMap' = Map.insert b b' varMap
                                                 e' <- newExpr varMap' e
                                                 return (NonRec b' e', varMap')
          changeLetBind (Rec bs) varMap = do (bs', varMap') <- changeRecBinds bs varMap
                                             return (Rec bs', varMap')
          changeRecBinds ((b, e):bs) varMap = do (bs', varMap') <- changeRecBinds bs varMap
                                                 let b' = changeBindType mod tcMap b
                                                 let varMap'' = Map.insert b b' varMap'
                                                 e' <- newExpr varMap'' e
                                                 return ((b',e'):bs', varMap'')
          changeRecBinds [] varMap = return ([], varMap)
          changeAlternative varMap m (DataAlt con, xs, e) = do let xs' = fmap (changeBindType mod tcMap) xs -- TODO comment
                                                               e' <- newExpr (Map.union varMap $ Map.fromList $ zip xs xs') e
                                                               xs'' <- mapM (\x -> createExpr mod (Var x) m) xs
                                                               let subst = extendSubstList emptySubst (zip xs' xs'')
                                                               let e'' = substExpr (ppr subst) subst e'
                                                               return (DataAlt con, xs, e'')
          changeAlternative varMap m (alt, [], e) = do {e' <- newExpr varMap e; return (alt, [], e')}

--mkCoreApp1 f x | pprTrace "mkCoreApp1" (ppr f <+> text "and" <+> ppr x) False = undefined
--mkCoreApp1 f x = mkCoreApp f x

changeCoercion :: HomeModInfo -> TyConMap -> Coercion -> Coercion
changeCoercion mod tcMap c = change c
    where change (Refl r t) = Refl r t
          change (TyConAppCo r tc cs) = TyConAppCo r (newTyCon tcMap tc) (change <$> cs)
          change (AppCo c1 c2) = AppCo (change c1) (change c2)
          change (ForAllCo tv c) = ForAllCo tv (change c)
          change (CoVarCo cv) = CoVarCo cv
          change (AxiomInstCo a i cs) = AxiomInstCo a i (change <$> cs)
          change (UnivCo n r t1 t2) = UnivCo n r (changeType mod tcMap t1) (changeType mod tcMap t2)
          change (SymCo c) = SymCo $ change c
          change (TransCo c1 c2) = TransCo (change c1) (change c2)
          change (AxiomRuleCo a ts cs) = AxiomRuleCo a (changeType mod tcMap <$> ts) (change <$> cs)
          change (NthCo i c) = NthCo i $ change c
          change (LRCo lr c) = LRCo lr $ change c
          change (InstCo c t) = InstCo (change c) (changeType mod tcMap t)
          change (SubCo c) = SubCo $ change c

dataConExpr :: HomeModInfo -> DataCon -> [Var] -> Int -> CoreM (CoreExpr)
--dataConExpr mod dc xs argNumber | pprTrace "dataConExpr" (ppr dc <+> ppr xs <+> ppr argNumber) False = undefined
dataConExpr mod dc xs argNumber =
    if argNumber == dataConSourceArity dc
    then do let revXs = reverse xs
            xs' <- mapM (changeVarName "" "'") revXs
            xValues <- mapM (valueExpr mod) (fmap Var $ xs')
            expr <- applyExprs (Var $ dataConWorkId dc) xValues
            mkLetUnionExpr (emptyMetaV mod) revXs xs' expr
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
-- Apply expression
----------------------------------------------------------------------------------------

splitType :: CoreExpr -> CoreM ([TyVar], [DictId], Type)
--splitType e | pprTrace "splitType" (ppr e <+> text "preds:" <+> ppr (let (tyVars, preds, ty') = tcSplitSigmaTy (exprType e) in preds)) False = undefined
splitType e =
    do let ty = exprType e
       let (tyVars, preds, ty') = tcSplitSigmaTy ty
       tyVars' <- mapM makeTyVarUnique tyVars
       let preds' = filter isClassPred preds
       let classTys = map getClassPredTys preds'
       predVars <- mapM mkPredVar classTys
       let subst = extendTvSubstList emptySubst (zip tyVars $ fmap TyVarTy tyVars')
       let ty'' = substTy subst ty'
       return (tyVars', [], ty'') -- FIXME predVars ???

applyExpr :: CoreExpr -> CoreExpr -> CoreM CoreExpr
--applyExpr fun e | pprTrace "applyExpr" (ppr fun <+> text "and" <+> ppr e) False = undefined
applyExpr fun e =
    do (tyVars, predVars, ty) <- splitType e
       let (funTyVars, _, funTy) = tcSplitSigmaTy $ exprType fun
       let subst = maybe (pprPanic "can't unify:" (ppr (funArgTy funTy) <+> text "and" <+> ppr ty <+> text "for apply:" <+> ppr fun <+> text "with" <+> ppr e))
                         id $ tcUnifyTy (funArgTy funTy) ty
       let funTyVarSubstExprs = fmap (Type . substTyVar subst) funTyVars
       let res = mkCoreLams tyVars $ mkCoreLams predVars $
                  mkCoreApp
                    (mkCoreApps fun $ funTyVarSubstExprs)
                    (mkCoreApps
                      (mkCoreApps e $ fmap Type $ mkTyVarTys tyVars)
                      (fmap Var predVars))
--       putMsg $ text "applyExpr" <+> ppr fun <+> text "and" <+> ppr e <+> text ">>>" <+> ppr res <+> text "::" <+> ppr (exprType res)
       return $ res

applyExprs :: CoreExpr -> [CoreExpr] -> CoreM CoreExpr
applyExprs = foldlM applyExpr

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

emptyExpr :: HomeModInfo -> CoreExpr -> CoreM CoreExpr
emptyExpr mod e = applyExpr (emptyV mod) e

valueExpr :: HomeModInfo -> CoreExpr -> CoreM CoreExpr
--valueExpr mod e | pprTrace "valueExpr" (ppr e) False = undefined
valueExpr mod e = applyExpr (valueV mod) e

metaExpr :: HomeModInfo -> CoreExpr -> CoreM CoreExpr
--metaExpr mod e | pprTrace "metaExpr" (ppr e) False = undefined
metaExpr mod e = applyExpr (metaV mod) e

unionExpr :: HomeModInfo -> CoreExpr -> CoreExpr -> CoreM CoreExpr
--unionExpr mod e1 e2 | pprTrace "unionExpr" (ppr e1 <+> ppr e2) False = undefined
unionExpr mod e1 e2 = do e <- applyExpr (unionV mod) e1
                         applyExpr e e2

createExpr :: HomeModInfo -> CoreExpr -> CoreExpr -> CoreM CoreExpr
--createExpr mod e1 e2 | pprTrace "createExpr" (ppr e1 <+> ppr e2) False = undefined
createExpr mod e1 e2 = do e <- applyExpr (createV mod) e1
                          applyExpr e e2


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
showBind b@(Rec _) = text "Rec [" {-<+> ppr b-} <+> text "]\n" -- TODO

showType :: Type -> SDoc
--showType = ppr
showType (TyVarTy v) = text "TyVarTy(" <> showVar v <> text ")"
showType (AppTy t1 t2) = text "AppTy(" <> showType t1 <+> showType t2 <> text ")"
showType (TyConApp tc ts) = text "TyConApp(" <> showTyCon tc <+> hsep (fmap showType ts) <> text ")"
showType (FunTy t1 t2) = text "FunTy(" <> showType t1 <+> showType t2 <> text ")"
showType (ForAllTy v t) = text "ForAllTy(" <> showVar v <+> showType t <> text ")"
showType (LitTy tl) = text "LitTy(" <> ppr tl <> text ")"

showTyCon :: TyCon -> SDoc
showTyCon tc = text "'" <> text (occNameString $ nameOccName $ tyConName tc) <> text "'"
--    <> text "{"
--    <> ppr (nameUnique $ tyConName tc)
--    <> (whenT (isAlgTyCon tc) "Alg,")
--    <> (whenT (isClassTyCon tc) "Class,")
--    <> (whenT (isFamInstTyCon tc) "FamInst,")
--    <> (whenT (isFunTyCon tc) "Fun, ")
--    <> (whenT (isPrimTyCon tc) "Prim, ")
--    <> (whenT (isTupleTyCon tc) "Tuple, ")
--    <> (whenT (isUnboxedTupleTyCon tc) "UnboxedTyple, ")
--    <> (whenT (isBoxedTupleTyCon tc) "BoxedTyple, ")
--    <> (whenT (isTypeSynonymTyCon tc) "TypeSynonym, ")
--    <> (whenT (isDecomposableTyCon tc) "Decomposable, ")
--    <> (whenT (isPromotedDataCon tc) "PromotedDataCon, ")
--    <> (whenT (isPromotedTyCon tc) "Promoted, ")
--    <> (text "dataConNames:" <+> (vcat $ fmap showName $ fmap dataConName $ tyConDataCons tc))
--    <> text "}"

showName :: Name -> SDoc
showName = ppr . nameOccName
--showName n = text "<"
--             <> ppr (nameOccName n)
--             <+> ppr (nameUnique n)
--             <+> text "("
--             <> ppr (nameModule_maybe n)
--             <> text ")"
--             <+> ppr (nameSrcLoc n)
--             <+> ppr (nameSrcSpan n)
--             <+> whenT (isInternalName n) "internal"
--             <+> whenT (isExternalName n) "external"
--             <+> whenT (isSystemName n) "system"
--             <+> whenT (isWiredInName n) "wired in"
--             <> text ">"

showOccName :: OccName -> SDoc
showOccName n = text "<"
                <> ppr n
                <+> pprNameSpace (occNameSpace n)
                <> whenT (isVarOcc n) " VarOcc"
                <> whenT (isTvOcc n) " TvOcc"
                <> whenT (isTcOcc n) " TcOcc"
                <> whenT (isDataOcc n) " DataOcc"
                <> whenT (isDataSymOcc n) " DataSymOcc"
                <> whenT (isSymOcc n) " SymOcc"
                <> whenT (isValOcc n) " ValOcc"
                <> text ">"

showVar :: Var -> SDoc
--showVar = ppr
showVar v = text "["
            <> showName (varName v)
--            <+> ppr (varUnique v)
--            <+> showType (varType v)
--            <+> showOccName (nameOccName $ varName v)
--            <> (when (isId v) (idDetails v))
--            <> (when (isId v) (cafInfo $ idInfo v))
--            <> (when (isId v) (arityInfo $ idInfo v))
--            <> (when (isId v) (unfoldingInfo $ idInfo v))
--            <> (when (isId v) (oneShotInfo $ idInfo v))
--            <> (when (isId v) (inlinePragInfo $ idInfo v))
--            <> (when (isId v) (occInfo $ idInfo v))
--            <> (when (isId v) (demandInfo $ idInfo v))
--            <> (when (isId v) (strictnessInfo $ idInfo v))
--            <> (when (isId v) (callArityInfo $ idInfo v))
--            <> (whenT (isId v) "Id")
--            <> (whenT (isDictId v) "DictId")
--            <> (whenT (isTKVar v) "TKVar")
--            <> (whenT (isTyVar v) "TyVar")
--            <> (whenT (isTcTyVar v) "TcTyVar")
--            <> (whenT (isLocalVar v) "LocalVar")
--            <> (whenT (isLocalId v) "LocalId")
--            <> (whenT (isGlobalId v) "GlobalId")
--            <> (whenT (isExportedId v) "ExportedId")
--            <> (whenT (isEvVar v) "EvVar")
--            <> (whenT (isId v && (isJust $ isClassOpId_maybe v)) "ClassOpId")
--            <> (whenT (isId v && isDFunId v) "DFunId")
--            <> (whenT (isId v && isPrimOpId v) "PrimOpId")
--            <> (whenT (isId v && isConLikeId v) "ConLikeId")
--            <> (whenT (isId v && isRecordSelector v) "RecordSelector")
--            <> (whenT (isId v && isFCallId v) "FCallId")
--            <> (whenT (isId v && hasNoBinding v) "NoBinding")
            <> text "]"

showExpr :: CoreExpr -> SDoc
showExpr (Var i) = text "<" <> showVar i <> text ">"
showExpr (Lit l) = text "Lit" <+> pprLiteral id l
showExpr (App e (Type t)) = showExpr e <+> text "@{" <+> showType t <> text "}"
showExpr (App e a) = text "(" <> showExpr e <> text " $ " <> showExpr a <> text ")"
showExpr (Lam b e) = text "(" <> showVar b <> text " -> " <> showExpr e <> text ")"
showExpr (Let b e) = text "Let" <+> showLetBind b <+> text "in" <+> showExpr e
showExpr (Case e b t as) = text "Case" <+> showExpr e <+> showVar b <+> text "::{" <+> showType t <> text "}" <+> hcat (showAlt <$> as)
showExpr (Cast e c) = text "Cast" <+> showExpr e <+> showCoercion c
showExpr (Tick t e) = text "Tick" <+> ppr t <+> showExpr e
showExpr (Type t) = text "Type" <+> showType t
showExpr (Coercion c) = text "Coercion" <+> text "`" <> showCoercion c <> text "`"

showLetBind (NonRec b e) = showVar b <+> text "=" <+> showExpr e
showLetBind (Rec bs) = hcat $ fmap (\(b,e) -> showVar b <+> text "=" <+> showExpr e) bs

showCoercion :: Coercion -> SDoc
showCoercion c = text "`" <> show c <> text "`"
    where show (Refl role typ) = text "Refl" <+> ppr role <+> showType typ
          show (TyConAppCo role tyCon cs) = text "TyConAppCo"
          show (AppCo c1 c2) = text "AppCo"
          show (ForAllCo tyVar c) = text "ForAllCo"
          show (CoVarCo coVar) = text "CoVarCo"
          show (AxiomInstCo coAxiom branchIndex cs) = text "AxiomInstCo" <+> ppr coAxiom <+> ppr branchIndex <+> vcat (fmap showCoercion cs)
          show (UnivCo fastString role type1 type2) = text "UnivCo"
          show (SymCo c) = text "SymCo" <+> show c
          show (TransCo c1 c2) = text "TransCo"
          show (AxiomRuleCo coAxiomRule types cs) = text "AxiomRuleCo"
          show (NthCo int c) = text "NthCo"
          show (LRCo leftOrRight c) = text "LRCo"
          show (InstCo c typ) = text "InstCo"
          show (SubCo c) = text "SubCo"

showAlt (con, bs, e) = text "|" <> ppr con <+> hcat (fmap showVar bs) <+> showExpr e <> text "|"

showClass :: Class -> SDoc
--showClass = ppr
showClass cls =
    let (tyVars, funDeps, scTheta, scSels, ats, opStuff) = classExtraBigSig cls
    in  text "Class{"
        <+> showName (className cls)
        <+> ppr (classKey cls)
        <+> ppr tyVars
        <+> brackets (fsep (punctuate comma (map (\(m,c) -> parens (sep [showVar m <> comma, ppr c])) opStuff)))
        <+> ppr (classMinimalDef cls)
        <+> ppr funDeps
        <+> ppr scTheta
        <+> ppr scSels
        <+> text "}"

showDataCon :: DataCon -> SDoc
showDataCon dc =
    text "DataCon{"
    <+> showName (dataConName dc)
--    <+> ppr (dataConFullSig dc)
--    <+> ppr (dataConFieldLabels dc)
--    <+> ppr (dataConTyCon dc)
--    <+> ppr (dataConTheta dc)
--    <+> ppr (dataConStupidTheta dc)
--    <+> ppr (dataConWorkId dc)
--    <+> ppr (dataConWrapId dc)
    <+> text "}"

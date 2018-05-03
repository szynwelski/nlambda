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
import Data.List (find, isInfixOf, isPrefixOf, isSuffixOf, intersperse, nub, partition)
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
import CoAxiom
import qualified BooleanFormula as BF

import Data.Map (Map)
import qualified Data.Map as Map
import Meta

import Debug.Trace (trace) --pprTrace

debug :: String -> SDoc -> a -> a
debug msg out x = go x
    where go x | pprTrace msg out False = undefined
          go x = x


plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  reinitializeGlobals
  env <- getHscEnv
  let metaPlug = CoreDoPluginPass "MetaPlugin" $ pass env False
  let showPlug = CoreDoPluginPass "ShowPlugin" $ pass env True
--  return $ showPlug:todo
  return $ metaPlug:todo
--  return $ metaPlug:todo ++ [showPlug]

modInfo :: Outputable a => String -> (ModGuts -> a) -> ModGuts -> CoreM ()
modInfo label fun guts = putMsg $ text label <> text ": " <> (ppr $ fun guts)

showMap :: String -> Map a a -> (a -> SDoc) -> CoreM ()
showMap header map showElem = putMsg $ text (header ++ ":") <+> (doubleQuotes $ vcat (concatMap (\(x,y) -> [showElem x <+> text "->" <+> showElem y]) $ Map.toList map))

pass :: HscEnv -> Bool -> ModGuts -> CoreM ModGuts
pass env onlyShow guts =
    if withMetaAnnotation guts
    then do putMsg $ text "Ignore module: " <+> (ppr $ mg_module guts)
            return guts
    else
         do putMsg $ text ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> start:"
                     <+> (ppr $ mg_module guts)
                     <+> if onlyShow then text "[only show]" else text ""

            let mod = getMetaModule env
            let (impNameMap, impVarMap, impTcMap) = getImportedMaps env guts

            -- names
            nameMap <- mkNamesMap guts impNameMap
            showMap "Names" nameMap showName

            -- classes and vars
            let modTcMap = mkTyConMap mod nameMap modVarMap (mg_tcs guts)
                modVarMap = mkVarMap guts mod nameMap modTcMap
                tcMap = Map.union modTcMap impTcMap
                varMap = Map.union modVarMap impVarMap
            showMap "TyCons" tcMap showTyCon
            showMap "Vars" varMap showVar

            guts' <- if onlyShow
                     then return guts
                     else do binds <- newBinds mod varMap tcMap (getDataCons guts) (mg_binds guts)
                             let exps = newExports (mg_exports guts) nameMap
                             return $ guts {mg_tcs = mg_tcs guts ++ Map.elems modTcMap, mg_binds = mg_binds guts ++ binds, mg_exports = mg_exports guts ++ exps}

            -- show info
--            putMsg $ text "binds:\n" <+> (foldr (<+>) (text "") $ map showBind $ mg_binds guts' ++ getImplicitBinds guts')
--            putMsg $ text "classes:\n" <+> (vcat $ fmap showClass $ getClasses guts')

--            modInfo "module" mg_module guts'
            modInfo "binds" mg_binds guts'
--            modInfo "dependencies" (dep_mods . mg_deps) guts'
--            modInfo "imported" getImportedModules guts'
--            modInfo "exports" mg_exports guts'
--            modInfo "type constructors" mg_tcs guts'
--            modInfo "used names" mg_used_names guts'
--            modInfo "global rdr env" mg_rdr_env guts'
--            modInfo "fixities" mg_fix_env guts'
--            modInfo "class instances" mg_insts guts'
--            modInfo "family instances" mg_fam_insts guts'
--            modInfo "pattern synonyms" mg_patsyns guts'
--            modInfo "core rules" mg_rules guts'
--            modInfo "vect decls" mg_vect_decls guts'
--            modInfo "vect info" mg_vect_info guts'
--            modInfo "files" mg_dependent_files guts'
--            modInfo "classes" getClasses guts'
--            modInfo "implicit binds" getImplicitBinds guts'
--            modInfo "annotations" mg_anns guts'
            putMsg $ text ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> end:" <+> (ppr $ mg_module guts')
            return guts'

----------------------------------------------------------------------------------------
-- Annotation
----------------------------------------------------------------------------------------

withMetaAnnotation :: ModGuts -> Bool
withMetaAnnotation guts = isJust $ find isMetaAnn $ mg_anns guts
    where isMetaAnn a = case fromSerialized deserializeWithData $ ann_value a of
                          Just "WithMeta" -> True
                          _ -> False

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

----------------------------------------------------------------------------------------
-- All names map
----------------------------------------------------------------------------------------

type NameMap = Map Name Name

getNameStr :: Name -> String
getNameStr = occNameString . nameOccName

newName :: NameMap -> Name -> Name
newName map name = Map.findWithDefault (pprPanic "unknown name: " (showName name <+> vcat (showName <$> Map.keys map))) name map

mkNamesMap :: ModGuts -> NameMap -> CoreM NameMap
mkNamesMap guts impNameMap = do nameMap <- mkSuffixNamesMap (getDataConsNames guts ++ getBindsNames guts ++ getClassesNames guts)
                                return $ Map.union nameMap impNameMap

name_suffix :: String
name_suffix = "_nlambda"

mkSuffixNamesMap :: [Name] -> CoreM NameMap
mkSuffixNamesMap names = do names' <- mapM (createNewName name_suffix) names
                            return $ Map.fromList $ zip names names'

createNewName :: String -> Name -> CoreM Name
createNewName suffix name = let occName = nameOccName name
                                newOccName = mkOccName (occNameSpace occName) (occNameString occName ++ suffix)
                            in newUniqueName $ tidyNameOcc name newOccName

newUniqueName :: Name -> CoreM Name
newUniqueName name = do uniq <- getUniqueM
                        return $ setNameLoc (setNameUnique name uniq) noSrcSpan

----------------------------------------------------------------------------------------
-- Data constructors
----------------------------------------------------------------------------------------

getDataCons :: ModGuts -> [DataCon]
getDataCons = concatMap tyConDataCons . filter (not . isClassTyCon) .filter isAlgTyCon . mg_tcs

getDataConsVars :: ModGuts -> [Var]
getDataConsVars = concatMap dataConImplicitIds . getDataCons

getDataConsNames :: ModGuts -> [Name]
getDataConsNames = concatMap (\dc -> dataConName dc : (idName <$> dataConImplicitIds dc)) . getDataCons

----------------------------------------------------------------------------------------
-- Variables map
----------------------------------------------------------------------------------------

type VarMap = Map Var Var

newVar :: VarMap -> Var -> Var
newVar map v = Map.findWithDefault (pprPanic "unknown variable: " (ppr v <+> ppr map)) v map

newVarDefault :: VarMap -> Var -> Var
newVarDefault map v = Map.findWithDefault v v map

mkVarMap :: ModGuts -> HomeModInfo -> NameMap -> TyConMap -> VarMap
mkVarMap guts mod nameMap tcMap = mkMapWithVars guts mod nameMap tcMap (getBindsVars guts ++ getClassesVars guts ++ getDataConsVars guts)

mkMapWithVars :: ModGuts -> HomeModInfo -> NameMap -> TyConMap -> [Var] -> VarMap
mkMapWithVars guts mod nameMap tcMap vars = Map.fromList $ zip vars $ fmap newVar vars
    where newVar v = let newIdInfo = setInlinePragInfo vanillaIdInfo (inlinePragInfo $ idInfo v)
                         v' = mkLocalIdWithInfo (newName nameMap $ varName v) (newBindType mod tcMap v) newIdInfo
                     in if isExportedId v then setIdExported v' else setIdNotExported v'

primVarName :: Var -> CoreM Var
primVarName v = do name <- createNewName "'" $ varName v
                   return $ setVarName v name

----------------------------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------------------------

getImportedModules :: ModGuts -> [Module]
getImportedModules = moduleEnvKeys . mg_dir_imps

getImportedMaps :: HscEnv -> ModGuts -> (NameMap, VarMap, TyConMap)
getImportedMaps env guts = (Map.fromList namePairs, Map.fromList varPairs, Map.fromList tcPairs)
    where mods = catMaybes $ lookupUFM (hsc_HPT env) <$> moduleName <$> getImportedModules guts
          types = mconcat $ md_types <$> hm_details <$> mods
          ids = eltsUFM types
          idNames = getName <$> ids
          findPair names name = (\n -> (name, n)) <$> find ((getNameStr name ++ name_suffix ==) . getNameStr) names
          namePairs = catMaybes $ findPair idNames <$> idNames
          findId = fromJust . lookupUFM types
          tyThingPairs = fmap (\(n1,n2) -> (findId n1, findId n2)) namePairs
          (tcThings, varThings) = partition (isTyThingTyCon . fst) tyThingPairs
          varPairs = fmap (\(tt1, tt2) -> (tyThingId tt1, tyThingId tt2)) varThings
          tcPairs = fmap (\(tt1, tt2) -> (tyThingTyCon tt1, tyThingTyCon tt2)) tcThings

----------------------------------------------------------------------------------------
-- Exports
----------------------------------------------------------------------------------------

newExports :: Avails -> NameMap -> Avails
newExports avls nameMap = concatMap go avls
    where go (Avail n) = [Avail $ newName nameMap n]
          go (AvailTC nm nms) | Map.member nm nameMap = [AvailTC (newName nameMap nm) (newName nameMap <$> nms)]
          go (AvailTC _ nms) = (Avail . newName nameMap) <$> (drop 1 nms)

----------------------------------------------------------------------------------------
-- Classes
----------------------------------------------------------------------------------------

getClasses :: ModGuts -> [Class]
getClasses = catMaybes . fmap tyConClass_maybe . mg_tcs

getClassDataCons :: Class -> [DataCon]
getClassDataCons = tyConDataCons . classTyCon

getClassesVars :: ModGuts -> [Var]
getClassesVars = concatMap (\c -> classAllSelIds c ++ (concatMap dataConImplicitIds $ getClassDataCons c)) . getClasses

getClassesNames :: ModGuts -> [Name]
getClassesNames = concatMap classNames . getClasses
    where classNames c = className c : (fmap idName $ classAllSelIds c) ++ dataConNames c
          dataConNames c = concatMap (\dc -> dataConName dc : (idName <$> dataConImplicitIds dc)) $ getClassDataCons c

type TyConMap = Map TyCon TyCon

newTyCon :: TyConMap -> TyCon -> TyCon
newTyCon tcMap tc = Map.findWithDefault tc tc tcMap -- FIXME - default only for some Prelude classes

mkTyConMap :: HomeModInfo -> NameMap -> VarMap -> [TyCon] -> TyConMap
mkTyConMap mod nameMap varMap tcs = let ctcs = filter isClassTyCon tcs
                                        ctcs' = fmap (newTyConClass mod nameMap varMap tcMap) ctcs
                                        tcMap = Map.fromList $ zip ctcs ctcs'
                                    in tcMap

newTyConClass :: HomeModInfo -> NameMap -> VarMap -> TyConMap-> TyCon -> TyCon
newTyConClass mod nameMap varMap tcMap tc = let tc' = createTyConClass nameMap cls rhs tc
                                                rhs = createAlgTyConRhs mod nameMap varMap tcMap (algTyConRhs tc)
                                                cls = createClass nameMap varMap tcMap tc' (fromJust $ tyConClass_maybe tc)
                                            in tc'

createTyConClass :: NameMap -> Class -> AlgTyConRhs -> TyCon -> TyCon
createTyConClass nameMap cls rhs tc = mkClassTyCon
                                       (newName nameMap $ tyConName tc)
                                       (tyConKind tc)
                                       (tyConTyVars tc) -- FIXME new unique ty vars?
                                       (tyConRoles tc)
                                       rhs
                                       cls
                                       (if isRecursiveTyCon tc then Recursive else NonRecursive)

createAlgTyConRhs :: HomeModInfo -> NameMap -> VarMap -> TyConMap -> AlgTyConRhs -> AlgTyConRhs
createAlgTyConRhs mod nameMap varMap tcMap rhs = create rhs
    where create (AbstractTyCon b) = AbstractTyCon b
          create DataFamilyTyCon = DataFamilyTyCon
          create (DataTyCon dcs isEnum) = DataTyCon (createDataCon mod nameMap varMap tcMap <$> dcs) isEnum
          create (NewTyCon dcs ntRhs ntEtadRhs ntCo) = NewTyCon dcs ntRhs ntEtadRhs ntCo -- TODO

createDataCon :: HomeModInfo -> NameMap -> VarMap -> TyConMap -> DataCon -> DataCon
createDataCon mod nameMap varMap tcMap dc =
    let name = newName nameMap $ dataConName dc
        workerName = newName nameMap $ idName $ dataConWorkId dc
        workerId = mkDataConWorkId workerName dc'
        (univ_tvs, ex_tvs, eq_spec, theta, arg_tys, res_ty) = dataConFullSig dc
        dc' = mkDataCon
                name
                (dataConIsInfix dc)
                []
                []
                univ_tvs -- FIXME new unique ty vars?
                ex_tvs -- FIXME new unique ty vars?
                ((\(tv, t) -> (tv, changeType mod tcMap t)) <$> eq_spec)
                (changePredType mod tcMap False <$> theta) -- False?
                (changeType mod tcMap <$> arg_tys)
                (changeType mod tcMap res_ty)
                (newTyCon tcMap $ dataConTyCon dc)
                (changePredType mod tcMap False <$> dataConStupidTheta dc) -- False?
                workerId
                NoDataConRep -- FIXME use mkDataConRep
    in dc'

createClass :: NameMap -> VarMap -> TyConMap -> TyCon -> Class -> Class
createClass nameMap varMap tcMap tc cls =
    let (tyVars, funDeps, scTheta, scSels, ats, opStuff) = classExtraBigSig cls
        scSels' = fmap (newVar varMap) scSels
        opStuff' = fmap (\(v, dm) -> (newVar varMap v, updateDefMeth dm)) opStuff
    in mkClass
         tyVars -- FIXME new unique ty vars?
         funDeps -- FIXME new unique ty vars?
         scTheta -- FIXME new predType?
         scSels'
         ats -- FIXME new associated types?
         opStuff'
         (updateMinDef $ classMinimalDef cls)
         tc
    where updateDefMeth NoDefMeth = NoDefMeth
          updateDefMeth (DefMeth n) = DefMeth $ newName nameMap n
          updateDefMeth (GenDefMeth n) = GenDefMeth $ newName nameMap n
          updateMinDef (BF.Var n) = BF.Var $ newName nameMap n
          updateMinDef (BF.And fs) = BF.And $ updateMinDef <$> fs
          updateMinDef (BF.Or fs) = BF.Or $ updateMinDef <$> fs

----------------------------------------------------------------------------------------
-- Binds
----------------------------------------------------------------------------------------

getBindVars :: CoreBind -> [Var]
getBindVars (NonRec v _) = [v]
getBindVars (Rec bs) = fmap fst bs

getBindsVars :: ModGuts -> [Var]
getBindsVars = concatMap getBindVars . mg_binds

getBindsNames :: ModGuts -> [Name]
getBindsNames = fmap varName . getBindsVars

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
dataBind mod varMap dc | noAtomsType $ dataConOrigResTy dc = return $ nonRecDataBind varMap dc $ Var $ dataConWrapId dc
dataBind mod varMap dc = do expr <- dataConExpr mod dc
                            return $ nonRecDataBind varMap dc expr

nonRecDataBind :: VarMap -> DataCon -> CoreExpr -> CoreBind
nonRecDataBind varMap dc = NonRec (newVar varMap $ dataConWrapId dc)

----------------------------------------------------------------------------------------
-- Type
----------------------------------------------------------------------------------------

newBindType :: HomeModInfo -> TyConMap -> CoreBndr -> Type
newBindType mod tcMap = changeType mod tcMap . varType

changeBindType :: HomeModInfo -> TyConMap -> CoreBndr -> CoreM CoreBndr
changeBindType mod tcMap x = do uniq <- getUniqueM
                                return $ setVarUnique (setVarType x $ newBindType mod tcMap x) uniq

changeType :: HomeModInfo -> TyConMap -> Type -> Type
changeType mod tcMap t = go mod tcMap (isDictTy t) t
    where go mod tcMap isDict t | noAtomsType t
                                = t
          go mod tcMap isDict t | isPredTy t
                                = changePredType mod tcMap isDict t
          go mod tcMap isDict t | (Just (tv, t')) <- splitForAllTy_maybe t
                                = mkForAllTy tv (go mod tcMap isDict t')
          go mod tcMap isDict t | (Just (t1, t2)) <- splitFunTy_maybe t
                                = mkFunTy (go mod tcMap isDict t1) (go mod tcMap isDict t2)
          go mod tcMap isDict t | isVoidTy t || isPredTy t || isPrimitiveType t
                                = t
          go mod tcMap isDict t = withMetaType mod t -- FIXME other cases?, maybe use makeTyVarUnique?

changePredType :: HomeModInfo -> TyConMap -> Bool -> PredType -> PredType
changePredType mod tcMap isDict t | (Just (tc, ts)) <- splitTyConApp_maybe t, isClassTyCon tc
                                  = mkTyConApp (newTyCon tcMap tc) (if isDict then changeType mod tcMap <$> ts else ts)
changePredType mod tcMap isDict t = t

getMainType :: Type -> Type
getMainType t = let (_,_,t') = tcSplitSigmaTy t in t'

isInternalType :: Type -> Bool
isInternalType t = let t' = getMainType t in isVoidTy t' || isPredTy t' || isPrimitiveType t' || isUnLiftedType t'

----------------------------------------------------------------------------------------
-- Checking type contains atoms
----------------------------------------------------------------------------------------

noAtomsType :: Type -> Bool
noAtomsType t = noAtomsTypeVars [] [] t

noAtomsTypeVars :: [TyCon] -> [TyVar] -> Type -> Bool
noAtomsTypeVars tcs vs t | Just t' <- coreView t = noAtomsTypeVars tcs vs t'
noAtomsTypeVars tcs vs (TyVarTy v) = elem v vs
noAtomsTypeVars tcs vs (AppTy t1 t2) = noAtomsTypeVars tcs vs t1 && noAtomsTypeVars tcs vs t2
noAtomsTypeVars tcs vs (TyConApp tc ts) = noAtomsTypeCon tcs tc (length ts) && (and $ fmap (noAtomsTypeVars tcs vs) ts)
noAtomsTypeVars tcs vs (FunTy t1 t2) = noAtomsTypeVars tcs vs t1 && noAtomsTypeVars tcs vs t2
noAtomsTypeVars tcs vs (ForAllTy _ _) = False
noAtomsTypeVars tcs vs (LitTy _ ) = True

noAtomsTypeCon :: [TyCon] -> TyCon -> Int -> Bool
noAtomsTypeCon tcs tc _ | elem tc tcs = True
noAtomsTypeCon _ tc _| isAtomsTypeName tc = False
noAtomsTypeCon _ tc _| isPrimTyCon tc = True
noAtomsTypeCon tcs tc n| isDataTyCon tc = and $ fmap (noAtomsTypeVars (nub $ tc : tcs) $ take n $ tyConTyVars tc) $ concatMap dataConOrigArgTys $ tyConDataCons tc
noAtomsTypeCon _ _ _ = True

isAtomsTypeName :: TyCon -> Bool
isAtomsTypeName tc = let nm = occNameString $ nameOccName $ tyConName tc in elem nm ["Atom", "Formula"] -- FIXME check namespace

----------------------------------------------------------------------------------------
-- Expr
----------------------------------------------------------------------------------------

getVarNameStr :: Var -> String
getVarNameStr = getNameStr . varName

-- FIXME use isPrimOpId or check type ??
isInternalVar :: Var -> Bool
isInternalVar v = let n = getVarNameStr v in isSuffixOf "#" n

changeExpr :: HomeModInfo -> VarMap -> TyConMap -> CoreExpr -> CoreM CoreExpr
changeExpr mod varMap tcMap e = newExpr varMap e
    where newExpr varMap e | noAtomsSubExpr e = return $ replaceVars varMap e
          newExpr varMap (Var v) | Map.member v varMap = return $ Var (newVar varMap v)
          newExpr varMap (Var v) | isMetaEquivalent v = getMetaEquivalent mod v
          newExpr varMap (Var v) = pprPanic "unknown variable" (showVar v <+> text "::" <+> ppr (varType v))
          newExpr varMap (Lit l) = noMetaExpr mod (Lit l)
          newExpr varMap a@(App (Var v) _) | isInternalVar v = noMetaExpr mod a
          newExpr varMap (App f (Type t)) = do f' <- newExpr varMap f
                                               return $ App f' $ Type (if noAtomsType t then t else changeType mod tcMap t)
          newExpr varMap (App f e) = do f' <- newExpr varMap f
                                        f'' <- if isWithMetaType mod $ exprType f'
                                               then valueExpr mod f'
                                               else return f'
                                        e' <- newExpr varMap e
                                        e'' <- if (isWithMetaType mod $ funArgTy $ exprType f'') && (not $ isWithMetaType mod $ exprType e')
                                               then noMetaExpr mod e' -- FIXME always correct?
                                               else return e'
                                        return $ mkCoreApp f'' e''
          newExpr varMap (Lam x e) | isTKVar x = do e' <- newExpr varMap e
                                                    return $ Lam x e'
          newExpr varMap (Lam x e) = do x' <- changeBindType mod tcMap x
                                        e' <- newExpr (Map.insert x x' varMap) e
                                        return $ Lam x' e'
          newExpr varMap (Let b e) = do (b', varMap') <- changeLetBind b varMap
                                        e' <- newExpr varMap' e
                                        return $ Let b' e'
          newExpr varMap (Case e b t as) = do e' <- newExpr varMap e
                                              e'' <- valueExpr mod e' -- FIXME always? even for primitive types?
                                              m <- metaExpr mod e'
                                              as' <- mapM (changeAlternative varMap m) as
                                              return $ Case e'' b t as'
          newExpr varMap (Cast e c) = do e' <- newExpr varMap e
                                         return $ Cast e' (changeCoercion mod tcMap c)
          newExpr varMap (Tick t e) = do e' <- newExpr varMap e
                                         return $ Tick t e'
          newExpr varMap (Type t) = undefined -- without changing type?
          newExpr varMap (Coercion c) = return $ Coercion $ changeCoercion mod tcMap c
          changeLetBind (NonRec b e) varMap = do b' <- changeBindType mod tcMap b
                                                 let varMap' = Map.insert b b' varMap
                                                 e' <- newExpr varMap' e
                                                 return (NonRec b' e', varMap')
          changeLetBind (Rec bs) varMap = do (bs', varMap') <- changeRecBinds bs varMap
                                             return (Rec bs', varMap')
          changeRecBinds ((b, e):bs) varMap = do (bs', varMap') <- changeRecBinds bs varMap
                                                 b' <- changeBindType mod tcMap b
                                                 let varMap'' = Map.insert b b' varMap'
                                                 e' <- newExpr varMap'' e
                                                 return ((b',e'):bs', varMap'')
          changeRecBinds [] varMap = return ([], varMap)
          changeAlternative varMap m (DataAlt con, xs, e) = do xs' <- mapM (changeBindType mod tcMap) xs
                                                               xs'' <- mapM (\x -> createExpr mod (Var x) m) xs'
                                                               e' <- newExpr (Map.union varMap $ Map.fromList $ zip xs xs') e
                                                               let subst = extendSubstList emptySubst (zip xs' xs'')
                                                               let e'' = substExpr (ppr subst) subst e' -- replace vars with expressions
                                                               return (DataAlt con, xs', e'')
          changeAlternative varMap m (alt, [], e) = do {e' <- newExpr varMap e; return (alt, [], e')}

noAtomsSubExpr :: CoreExpr -> Bool
noAtomsSubExpr e | not $ noAtomsType $ exprType e = False
noAtomsSubExpr (App f e) = noAtomsSubExpr f && noAtomsSubExpr e
noAtomsSubExpr (Lam x e) = noAtomsSubExpr e
noAtomsSubExpr (Let b e) = noAtomsBind b && noAtomsSubExpr e
    where noAtomsBind (NonRec x e) = noAtomsSubExpr e
          noAtomsBind (Rec bs) = all (noAtomsSubExpr . snd) bs
noAtomsSubExpr (Case e b t as) = noAtomsSubExpr e && all noAtomsAlt as
    where noAtomsAlt (alt, xs, e) = noAtomsSubExpr e
noAtomsSubExpr (Cast e c) = noAtomsSubExpr e
noAtomsSubExpr e = True

replaceVars :: VarMap -> CoreExpr -> CoreExpr
replaceVars varMap (Var x) = Var (newVarDefault varMap x)
replaceVars varMap (Lit l) = Lit l
replaceVars varMap (App f e) = App (replaceVars varMap f) (replaceVars varMap e)
replaceVars varMap (Lam x e) = Lam (newVarDefault varMap x) (replaceVars varMap e)
replaceVars varMap (Let b e) = Let (replaceVarsInBind b) (replaceVars varMap e)
    where replaceVarsInBind (NonRec x e) = NonRec (newVarDefault varMap x) (replaceVars varMap e)
          replaceVarsInBind (Rec bs) = Rec $ fmap (\(x, e) -> (newVarDefault varMap x, replaceVars varMap e)) bs
replaceVars varMap (Case e x t as) = Case (replaceVars varMap e) (newVarDefault varMap x) t (replaceVarsInAlt <$> as)
    where replaceVarsInAlt (con, bs, e) = (con, fmap (newVarDefault varMap) bs, replaceVars varMap e)
replaceVars varMap (Cast e c) = Cast (replaceVars varMap e) c
replaceVars varMap (Tick t e) = Tick t (replaceVars varMap e)
replaceVars varMap e = e

changeCoercion :: HomeModInfo -> TyConMap -> Coercion -> Coercion
changeCoercion mod tcMap c = change c
    where change (Refl r t) = Refl r t -- FIXME not changeType ?
          change (TyConAppCo r tc cs) = TyConAppCo r (newTyCon tcMap tc) (change <$> cs)
          change (AppCo c1 c2) = AppCo (change c1) (change c2)
          change (ForAllCo tv c) = ForAllCo tv (change c)
          change (CoVarCo cv) = CoVarCo cv
          change (AxiomInstCo a i cs) = AxiomInstCo (changeCoAxiom tcMap a) i (change <$> cs)
          change (UnivCo n r t1 t2) = UnivCo n r (changeType mod tcMap t1) (changeType mod tcMap t2)
          change (SymCo c) = SymCo $ change c
          change (TransCo c1 c2) = TransCo (change c1) (change c2)
          change (AxiomRuleCo a ts cs) = AxiomRuleCo a (changeType mod tcMap <$> ts) (change <$> cs)
          change (NthCo i c) = NthCo i $ change c
          change (LRCo lr c) = LRCo lr $ change c
          change (InstCo c t) = InstCo (change c) (changeType mod tcMap t)
          change (SubCo c) = SubCo $ change c

changeCoAxiom :: TyConMap -> CoAxiom a -> CoAxiom a
changeCoAxiom tcMap (CoAxiom u n r tc bs i) = CoAxiom u n r (newTyCon tcMap tc) bs i

dataConExpr :: HomeModInfo -> DataCon -> CoreM CoreExpr
dataConExpr mod dc | dataConSourceArity dc == 0 = noMetaExpr mod (Var $ dataConWrapId dc)
dataConExpr mod dc | dataConSourceArity dc == 1 = idOpExpr mod (Var $ dataConWrapId dc)
dataConExpr mod dc = do xs <- mkArgs $ dataConSourceArity dc
                        ms <- mkMetaList xs
                        ux <- mkUnionVar
                        ue <- unionExpr mod ms
                        rs <- renameValues (Var ux) xs
                        m <- getMetaExpr mod (Var ux)
                        e <- applyExprs (Var $ dataConWrapId dc) rs
                        me <- createExpr mod e m
                        return $ mkLam xs $ bindNonRec ux ue me
    where mkArgs 0 = return []
          mkArgs n = do uniq <- getUniqueM
                        let xnm = mkInternalName uniq (mkVarOcc $ "x" ++ show n) noSrcSpan
                        let ty = withMetaType mod $ dataConOrigArgTys dc !! (dataConSourceArity dc - n)
                        let x = mkLocalId xnm ty
                        args <- mkArgs $ pred n
                        return $ x : args
          mkMetaList [] = return $ emptyListV mod
          mkMetaList (x:xs) = do meta <- metaExpr mod $ Var x
                                 list <- mkMetaList xs
                                 colonExpr mod meta list
          mkUnionVar = do uniq <- getUniqueM
                          let nm = mkInternalName uniq (mkVarOcc "u") noSrcSpan
                          return $ mkLocalId nm $ unionType mod
          renameValues u [] = return []
          renameValues u (x:xs) = do df <- getDynFlags
                                     let n = mkConApp intDataCon [Lit $ mkMachInt df $ toInteger (dataConSourceArity dc - length xs - 1)]
                                     v <- valueExpr mod $ Var x
                                     r <- renameExpr mod u n v
                                     rs <- renameValues u xs
                                     return (r : rs)
          mkLam [] e = e
          mkLam (x:xs) e = Lam x $ mkLam xs e



----------------------------------------------------------------------------------------
-- Apply expression
----------------------------------------------------------------------------------------

data ExprVar = TV TyVar | DI DictId

exprVarToVar :: [ExprVar] -> [CoreBndr]
exprVarToVar = fmap toCoreBndr
    where toCoreBndr (TV v) = v
          toCoreBndr (DI i) = i

exprVarToExpr :: [ExprVar] -> [CoreExpr]
exprVarToExpr = fmap toExpr
    where toExpr (TV v) = Type $ mkTyVarTy v
          toExpr (DI i) = Var i

splitTypeTyVars :: Type -> CoreM ([ExprVar], Type)
splitTypeTyVars ty =
    let (tyVars, preds, ty') = tcSplitSigmaTy ty
    in if null tyVars && null preds
       then return ([], ty')
       else do tyVars' <- mapM makeTyVarUnique tyVars
               let preds' = filter isClassPred preds -- TODO other preds
               let classTys = fmap getClassPredTys preds'
               predVars <- mapM mkPredVar classTys
               let subst = extendTvSubstList emptySubst (zip tyVars $ fmap TyVarTy tyVars')
               (resTyVars, resTy) <- splitTypeTyVars $ substTy subst ty'
               return ((TV <$> tyVars') ++ (DI <$> predVars) ++ resTyVars, resTy)

applyExpr :: CoreExpr -> CoreExpr -> CoreM CoreExpr
applyExpr fun e =
    do (eTyVars, ty) <- splitTypeTyVars $ exprType e
       let (funTyVars, _, funTy) = tcSplitSigmaTy $ exprType fun
       let subst = maybe (pprPanic "can't unify:" (ppr (funArgTy funTy) <+> text "and" <+> ppr ty <+> text "for apply:" <+> ppr fun <+> text "with" <+> ppr e))
                         id $ tcUnifyTy (funArgTy funTy) ty
       let funTyVarSubstExprs = fmap (Type . substTyVar subst) funTyVars
       return $ mkCoreLams (exprVarToVar eTyVars)
                  (mkCoreApp
                    (mkCoreApps fun funTyVarSubstExprs)
                    (mkCoreApps e $ exprVarToExpr eTyVars))

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

getMetaVar :: HomeModInfo -> String -> CoreExpr
getMetaVar mod = Var . getVar mod

noMetaV mod = getMetaVar mod "noMeta"
unionV mod = getMetaVar mod "union"
metaV mod = getMetaVar mod "meta"
valueV mod = getMetaVar mod "value"
createV mod = getMetaVar mod "create"
getMetaV mod = getMetaVar mod "getMeta"
renameV mod = getMetaVar mod "rename"
emptyListV mod = getMetaVar mod "emptyList"
colonV mod = getMetaVar mod "colon"
idOpV mod = getMetaVar mod "idOp"
withMetaC mod = getTyCon mod "WithMeta"
unionC mod = getTyCon mod "Union"

withMetaType :: HomeModInfo -> Type -> Type
withMetaType mod ty = mkTyConApp (withMetaC mod) [ty]

isWithMetaType :: HomeModInfo -> Type -> Bool
isWithMetaType mod t = let (_, _, ty) = tcSplitSigmaTy t in go ty
    where go ty | Just (tc, _) <- splitTyConApp_maybe ty = tc == withMetaC mod
                | otherwise = False

unionType :: HomeModInfo -> Type
unionType = mkTyConTy . unionC

mkPredVar :: (Class, [Type]) -> CoreM DictId
mkPredVar (cls, tys) = do uniq <- getUniqueM
                          let name = mkSystemName uniq (mkDictOcc (getOccName cls))
                          return (mkLocalId name (mkClassPred cls tys))

makeTyVarUnique :: TyVar -> CoreM TyVar
makeTyVarUnique v = do uniq <- getUniqueM
                       return $ mkTyVar (setNameUnique (tyVarName v) uniq) (tyVarKind v)

noMetaExpr :: HomeModInfo -> CoreExpr -> CoreM CoreExpr
noMetaExpr mod e | isInternalType $ exprType e = return e
noMetaExpr mod e = applyExpr (noMetaV mod) e

valueExpr :: HomeModInfo -> CoreExpr -> CoreM CoreExpr
valueExpr mod e | not $ isWithMetaType mod $ exprType e = return e
valueExpr mod e = applyExpr (valueV mod) e

metaExpr :: HomeModInfo -> CoreExpr -> CoreM CoreExpr
metaExpr mod e = applyExpr (metaV mod) e

unionExpr :: HomeModInfo -> CoreExpr -> CoreM CoreExpr
unionExpr mod e = applyExpr (unionV mod) e

createExpr :: HomeModInfo -> CoreExpr -> CoreExpr -> CoreM CoreExpr
createExpr mod e  _  | isInternalType $ exprType e = return e
createExpr mod e1 e2 = applyExprs (createV mod) [e1, e2]

getMetaExpr :: HomeModInfo -> CoreExpr -> CoreM CoreExpr
getMetaExpr mod e = applyExpr (getMetaV mod) e

renameExpr :: HomeModInfo -> CoreExpr -> CoreExpr -> CoreExpr -> CoreM CoreExpr
renameExpr mod e1 e2 e3 = applyExprs (renameV mod) [e1, e2, e3]

colonExpr :: HomeModInfo -> CoreExpr -> CoreExpr -> CoreM CoreExpr
colonExpr mod e1 e2 = applyExprs (colonV mod) [e1, e2]

idOpExpr :: HomeModInfo -> CoreExpr -> CoreM CoreExpr
idOpExpr mod e = applyExpr (idOpV mod) e

----------------------------------------------------------------------------------------
-- Meta Equivalents
----------------------------------------------------------------------------------------

isMetaEquivalent :: Var -> Bool
--isMetaEquivalent v | pprTrace "isMetaEquivalent" (ppr v <+> text "::" <+> ppr (exprType $ Var v) <+> text "from" <+> ppr (nameModule_maybe $ varName v)) False = undefined
isMetaEquivalent v = (maybe False isMetaModule $ nameModule_maybe $ varName v) && (hasMetaEquivalent $ metaEquivalent $ getVarNameStr v)
    where isMetaModule = (`elem` metaEquivalentModules) . moduleNameString . moduleName
          hasMetaEquivalent NoEquivalent = False
          hasMetaEquivalent _ = True

getMetaEquivalent :: HomeModInfo -> Var -> CoreM CoreExpr
getMetaEquivalent mod v = case metaEquivalent (getVarNameStr v) of
                            OrigFun -> return $ Var v
                            MetaFun name -> return $ getMetaVar mod name
                            MetaConvertFun name -> applyExpr (getMetaVar mod name) (Var v)
                            NoEquivalent -> pprPanic "no meta equivalent for:" (showVar v)

----------------------------------------------------------------------------------------
-- Show
----------------------------------------------------------------------------------------

when c v = if c then text " " <> ppr v else text ""
whenT c v = if c then text " " <> text v else text ""

showBind :: CoreBind -> SDoc
showBind (NonRec b e) = showBindExpr (b, e)
showBind (Rec bs) = hcat $ map showBindExpr bs

showBindExpr :: (CoreBndr, CoreExpr) -> SDoc
showBindExpr (b,e) = text "===> "
                        <+> showVar b
                        <+> text "::"
                        <+> showType (varType b)
                        <+> (if noAtomsType $ varType b then text "[no atoms]" else text "[atoms]")
                        <> text "\n"
                        <+> showExpr e
                        <> text "\n"

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
showVar = ppr
--showVar v = text "["
--            <> showName (varName v)
--            <+> ppr (varUnique v)
--            <+> showType (varType v)
----            <+> showOccName (nameOccName $ varName v)
--            <> (when (isId v) (idDetails v))
--            <> (when (isId v) (arityInfo $ idInfo v))
----            <> (when (isId v) (specInfo $ idInfo v))
--            <> (when (isId v) (unfoldingInfo $ idInfo v))
--            <> (when (isId v) (cafInfo $ idInfo v))
--            <> (when (isId v) (oneShotInfo $ idInfo v))
--            <> (when (isId v) (inlinePragInfo $ idInfo v))
--            <> (when (isId v) (occInfo $ idInfo v))
--            <> (when (isId v) (strictnessInfo $ idInfo v))
--            <> (when (isId v) (demandInfo $ idInfo v))
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
--            <> (whenT (isId v && isDataConWorkId v) "DataConWorkId")
--            <> (whenT (isId v && isRecordSelector v) "RecordSelector")
--            <> (whenT (isId v && (isJust $ isClassOpId_maybe v)) "ClassOpId")
--            <> (whenT (isId v && isDFunId v) "DFunId")
--            <> (whenT (isId v && isPrimOpId v) "PrimOpId")
--            <> (whenT (isId v && isConLikeId v) "ConLikeId")
--            <> (whenT (isId v && isRecordSelector v) "RecordSelector")
--            <> (whenT (isId v && isFCallId v) "FCallId")
--            <> (whenT (isId v && hasNoBinding v) "NoBinding")
--            <> text "]"

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
          show (SymCo c) = text "SymCo" <+> showCoercion c
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
    <+> ppr (dataConFullSig dc)
    <+> ppr (dataConFieldLabels dc)
    <+> ppr (dataConTyCon dc)
    <+> ppr (dataConTheta dc)
    <+> ppr (dataConStupidTheta dc)
    <+> ppr (dataConWorkId dc)
    <+> ppr (dataConWrapId dc)
    <+> text "}"

module MetaPlugin where
import GhcPlugins
import PprCore
import Data.IORef
import System.IO.Unsafe
import Unique
import Avail
import Serialized
import Annotations
import GHC hiding (exprType, typeKind)
import Control.Exception (assert)
import Control.Monad (liftM, unless)
import Data.Char (isLetter)
import Data.Data (Data)
import Data.List (find, findIndex, isInfixOf, isPrefixOf, isSuffixOf, intersperse, nub, partition)
import Data.Maybe (fromJust)
import Data.String.Utils (replace)
import TypeRep
import Maybes
import TcType (tcSplitSigmaTy, tcSplitPhiTy)
import TyCon
import Unify
import CoreSubst
import Data.Foldable
import InstEnv
import Class
import MkId
import CoAxiom
import Kind
import qualified BooleanFormula as BF

import Data.Map (Map)
import qualified Data.Map as Map
import Meta

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

            -- list all meta ids with types
--            putMsg $ vcat $ fmap (\x -> showVar x <+> text "::" <+> showType (varType x)) $ fmap tyThingId $ filter isTyThingId $ eltsUFM $ md_types $ hm_details $ mod

            -- names
            let metaNameMap = getMetaPreludeNameMaps mod env guts
            nameMap <- mkNamesMap guts $ Map.union impNameMap metaNameMap
--            showMap "Names" nameMap showName

            -- classes and vars
            let modTcMap = mkTyConMap mod nameMap modVarMap (mg_tcs guts)
                modVarMap = mkVarMap guts mod nameMap modTcMap
                tcMap = Map.union modTcMap impTcMap
                varMap = unionVarMaps modVarMap impVarMap
            showMap "TyCons" tcMap showTyCon
            showMap "Vars" (fst varMap) showVar

            guts' <- if onlyShow
                     then return guts
                     else do binds <- newBinds mod varMap tcMap (getDataCons guts) (mg_binds guts)
                             let exps = newExports (mg_exports guts) nameMap
                             return $ guts {mg_tcs = mg_tcs guts ++ Map.elems modTcMap, mg_binds = mg_binds guts ++ binds, mg_exports = mg_exports guts ++ exps}

            -- show info
--            putMsg $ text "binds:\n" <+> (foldr (<+>) (text "") $ map showBind $ mg_binds guts' ++ getImplicitBinds guts')
--            putMsg $ text "classes:\n" <+> (vcat $ fmap showClass $ getClasses guts')

--            modInfo "test binds" (filter (isPrefixOf "test" . getNonRecName) . bindsToList . mg_binds) guts'
--            modInfo "module" mg_module guts'
            modInfo "binds" (bindsToList . mg_binds) guts'
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

nameSuffix :: String -> String
nameSuffix name = if any isLetter name then name_suffix else op_suffix

nlambdaName :: String -> String
nlambdaName name = name ++ nameSuffix name

mkSuffixNamesMap :: [Name] -> CoreM NameMap
mkSuffixNamesMap names = do names' <- mapM (createNewName nameSuffix) names
                            return $ Map.fromList $ zip names names'

createNewName :: (String -> String) -> Name -> CoreM Name
createNewName suffix name = let occName = nameOccName name
                                nameStr = occNameString occName
                                newOccName = mkOccName (occNameSpace occName) (nameStr ++ suffix nameStr)
                            in newUniqueName $ tidyNameOcc name newOccName

newUniqueName :: Name -> CoreM Name
newUniqueName name = do uniq <- getUniqueM
                        return $ setNameLoc (setNameUnique name uniq) noSrcSpan

-- TODO check package is the same
findNamePair :: [Name] -> Name -> Maybe (Name, Name)
findNamePair names name = (\n -> (name, n)) <$> find sameName names
    where nameStr = getNameStr name
          sameName nameWithSuffix = getNameStr nameWithSuffix == nlambdaName nameStr
                                    || replace name_suffix "" (getNameStr nameWithSuffix) == nameStr

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

type VarMap = (Map Var Var, [Var])

unionVarMaps :: VarMap -> VarMap -> VarMap
unionVarMaps (m1,l1) (m2,l2) = (Map.union m1 m2, l1 ++ l2)

newVar :: VarMap -> Var -> Var
newVar (map,_) v = Map.findWithDefault (pprPanic "unknown variable: " (ppr v <+> ppr map)) v map

varMapMember :: Var -> VarMap -> Bool
varMapMember v = Map.member v . fst

mkVarMap :: ModGuts -> MetaModule -> NameMap -> TyConMap -> VarMap
mkVarMap guts mod nameMap tcMap = mkMapWithVars mod nameMap tcMap (getBindsVars guts ++ getClassesVars guts ++ getDataConsVars guts)

mkMapWithVars :: MetaModule -> NameMap -> TyConMap -> [Var] -> VarMap
mkMapWithVars mod nameMap tcMap vars = (Map.fromList $ zip vars' $ fmap newVar vars', vars'')
    where (vars',vars'') = partition (not . isIgnoreImportType . varType) vars
          newVar v = let newIdInfo = setInlinePragInfo vanillaIdInfo (inlinePragInfo $ idInfo v)
                         v' = mkLocalIdWithInfo (newName nameMap $ varName v) (newBindType mod tcMap v) newIdInfo
                     in if isExportedId v then setIdExported v' else setIdNotExported v'

primVarName :: Var -> CoreM Var
primVarName v = do name <- createNewName (const "'") (varName v)
                   return $ setVarName v name

getVarNameStr :: Var -> String
getVarNameStr = getNameStr . varName

getModuleNameStr :: Var -> String
getModuleNameStr = maybe "" (moduleNameString . moduleName) . nameModule_maybe . varName

mkVarUnique :: Var -> CoreM Var
mkVarUnique v = do uniq <- getUniqueM
                   return $ setVarUnique v uniq

----------------------------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------------------------

importsToIgnore :: [String]
importsToIgnore = ["Meta", "GHC.Generics"]

isIgnoreImport :: Module -> Bool
isIgnoreImport = (`elem` importsToIgnore) . moduleNameString . moduleName

getImportedModules :: ModGuts -> [Module]
getImportedModules = filter (not . isIgnoreImport) . moduleEnvKeys . mg_dir_imps

getImportedMaps :: HscEnv -> ModGuts -> (NameMap, VarMap, TyConMap)
getImportedMaps env guts = (Map.fromList namePairs, (Map.fromList varPairs, []), Map.fromList tcPairs)
    where mods = catMaybes $ lookupUFM (hsc_HPT env) <$> moduleName <$> getImportedModules guts
          types = mconcat $ md_types <$> hm_details <$> mods
          ids = eltsUFM types
          idNames = getName <$> ids
          namePairs = catMaybes $ findNamePair idNames <$> idNames
          findId = fromJust . lookupUFM types
          tyThingPairs = fmap (\(n1,n2) -> (findId n1, findId n2)) namePairs
          (tcThings, varThings) = partition (isTyThingTyCon . fst) tyThingPairs
          varPairs = fmap (\(tt1, tt2) -> (tyThingId tt1, tyThingId tt2)) varThings
          tcPairs = fmap (\(tt1, tt2) -> (tyThingTyCon tt1, tyThingTyCon tt2)) tcThings

-- FIXME check if isAbstractTyCon should be used here
isIgnoreImportType :: Type -> Bool
isIgnoreImportType = anyNameEnv (\tc -> (isIgnoreImport $ nameModule $ getName tc) || isAbstractTyCon tc) . tyConsOfType

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

newTyCon :: MetaModule -> TyConMap -> TyCon -> TyCon
newTyCon mod tcMap tc = Map.findWithDefault metaPrelude tc tcMap
    where metaPrelude = fromMaybe (pprPanic "unknown type constructor: " $ showTyCon tc) (getMetaPreludeTyCon mod tc)

mkTyConMap :: MetaModule -> NameMap -> VarMap -> [TyCon] -> TyConMap
mkTyConMap mod nameMap varMap tcs = let ctcs = filter isClassTyCon tcs
                                        ctcs' = fmap (newTyConClass mod nameMap varMap tcMap) ctcs
                                        tcMap = Map.fromList $ zip ctcs ctcs'
                                    in tcMap

newTyConClass :: MetaModule -> NameMap -> VarMap -> TyConMap-> TyCon -> TyCon
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

createAlgTyConRhs :: MetaModule -> NameMap -> VarMap -> TyConMap -> AlgTyConRhs -> AlgTyConRhs
createAlgTyConRhs mod nameMap varMap tcMap rhs = create rhs
    where create (AbstractTyCon b) = AbstractTyCon b
          create DataFamilyTyCon = DataFamilyTyCon
          create (DataTyCon dcs isEnum) = DataTyCon (createDataCon mod nameMap varMap tcMap <$> dcs) isEnum
          create (NewTyCon dcs ntRhs ntEtadRhs ntCo) = NewTyCon dcs ntRhs ntEtadRhs ntCo -- TODO

createDataCon :: MetaModule -> NameMap -> VarMap -> TyConMap -> DataCon -> DataCon
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
                (changePredType mod tcMap <$> theta)
                (changeType mod tcMap <$> arg_tys)
                (changeType mod tcMap res_ty)
                (newTyCon mod tcMap $ dataConTyCon dc)
                (changePredType mod tcMap <$> dataConStupidTheta dc)
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

bindsToList :: CoreProgram -> CoreProgram
bindsToList bs = sortWith getNonRecName (concatMap toList bs)
    where toList (Rec bs) = uncurry NonRec <$> bs
          toList b = [b]

getNonRecName :: CoreBind -> String
getNonRecName (NonRec b _) = getVarNameStr b

getBindVars :: CoreBind -> [Var]
getBindVars (NonRec v _) = [v]
getBindVars (Rec bs) = fmap fst bs

getBindsVars :: ModGuts -> [Var]
getBindsVars = concatMap getBindVars . mg_binds

getBindsNames :: ModGuts -> [Name]
getBindsNames = fmap varName . getBindsVars

newBinds :: MetaModule -> VarMap -> TyConMap -> [DataCon] -> CoreProgram -> CoreM CoreProgram
newBinds mod varMap tcMap dcs bs = do bs' <- mapM (changeBind mod varMap tcMap) $ filter isInVarMap bs
                                      bs'' <- mapM (dataBind mod varMap) dcs
                                      return $ checkBinds $ bs' ++ bs''
    where isInVarMap (NonRec b e) = varMapMember b varMap
          isInVarMap (Rec bs) = all (`varMapMember` varMap) $ fst <$> bs

changeBind :: MetaModule -> VarMap -> TyConMap -> CoreBind -> CoreM CoreBind
changeBind mod varMap tcMap (NonRec b e) = do (b',e') <- changeBindExpr mod varMap tcMap (b, e)
                                              return (NonRec b' e')
changeBind mod varMap tcMap (Rec bs) = do bs' <- mapM (changeBindExpr mod varMap tcMap) bs
                                          return (Rec bs')

changeBindExpr :: MetaModule -> VarMap -> TyConMap -> (CoreBndr, CoreExpr) -> CoreM (CoreBndr, CoreExpr)
changeBindExpr mod varMap tcMap (b, e) = do e' <- changeExpr mod varMap tcMap b e
                                            let b' = newVar varMap b
                                            e'' <- convertMetaType mod e' $ varType b'
                                            return (b', e'')

dataBind :: MetaModule -> VarMap -> DataCon -> CoreM CoreBind
dataBind mod varMap dc | noAtomsType $ dataConOrigResTy dc = return $ nonRecDataBind varMap dc $ Var $ dataConWrapId dc
dataBind mod varMap dc = do expr <- dataConExpr mod dc
                            return $ nonRecDataBind varMap dc expr

nonRecDataBind :: VarMap -> DataCon -> CoreExpr -> CoreBind
nonRecDataBind varMap dc = NonRec (newVar varMap $ dataConWrapId dc)

-- check var type is equal to expression type in every bind
checkBinds :: CoreProgram -> CoreProgram
checkBinds bs = fmap checkBind bs
    where checkBind (NonRec b e) = uncurry NonRec $ check (b, e)
          checkBind (Rec bs) = Rec (check <$> bs)
          check (b,e) | not (varType b `eqType` exprType e) = pprTrace "\n======================= INCONSISTENT TYPES ============================="
            (vcat [text "var: " <+> showVar b,
                   text "var type:" <+> ppr (varType b),
                   text "expr:" <+> ppr e,
                   text "expr type:" <+> ppr (exprType e),
                   text "\n=======================================================================|"]) (b, e)
          check (b,e) = (b,e)

----------------------------------------------------------------------------------------
-- Type
----------------------------------------------------------------------------------------

newBindType :: MetaModule -> TyConMap -> CoreBndr -> Type
newBindType mod tcMap = changeType mod tcMap . varType

changeBindType :: MetaModule -> TyConMap -> CoreBndr -> CoreBndr
changeBindType mod tcMap x = setVarType x $ newBindType mod tcMap x

changeBindTypeAndUniq :: MetaModule -> TyConMap -> CoreBndr -> CoreM CoreBndr
changeBindTypeAndUniq mod tcMap x = mkVarUnique (changeBindType mod tcMap x)

changeType :: MetaModule -> TyConMap -> Type -> Type
changeType mod tcMap t = go t
    where go t | noAtomsType t = t
          go t | isPredTy t = changePredType mod tcMap t
          go t | (Just (tv, t')) <- splitForAllTy_maybe t = mkForAllTy tv (go t')
          go t | (Just (t1, t2)) <- splitFunTy_maybe t = mkFunTy (go t1) (go t2)
          go t | isVoidTy t || isPredTy t || isPrimitiveType t = t
          go t = withMetaType mod t -- FIXME other cases?, maybe use makeTyVarUnique?

changePredType :: MetaModule -> TyConMap -> PredType -> PredType
changePredType mod tcMap t | (Just (tc, ts)) <- splitTyConApp_maybe t, isClassTyCon tc = mkTyConApp (newTyCon mod tcMap tc) ts
changePredType mod tcMap t = t

getMainType :: Type -> Type
getMainType t = if t == t' then t else getMainType t'
    where (tvs, ps ,t') = tcSplitSigmaTy t

getForAllTyVar :: Type -> TyVar
getForAllTyVar t = fromMaybe (pprPanic "getForAllTyVar" $ ppr t) $ listToMaybe $ fst $ splitForAllTys t

getFunTypeParts :: Type -> [Type]
getFunTypeParts t | not $ isFunTy t = [t]
getFunTypeParts t = argTys ++ [resTy]
    where (argTys, resTy) = splitFunTys $ getMainType t

isTyVarWrappedByWithMeta :: MetaModule -> TyVar -> Type -> Bool
isTyVarWrappedByWithMeta mod tv = all wrappedByWithMeta . getFunTypeParts
    where wrappedByWithMeta t | isWithMetaType mod t = True
          wrappedByWithMeta (TyVarTy tv') = tv /= tv'
          wrappedByWithMeta (AppTy t1 t2) = wrappedByWithMeta t1 && wrappedByWithMeta t2
          wrappedByWithMeta (TyConApp tc ts) = isMetaPreludeNamedThing mod tc || all wrappedByWithMeta ts
          wrappedByWithMeta (FunTy t1 t2) = wrappedByWithMeta t1 && wrappedByWithMeta t2
          wrappedByWithMeta (LitTy _) = True

isWithMetaType :: MetaModule -> Type -> Bool
isWithMetaType mod t
    | Just (tc, _) <- splitTyConApp_maybe (getMainType t) = tc == withMetaC mod
    | otherwise = False

addWithMetaType :: MetaModule -> Type -> Type
addWithMetaType mod t = mkTyConApp (withMetaC mod) [t]

getMetaLevelPred :: MetaModule -> Type -> Maybe Type
getMetaLevelPred mod t = maybe Nothing metaLevelPred $ listToMaybe preds
    where (preds, t') = tcSplitPhiTy t
          metaLevelPred pred
            | Just tc <- tyConAppTyCon_maybe pred, isClassTyCon tc, tc == metaLevelC mod = Just pred
            | otherwise = Nothing

getOnlyArgTypeFromDict :: Type -> Type
getOnlyArgTypeFromDict t
    | isDictTy t, Just ts <- tyConAppArgs_maybe t, length ts == 1 = head ts
    | otherwise = pprPanic "getOnlyArgTypeFromDict" (text "given type" <+> ppr t <+> text " is not dict or has more than one arguments")

isPreludeDictType :: Type -> Bool
isPreludeDictType t | Just (cl, _) <- getClassPredTys_maybe t, Just m <- nameModule_maybe $ getName cl
                    = elem (moduleNameString $ moduleName m) preludeModules
isPreludeDictType t = False

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
noAtomsTypeCon _ tc _| isClassTyCon tc = False -- we do not know without checking if class has methods with atoms
noAtomsTypeCon _ tc _| isPrimTyCon tc = True
noAtomsTypeCon tcs tc n| isDataTyCon tc = and $ fmap (noAtomsTypeVars (nub $ tc : tcs) $ take n $ tyConTyVars tc) $ concatMap dataConOrigArgTys $ tyConDataCons tc
noAtomsTypeCon _ _ _ = True

isAtomsTypeName :: TyCon -> Bool
isAtomsTypeName tc = let nm = occNameString $ nameOccName $ tyConName tc in elem nm ["Atom", "Formula"] -- FIXME check namespace

----------------------------------------------------------------------------------------
-- Expressions map
----------------------------------------------------------------------------------------

type ExprMap = Map Var CoreExpr

mkExprMap :: VarMap -> ExprMap
mkExprMap = Map.map Var . fst

getExpr :: ExprMap -> Var -> CoreExpr
getExpr map v = Map.findWithDefault (pprPanic "no expression for variable: " (ppr v <+> ppr map)) v map

insertVarExpr :: Var -> Var -> ExprMap -> ExprMap
insertVarExpr v v' = Map.insert v (Var v')

----------------------------------------------------------------------------------------
-- Expressions
----------------------------------------------------------------------------------------

changeExpr :: MetaModule -> VarMap -> TyConMap -> CoreBndr -> CoreExpr -> CoreM CoreExpr
changeExpr mod varMap tcMap b e = newExpr (mkExprMap varMap) e
    where newExpr :: ExprMap -> CoreExpr -> CoreM CoreExpr
--          newExpr eMap e | pprTrace "newExpr" (ppr e) False = undefined
          newExpr eMap e | noAtomsSubExpr e = return $ replaceVars eMap e
          newExpr eMap (Var v) | Map.member v eMap = return $ getExpr eMap v
          newExpr eMap (Var v) | isMetaEquivalent mod v = getMetaEquivalent mod eMap varMap tcMap b v Nothing
          newExpr eMap (Var v) = pprPanic "unknown variable"
            (showVar v <+> text "::" <+> ppr (varType v) <+> text ("\nProbably module " ++ getModuleNameStr v ++ " is not compiled with NLambda Plugin."))
          newExpr eMap (Lit l) = noMetaExpr mod (Lit l)
          newExpr eMap (App (Var v) (Type t)) | isMetaEquivalent mod v, isDataConWorkId v = getMetaEquivalent mod eMap varMap tcMap b v $ Just t
          newExpr eMap (App f (Type t)) = do f' <- newExpr eMap f
                                             let (tyVars, t') = splitForAllTys $ exprType f'
                                             let t'' = if isTyVarWrappedByWithMeta mod (head tyVars) t' then t else changeType mod tcMap t
                                             return $ App f' $ Type t''
          newExpr eMap (App f e) = do f' <- newExpr eMap f
                                      f'' <- if isWithMetaType mod $ exprType f'
                                             then valueExpr mod f'
                                             else return f'
                                      e' <- newExpr eMap e
                                      e'' <- convertMetaType mod e' $ funArgTy $ exprType f''
                                      return $ checkTypeConsistency f'' e'' (mkCoreApp f'' e'')
          newExpr eMap (Lam x e) | isTKVar x = do e' <- newExpr eMap e
                                                  return $ Lam x e' -- FIXME new uniq for x (and then replace all occurrences)?
          newExpr eMap (Lam x e) = do x' <- changeBindTypeAndUniq mod tcMap x
                                      e' <- newExpr (insertVarExpr x x' eMap) e
                                      return $ Lam x' e'
          newExpr eMap (Let b e) = do (b', eMap) <- changeLetBind b eMap
                                      e' <- newExpr eMap e
                                      return $ Let b' e'
          newExpr eMap (Case e b t as) = do e' <- newExpr eMap e
                                            e'' <- if isWithMetaType mod $ exprType e'
                                                   then valueExpr mod e'
                                                   else return e'
                                            m <- metaExpr mod e'
                                            as' <- mapM (changeAlternative eMap m) as
                                            let t' = changeType mod tcMap t
                                            return $ Case e'' b t' as'
          newExpr eMap (Cast e c) = do e' <- newExpr eMap e
                                       return $ Cast e' (changeCoercion mod tcMap c)
          newExpr eMap (Tick t e) = do e' <- newExpr eMap e
                                       return $ Tick t e'
          newExpr eMap (Type t) = undefined -- type should be served in (App f (Type t)) case
          newExpr eMap (Coercion c) = return $ Coercion $ changeCoercion mod tcMap c
          changeLetBind (NonRec b e) eMap = do b' <- changeBindTypeAndUniq mod tcMap b
                                               let eMap' = insertVarExpr b b' eMap
                                               e' <- newExpr eMap' e
                                               return (NonRec b' e', eMap')
          changeLetBind (Rec bs) eMap = do (bs', eMap') <- changeRecBinds bs eMap
                                           return (Rec bs', eMap')
          changeRecBinds ((b, e):bs) eMap = do (bs', eMap') <- changeRecBinds bs eMap
                                               b' <- changeBindTypeAndUniq mod tcMap b
                                               let eMap'' = insertVarExpr b b' eMap'
                                               e' <- newExpr eMap'' e
                                               return ((b',e'):bs', eMap'')
          changeRecBinds [] eMap = return ([], eMap)
          changeAlternative eMap m (DataAlt con, xs, e) = do xs' <- mapM mkVarUnique xs
                                                             es <- mapM (\x -> if (isFunTy $ varType x) then return $ Var x else createExpr mod (Var x) m) xs'
                                                             e' <- newExpr (Map.union eMap $ Map.fromList $ zip xs es) e
                                                             return (DataAlt con, xs', e')
          changeAlternative eMap m (alt, [], e) = do e' <- newExpr eMap e
                                                     return (alt, [], e')
          checkTypeConsistency f e | not $ eqType (funArgTy $ exprType f) (exprType e)
                                   = pprTrace "\n======================= INCONSISTENT TYPES IN APPLICATION =============="
                                       (vcat [text "fun: " <+> ppr f,
                                              text "fun type: " <+> ppr (exprType f),
                                              text "fun arg type: " <+> ppr (funArgTy $ exprType f),
                                              text "arg: " <+> ppr e,
                                              text "arg type: " <+> ppr (exprType e),
                                              text "\n=======================================================================|"]) id
          checkTypeConsistency f e = id

-- the type of expression is not open for atoms and there are no free variables open for atoms
noAtomsSubExpr :: CoreExpr -> Bool
noAtomsSubExpr e = (noAtomsType $ exprType e) && noAtomFreeVars
    where noAtomFreeVars = isEmptyUniqSet $ filterUniqSet (not . noAtomsType . varType) $ exprFreeIds e

replaceVars :: ExprMap -> CoreExpr -> CoreExpr
replaceVars eMap (Var x) | Just e <- Map.lookup x eMap
                         = if eqType (varType x) (exprType e) then e else pprPanic "replaceVars - inconsistent types: " (ppr x <+> ppr e)
replaceVars eMap (Lit l) = Lit l
replaceVars eMap (App f e) = App (replaceVars eMap f) (replaceVars eMap e)
replaceVars eMap (Lam x e) = Lam x (replaceVars eMap e)
replaceVars eMap (Let b e) = Let (replaceVarsInBind b) (replaceVars eMap e)
    where replaceVarsInBind (NonRec x e) = NonRec x (replaceVars eMap e)
          replaceVarsInBind (Rec bs) = Rec $ fmap (\(x, e) -> (x, replaceVars eMap e)) bs
replaceVars eMap (Case e x t as) = Case (replaceVars eMap e) x t (replaceVarsInAlt <$> as)
    where replaceVarsInAlt (con, bs, e) = (con, bs, replaceVars eMap e)
replaceVars eMap (Cast e c) = Cast (replaceVars eMap e) c
replaceVars eMap (Tick t e) = Tick t (replaceVars eMap e)
replaceVars eMap e = e

changeCoercion :: MetaModule -> TyConMap -> Coercion -> Coercion
changeCoercion mod tcMap c = change c
    where change (Refl r t) = Refl r t -- FIXME not changeType ?
          change (TyConAppCo r tc cs) = TyConAppCo r (newTyCon mod tcMap tc) (change <$> cs)
          change (AppCo c1 c2) = AppCo (change c1) (change c2)
          change (ForAllCo tv c) = ForAllCo tv (change c)
          change (CoVarCo cv) = CoVarCo cv
          change (AxiomInstCo a i cs) = AxiomInstCo (changeCoAxiom mod tcMap a) i (change <$> cs)
          change (UnivCo n r t1 t2) = UnivCo n r (changeType mod tcMap t1) (changeType mod tcMap t2)
          change (SymCo c) = SymCo $ change c
          change (TransCo c1 c2) = TransCo (change c1) (change c2)
          change (AxiomRuleCo a ts cs) = AxiomRuleCo a (changeType mod tcMap <$> ts) (change <$> cs)
          change (NthCo i c) = NthCo i $ change c
          change (LRCo lr c) = LRCo lr $ change c
          change (InstCo c t) = InstCo (change c) (changeType mod tcMap t)
          change (SubCo c) = SubCo $ change c

changeCoAxiom :: MetaModule -> TyConMap -> CoAxiom a -> CoAxiom a
changeCoAxiom mod tcMap (CoAxiom u n r tc bs i) = CoAxiom u n r (newTyCon mod tcMap tc) bs i

dataConExpr :: MetaModule -> DataCon -> CoreM CoreExpr
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

isTV :: ExprVar -> Bool
isTV (TV _) = True
isTV (DI _) = False

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
    in if ty == ty'
       then return ([], ty)
       else do tyVars' <- mapM makeTyVarUnique tyVars
               let preds' = filter isClassPred preds -- TODO other preds
               let classTys = fmap getClassPredTys preds'
               let subst = extendTvSubstList emptySubst (zip tyVars $ fmap TyVarTy tyVars')
               predVars <- mapM mkPredVar ((\(c,tys) -> (c, substTy subst <$> tys)) <$> classTys)
               (resTyVars, resTy) <- splitTypeTyVars $ substTy subst ty'
               return ((TV <$> tyVars') ++ (DI <$> predVars) ++ resTyVars, resTy)

unifyTypes :: Type -> Type -> Maybe TvSubst
unifyTypes t1 t2 = maybe unifyWithOpenKinds Just (tcUnifyTy t1 t2)
    where unifyWithOpenKinds = tcUnifyTy (replaceOpenKinds t1) (replaceOpenKinds t2)
          replaceOpenKinds (TyVarTy v) | isOpenTypeKind (tyVarKind v) = TyVarTy $ setTyVarKind v (defaultKind $ tyVarKind v)
          replaceOpenKinds (TyVarTy v) = TyVarTy v
          replaceOpenKinds (AppTy t1 t2) = AppTy (replaceOpenKinds t1) (replaceOpenKinds t2)
          replaceOpenKinds (TyConApp tc ts) = TyConApp tc (fmap replaceOpenKinds ts)
          replaceOpenKinds (FunTy t1 t2) = FunTy (replaceOpenKinds t1) (replaceOpenKinds t2)
          replaceOpenKinds (ForAllTy v t) = ForAllTy v (replaceOpenKinds t)
          replaceOpenKinds (LitTy tl) = LitTy tl

canUnifyTypes :: Type -> Type -> Bool
canUnifyTypes t1 t2 = isJust $ unifyTypes (snd $ splitForAllTys t1) (snd $ splitForAllTys t2)

sameTypes :: Type -> Type -> Bool
sameTypes t1 t2
    | eqType t1 t2 = True
    | Just s <- unifyTypes t1 t2 = all isTyVarTy $ eltsUFM $ getTvSubstEnv s
    | otherwise = False

applyExpr :: CoreExpr -> CoreExpr -> CoreM CoreExpr
applyExpr fun e =
    do (eTyVars, ty) <- splitTypeTyVars $ exprType e
       let (funTyVars, funTy) = splitForAllTys $ exprType fun
       let subst = fromMaybe
                     (pprPanic "applyExpr - can't unify:" (ppr (funArgTy funTy) <+> text "and" <+> ppr ty <+> text "for apply:" <+> ppr fun <+> text "with" <+> ppr e))
                     (unifyTypes (funArgTy funTy) ty)
       let funTyVarSubstExprs = fmap (Type . substTyVar subst) funTyVars
       return $ mkCoreLams (exprVarToVar eTyVars) -- FIXME case where length funTyVars > length eTyVars
                  (mkCoreApp
                    (mkCoreApps fun funTyVarSubstExprs)
                    (mkCoreApps e $ exprVarToExpr eTyVars))

applyExprs :: CoreExpr -> [CoreExpr] -> CoreM CoreExpr
applyExprs = foldlM applyExpr

----------------------------------------------------------------------------------------
-- Meta
----------------------------------------------------------------------------------------

type MetaModule = HomeModInfo

getMetaModule :: HscEnv -> MetaModule
getMetaModule = fromJust . find ((== "Meta") . moduleNameString . moduleName . mi_module . hm_iface) . eltsUFM . hsc_HPT

getTyThing :: MetaModule -> String -> (TyThing -> Bool) -> (TyThing -> a) -> (a -> Name) -> a
getTyThing mod nm cond fromThing getName = fromMaybe (pprPanic "getTyThing - name not found in module Meta:" $ text nm)
                                                     (getTyThingMaybe mod nm cond fromThing getName)

getTyThingMaybe :: MetaModule -> String -> (TyThing -> Bool) -> (TyThing -> a) -> (a -> Name) -> Maybe a
getTyThingMaybe mod nm cond fromThing getName =
    fmap fromThing $ listToMaybe $ nameEnvElts $ filterNameEnv (\t -> cond t && hasName nm (getName $ fromThing t))
                                                                  (md_types $ hm_details mod)
    where hasName nmStr nm = occNameString (nameOccName nm) == nmStr

isTyThingId :: TyThing -> Bool
isTyThingId (AnId _) = True
isTyThingId _        = False

getVar :: MetaModule -> String -> Var
getVar mod nm = getTyThing mod nm isTyThingId tyThingId varName

getVarMaybe :: MetaModule -> String -> Maybe Var
getVarMaybe mod nm = getTyThingMaybe mod nm isTyThingId tyThingId varName

metaNames :: MetaModule -> [Name]
metaNames = fmap getName . nameEnvElts . md_types . hm_details

isTyThingTyCon :: TyThing -> Bool
isTyThingTyCon (ATyCon _) = True
isTyThingTyCon _          = False

getTyCon :: MetaModule -> String -> TyCon
getTyCon mod nm = getTyThing mod nm isTyThingTyCon tyThingTyCon tyConName

getMetaVar :: MetaModule -> String -> CoreExpr
getMetaVar mod = Var . getVar mod

getMetaVarMaybe :: MetaModule -> String -> Maybe CoreExpr
getMetaVarMaybe mod = fmap Var . getVarMaybe mod

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
metaLevelC mod = getTyCon mod "MetaLevel"

withMetaType :: MetaModule -> Type -> Type
withMetaType mod ty = mkTyConApp (withMetaC mod) [ty]

unionType :: MetaModule -> Type
unionType = mkTyConTy . unionC

getMetaLevelInstance :: MetaModule -> VarMap -> Type -> CoreExpr
getMetaLevelInstance mod (_, vs) t | Just tc <- tyConAppTyCon_maybe t = getInstance ("$fMetaLevel" ++ (occNameString $ getOccName tc))
    where getInstance iname
              | Just v <- getMetaVarMaybe mod iname = v
              | Just v <- listToMaybe $ filter ((== iname) . getVarNameStr) vs = Var v
              | otherwise = pprPanic "NLambda plungin requires MetaLevel instance for type:" (ppr t)

mkPredVar :: (Class, [Type]) -> CoreM DictId
mkPredVar (cls, tys) = do uniq <- getUniqueM
                          let name = mkSystemName uniq (mkDictOcc (getOccName cls))
                          return (mkLocalId name (mkClassPred cls tys))

makeTyVarUnique :: TyVar -> CoreM TyVar
makeTyVarUnique v = do uniq <- getUniqueM
                       return $ mkTyVar (setNameUnique (tyVarName v) uniq) (tyVarKind v)

noMetaExpr :: MetaModule -> CoreExpr -> CoreM CoreExpr
noMetaExpr mod e | isInternalType $ exprType e = return e
noMetaExpr mod e = applyExpr (noMetaV mod) e

valueExpr :: MetaModule -> CoreExpr -> CoreM CoreExpr
valueExpr mod e | not $ isWithMetaType mod $ exprType e = return e
valueExpr mod e = applyExpr (valueV mod) e

metaExpr :: MetaModule -> CoreExpr -> CoreM CoreExpr
metaExpr mod e = applyExpr (metaV mod) e

unionExpr :: MetaModule -> CoreExpr -> CoreM CoreExpr
unionExpr mod e = applyExpr (unionV mod) e

createExpr :: MetaModule -> CoreExpr -> CoreExpr -> CoreM CoreExpr
createExpr mod e  _  | isInternalType $ exprType e = return e
createExpr mod e1 e2 = applyExprs (createV mod) [e1, e2]

getMetaExpr :: MetaModule -> CoreExpr -> CoreM CoreExpr
getMetaExpr mod e = applyExpr (getMetaV mod) e

renameExpr :: MetaModule -> CoreExpr -> CoreExpr -> CoreExpr -> CoreM CoreExpr
renameExpr mod e1 e2 e3 = applyExprs (renameV mod) [e1, e2, e3]

colonExpr :: MetaModule -> CoreExpr -> CoreExpr -> CoreM CoreExpr
colonExpr mod e1 e2 = applyExprs (colonV mod) [e1, e2]

idOpExpr :: MetaModule -> CoreExpr -> CoreM CoreExpr
idOpExpr mod e = applyExpr (idOpV mod) e

----------------------------------------------------------------------------------------
-- Convert meta types
----------------------------------------------------------------------------------------

convertMetaType :: MetaModule -> CoreExpr -> Type -> CoreM CoreExpr
convertMetaType mod e t | canUnifyTypes (getMainType $ exprType e) (getMainType t) = return e
convertMetaType mod e t | isClassPred t,
                          Just (cl, _) <- getClassPredTys_maybe $ exprType e,
                          (_, preds, ids, _) <- classBigSig cl,
                          Just idx <- findIndex (canUnifyTypes t) preds
                        = applyExpr (Var $ ids !! idx) e
convertMetaType mod e t | length ets == length ts = applyExpr (convertMetaFun mod ets ts) e
    where ets = getFunTypeParts $ getMainType $ exprType e
          ts = getFunTypeParts $ getMainType t
convertMetaType mod e t | isWithMetaType mod t, Just (_, ts) <- splitTyConApp_maybe (getMainType t), canUnifyTypes (exprType e) (head ts)
                        = noMetaExpr mod e
convertMetaType mod e t | isWithMetaType mod (exprType e), Just (_, ts) <- splitTyConApp_maybe (getMainType $ exprType e), canUnifyTypes t (head ts)
                        = valueExpr mod e
convertMetaType mod e t = return e

convertMetaFun :: MetaModule -> [Type] -> [Type] -> CoreExpr
convertMetaFun mod ts1 ts2 = getMetaVar mod $ funName "convert" ts1 ts2
    where funName name (t1:ts1) (t2:ts2)
            | canUnifyTypes t1 t2 = funName (name ++ "x") ts1 ts2
            | isWithMetaType mod t1 && canUnifyTypes t1 (addWithMetaType mod t2) = funName (name ++ "0") ts1 ts2
            | isWithMetaType mod t2 && canUnifyTypes (addWithMetaType mod t1) t2 = funName (name ++ "1") ts1 ts2
            | otherwise = pprPanic "convertMetaFun - can't unify meta types:" (ppr (t1:ts1) <+> ppr (t2:ts2))
          funName name [] [] = name

----------------------------------------------------------------------------------------
-- Meta Equivalents
----------------------------------------------------------------------------------------

getMetaPreludeNameMaps :: MetaModule -> HscEnv -> ModGuts -> NameMap
getMetaPreludeNameMaps mod env guts = Map.fromList metaPairs
    where fromPrelude e | Imported ss <- gre_prov e = elem "Prelude" $ moduleNameString <$> is_mod <$> is_decl <$> ss
          fromPrelude e = False
          preludeNames = fmap gre_name $ filter fromPrelude $ concat $ occEnvElts $ mg_rdr_env guts
          metaPairs = catMaybes $ fmap (findNamePair $ metaNames mod) preludeNames

isMetaPreludeNamedThing :: NamedThing a => MetaModule -> a -> Bool
isMetaPreludeNamedThing mod nt = any (== getName nt) (metaNames mod)

isMetaPreludeDict :: MetaModule -> Type -> Bool
isMetaPreludeDict mod t | Just (cl, _) <- getClassPredTys_maybe t = isMetaPreludeNamedThing mod cl
isMetaPreludeDict mod t = False

getMetaPreludeTyCon :: MetaModule -> TyCon -> Maybe TyCon
getMetaPreludeTyCon mod tc = (getTyCon mod . getNameStr . snd) <$> findNamePair (metaNames mod) (getName tc)

getDefinedMetaEquivalentVar :: MetaModule -> Var -> Maybe Var
getDefinedMetaEquivalentVar mod v =  getVar mod . getNameStr . snd <$> findNamePair (metaNames mod) (varName v)

isMetaEquivalent :: MetaModule -> Var -> Bool
isMetaEquivalent mod v = case metaEquivalent (getModuleNameStr v) (getVarNameStr v) of
                           NoEquivalent -> isJust $ getDefinedMetaEquivalentVar mod v
                           _            -> True

getMetaEquivalent :: MetaModule -> ExprMap -> VarMap -> TyConMap -> CoreBndr -> Var -> Maybe Type -> CoreM CoreExpr
getMetaEquivalent mod eMap varMap tcMap b v mt =
    case metaEquivalent (getModuleNameStr v) (getVarNameStr v) of
      OrigFun -> return $ appType $ Var v
      MetaFun name -> return $ appType $ getMetaVar mod name
      MetaConvertFun name -> liftM appType $ applyExpr (getMetaVar mod name) (Var v)
      NoEquivalent -> addDependencies mod eMap varMap tcMap b v mt $ fromMaybe
        (pprPanic "no meta equivalent for:" (showVar v <+> text "from module:" <+> text (getModuleNameStr v)))
        (getDefinedMetaEquivalentVar mod v)
    where appType :: CoreExpr -> CoreExpr
          appType e = maybe e (mkCoreApp e . Type) mt

addDependencies :: MetaModule -> ExprMap -> VarMap -> TyConMap -> CoreBndr -> Var -> Maybe Type -> Var -> CoreM CoreExpr
addDependencies mod eMap varMap tcMap b var mt metaVar | pprTrace "addDependencies" (ppr var <+> ppr metaVar) False = undefined
addDependencies mod eMap varMap tcMap b var mt metaVar
    | isDataConWorkId metaVar, isFun
    = do vars <- liftM fst $ splitTypeTyVars $ changeType mod tcMap $ varType var
         let metaE = addMetaLevelPred $ mkCoreApp (Var metaVar) (Type $ fromJust mt)
         let dictVars = (exprVarToVar $ filter (not . isTV) vars) ++ (catMaybes $ localDictVar <$> Map.elems eMap)
         addDictDeps metaE (Var b) dictVars False
    | isDFunId metaVar, isFun
    = do vars <- liftM fst $ splitTypeTyVars $ changeType mod tcMap $ varType var
         let tyVars = filter isTV vars
         let metaE = addMetaLevelPred $ mkCoreApps (Var metaVar) (exprVarToExpr tyVars)
         let dictVars = exprVarToVar $ filter (not . isTV) vars
         metaE' <- addDictDeps metaE (Var var) dictVars True
         let argVars = filter (isMetaPreludeDict mod . varType) dictVars
         return $ mkCoreLams (exprVarToVar tyVars ++ argVars) metaE'
    | otherwise = return $ Var metaVar
    where isFun = isFunTy $ dropForAlls $ varType metaVar
          localDictVar (Var v) | isLocalId v && not (isExportedId v) && isDictId v = Just v
          localDictVar _ = Nothing
          addMetaLevelPred metaE | Just t <- getMetaLevelPred mod $ exprType metaE
                                 = mkCoreApp metaE $ getMetaLevelInstance mod varMap $ getOnlyArgTypeFromDict t
          addMetaLevelPred metaE = metaE
          addDictDeps metaE e vars allowMetaDeps | Just (t,_) <- splitFunTy_maybe $ exprType metaE, isPredTy t
                                                 = addDictDep metaE t e vars allowMetaDeps
          addDictDeps metaE e _ allowMetaDeps = return metaE
          addDictDep metaE t e (v:vars) allowMetaDeps | allowMetaDeps, isMetaPreludeDict mod t -- e.g. for case $fShow_nlambda[]
                                                      = if sameTypes t $ varType v
                                                        then addDictDeps (mkCoreApp metaE $ Var v) e vars allowMetaDeps
                                                        else addDictDep metaE t e vars allowMetaDeps
          addDictDep metaE t e vars allowMetaDeps | not $ isMetaPreludeDict mod t
                                                  = do dep <- noMetaDep e vars
                                                       let metaExp' = mkCoreApp metaE dep
                                                       addDictDeps metaExp' e vars allowMetaDeps
          addDictDep metaE t e vars allowMetaDeps = return metaE
          noMetaDep e vars | Just (t,_) <- splitFunTy_maybe $ dropForAlls $ exprType e
                           = do (sc, vars') <- findSuperClass t vars
                                e' <- applyExpr e sc
                                noMetaDep e' vars'
          noMetaDep e vars = return e
          findSuperClass t (v:vars) | Just (cl, _) <- getClassPredTys_maybe $ varType v, (_, _, ids, _) <- classBigSig cl
                                    = do es <- mapM (\i -> applyExpr (Var i) (Var v)) ids
                                         let i = findIndex (sameTypes t . exprType) es
                                         if isJust i
                                         then return (es !! fromJust i, vars)
                                         else findSuperClass t vars
          findSuperClass t (v:vars) = findSuperClass t vars
          findSuperClass t [] = pprPanic "findSuperClass - no super class with proper type found" (ppr t)

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
showType = ppr
--showType (TyVarTy v) = text "TyVarTy(" <> showVar v <> text ")"
--showType (AppTy t1 t2) = text "AppTy(" <> showType t1 <+> showType t2 <> text ")"
--showType (TyConApp tc ts) = text "TyConApp(" <> showTyCon tc <+> hsep (fmap showType ts) <> text ")"
--showType (FunTy t1 t2) = text "FunTy(" <> showType t1 <+> showType t2 <> text ")"
--showType (ForAllTy v t) = text "ForAllTy(" <> showVar v <+> showType t <> text ")"
--showType (LitTy tl) = text "LitTy(" <> ppr tl <> text ")"

showTyCon :: TyCon -> SDoc
showTyCon tc = text "'" <> text (occNameString $ nameOccName $ tyConName tc) <> text "'"
--    <> text "{"
--    <> ppr (nameUnique $ tyConName tc)
--    <> (whenT (isAlgTyCon tc) ",Alg")
--    <> (whenT (isClassTyCon tc) ",Class")
--    <> (whenT (isFamInstTyCon tc) ",FamInst")
--    <> (whenT (isFunTyCon tc) ",Fun")
--    <> (whenT (isPrimTyCon tc) ",Prim")
--    <> (whenT (isTupleTyCon tc) ",Tuple")
--    <> (whenT (isUnboxedTupleTyCon tc) ",UnboxedTuple")
--    <> (whenT (isBoxedTupleTyCon tc) ",BoxedTuple")
--    <> (whenT (isTypeSynonymTyCon tc) ",TypeSynonym")
--    <> (whenT (isDecomposableTyCon tc) ",Decomposable")
--    <> (whenT (isPromotedDataCon tc) ",PromotedDataCon")
--    <> (whenT (isPromotedTyCon tc) ",Promoted")
--    <> (whenT (isDataTyCon tc) ",DataTyCon")
--    <> (whenT (isProductTyCon tc) ",ProductTyCon")
--    <> (whenT (isEnumerationTyCon tc) ",EnumerationTyCon")
--    <> (whenT (isNewTyCon tc) ",NewTyCon")
--    <> (whenT (isAbstractTyCon tc) ",AbstractTyCon")
--    <> (whenT (isFamilyTyCon tc) ",FamilyTyCon")
--    <> (whenT (isOpenFamilyTyCon tc) ",OpenFamilyTyCon")
--    <> (whenT (isTypeFamilyTyCon tc) ",TypeFamilyTyCon")
--    <> (whenT (isDataFamilyTyCon tc) ",DataFamilyTyCon")
--    <> (whenT (isOpenTypeFamilyTyCon tc) ",OpenTypeFamilyTyCon")
--    <> (whenT (isUnLiftedTyCon tc) ",UnLiftedTyCon")
--    <> (whenT (isGadtSyntaxTyCon tc) ",GadtSyntaxTyCon")
--    <> (whenT (isDistinctTyCon tc) ",DistinctTyCon")
----    <> (whenT (isDistinctAlgRhs tc) ",DistinctAlgRhs")
----    <> (whenT (isInjectiveTyCon tc) ",InjectiveTyCon")
----    <> (whenT (isGenerativeTyCon tc) ",GenerativeTyCon")
----    <> (whenT (isGenInjAlgRhs tc) ",GenInjAlgRhs")
--    <> (whenT (isTyConAssoc tc) ",TyConAssoc")
--    <> (whenT (isRecursiveTyCon tc) ",RecursiveTyCon")
--    <> (whenT (isImplicitTyCon tc) ",ImplicitTyCon")
--    <> (text ",dataConNames:" <+> (vcat $ fmap showName $ fmap dataConName $ tyConDataCons tc))
--    <> text "}"

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

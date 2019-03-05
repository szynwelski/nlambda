module Nominal.Meta.Plugin where

import Avail
import Bag (isEmptyBag)
import qualified BooleanFormula as BF
import Class
import CoAxiom hiding (toUnbranchedList)
import Control.Applicative ((<|>))
import Control.Monad (liftM, msum)
import Data.Char (isLetter, isLower, isUpper)
import Data.Foldable (foldlM)
import Data.List ((\\), delete, elemIndex, find, findIndex, intersect, isInfixOf, isPrefixOf, nub, partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.String.Utils (replace)
import ErrUtils (pprMessageBag)
import GhcPlugins hiding (ModuleName, mkApps, mkLocalVar, substTy)
import InstEnv (ClsInst, instanceDFunId, instanceHead, instanceRoughTcs, instEnvElts, is_cls, is_flag, is_orphan, mkImportedInstance)
import Kind (defaultKind, isOpenTypeKind)
import MkId (mkDataConWorkId, mkDictSelRhs)
import Nominal.Meta
import Nominal.Meta.CoreLint (lintCoreBindings)
import Nominal.Meta.Modules
import Pair (pFst, pSnd)
import PrelNames (anyTyConKey)
import TypeRep
import TcType (tcSplitSigmaTy, tcSplitPhiTy)
import Unify (tcUnifyTy)

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  reinitializeGlobals
  env <- getHscEnv
  return $ (metaPlugin env) : todo

metaPlugin :: HscEnv -> CoreToDo
metaPlugin = CoreDoPluginPass "MetaPlugin" . pass

modInfo :: Outputable a => String -> (ModGuts -> a) -> ModGuts -> CoreM ()
modInfo label fun guts = putMsg $ text label <> text ": " <> (ppr $ fun guts)

headPanic :: String -> SDoc -> [a] -> a
headPanic msg doc [] = pprPanic ("headPanic - " ++ msg) doc
headPanic _ _ l = head l

tailPanic :: String -> SDoc -> [a] -> [a]
tailPanic msg doc [] = pprPanic ("tailPanic - " ++ msg) doc
tailPanic _ _ l = tail l

pprE :: String -> CoreExpr -> SDoc
pprE n e = text (n ++ " =") <+> ppr e <+> text "::" <+> ppr (exprType e)

pprV :: String -> CoreBndr -> SDoc
pprV n v = text (n ++ " =") <+> ppr v <+> text "::" <+> ppr (varType v)

for :: ModInfo -> String -> SDoc -> CoreM ()
for mod modName doc
    | modName == getModuleNameStr (mg_module $ guts mod) = putMsg doc
    | otherwise = return ()

pass :: HscEnv -> ModGuts -> CoreM ModGuts
pass env guts =
         do putMsg $ text ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> start:" <+> (ppr $ mg_module guts)

            -- mod info - all info in one place
            let metaMods = getMetaModules env
            mod <- modInfoEmptyMaps env guts metaMods

            -- imported maps
            let (impNameMap, impVarMap, impTcMap) = getImportedMaps mod

            -- names
            let metaNameMap = getMetaPreludeNameMap mod
            nameMap <- mkNamesMap guts $ Map.union impNameMap metaNameMap
            let mod' = mod {nameMap = nameMap}

            -- classes and vars
            let tcMap = mkTyConMap mod' {varMap = unionVarMaps varMap impVarMap} (mg_tcs guts)
                varMap = mkVarMap mod' {tcMap = unionTcMaps tcMap impTcMap}
            let mod'' = mod' {tcMap = unionTcMaps tcMap impTcMap, varMap = unionVarMaps varMap impVarMap}

            guts' <- newGuts mod'' guts

            -- show info
--            putMsg $ text "binds:\n" <+> (foldr (<+>) (text "") $ map showBind $ mg_binds guts' ++ getImplicitBinds guts')
--            putMsg $ text "classes:\n" <+> (vcat $ fmap showClass $ getClasses guts')

--            modInfo "module" mg_module guts'
--            modInfo "binds" (sortBinds . mg_binds) guts'
--            modInfo "dependencies" (dep_mods . mg_deps) guts'
--            modInfo "imported" (moduleEnvKeys . mg_dir_imps) guts'
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
-- Mod info
----------------------------------------------------------------------------------------

data ModInfo = ModInfo {env :: HscEnv,
                        guts :: ModGuts,
                        eps :: ExternalPackageState,
                        metaModules :: [MetaModule],
                        nameMap :: NameMap,
                        tcMap :: TyConMap,
                        varMap :: VarMap}

modInfoEmptyMaps :: HscEnv -> ModGuts -> [MetaModule] -> CoreM ModInfo
modInfoEmptyMaps env guts metaMods = do eps <- liftIO $ hscEPS env
                                        return $ ModInfo env guts eps metaMods Map.empty emptyTcMap emptyVarMap

instance Outputable ModInfo where
    ppr mod = text "\n======== ModInfo =========================================================="
              <+> showMap "\nNames" (nameMap mod) showName
              <+> showMap "\nTyCons" (tcsWithPairs mod) showTyCon
              <+> showMap "\nVars" (varsWithPairs mod) showVar
              <+> text "\nAll type cons:" <+> vcat (fmap showTyCon $ allTcs mod)
              <+> text "\nAll vars:" <+> vcat (fmap showVar $ allVars mod)
              <+> text "\n==========================================================================="

showMap :: String -> Map a a -> (a -> SDoc) -> SDoc
showMap header map showElem = text (header ++ ":\n")
                              <+> (vcat (concatMap (\(x,y) -> [showElem x <+> text "->" <+> showElem y]) $ Map.toList map))

----------------------------------------------------------------------------------------
-- Guts
----------------------------------------------------------------------------------------

newGuts :: ModInfo -> ModGuts -> CoreM ModGuts
newGuts mod guts = do binds <- newBinds mod (getDataCons guts) (mg_binds guts)
                      let binds' = checkCoreProgram $ simplifyBinds binds
                      binds'' <- replaceMocksByInstancesInProgram mod binds'
                      let binds''' = checkCoreProgram $ simplifyBinds binds''
                      let allBinds = checkCoreLint mod (mg_binds guts ++ binds''')
                      let exps = newExports mod (mg_exports guts)
                      let usedNames = newUsedNames mod (mg_used_names guts)
                      let clsInsts = newClassInstances mod (mg_insts guts)
                      let tcs = filter (inCurrentModule mod) $ Map.elems (tcsWithPairs mod)
                      let gre = newGlobalRdrEnv mod (mg_rdr_env guts)
                      return $ guts {mg_tcs = mg_tcs guts ++ tcs,
                                     mg_binds = allBinds,
                                     mg_exports = mg_exports guts ++ exps,
                                     mg_insts = mg_insts guts ++ clsInsts,
                                     mg_rdr_env = gre,
                                     mg_used_names = usedNames}

----------------------------------------------------------------------------------------
-- Annotation
----------------------------------------------------------------------------------------

annotatedWithNoMetaFunction :: ModInfo -> Var -> Bool
annotatedWithNoMetaFunction mod v = any isNoMetaFunAnnotation $ filter hasName $ mg_anns $ guts mod
    where hasName a = getAnnTargetName_maybe (ann_target a) == Just (getName v)
          isNoMetaFunAnnotation a = case fromSerialized deserializeWithData $ ann_value a of
                                      Just NoMetaFunction -> True
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
-- Names
----------------------------------------------------------------------------------------

type NameMap = Map Name Name

nameMember :: ModInfo -> Name -> Bool
nameMember mod name = Map.member name $ nameMap mod

newName :: ModInfo -> Name -> Name
newName mod name = Map.findWithDefault (pprPanic "unknown name: " (showName name <+> vcat (showName <$> Map.keys map))) name map
    where map = nameMap mod

mkNamesMap :: ModGuts -> NameMap -> CoreM NameMap
mkNamesMap guts impNameMap = do nameMap <- mkPrefixNamesMap (getDataConsNames guts ++ getBindsNames guts ++ getClassesNames guts)
                                return $ Map.union nameMap impNameMap

classPrefix :: String
classPrefix = "NLambda_"

opPrefix :: String
opPrefix = "###"

varPrefix :: String
varPrefix = "nlambda_"

ghcClassPrefixes :: [String]
ghcClassPrefixes = ["$f", "$p1", "$p2", "$p3", "$p4", "$p5", "$p6", "T:", "D:", "NTCo:", "TFCo:"]

ghcPrefixes :: [String]
ghcPrefixes = ["$c", "$dm", "$d", "$s", "$w", "$W"] ++ ghcClassPrefixes

isPrefixForClass :: String -> Bool
isPrefixForClass p = elem p ghcClassPrefixes

namePrefix :: NameSpace -> String -> String -> String
namePrefix space prefix name
    | all (not . isLetter) (replace "$dm" "" name) = opPrefix
    | isTcClsNameSpace space = classPrefix
    | isPrefixForClass prefix, isUpper $ headPanic "namePrefix" (text name) name = classPrefix
    | otherwise = varPrefix

replaceReservedOperators :: String -> String
replaceReservedOperators "[]" = "@@"
replaceReservedOperators "(,)" = "#"
replaceReservedOperators "(,,)" = "##"
replaceReservedOperators "(,,,)" = "###"
replaceReservedOperators "(,,,,)" = "####"
replaceReservedOperators "(,,,,,)" = "#####"
replaceReservedOperators "(,,,,,,)" = "######"
replaceReservedOperators n = n

nlambdaName :: OccName -> OccName
nlambdaName name = mkOccName space (prefix ++ namePrefix space prefix nameWithoutPrefix ++ replaceReservedOperators nameWithoutPrefix)
    where space = occNameSpace name
          nameStr = occNameString name
          maybePrefix = find (`isPrefixOf` nameStr) ghcPrefixes
          prefix = fromMaybe "" maybePrefix
          nameWithoutPrefix = maybe nameStr (\p -> replace p "" nameStr) maybePrefix

isNLambdaName :: String -> Bool
isNLambdaName name = any (`isInfixOf` name) [classPrefix, opPrefix, varPrefix]

mkPrefixNamesMap :: [Name] -> CoreM NameMap
mkPrefixNamesMap names = do notPairs' <- mapM createNewName notPairs
                            return $ Map.fromList (pairs ++ zip notPairs notPairs')
    where pairs = mapMaybe (\n -> (\n' -> (n,n')) <$> find (isNamePair n) names) names
          notPairs = filter (\n -> all (\(n1,n2) -> n1 /= n && n2 /= n) pairs) names
          createNewName name = newUniqueName $ tidyNameOcc name $ nlambdaName $ nameOccName name

newUniqueName :: Name -> CoreM Name
newUniqueName name = do uniq <- getUniqueM
                        return $ setNameLoc (setNameUnique name uniq) noSrcSpan

getNameStr :: NamedThing a => a -> String
getNameStr = occNameString . nameOccName . getName

getModuleStr :: NamedThing a => a -> String
getModuleStr = maybe "" getModuleNameStr . nameModule_maybe . getName

getModuleNameStr :: Module -> String
getModuleNameStr = moduleNameString . moduleName

inCurrentModule :: NamedThing a => ModInfo -> a -> Bool
inCurrentModule mod x = mg_module (guts mod) == nameModule (getName x)

newUsedNames :: ModInfo -> NameSet -> NameSet
newUsedNames mod ns = mkNameSet (nms ++ nms')
    where nms = nameSetElems ns
          nms' = fmap (newName mod) $ filter (nameMember mod) nms

isNamePair :: Name -> Name -> Bool
isNamePair name nameWithPrefix = nameOccName nameWithPrefix == nlambdaName (nameOccName name)

newGlobalRdrEnv :: ModInfo -> GlobalRdrEnv -> GlobalRdrEnv
newGlobalRdrEnv mod gre = plusGlobalRdrEnv gre gre'
    where gre' = mkGlobalRdrEnv $ fmap newElem $ filter (nameMember mod . gre_name) $ globalRdrEnvElts gre
          newElem e = GRE (newName mod $ gre_name e) (newParent $ gre_par e) (gre_prov e)
          newParent NoParent = NoParent
          newParent (ParentIs n) = ParentIs (newName mod n)

----------------------------------------------------------------------------------------
-- Data constructors
----------------------------------------------------------------------------------------

getDataCons :: ModGuts -> [DataCon]
getDataCons = concatMap tyConDataCons . filter (not . isClassTyCon) .filter isAlgTyCon . mg_tcs

getDataConsVars :: ModGuts -> [Var]
getDataConsVars = concatMap dataConImplicitIds . getDataCons

getDataConsNames :: ModGuts -> [Name]
getDataConsNames = concatMap (\dc -> idName <$> dataConImplicitIds dc) . getDataCons

----------------------------------------------------------------------------------------
-- Variables
----------------------------------------------------------------------------------------

type VarMap = (Map Var Var, [Var])

emptyVarMap :: VarMap
emptyVarMap = (Map.empty, [])

varsWithPairs :: ModInfo -> Map Var Var
varsWithPairs = fst . varMap

allVars :: ModInfo -> [Var]
allVars mod = snd vm ++ Map.keys (fst vm) ++ Map.elems (fst vm)
    where vm = varMap mod

unionVarMaps :: VarMap -> VarMap -> VarMap
unionVarMaps (m1,l1) (m2,l2) = (Map.union m1 m2, nub $ l1 ++ l2)

newVar :: ModInfo -> Var -> Var
newVar mod v = Map.findWithDefault (pprPanic "unknown variable: " (pprV "v" v <+> ppr (Map.assocs map))) v map
    where map = varsWithPairs mod

varMapMember :: ModInfo -> Var -> Bool
varMapMember mod v = Map.member v $ varsWithPairs mod

mkVarMap :: ModInfo -> VarMap
mkVarMap mod = let g = guts mod in mkMapWithVars mod (getBindsVars g ++ getClassesVars g ++ getDataConsVars g)

mkMapWithVars :: ModInfo -> [Var] -> VarMap
mkMapWithVars mod vars = (Map.fromList (varsPairs ++ varsNewPairs), nub (varsWithoutPairs ++ varsFromExprs))
    where (notToChangeVars, toChangeVars) = partition isIgnoreVar vars
          varsPairs = mapMaybe (\v -> (\v' -> (v,v')) <$> find (isVarPair v) notToChangeVars) toChangeVars
          varsForPairs = filter notInPairs toChangeVars
          varsWithoutPairs = filter notInPairs notToChangeVars
          notInPairs v = all (\(v1,v2) -> v1 /= v && v2 /= v) varsPairs
          varsNewPairs = zip varsForPairs $ fmap newVar varsForPairs
          varsFromExprs = getAllVarsFromBinds mod \\ varsForPairs
          newVar v = let t = changeType mod $ varType v
                         v' = mkExportedLocalVar
                                (newIdDetails $ idDetails v)
                                (newName mod $ varName v)
                                (maybe t (\c -> addVarContextForHigherOrderClass mod c t) (userClassId mod v))
                                vanillaIdInfo
                     in if isGlobalId v && (isNothing (isDataConId_maybe v) || isJust (userClassConId mod v))
                        then globaliseId v'
                        else if isExportedId v then setIdExported v' else setIdNotExported v'
          newIdDetails (RecSelId tc naughty) = RecSelId (newTyCon mod tc) naughty
          newIdDetails (ClassOpId cls) = ClassOpId $ newClass mod cls
          newIdDetails (PrimOpId op) = PrimOpId op
          newIdDetails (FCallId call) = FCallId call
          newIdDetails (DFunId n b) = DFunId n b
          newIdDetails _ = VanillaId
          isIgnoreVar v = isVarDict mod v || isVarInstanceMethod v || hasMetaTyCon v || annotatedWithNoMetaFunction mod v || isNLambdaName (getNameStr v)

getAllVarsFromBinds :: ModInfo -> [Var]
getAllVarsFromBinds = nub . concatMap getAllNotLocalVarsFromExpr . concatMap rhssOfBind . mg_binds . guts

mkVarUnique :: Var -> CoreM Var
mkVarUnique v = do uniq <- getUniqueM
                   return $ setVarUnique v uniq

mkLocalVar :: String -> Type -> CoreM Var
mkLocalVar varName ty = do uniq <- getUniqueM
                           let nm = mkInternalName uniq (mkVarOcc varName) noSrcSpan
                           return $ mkLocalId nm ty

mkPredVar :: (Class, [Type]) -> CoreM DictId
mkPredVar (cls, tys) = do uniq <- getUniqueM
                          let name = mkSystemName uniq (mkDictOcc (getOccName cls))
                          return $ mkLocalId name $ mkClassPred cls tys

isVar :: CoreExpr -> Bool
isVar (Var _) = True
isVar _ = False

isVarPair :: Var -> Var -> Bool
isVarPair v1 v2 = isNamePair n1 n2 && nameModule n1 == nameModule n2
    where (n1, n2) = (varName v1, varName v2)

----------------------------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------------------------

getImportedMaps :: ModInfo -> (NameMap, VarMap, TyConMap)
getImportedMaps mod = (Map.fromList namePairs, (Map.fromList varPairs, varWithoutPair), (Map.fromList tcPairs, tcWithoutPair))
    where mods = eltsUFM $ hsc_HPT $ env mod
          things = eltsUFM $ getModulesTyThings mods
          (tcThings, varThings) = fmap (filter isTyThingId) $ partition isTyThingTyCon things
          tcPairs = fmap (\(tt1, tt2) -> (tyThingTyCon tt1, tyThingTyCon tt2)) $ catMaybes $ findPair things TyThingTyCon <$> getName <$> tcThings
          varPairs = fmap (\(tt1, tt2) -> (tyThingId tt1, tyThingId tt2)) $ catMaybes $ findPair things TyThingId <$> getName <$> varThings
          tcWithoutPair = (tyThingTyCon <$> tcThings) \\ (uncurry (++) $ unzip $ tcPairs)
          varWithoutPair = (tyThingId <$> varThings) \\ (uncurry (++) $ unzip $ varPairs)
          getNamePair (x, y) = (getName x, getName y)
          namePairs = (getNamePair <$> tcPairs) ++ (getNamePair <$> varPairs)

----------------------------------------------------------------------------------------
-- Exports
----------------------------------------------------------------------------------------

newExports :: ModInfo -> Avails -> Avails
newExports mod avls = concatMap go avls
    where go (Avail n) = if nameMember mod n then [Avail $ newName mod n] else []
          go (AvailTC nm nms) | nameMember mod nm = [AvailTC (newName mod nm) (newName mod <$> nms)]
          go (AvailTC _ nms) = (Avail . newName mod) <$> (filter (nameMember mod) nms)

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

type TyConMap = (Map TyCon TyCon, [TyCon])

emptyTcMap :: TyConMap
emptyTcMap = (Map.empty, [])

tcsWithPairs :: ModInfo -> Map TyCon TyCon
tcsWithPairs = fst . tcMap

allTcs :: ModInfo -> [TyCon]
allTcs mod = snd tcm ++ Map.keys (fst tcm) ++ Map.elems (fst tcm)
    where tcm = tcMap mod

allClasses :: ModInfo -> [Class]
allClasses mod = [c | Just c <- tyConClass_maybe <$> allTcs mod]

unionTcMaps :: TyConMap -> TyConMap -> TyConMap
unionTcMaps (m1, l1) (m2, l2) = (Map.union m1 m2, nub $ l1 ++ l2)

newTyCon :: ModInfo -> TyCon -> TyCon
newTyCon mod tc
    | isVarTyCon mod tc = tc
    | isClassTyCon tc = Map.findWithDefault metaPrelude tc $ fst $ tcMap mod
    | otherwise = tc
    where metaPrelude = fromMaybe
                          (pprPgmError "Unknown type constructor:"
                            (ppr tc <+> text ("\nProbably module " ++ getModuleStr tc ++ " is not compiled with NLambda Plugin or not Nominal.Meta implementation provided.")))
                          (getMetaEquivalentTyCon mod tc)

newClass :: ModInfo -> Class -> Class
newClass mod = fromJust . tyConClass_maybe . newTyCon mod . classTyCon

mkTyConMap :: ModInfo -> [TyCon] -> TyConMap
mkTyConMap mod tcs = let ctcs = filter isClassTyCon tcs
                         pairs = mapMaybe (\c -> fmap (\c' -> (c,c')) $ find (isTyConPair c) ctcs) ctcs
                         withoutPairs = filter notInPairs ctcs
                         notInPairs tc = all (\(tc1,tc2) -> tc1 /= tc && tc2 /= tc) pairs
                         withoutPairs' = fmap (newTyConClass mod {tcMap = tcMap}) withoutPairs
                         tcMap = (Map.fromList $ pairs ++ zip withoutPairs withoutPairs', filter isAlgTyCon tcs)
                     in tcMap

newTyConClass :: ModInfo -> TyCon -> TyCon
newTyConClass mod tc = let tc' = createTyConClass mod cls rhs tc
                           rhs = createAlgTyConRhs mod cls $ algTyConRhs tc
                           cls = createClass mod tc' $ fromJust $ tyConClass_maybe tc
                       in tc'

createTyConClass :: ModInfo -> Class -> AlgTyConRhs -> TyCon -> TyCon
createTyConClass mod cls rhs tc = mkClassTyCon
                                    (newName mod $ tyConName tc)
                                    (tyConKind tc)
                                    (tyConTyVars tc) -- FIXME new unique ty vars?
                                    (tyConRoles tc)
                                    rhs
                                    cls
                                    (if isRecursiveTyCon tc then Recursive else NonRecursive)

createAlgTyConRhs :: ModInfo -> Class -> AlgTyConRhs -> AlgTyConRhs
createAlgTyConRhs mod cls rhs = create rhs
    where create (AbstractTyCon b) = AbstractTyCon b
          create DataFamilyTyCon = DataFamilyTyCon
          create (DataTyCon dcs isEnum) = DataTyCon (createDataCon mod cls <$> dcs) isEnum
          create (NewTyCon dc ntRhs ntEtadRhs ntCo) = NewTyCon (createDataCon mod cls dc)
                                                               (changeType mod ntRhs)
                                                               (changeType mod <$> ntEtadRhs)
                                                               (changeUnbranchedCoAxiom mod cls ntCo)

createDataCon :: ModInfo -> Class -> DataCon -> DataCon
createDataCon mod cls dc =
    let name = newName mod $ dataConName dc
        workerName = newName mod $ idName $ dataConWorkId dc
        workerId = mkDataConWorkId workerName dc'
        (univ_tvs, ex_tvs, eq_spec, theta, arg_tys, res_ty) = dataConFullSig dc
        dc' = mkDataCon
                name
                (dataConIsInfix dc)
                []
                []
                univ_tvs
                ex_tvs
                ((\(tv, t) -> (tv, changeType mod t)) <$> eq_spec)
                (changePredType mod <$> theta)
                (addVarContextForHigherOrderClass mod cls <$> changeType mod <$> arg_tys)
                (changeType mod res_ty)
                (newTyCon mod $ dataConTyCon dc)
                (changePredType mod <$> dataConStupidTheta dc)
                workerId
                NoDataConRep -- FIXME use mkDataConRep
    in dc'

createClass :: ModInfo -> TyCon -> Class -> Class
createClass mod tc cls =
    let (tyVars, funDeps, scTheta, scSels, ats, opStuff) = classExtraBigSig cls
        scTheta' = fmap (uncurry mkClassPred . (\(c,ts) -> (newClass mod c, ts)) . getClassPredTys) scTheta
        scSels' = fmap (newVar mod) scSels
        opStuff' = fmap (\(v, dm) -> (newVar mod v, updateDefMeth dm)) opStuff
    in mkClass
         tyVars
         funDeps
         scTheta'
         scSels'
         ats -- FIXME new associated types?
         opStuff'
         (updateMinDef $ classMinimalDef cls)
         tc
    where updateDefMeth NoDefMeth = NoDefMeth
          updateDefMeth (DefMeth n) = DefMeth $ newName mod n
          updateDefMeth (GenDefMeth n) = GenDefMeth $ newName mod n
          updateMinDef (BF.Var n) = BF.Var $ newName mod n
          updateMinDef (BF.And fs) = BF.And $ updateMinDef <$> fs
          updateMinDef (BF.Or fs) = BF.Or $ updateMinDef <$> fs

getClassInstance :: ModInfo -> Class -> Maybe Type -> CoreExpr
getClassInstance mod cl t
     -- for instances in Meta module
    | Just (Var v) <- getMetaVarMaybe mod Nothing name, isClassInst v = Var v
     -- for instances in external modules
    | Just v <- find isClassInst $ fmap instanceDFunId $ instEnvElts $ eps_inst_env $ eps mod = Var v
     -- for instances in user modules
    | Just v <- find isClassInst $ fmap instanceDFunId $ mg_insts $ guts mod = Var v
    | Just v <- find isClassInst $ filter (isPrefixOf name . getNameStr) (allVars mod) = Var v
    | otherwise = pgmError ("NLambda plugin requires " ++ className ++ " instance"
                  ++ maybe "" (\tc -> " for type: " ++ tcName ++ " (from " ++ getModuleStr tc ++ ")") tcMaybe)
    where tcMaybe = maybe Nothing tyConAppTyCon_maybe t
          tcName = maybe "" getNameStr tcMaybe
          className = getNameStr cl
          name = "$f" ++ className ++ tcName
          isClassInst v = case splitTyConApp_maybe (getMainType $ varType v) of
                            Just (vTc, vTs) -> let vt = maybe Nothing tyConAppTyCon_maybe (listToMaybe vTs)
                                               in isDFunId v && Just cl == tyConClass_maybe vTc && tcMaybe == vt
                            _               -> False

findSuperClass :: Type -> [CoreExpr] -> Maybe CoreExpr
findSuperClass t (e:es)
    | Just (cl, ts) <- getClassPredTys_maybe (exprType e)
    = let (_, _, ids, _) = classBigSig cl
          es' = fmap (\i -> mkApps (Var i) (fmap Type ts ++ [e])) ids
      in find (eqType t . exprType) es' <|> findSuperClass t (es ++ es')
    | otherwise = findSuperClass t es
findSuperClass t [] = Nothing

newClassInstances :: ModInfo -> [ClsInst] -> [ClsInst]
newClassInstances mod is = catMaybes $ fmap new is
    where new i = let dFunId = instanceDFunId i
                      tc = classTyCon $ is_cls i
                  in if varMapMember mod dFunId
                     then Just $ mkImportedInstance
                                   (tyConName $ newTyCon mod tc)
                                   (instanceRoughTcs i)
                                   (newVar mod dFunId)
                                   (is_flag i)
                                   (is_orphan i)
                     else Nothing

userClassOpId :: ModInfo -> Id -> Maybe Class
userClassOpId mod v
    | Just cls <- isClassOpId_maybe v, inCurrentModule mod cls = Just cls
    | isPrefixOf classOpPrefix (getNameStr v) = lookup (getNameStr v) userClassMethods
    | otherwise = Nothing
    where classOpPrefix = "$c"
          modClasses = filter (inCurrentModule mod) $ allClasses mod
          userClassMethods = concatMap (\c -> fmap (\m -> (classOpPrefix ++ getNameStr m, c)) (classMethods c)) modClasses

userClassConId :: ModInfo -> Id -> Maybe Class
userClassConId mod v
    | isPrefixOf classConPrefix (getNameStr v) = lookup className userClasses
    | otherwise = Nothing
    where classConPrefix = "D:"
          className = drop (length classConPrefix) (getNameStr v)
          modClasses = filter (inCurrentModule mod) $ allClasses mod
          userClasses = fmap (\c -> (getNameStr c, c)) modClasses

userClassId :: ModInfo -> Id -> Maybe Class
userClassId mod v = msum [userClassOpId mod v, userClassConId mod v]

isTyConPair :: TyCon -> TyCon -> Bool
isTyConPair tc1 tc2 = isNamePair n1 n2 && nameModule n1 == nameModule n2
    where (n1, n2) = (getName tc1, getName tc2)

----------------------------------------------------------------------------------------
-- Binds
----------------------------------------------------------------------------------------

sortBinds :: CoreProgram -> CoreProgram
sortBinds = fmap (uncurry NonRec) . sortWith (getNameStr . fst) . flattenBinds

getBindsVars :: ModGuts -> [Var]
getBindsVars = bindersOfBinds . mg_binds

getBindsNames :: ModGuts -> [Name]
getBindsNames = fmap varName . getBindsVars

newBinds :: ModInfo -> [DataCon] -> CoreProgram -> CoreM CoreProgram
newBinds mod dcs bs = do bs' <- mapM (changeBind mod) $ filter withoutPair bs
                         bs'' <- mapM (dataBind mod) dcs
                         return $ simplifyBinds $ bs' ++ bs''
    where ids = fst <$> flattenBinds bs
          -- ids that are in var map and their pair has no bind
          idsWithoutPair = filter (\v -> notElem (newVar mod v) ids) $ filter (varMapMember mod) ids
          withoutPair (NonRec b e) = elem b idsWithoutPair
          withoutPair (Rec bs) = all (`elem` idsWithoutPair) $ fmap fst bs

changeBind :: ModInfo -> CoreBind -> CoreM CoreBind
changeBind mod (NonRec b e) = do (b',e') <- changeBindExpr mod (b, e)
                                 return (NonRec b' e')
changeBind mod (Rec bs) = do bs' <- mapM (changeBindExpr mod) bs
                             return (Rec bs')

changeBindExpr :: ModInfo -> (CoreBndr, CoreExpr) -> CoreM (CoreBndr, CoreExpr)
changeBindExpr mod (b, e) = do e' <- changeExpr mod e
                               let b' = newVar mod b
                               e'' <- convertMetaType mod e' $ varType b'
                               return (b', e'')

dataBind :: ModInfo -> DataCon -> CoreM CoreBind
dataBind mod dc
    | noAtomsType $ dataConOrigResTy dc = return $ NonRec b' (Var b)
    | otherwise = do e <- dataConExpr mod dc
                     e' <- convertMetaType mod e $ varType b'
                     return $ NonRec b' e'
    where b = dataConWrapId dc
          b' = newVar mod b

simplifyBinds :: CoreProgram -> CoreProgram
simplifyBinds bs = simplify <$> bs
    where simplify (NonRec b e) = NonRec b (simpleOptExpr e)
          simplify (Rec bs) = Rec $ (\(b,e) -> (b, simpleOptExpr e)) <$> bs

----------------------------------------------------------------------------------------
-- Type
----------------------------------------------------------------------------------------

changeBindTypeAndUniq :: ModInfo -> CoreBndr -> CoreM CoreBndr
changeBindTypeAndUniq mod x = mkVarUnique $ setVarType x $ changeType mod $ varType x

changeBindTypeUnderWithMetaAndUniq :: ModInfo -> CoreBndr -> CoreM CoreBndr
changeBindTypeUnderWithMetaAndUniq mod x = mkVarUnique $ setVarType x $ changeTypeUnderWithMeta mod $ varType x

changeType :: ModInfo -> Type -> Type
changeType mod = change
    where change t | noAtomsType t = t
          change t | isVoidTy t = t
          change t | isPrimitiveType t = t
          change t | isPredTy t = changePredType mod t
          change t | (Just (tv, t')) <- splitForAllTy_maybe t = mkForAllTy tv (change t')
          change t | (Just (t1, t2)) <- splitFunTy_maybe t = mkFunTy (change t1) (change t2)
          change t | (Just (t1, t2)) <- splitAppTy_maybe t
                   = withMetaType mod $ mkAppTy (changeTypeUnderWithMeta mod t1) (changeTypeUnderWithMeta mod t2)
          change t | (Just (tc, ts)) <- splitTyConApp_maybe t
                   = withMetaType mod $ mkTyConApp tc (changeTypeUnderWithMeta mod <$> ts)
          change t = withMetaType mod t

changeTypeUnderWithMeta :: ModInfo -> Type -> Type
changeTypeUnderWithMeta mod = change
    where change t | noAtomsType t = t
          change t | (Just (tv, t')) <- splitForAllTy_maybe t = mkForAllTy tv (change t')
          change t | (Just (t1, t2)) <- splitFunTy_maybe t = mkFunTy (changeType mod t1) (changeType mod t2)
          change t | (Just (t1, t2)) <- splitAppTy_maybe t = mkAppTy (change t1) (change t2)
          change t | (Just (tc, ts)) <- splitTyConApp_maybe t = mkTyConApp tc (change <$> ts)
          change t = t

changePredType :: ModInfo -> PredType -> PredType
changePredType mod t
    | (Just (tc, ts)) <- splitTyConApp_maybe t, isClassTyCon tc = mkTyConApp (newTyCon mod tc) (changeTypeUnderWithMeta mod <$> ts)
    | otherwise = t

changeTypeAndApply :: ModInfo -> CoreExpr -> Type -> CoreExpr
changeTypeAndApply mod e t
    | otherwise = (mkApp e . Type . change) t
    where (tyVars, eTy) = splitForAllTys $ exprType e
          tyVar = headPanic "changeTypeAndApply" (pprE "e" e) tyVars
          change t = if isFunTy t && isMonoType t then changeType mod t else t

addVarContextForHigherOrderClass :: ModInfo -> Class -> Type -> Type
addVarContextForHigherOrderClass mod cls t
    | all isMonoType $ mkTyVarTys $ classTyVars cls = t
    | Just (t1,t2) <- splitFunTy_maybe t = mkFunTy (addVarContextForHigherOrderClass mod cls t1) (addVarContextForHigherOrderClass mod cls t2)
    | null tvs = t
    | otherwise = mkForAllTys tvs $ mkFunTys (nub $ preds ++ preds') (addVarContextForHigherOrderClass mod cls t')
    where (tvs, preds, t') = tcSplitSigmaTy t
          preds' = fmap (varPredType mod) $ filter isMonoType $ mkTyVarTys tvs

getMainType :: Type -> Type
getMainType t = if t == t' then t else getMainType t'
    where (tvs, ps, t') = tcSplitSigmaTy t

getFunTypeParts :: Type -> [Type]
getFunTypeParts t
    | t /= getMainType t = getFunTypeParts $ getMainType t
    | not $ isFunTy t = [t]
    | otherwise = argTys ++ [resTy]
    where (argTys, resTy) = splitFunTys t

isWithMetaType :: ModInfo -> Type -> Bool
isWithMetaType mod t
    | Just (tc, _) <- splitTyConApp_maybe (getMainType t) = tc == withMetaC mod
    | otherwise = False

isNotWithMetaType :: ModInfo -> Type -> Bool
isNotWithMetaType mod = not . isWithMetaType mod

getWithoutWithMetaType :: ModInfo -> Type -> Maybe Type
getWithoutWithMetaType mod t
    | isWithMetaType mod t, Just ts <- tyConAppArgs_maybe t = Just $ headPanic "getWithoutWithMetaType" (ppr t) ts
    | otherwise = Nothing

getAllPreds :: Type -> ThetaType
getAllPreds t
    | null preds = []
    | otherwise = preds ++ getAllPreds t'
    where (preds, t') = tcSplitPhiTy $ dropForAlls t

getCommonPreds :: Type -> Type -> ThetaType
getCommonPreds t1 t2
    | length tvs1 > 0, length tvs1 == length tvs2 = intersect preds1 preds2' ++ getCommonPreds t1'' t2''
    | otherwise = []
    where (tvs1, t1') = splitForAllTys t1
          (tvs2, t2') = splitForAllTys t2
          (preds1, t1'') = tcSplitPhiTy t1'
          (preds2, t2'') = tcSplitPhiTy t2'
          preds2' = substTys (substFromLists tvs2 tvs1) preds2

isInternalType :: Type -> Bool
isInternalType t = let t' = getMainType t in isVoidTy t' || isPredTy t' || isPrimitiveType t' || isUnLiftedType t'

isMonoType :: Type -> Bool
isMonoType = not . isFunTy . typeKind

isAnyType :: Type -> Bool
isAnyType = maybe False ((== anyTyConKey) . getUnique) . tyConAppTyCon_maybe

----------------------------------------------------------------------------------------
-- Checking type contains atoms
----------------------------------------------------------------------------------------

noAtomsType :: Type -> Bool
noAtomsType t | hasNestedFunType t = False
noAtomsType t = noAtomsTypeVars [] [] t
    where noAtomsTypeVars :: [TyCon] -> [TyVar] -> Type -> Bool
          noAtomsTypeVars tcs vs t | Just t' <- coreView t = noAtomsTypeVars tcs vs t'
          noAtomsTypeVars tcs vs (TyVarTy v) = elem v vs
          noAtomsTypeVars tcs vs (AppTy t1 t2) = noAtomsTypeVars tcs vs t1 && noAtomsTypeVars tcs vs t2
          noAtomsTypeVars tcs vs (TyConApp tc ts) = noAtomsTypeCon tcs tc (length ts) && (all (noAtomsTypeVars tcs vs) ts)
          noAtomsTypeVars tcs vs (FunTy t1 t2) = noAtomsTypeVars tcs vs t1 && noAtomsTypeVars tcs vs t2
          noAtomsTypeVars tcs vs (ForAllTy v t) = False
          noAtomsTypeVars tcs vs (LitTy _ ) = True
          -- tcs - for recursive definitions, n - number of applied args to tc
          noAtomsTypeCon :: [TyCon] -> TyCon -> Int -> Bool
          noAtomsTypeCon tcs tc _ | elem tc tcs = True
          noAtomsTypeCon _   tc _ | isAtomsTypeName tc = False
          noAtomsTypeCon _   tc _ | isClassTyCon tc = False -- classes should be replaced by meta equivalent
          noAtomsTypeCon _   tc _ | isPrimTyCon tc = True
          noAtomsTypeCon tcs tc n | isDataTyCon tc || isNewTyCon tc = all (noAtomsTypeVars (nub $ tc : tcs) $ take n $ tyConTyVars tc)
                                                                          (concatMap dataConOrigArgTys $ tyConDataCons tc)
          noAtomsTypeCon _ _ _ = True
          isAtomsTypeName :: TyCon -> Bool
          isAtomsTypeName tc = let nm = tyConName tc in getNameStr nm == "Variable" && moduleNameString (moduleName $ nameModule nm) == varModuleName

hasNestedFunType :: Type -> Bool
hasNestedFunType (TyVarTy v) = False
hasNestedFunType (AppTy _ t) = isFunTy t || hasNestedFunType t
hasNestedFunType (TyConApp _ ts) = any (\t -> isFunTy t || hasNestedFunType t) ts
hasNestedFunType (FunTy t1 t2) = hasNestedFunType t1 || hasNestedFunType t2
hasNestedFunType (ForAllTy _ t) = hasNestedFunType t
hasNestedFunType (LitTy _ ) = False

----------------------------------------------------------------------------------------
-- Type thing
----------------------------------------------------------------------------------------

data TyThingType = TyThingId | TyThingTyCon | TyThingConLike | TyThingCoAxiom deriving (Show, Eq)

tyThingType :: TyThing -> TyThingType
tyThingType (AnId _) = TyThingId
tyThingType (ATyCon _) = TyThingTyCon
tyThingType (AConLike _) = TyThingConLike
tyThingType (ACoAxiom _) = TyThingCoAxiom

isTyThingId :: TyThing -> Bool
isTyThingId = (== TyThingId) . tyThingType

isTyThingTyCon :: TyThing -> Bool
isTyThingTyCon = (== TyThingTyCon) . tyThingType

getModulesTyThings :: [HomeModInfo] -> TypeEnv
getModulesTyThings = mconcat . fmap md_types . fmap hm_details

findThingByConds :: (Name -> Name -> Bool) -> (Module -> Module -> Bool) -> [TyThing] -> TyThingType -> Name -> Maybe TyThing
findThingByConds nameCond moduleCond things ty name = find cond things
    where sameType = (== ty) . tyThingType
          cond t = sameType t && nameCond name (getName t) && moduleCond (nameModule name) (nameModule $ getName t)

findThing :: [TyThing] -> TyThingType -> Name -> Maybe TyThing
findThing = findThingByConds (==) (==)

findPairThing :: [TyThing] -> TyThingType -> Name -> Maybe TyThing
findPairThing things ty name = findThingByConds isNamePair equivalentModule things ty name
    where equivalentModule m1 m2 = m1 == m2 || getMetaEquivalentModule (getModuleNameStr m1) == getModuleNameStr m2

findPair :: [TyThing] -> TyThingType -> Name -> Maybe (TyThing, TyThing)
findPair things ty name = (,) <$> findThing things ty name <*>  findPairThing things ty name

findThingByNameStr :: [TyThing] -> TyThingType -> Maybe ModuleName -> MetaName -> Maybe TyThing
findThingByNameStr things ty modName name = find cond things
    where sameType = (== ty) . tyThingType
          cond t = sameType t && getNameStr t == name && maybe True (== getModuleStr t) modName

----------------------------------------------------------------------------------------
-- Expressions map
----------------------------------------------------------------------------------------

type ExprMap = Map Var CoreExpr

mkExprMap :: ModInfo -> ExprMap
mkExprMap = Map.map Var . varsWithPairs

getExpr :: ExprMap -> Var -> CoreExpr
getExpr map v = Map.findWithDefault (pprPanic "no expression for variable: " (ppr v <+> ppr (Map.assocs map))) v map

insertVarExpr :: Var -> Var -> ExprMap -> ExprMap
insertVarExpr v v' = Map.insert v (Var v')

insertVarExprs :: [(Var, Var)] -> ExprMap -> ExprMap
insertVarExprs vs = Map.union (Map.fromList $ fmap (fmap Var) vs)

insertExpr :: Var -> CoreExpr -> ExprMap -> ExprMap
insertExpr = Map.insert

----------------------------------------------------------------------------------------
-- Expressions
----------------------------------------------------------------------------------------

changeExpr :: ModInfo -> CoreExpr -> CoreM CoreExpr
changeExpr mod e = newExpr (mkExprMap mod) e
    where newExpr :: ExprMap -> CoreExpr -> CoreM CoreExpr
          newExpr eMap e | noAtomsSubExpr e = replaceVars mod eMap e
          newExpr eMap (Var v) | Map.member v eMap = addMockedInstances mod $ getExpr eMap v
          newExpr eMap (Var v) | isVarDict mod v = return $ Var v
          newExpr eMap (Var v) | hasMetaEquivalent mod v = getMetaEquivalent mod v
          newExpr eMap (Var v) = pprPgmError "Unknown variable:"
            (showVar v <+> text "::" <+> ppr (varType v) <+> text ("\nProbably module " ++ getModuleStr v ++ " is not compiled with NLambda Plugin or not Nominal.Meta implementation provided."))
          newExpr eMap (Lit l) = noMetaExpr mod (Lit l)
          newExpr eMap (App f (Type t)) = do f' <- newExpr eMap f
                                             return $ changeTypeAndApply mod f' t
          newExpr eMap (App f e) = do f' <- newExpr eMap f
                                      e' <- newExpr eMap e
                                      appWithMock mod f' e'
          newExpr eMap (Lam x e) | isTKVar x = do e' <- newExpr eMap e
                                                  return $ Lam x e' -- FIXME new uniq for x (and then replace all occurrences)?
          newExpr eMap (Lam x e) = do x' <- changeBindTypeAndUniq mod x
                                      e' <- newExpr (insertVarExpr x x' eMap) e
                                      return $ Lam x' e'
          newExpr eMap (Let b e) = do (b', eMap') <- newLetBind b eMap
                                      e' <- newExpr eMap' e
                                      return $ Let b' e'
          newExpr eMap (Case e b t as) = do e' <- newExpr eMap e
                                            e'' <- if isWithMetaType mod $ exprType e'
                                                   then valueExpr mod e'
                                                   else return e'
                                            m <- if isWithMetaType mod $ exprType e'
                                                   then metaExpr mod e'
                                                   else return $ emptyMetaV mod
                                            b' <- changeBindTypeUnderWithMetaAndUniq mod b
                                            be <- createExpr mod (Var b') m
                                            let t' = changeType mod t
                                            as' <- mapM (newAlternative (insertExpr b be eMap) m t') as
                                            return $ Case e'' b' t' as'
          newExpr eMap (Cast e c) = do e' <- newExpr eMap e
                                       let c' = changeCoercion mod c
                                       e'' <- convertMetaType mod e' $ pFst $ coercionKind c'
                                       return $ Cast e'' c'
          newExpr eMap (Tick t e) = do e' <- newExpr eMap e
                                       return $ Tick t e'
          newExpr eMap (Type t) = pprPanic "type should be served in (App f (Type t)) case" (ppr t)
          newExpr eMap (Coercion c) = pprPanic "coercion should be served in (Cast e c) case" (ppr c)
          newLetBind (NonRec b e) eMap = do b' <- changeBindTypeAndUniq mod b
                                            let eMap' = insertVarExpr b b' eMap
                                            e' <- newExpr eMap' e
                                            e'' <- convertMetaType mod e' $ varType b'
                                            return (NonRec b' e'', eMap')
          newLetBind (Rec bs) eMap = do let (xs, es) = unzip bs
                                        xs' <- mapM (changeBindTypeAndUniq mod) xs
                                        let eMap' = insertVarExprs (zip xs xs') eMap
                                        es' <- mapM (newExpr eMap') es
                                        es'' <- mapM (\(x,e) -> convertMetaType mod e $ varType x) (zip xs' es')
                                        return (Rec $ zip xs' es'', eMap')
          newAlternative eMap m t (DataAlt con, xs, e) = do xs' <- mapM (changeBindTypeUnderWithMetaAndUniq mod) xs
                                                            es <- mapM (\x -> if (isFunTy $ varType x) then return $ Var x else createExpr mod (Var x) m) xs'
                                                            e' <- newExpr (Map.union (Map.fromList $ zip xs es) eMap) e
                                                            e'' <- convertMetaType mod e' t
                                                            return (DataAlt con, xs', e'')
          newAlternative eMap m t (alt, [], e) = do e' <- newExpr eMap e
                                                    e'' <- convertMetaType mod e' t
                                                    return (alt, [], e'')

-- the type of expression is not open for atoms and there are no free variables open for atoms
noAtomsSubExpr :: CoreExpr -> Bool
noAtomsSubExpr e = (noAtomsType $ exprType e) && noAtomFreeVars
    where noAtomFreeVars = isEmptyUniqSet $ filterUniqSet (not . noAtomsType . varType) $ exprFreeIds e

replaceVars :: ModInfo -> ExprMap -> CoreExpr -> CoreM CoreExpr
replaceVars mod eMap e = replace e
    where replace (Var x) | isLocalId x, Just e <- Map.lookup x eMap = convertMetaType mod e $ varType x
          replace (App f e) = do f' <- replace f
                                 e' <- replace e
                                 return $ mkApp f' e'
          replace (Lam x e) = do e' <- replace e
                                 return $ Lam x e'
          replace (Let b e) = do b' <- replaceInBind b
                                 e' <- replace e
                                 return $ Let b' e'
          replace (Case e x t as) = do e' <- replace e
                                       as' <- mapM replaceInAlt as
                                       return $ Case e' x t as'
          replace (Cast e c) = do e' <- replace e
                                  return $ Cast e' c
          replace (Tick t e) = do e' <- replace e
                                  return $ Tick t e'
          replace e = return e
          replaceInBind (NonRec x e) = do e' <- replace e
                                          return $ NonRec x e'
          replaceInBind (Rec bs) = do bs' <- mapM replaceInRecBind bs
                                      return $ Rec bs'
          replaceInRecBind (b, e) = do e' <- replace e
                                       return (b, e')
          replaceInAlt (con, bs, e) = do e' <- replace e
                                         return (con, bs, e')

getAllNotLocalVarsFromExpr :: CoreExpr -> [Var]
getAllNotLocalVarsFromExpr = nub . get
    where get (Var x) = [x]
          get (Lit l) = []
          get (App f e) = get f ++ get e
          get (Lam x e) = delete x $ get e
          get (Let b e) = filter (`notElem` (bindersOf b)) (get e ++ concatMap getAllNotLocalVarsFromExpr (rhssOfBind b))
          get (Case e x t as) = delete x (get e ++ concatMap getFromAlt as)
          get (Cast e c) = get e
          get (Tick t e) = get e
          get (Type t) = []
          getFromAlt (con, bs, e) = filter (`notElem` bs) (get e)

dataConExpr :: ModInfo -> DataCon -> CoreM CoreExpr
dataConExpr mod dc
    | arity == 0 = noMetaExpr mod dcv
    | arity == 1 = idOpExpr mod dcv
    | otherwise = do (vars, ty, subst) <- splitTypeToExprVarsWithSubst $ exprType dcv
                     xs <- mkArgs subst arity
                     let (vvs, evs) = (exprVarsToVars vars, exprVarsToExprs vars)
                     ra <- renameAndApplyExpr mod arity
                     ra' <- applyExpr ra (mkApps dcv evs)
                     return $ mkCoreLams (vvs ++ xs) $ mkApps ra' (Var <$> xs)
    where arity = dataConSourceArity dc
          dcv = Var $ dataConWrapId dc
          mkArgs subst 0 = return []
          mkArgs subst n = do let ty = substTy subst $ withMetaType mod $ dataConOrigArgTys dc !! (arity - n)
                              x <- mkLocalVar ("x" ++ show (arity - n)) ty
                              args <- mkArgs subst $ pred n
                              return $ x : args

----------------------------------------------------------------------------------------
-- Coercion
----------------------------------------------------------------------------------------

changeCoercion :: ModInfo -> Coercion -> Coercion
changeCoercion mod c = change True c
    where change topLevel (Refl r t) = Refl r $ changeTy topLevel t
          change topLevel (TyConAppCo r tc cs) = TyConAppCo r (newTyCon mod tc) (change False <$> cs)
          change topLevel (ForAllCo tv c) = ForAllCo tv (change topLevel c)
          change topLevel (CoVarCo cv) = let (t1,t2) = coVarKind cv in CoVarCo $ mkCoVar (coVarName cv) (mkCoercionType (coVarRole cv) t1 t2)
          change topLevel (UnivCo n r t1 t2) = UnivCo n r (changeTy topLevel t1) (changeTy topLevel t2)
          change topLevel (TransCo c1 c2) = TransCo (change topLevel c1) (change topLevel c2)
          change topLevel (AxiomRuleCo a ts cs) = AxiomRuleCo a (changeTy topLevel <$> ts) (change topLevel <$> cs)
          change topLevel (NthCo i c) = NthCo i $ change topLevel c
          change topLevel (LRCo lr c) = LRCo lr $ change topLevel c
          change topLevel (InstCo c t) = InstCo (change topLevel c) (changeTy topLevel t)
          change topLevel (SubCo c) = SubCo $ change topLevel c
          change topLevel c = let (c', addWithMeta) = changeWithMeta topLevel c
                              in if addWithMeta then withMetaCoercion c' else c'
          changeWithMeta topLevel (AppCo c1 c2) = let (c1', addWithMeta) = changeWithMeta topLevel c1
                                                  in (AppCo c1' (change False c2), addWithMeta)
          changeWithMeta topLevel (AxiomInstCo a i cs) = (AxiomInstCo (changeBranchedCoAxiom mod a) i (change False <$> cs), isDataTypeCoAxiom a)
          changeWithMeta topLevel (SymCo c) = let (c', addWithMeta) = changeWithMeta topLevel c in (SymCo c', addWithMeta)
          changeWithMeta topLevel c = (change topLevel c, False)
          changeTy topLevel = if topLevel then changeType mod else changeTypeUnderWithMeta mod
          withMetaCoercion c = mkTyConAppCo Representational (withMetaC mod) [c]

changeBranchedCoAxiom :: ModInfo -> CoAxiom Branched -> CoAxiom Branched
changeBranchedCoAxiom mod = changeCoAxiom mod toBranchList Nothing

changeUnbranchedCoAxiom :: ModInfo -> Class -> CoAxiom Unbranched -> CoAxiom Unbranched
changeUnbranchedCoAxiom mod cls = changeCoAxiom mod toUnbranchedList $ Just cls

changeCoAxiom :: ModInfo -> ([CoAxBranch] -> BranchList CoAxBranch a) -> Maybe Class -> CoAxiom a -> CoAxiom a
changeCoAxiom mod toList cls (CoAxiom u n r tc bs imp) = CoAxiom u n r (newTyCon mod tc) (changeBranchList mod toList cls bs) imp

changeBranchList :: ModInfo -> ([CoAxBranch] -> BranchList CoAxBranch a) -> Maybe Class -> BranchList CoAxBranch a -> BranchList CoAxBranch a
changeBranchList mod toList cls = toList . fmap (changeCoAxBranch mod cls) . fromBranchList

changeCoAxBranch :: ModInfo -> Maybe Class -> CoAxBranch -> CoAxBranch
changeCoAxBranch mod cls (CoAxBranch loc tvs roles lhs rhs incpoms)
    = CoAxBranch
        loc
        tvs
        roles
        (changeTypeUnderWithMeta mod <$> lhs)
        (newRhs cls)
        (changeCoAxBranch mod cls <$> incpoms)
    where rhs' = changeTypeUnderWithMeta mod rhs
          newRhs (Just c) = addVarContextForHigherOrderClass mod c rhs'
          newRhs _ = rhs'

toUnbranchedList :: [CoAxBranch] -> BranchList CoAxBranch Unbranched
toUnbranchedList [b] = FirstBranch b
toUnbranchedList _ = pprPanic "toUnbranchedList" empty

isDataTypeCoAxiom :: CoAxiom a -> Bool
isDataTypeCoAxiom a = let tc = coAxiomTyCon a in isDataTyCon tc || isNewTyCon tc && not (isClassTyCon tc)

updateCoercionType :: Type -> Coercion -> Coercion
updateCoercionType = update True
    where update left t c | t == (if left then pFst else pSnd) (coercionKind c) = c
          update left t (Refl r t') = Refl r t
          update left t (SymCo c) = SymCo $ update (not left) t c
           -- TODO update cs after try unify
          update left t (AxiomInstCo a i cs) = AxiomInstCo (updateCoAxiomType left t i a) i cs
          -- TODO other cases
          update left t c = c

updateCoAxiomType :: Bool -> Type -> BranchIndex -> CoAxiom Branched -> CoAxiom Branched
updateCoAxiomType left t i a@(CoAxiom u n r tc bs imp)
    | left, Just (tc', ts) <- splitTyConApp_maybe t, tc == tc' = axiom $ updateCoAxBranchesType left ts i bs
    | left = pprPanic "updateCoAxiomType" (text "inconsistent type:" <+> ppr t <+> text "with co axiom:" <+> ppr a)
    | otherwise = axiom $ updateCoAxBranchesType left [t] i bs
    where axiom bs = CoAxiom u n r tc bs imp

updateCoAxBranchesType :: Bool -> [Type] -> BranchIndex -> BranchList CoAxBranch Branched -> BranchList CoAxBranch Branched
updateCoAxBranchesType left ts i bs = toBranchList $ fmap update $ zip (fromBranchList bs) [0..]
    where update (b, bi) = if bi == i then updateCoAxBranchType left ts b else b

updateCoAxBranchType :: Bool -> [Type] -> CoAxBranch -> CoAxBranch
updateCoAxBranchType True ts (CoAxBranch loc tvs roles lhs rhs incpoms) = CoAxBranch loc tvs roles ts rhs incpoms
updateCoAxBranchType False [t] (CoAxBranch loc tvs roles lhs rhs incpoms) = CoAxBranch loc tvs roles lhs t incpoms

----------------------------------------------------------------------------------------
-- Core program validation
----------------------------------------------------------------------------------------

checkCoreProgram :: CoreProgram -> CoreProgram
checkCoreProgram bs = if all checkBinds bs then bs else pprPanic "checkCoreProgram failed" (ppr bs)
    where checkBinds (NonRec b e) = checkBind (b,e)
          checkBinds (Rec bs) = all checkBind bs
          checkBind (b,e) | varType b /= exprType e
                          = pprPanic "\n================= INCONSISTENT TYPES IN BIND ==========================="
                              (vcat [text "bind: " <+> showVar b,
                                     text "bind type:" <+> ppr (varType b),
                                     text "expr:" <+> ppr e,
                                     text "expr type:" <+> ppr (exprType e),
                                     text "\n=======================================================================|"])
          checkBind (b,e) = checkExpr (b,e) e
          checkExpr be (Var v) = True
          checkExpr be (Lit l) = True
          checkExpr be (App f (Type t)) | not $ isForAllTy $ exprType f
                                        = pprPanic "\n================= NOT FUNCTION IN APPLICATION ========================="
                                            (vcat [text "fun expr: " <+> ppr f,
                                                   text "fun type: " <+> ppr (exprType f),
                                                   text "arg: " <+> ppr t,
                                                   text "for bind: " <+> ppr (fst be) <+> text "::" <+> ppr (varType $ fst be),
                                                   text "bind expr:" <+> ppr (snd be) <+> text "::" <+> ppr (exprType $ snd be),
                                                   text "\n=======================================================================|"])
          checkExpr be (App f (Type t)) = checkExpr be f
          checkExpr be (App f x) | not $ isFunTy $ exprType f
                                 = pprPanic "\n================= NOT FUNCTION IN APPLICATION ========================="
                                     (vcat [text "fun expr: " <+> ppr f,
                                            text "fun type: " <+> ppr (exprType f),
                                            text "arg: " <+> ppr x,
                                            text "arg type: " <+> ppr (exprType x),
                                            text "for bind: " <+> ppr (fst be) <+> text "::" <+> ppr (varType $ fst be),
                                            text "bind expr:" <+> ppr (snd be) <+> text "::" <+> ppr (exprType $ snd be),
                                           text "\n=======================================================================|"])
          checkExpr be (App f x) | funArgTy (exprType f) /= exprType x
                                 = pprPanic "\n================= INCONSISTENT TYPES IN APPLICATION ===================="
                                     (vcat [text "fun: " <+> ppr f,
                                            text "fun type: " <+> ppr (exprType f),
                                            text "fun arg type: " <+> ppr (funArgTy $ exprType f),
                                            text "arg: " <+> ppr x,
                                            text "arg type: " <+> ppr (exprType x),
                                            text "for bind: " <+> ppr (fst be) <+> text "::" <+> ppr (varType $ fst be),
                                            text "bind expr:" <+> ppr (snd be) <+> text "::" <+> ppr (exprType $ snd be),
                                           text "\n=======================================================================|"])
          checkExpr be (App f x) = checkExpr be f && checkExpr be x
          checkExpr be (Lam x e) = checkExpr be e
          checkExpr be (Let x e) = checkBinds x && checkExpr be e
          checkExpr be (Case e x t as) = checkBind (x,e) && all (checkAlternative be t) as && all (checkAltConType be $ exprType e) as
          checkExpr be (Cast e c) | exprType e /= pFst (coercionKind c)
                                  = pprPanic "\n================= INCONSISTENT TYPES IN CAST ==========================="
                                     (vcat [text "expr: " <+> ppr e,
                                            text "expr type: " <+> ppr (exprType e),
                                            text "coercion: " <+> ppr c,
                                            text "coercion: " <+> showCoercion c,
                                            text "coercion type: " <+> ppr (coercionType c),
                                            text "for bind: " <+> ppr (fst be) <+> text "::" <+> ppr (varType $ fst be),
                                            text "bind expr:" <+> ppr (snd be) <+> text "::" <+> ppr (exprType $ snd be),
                                            text "\n=======================================================================|"])
          checkExpr be (Cast e c) = checkExpr be e
          checkExpr be (Tick t e) = checkExpr be e
          checkExpr be (Type t) = pprPanic "type should be handled in (App f (Type t)) case" (ppr t)
          checkExpr be (Coercion c) = True
          checkAlternative be t (ac, xs, e) | t /= exprType e
                                            = pprPanic "\n================= INCONSISTENT TYPES IN CASE ALTERNATIVE ==============="
                                                (vcat [text "type in case: " <+> ppr t,
                                                       text "case alternative expression: " <+> ppr e,
                                                       text "case alternative expression type: " <+> ppr (exprType e),
                                                       text "for bind: " <+> ppr (fst be) <+> text "::" <+> ppr (varType $ fst be),
                                                       text "bind expr:" <+> ppr (snd be) <+> text "::" <+> ppr (exprType $ snd be),
                                                       text "\n=======================================================================|"])
          checkAlternative be t (ac, xs, e) = checkExpr be e
          checkAltConType be t (DataAlt dc, xs, e) = checkAltConTypes be (ppr dc) (dataConOrigResTy dc) t xs -- FIXME better evaluation of pattern type
          checkAltConType be t (LitAlt l, xs, e) = checkAltConTypes be (ppr l) (literalType l) t xs
          checkAltConType be _ _ = True
          checkAltConTypes be conDoc pt vt xs | not $ canUnifyTypes pt vt
                                              = pprPanic "\n========== INCONSISTENT TYPES IN CASE ALTERNATIVE PATTERN =============="
                                                  (vcat [text "type of value: " <+> ppr vt,
                                                         text "case alternative constructor: " <+> conDoc,
                                                         text "case alternative arguments: " <+> hcat ((\x -> ppr x <+> text "::" <+> ppr (varType x)) <$> xs),
                                                         text "case alternative pattern type: " <+> ppr pt,
                                                         text "for bind: " <+> ppr (fst be) <+> text "::" <+> ppr (varType $ fst be),
                                                         text "bind expr:" <+> ppr (snd be) <+> text "::" <+> ppr (exprType $ snd be),
                                                         text "\n=======================================================================|"])
          checkAltConTypes be conDoc pt vt xs = True

checkCoreLint :: ModInfo -> CoreProgram -> CoreProgram
checkCoreLint mod bs = if isEmptyBag err then bs else pprPanic "checkCoreLint" (pprMessageBag err)
    where (warn, err) = lintCoreBindings (metaPlugin $ env mod) [] bs -- TODO show warn

----------------------------------------------------------------------------------------
-- Apply expression
----------------------------------------------------------------------------------------

data ExprVar = TV TyVar | DI DictId

instance Outputable ExprVar where
    ppr (TV v) = ppr v
    ppr (DI i) = ppr i

isTV :: ExprVar -> Bool
isTV (TV _) = True
isTV (DI _) = False

substTy :: TvSubst -> Type -> Type
substTy subst t = headPanic "substTy" (ppr subst <+> ppr t) (substTys subst [t])

substFromLists :: [TyVar] -> [TyVar] -> TvSubst
substFromLists tvs = mkTopTvSubst . zip tvs . mkTyVarTys

substDictId :: TvSubst -> DictId -> DictId
substDictId subst id = setVarType id ty
    where (cl, ts) = getClassPredTys $ varType id
          ts' = substTys subst ts
          ty = mkClassPred cl ts'

exprVarsToVarsWithSubst :: TvSubst -> [ExprVar] -> [CoreBndr]
exprVarsToVarsWithSubst subst = catMaybes . fmap toCoreBndr
    where toCoreBndr (TV v) | isNothing (lookupTyVar subst v) = Just v
          toCoreBndr (TV v) = Nothing
          toCoreBndr (DI i) = Just $ substDictId subst i

exprVarsToVars :: [ExprVar] -> [CoreBndr]
exprVarsToVars = exprVarsToVarsWithSubst emptyTvSubst

exprVarsToExprsWithSubst :: TvSubst -> [ExprVar] -> [CoreExpr]
exprVarsToExprsWithSubst subst = fmap toExpr
    where toExpr (TV v) = Type $ substTyVar subst v
          toExpr (DI i) = Var $ substDictId subst i

exprVarsToExprs :: [ExprVar] -> [CoreExpr]
exprVarsToExprs = exprVarsToExprsWithSubst emptyTvSubst

splitTypeToExprVarsWithSubst :: Type -> CoreM ([ExprVar], Type, TvSubst)
splitTypeToExprVarsWithSubst ty
    | ty == ty' = return ([], ty, emptyTvSubst)
    | otherwise = do tyVars' <- mapM mkTyVarUnique tyVars
                     let subst = substFromLists tyVars tyVars'
                     let preds' = filter isClassPred preds
                     let classTys = fmap getClassPredTys preds'
                     predVars <- mapM mkPredVar ((\(c,tys) -> (c, substTys subst tys)) <$> classTys)
                     (resTyVars, resTy, resSubst) <- splitTypeToExprVarsWithSubst $ substTy subst ty'
                     return ((TV <$> tyVars') ++ (DI <$> predVars) ++ resTyVars, resTy, unionTvSubst subst resSubst)
    where (tyVars, preds, ty') = tcSplitSigmaTy ty
          mkTyVarUnique v = do uniq <- getUniqueM
                               return $ mkTyVar (setNameUnique (tyVarName v) uniq) (tyVarKind v)

splitTypeToExprVars :: Type -> CoreM ([ExprVar], Type)
splitTypeToExprVars t = liftM (\(vars, ty, _) -> (vars, ty)) (splitTypeToExprVarsWithSubst t)

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
canUnifyTypes t1 t2 = isJust $ unifyTypes (dropForAlls t1) (dropForAlls t2)

applyExpr :: CoreExpr -> CoreExpr -> CoreM CoreExpr
applyExpr fun e =
    do (eVars, eTy) <- splitTypeToExprVars $ exprType e
       (fVars, fTy) <- if isPredTy eTy
                       then return $ (\(tvs, t) -> (TV <$> tvs, t)) (splitForAllTys $ exprType fun)
                       else splitTypeToExprVars $ exprType fun
       let subst = fromMaybe
                     (pprPanic "applyExpr - can't unify:" (ppr (funArgTy fTy) <+> text "and" <+> ppr eTy <+> text "for apply:" <+> ppr fun <+> text "with" <+> ppr e))
                     (unifyTypes (funArgTy fTy) eTy)
       let fVars' = exprVarsToVarsWithSubst subst fVars
       let fVarExprs = exprVarsToExprsWithSubst subst fVars
       let eVars' = exprVarsToVarsWithSubst subst eVars
       let eVarExprs = exprVarsToExprsWithSubst subst eVars
       return $ mkCoreLams (sortVars [] $ reverse $ nub $ fVars' ++ eVars')
                  (mkApp
                    (mkApps fun fVarExprs)
                    (mkApps e eVarExprs))
    where sortVars res (v:vs)
            | not $ isTyVar v, Just i <- findIndex (\x -> isTyVar x && elemVarSet x (tyVarsOfType $ varType v)) res
            = let (vs1, vs2) = splitAt i res in sortVars (vs1 ++ [v] ++ vs2) vs
            | otherwise = sortVars (v:res) vs
          sortVars res [] = res

applyExprs :: CoreExpr -> [CoreExpr] -> CoreM CoreExpr
applyExprs = foldlM applyExpr

mkAppOr :: CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr
mkAppOr f x ifNotMatch
    | isTypeArg x, not $ isForAllTy $ exprType f = ifNotMatch
    | not $ isTypeArg x, not $ isFunTy $ exprType f = ifNotMatch
    | not $ isTypeArg x, funArgTy (exprType f) /= exprType x = ifNotMatch
    | otherwise = mkCoreApp f x

mkApp :: CoreExpr -> CoreExpr -> CoreExpr
mkApp f x = mkAppOr f x $ pprPanic "mkApp - inconsistent types:" (pprE "f" f <+> text "," <+> pprE "x" x)

mkAppIfMatch :: CoreExpr -> CoreExpr -> CoreExpr
mkAppIfMatch f x = mkAppOr f x f

mkApps :: CoreExpr -> [CoreExpr] -> CoreExpr
mkApps = foldl mkApp

mkAppsIfMatch :: CoreExpr -> [CoreExpr] -> CoreExpr
mkAppsIfMatch = foldl mkAppIfMatch

----------------------------------------------------------------------------------------
-- Meta
----------------------------------------------------------------------------------------

type ModuleName = String
type MetaName = String
type MetaModule = HomeModInfo

varModuleName :: ModuleName
varModuleName = "Nominal.Variable"

metaModuleName :: ModuleName
metaModuleName = "Nominal.Meta"

metaModuleNames :: [ModuleName]
metaModuleNames = varModuleName : metaModuleName : modules

getMetaModules :: HscEnv -> [MetaModule]
getMetaModules = filter ((`elem` metaModuleNames) . moduleNameString . moduleName . mi_module . hm_iface) . eltsUFM . hsc_HPT

getMetaVarMaybe :: ModInfo -> Maybe ModuleName -> MetaName -> Maybe CoreExpr
getMetaVarMaybe mod mn vn = (Var . tyThingId) <$> findThingByNameStr (metaTyThings mod) TyThingId mn vn

getMetaTyConMaybe :: ModInfo -> ModuleName -> MetaName -> Maybe TyCon
getMetaTyConMaybe mod mn tn = tyThingTyCon <$> findThingByNameStr (metaTyThings mod) TyThingTyCon (Just mn) tn

getMetaTyCon :: ModInfo -> ModuleName -> MetaName -> TyCon
getMetaTyCon mod mn tn = fromMaybe (pprPanic "No meta type constructor" (text mn <+> text tn)) (getMetaTyConMaybe mod mn tn)

metaTyThings :: ModInfo -> [TyThing]
metaTyThings = nameEnvElts . getModulesTyThings . metaModules

getMetaModuleVar :: MetaName -> ModInfo -> CoreExpr
getMetaModuleVar var mod = fromMaybe (pprPanic "No var in Meta module:" (ppr var)) (getMetaVarMaybe mod (Just metaModuleName) var)

noMetaV :: ModInfo -> CoreExpr
noMetaV = getMetaModuleVar "noMeta"

metaV :: ModInfo -> CoreExpr
metaV = getMetaModuleVar "meta"

valueV :: ModInfo -> CoreExpr
valueV = getMetaModuleVar "value"

createV :: ModInfo -> CoreExpr
createV = getMetaModuleVar "create"

emptyMetaV :: ModInfo -> CoreExpr
emptyMetaV = getMetaModuleVar "emptyMeta"

idOpV :: ModInfo -> CoreExpr
idOpV = getMetaModuleVar "idOp"

renameAndApplyV :: ModInfo -> Int -> CoreExpr
renameAndApplyV mod n = getMetaModuleVar ("renameAndApply" ++ show n) mod

withMetaC :: ModInfo -> TyCon
withMetaC mod = getMetaTyCon mod metaModuleName "WithMeta"

metaLevelC :: ModInfo -> TyCon
metaLevelC mod = getMetaTyCon mod metaModuleName "MetaLevel"

varC :: ModInfo -> TyCon
varC mod = getMetaTyCon mod varModuleName "Var"

hasMetaTyCon :: Var -> Bool
hasMetaTyCon = anyNameEnv ((`elem` metaModuleName : modules) . getModuleNameStr . nameModule . getName) . tyConsOfType . varType

withMetaType :: ModInfo -> Type -> Type
withMetaType mod t = mkTyConApp (withMetaC mod) [t]

varPredType :: ModInfo -> Type -> Type
varPredType mod t = mkTyConApp (varC mod) [t]

isVarTyCon :: ModInfo -> TyCon -> Bool
isVarTyCon mod tc = varC mod == tc

isVarDict :: ModInfo -> Var -> Bool
isVarDict mod v | isDFunId v, Just tc <- tyConAppTyCon_maybe $ getMainType $ varType v = isVarTyCon mod tc
isVarDict mod v = False

isVarInstanceMethod :: Var -> Bool
isVarInstanceMethod v = elem (getNameStr v) ["$cfoldVariables", "$cmapVariables", "$crenameVariables"]

noMetaExpr :: ModInfo -> CoreExpr -> CoreM CoreExpr
noMetaExpr mod e | isInternalType $ exprType e = return e
noMetaExpr mod e = applyExpr (noMetaV mod) e

valueExpr :: ModInfo -> CoreExpr -> CoreM CoreExpr
valueExpr mod e | not $ isWithMetaType mod $ exprType e = return e
valueExpr mod e = applyExpr (valueV mod) e

metaExpr :: ModInfo -> CoreExpr -> CoreM CoreExpr
metaExpr mod e = applyExpr (metaV mod) e

createExpr :: ModInfo -> CoreExpr -> CoreExpr -> CoreM CoreExpr
createExpr mod e  _  | isInternalType $ exprType e = return e
createExpr mod e1 e2 = applyExprs (createV mod) [e1, e2]

idOpExpr :: ModInfo -> CoreExpr -> CoreM CoreExpr
idOpExpr mod e = applyExpr (idOpV mod) e

renameAndApplyExpr :: ModInfo -> Int -> CoreM CoreExpr
renameAndApplyExpr mod n = addMockedInstances mod (renameAndApplyV mod n)

----------------------------------------------------------------------------------------
-- Convert meta types
----------------------------------------------------------------------------------------

convertMetaType :: ModInfo -> CoreExpr -> Type -> CoreM CoreExpr
convertMetaType mod e t
    | et == t = return e
    | isForAllTy t, isForAllTy et, tyVarsKinds t == tyVarsKinds et = do (vs, _, subst) <- splitTypeToExprVarsWithSubst t
                                                                        let (vvs, evs) = (exprVarsToVars vs, exprVarsToExprs vs)
                                                                        e' <- addMockedInstances mod $ mkAppsIfMatch e evs
                                                                        e'' <- convertMetaType mod e' $ substTy subst $ getMainType t
                                                                        return $ mkCoreLams vvs e''
    | not $ null (getAllPreds et \\ getAllPreds t)
    = do e' <- addMockedInstancesExcept mod (getAllPreds t) e
         if not $ null (getAllPreds (exprType e') \\ getAllPreds t)
         then pprPanic "convertMetaType - invalid number of predicates" (pprE "e" e <+> pprE "e'" e' <+> text "|" <+> ppr t)
         else convertMetaType mod e' t
    | isClassPred t, Just e' <- findSuperClass t [e] = return e'
    | Just t' <- getWithoutWithMetaType mod t, isNotWithMetaType mod et = do e' <- convertMetaType mod e t'
                                                                             noMetaExpr mod e'
    | isWithMetaType mod et, isNotWithMetaType mod t = do e' <- valueExpr mod e
                                                          convertMetaType mod e' t
    | isFunTy t, isFunTy et = do let (arg, res) = splitFunTy t
                                 x <- mkLocalVar "x" arg
                                 ex <- convertMetaType mod (Var x) (funArgTy et)
                                 let e' = mkApp e ex
                                 e'' <- convertMetaType mod e' res
                                 return $ Lam x e''
    | otherwise = pprPanic "convertMetaType" (text "can't convert (" <+> pprE "e" e <+> text ") to type:" <+> ppr t)
    where et = exprType e
          tyVarsKinds = fmap tyVarKind . fst . splitForAllTys

----------------------------------------------------------------------------------------
-- Meta Equivalents
----------------------------------------------------------------------------------------

getMetaPreludeNameMap :: ModInfo -> NameMap
getMetaPreludeNameMap mod = Map.fromList $ catMaybes metaPairs
    where fromPrelude e | Imported ss <- gre_prov e = not $ null $ intersect metaModuleNames (moduleNameString <$> is_mod <$> is_decl <$> ss)
          fromPrelude e = False
          preludeNames = fmap gre_name $ filter fromPrelude $ concat $ occEnvElts $ mg_rdr_env $ guts mod
          preludeNamesWithTypes = fmap (\n -> if isLower $ headPanic "getMetaPreludeNameMap" (ppr n) (getNameStr n)
                                              then (TyThingId, n)
                                              else (TyThingTyCon, n)) preludeNames
          metaPairs = fmap (\(ty, n) -> (\t -> (n, getName t)) <$> findPairThing (metaTyThings mod) ty n) preludeNamesWithTypes

isMetaPreludeTyCon :: ModInfo -> TyCon -> Bool
isMetaPreludeTyCon mod tc = any (\t -> getName t == getName tc && isTyThingTyCon t) $ metaTyThings mod

getMetaEquivalentModule :: ModuleName -> ModuleName
getMetaEquivalentModule moduleName = metaModuleName ++ "." ++ moduleName

hasMetaEquivalent :: NamedThing a => ModInfo -> a -> Bool
hasMetaEquivalent mod x = elem (getMetaEquivalentModule $ getModuleStr x) modules

getMetaEquivalent :: ModInfo -> Var -> CoreM CoreExpr
getMetaEquivalent mod v
    | Just v' <- getMetaVarMaybe mod (Just metaModule) metaName = addMockedInstances mod v'
    | hasMetaEquivalent mod v = pprPanic ("getMetaEquivalent - no meta equivalent variable " ++ metaName ++ " in module " ++ metaModule) (pprV "v" v)
    | otherwise = pprPanic ("getMetaEquivalent - no meta equivalent module " ++ metaModule) (pprV "v" v)
    where metaModule = getMetaEquivalentModule $ getModuleStr v
          metaName = occNameString $ nlambdaName $ nameOccName $ getName v

getMetaEquivalentTyCon :: ModInfo -> TyCon -> Maybe TyCon
getMetaEquivalentTyCon mod tc = tyThingTyCon <$> findPairThing (metaTyThings mod) TyThingTyCon (getName tc)

----------------------------------------------------------------------------------------
-- Mock class instances
----------------------------------------------------------------------------------------

isPredicateWith :: ModInfo -> (TyCon -> Bool) -> Type -> Bool
isPredicateWith mod cond t
    | Just tc <- tyConAppTyCon_maybe t, isClassTyCon tc = cond tc
    | otherwise = False

isPredicate :: ModInfo -> Type -> Bool
isPredicate mod = isPredicateWith mod $ const True

isPredicateForMock :: ModInfo -> Type -> Bool
isPredicateForMock mod = isPredicateWith mod (\tc -> isVarTyCon mod tc || metaLevelC mod == tc || hasMetaEquivalent mod tc || isMetaPreludeTyCon mod tc)

mockInstance :: Type -> CoreExpr
mockInstance t = (mkApp (Var uNDEFINED_ID) . Type) t

isMockInstance :: ModInfo -> CoreExpr -> Bool
isMockInstance mod (App (Var x) (Type t)) = uNDEFINED_ID == x && maybe True (not . isInternalRep) (listToMaybe $ snd $ getClassPredTys t)
    where isInternalRep t
            | isAnyType t = True
            | Just tc <- tyConAppTyCon_maybe t, isMonoType t = isAbstractTyCon tc -- for empty datatypes generated for Generics
            | otherwise = False
isMockInstance _ _ = False

addMockedInstances :: ModInfo -> CoreExpr -> CoreM CoreExpr
addMockedInstances mod = addMockedInstancesExcept mod []

addMockedInstancesExcept :: ModInfo -> [PredType] -> CoreExpr -> CoreM CoreExpr
addMockedInstancesExcept mod exceptTys e
    = do (vs, ty, subst) <- splitTypeToExprVarsWithSubst $ exprType e
         let exceptTys' = substTy subst <$> exceptTys
         let (vvs, evs) = (exprVarsToVars vs, exprVarsToExprs vs)
         let vvs' = filter (\v -> isTypeVar v || not (forMock exceptTys' $ varType v)) vvs
         let evs' = mockPredOrDict exceptTys' <$> evs
         let e' = mkApps e evs'
         let argTys = init $ getFunTypeParts $ exprType e'
         args <- mkMockedArgs (length argTys) argTys
         let (xs, es) = unzip args
         return $ if all isVar es then mkCoreLams vvs' e' else mkCoreLams (vvs' ++ xs) $ mkApps e' es
    where -- isTypeArg checked before call exprType on e
          forMock exceptTys t = isPredicateForMock mod t && notElem t exceptTys
          mockPredOrDict exceptTys e = if not (isTypeArg e) && forMock exceptTys (exprType e) then mockInstance (exprType e) else e
          mkMockedArgs n (t:ts) = do x <- mkLocalVar ("x" ++ show (n - length ts)) (typeWithoutDictPreds t)
                                     (vs, t') <- splitTypeToExprVars t
                                     let (vvs, evs) = (exprVarsToVars vs, exprVarsToExprs vs)
                                     let evs' = filter (\ev -> isTypeArg ev || not (isPredicateForMock mod $ exprType ev)) evs
                                     let e = if length evs == length evs' then Var x else mkCoreLams vvs $ mkApps (Var x) evs'
                                     args <- mkMockedArgs n ts
                                     return ((x, e) : args)
          mkMockedArgs _ [] = return []
          typeWithoutDictPreds t
              | Just (tv, t') <- splitForAllTy_maybe t = mkForAllTy tv $ typeWithoutDictPreds t'
              | Just (t1, t2) <- splitFunTy_maybe t = if isPredicateForMock mod t1
                                                      then typeWithoutDictPreds t2
                                                      else mkFunTy t1 $ typeWithoutDictPreds t2
              | otherwise = t


appWithMock :: ModInfo -> CoreExpr -> CoreExpr -> CoreM CoreExpr
appWithMock mod f e = do f' <- if isWithMetaType mod (exprType f) && not (isTypeArg e) then valueExpr mod f else return f
                         appFunWithMock mod f' e

appFunWithMock :: ModInfo -> CoreExpr -> CoreExpr -> CoreM CoreExpr
appFunWithMock mod f e
    | isTypeArg e, isForAllTy fTy = return $ mkApp f e
    | isTypeArg e = pprPanic "appFunWithMock - isTypeArg" (pprE "f" f <+> pprE "e" e)
    | not (isFunTy fTy), dictExpr = return f
    | not (isFunTy fTy) = pprPanic "appFunWithMock - not isFunTy" (pprE "f" f <+> pprE "e" e)
    | isForAllTy fTy, dictExpr = return f
    | isForAllTy fTy = pprPanic "appFunWithMock - isForAllTy" (pprE "f" f <+> pprE "e" e)
    | argTy == eTy = return $ mkApp f e
    | argTy == getMainType eTy = do e' <- addMockedInstances mod e
                                    appWithMock mod f e'
    | isJust $ findSuperClass argTy [e], willMatch = convertApp
    | isJust $ findSuperClass argTy [e],
      tc1 <- fromJust $ tyConAppTyCon_maybe argTy,
      tc2 <- fromJust $ tyConAppTyCon_maybe eTy,
      isNamePair (getName tc1) (getName tc2) = convertApp
    | dictArg, not dictExpr || willMatch = appWithMock mod (mkApp f $ mockInstance argTy) e
    | not dictArg, dictExpr = return f
    | dictArg || dictExpr = pprPanic "appFunWithMock - isDict" (pprE "f" f <+> pprE "e" e)
    | otherwise = convertApp
    where fTy = exprType f
          argTy = funArgTy fTy
          eTy = exprType e
          dictArg = isFunTy fTy && isPredicateForMock mod argTy
          dictExpr = isPredicateForMock mod eTy
          match t = t == eTy || isJust (findSuperClass t [e])
          willMatch = any match $ safeTail $ getAllPreds fTy
          safeTail [] = []
          safeTail xs = tail xs
          convertApp = do e' <- convertMetaType mod e argTy
                          return $ mkApp f e'

----------------------------------------------------------------------------------------
-- Replace mocked class instances with real ones
----------------------------------------------------------------------------------------

type DictInstances = [(Type, DictId)]
type ReplaceVars = [(CoreBndr, CoreBndr)]

data ReplaceInfo = ReplaceInfo {dictInstances :: DictInstances, replaceBinds :: ReplaceVars, nextReplaceBinds :: ReplaceVars, mocksSize :: (Int,Int)}

instance Outputable ReplaceInfo where
    ppr (ReplaceInfo dis rb nrb nm) = text "ReplaceInfo{" <+> ppr dis <+> text ","
                                      <+> vcat (fmap (\(x,y) -> ppr x <+> ppr (varType x) <+> text "~>" <+> ppr (varType y)) rb) <+> text ","
                                      <+> vcat (fmap (\(x,y) -> ppr x <+> ppr (varType x) <+> text "~>" <+> ppr (varType y)) nrb) <+> text ","
                                      <+> ppr nm <+> text "}"

emptyReplaceInfo :: ReplaceInfo
emptyReplaceInfo = ReplaceInfo [] [] [] (0,0)

noMoreReplaceBinds :: ReplaceInfo -> Bool
noMoreReplaceBinds = null . nextReplaceBinds

noDictInstances :: ReplaceInfo -> Bool
noDictInstances = null . dictInstances

findDictInstance :: Type -> ReplaceInfo -> Maybe DictId
findDictInstance t = lookup t . dictInstances

addDictInstance :: (Type, DictId) -> ReplaceInfo -> ReplaceInfo
addDictInstance (t, v) ri = ri {dictInstances = (t, v) : (dictInstances ri)}

addBindToReplace :: (CoreBndr, CoreBndr) -> ReplaceInfo -> ReplaceInfo
addBindToReplace (b, b') ri = ri {nextReplaceBinds = (b, b') : (nextReplaceBinds ri)}

findReplaceBind :: ModInfo -> Var -> ReplaceInfo -> Maybe Var
findReplaceBind mod x = fmap snd . find (\(x',_) -> x == x' && varType x == varType x') . replaceBinds

nextReplaceInfo :: ReplaceInfo -> ReplaceInfo
nextReplaceInfo ri = ReplaceInfo [] (replaceBinds ri ++ nextReplaceBinds ri) [] (mocksSize ri)

newMocksSize :: Int -> ReplaceInfo -> ReplaceInfo
newMocksSize size ri = let (n,m) = mocksSize ri in ri {mocksSize = (size, if n == size then m + 1 else 0)}

-- TODO change to hasMocks :: ... -> Bool
getMocks :: ModInfo -> CoreProgram -> [Type]
getMocks mod = nub . concatMap get . fmap snd . flattenBinds
    where get e | isMockInstance mod e = [exprType e]
          get (App f e) = get f ++ get e
          get (Lam x e) = get e
          get (Let b e) = getMocks mod [b] ++ get e
          get (Case e x t as) = get e ++ concatMap getAlt as
          get (Cast e c) = get e
          get (Tick t e) = get e
          get _ = []
          getAlt (con, bs, e) = get e

replaceMocksByInstancesInProgram :: ModInfo -> CoreProgram -> CoreM CoreProgram
replaceMocksByInstancesInProgram mod bs = go bs emptyReplaceInfo
    where go bs ri = do (bs', ri') <- replace ri bs
                        let bs'' = checkCoreProgram bs'
                        let mocks = getMocks mod bs''
                        putMsg $ ppWhen (snd (mocksSize ri') > 10) (text "FIX POINT:" <+> ppr mocks) -- FIXME
                        if noMoreReplaceBinds ri' && (null mocks || snd (mocksSize ri') > 10) -- TODO check actual replaces (not only empty new added)
                        then return bs''
                        else go bs'' $ nextReplaceInfo $ newMocksSize (length mocks) ri'
          replace ri (b:bs) = do (bs', ri') <- replace ri bs
                                 (b', ri'') <- replaceMocksByInstancesInBind mod (b, [], ri')
                                 if noDictInstances ri''
                                 then return (b':bs', ri'')
                                 else pprPanic "replaceMocksByInstancesInProgram - not empty var instances to insert:" (ppr ri'' <+> ppr b')
          replace ri [] = return ([], ri)

-- args: mod info, (bind, dict instances from surrounding expression, replace info)
replaceMocksByInstancesInBind :: ModInfo -> (CoreBind, DictInstances, ReplaceInfo) -> CoreM (CoreBind, ReplaceInfo)
replaceMocksByInstancesInBind mod (b, dis, ri) = replace b dis ri
    where replace (NonRec b e) dis ri = do ((b', e'), ri') <- replaceBind (b, e) dis ri
                                           return (NonRec b' e', ri')
          replace (Rec bs) dis ri = do (bs', ri') <- replaceBinds bs dis ri
                                       return (Rec bs', ri')
          replaceBinds (b:bs) dis ri = do (bs', ri') <- replaceBinds bs dis ri
                                          (b', ri'') <- replaceBind b dis ri'
                                          return (b':bs', ri'')
          replaceBinds [] dis ri = return ([], ri)
          replaceBind (b, e) dis ri = do (e', ri') <- replaceMocksByInstancesInExpr mod (e, dis, ri)
                                         if varType b == exprType e'
                                         then return ((b, e'), ri')
                                         else let b' = setVarType b $ exprType e'
                                              in return ((b', e'), addBindToReplace (b, b') ri')

-- args: mod info, (expression, dict instances from surrounding expression, replace info)
replaceMocksByInstancesInExpr :: ModInfo -> (CoreExpr, DictInstances, ReplaceInfo) -> CoreM (CoreExpr, ReplaceInfo)
replaceMocksByInstancesInExpr mod (e, dis, ri) = replace e dis ri
    where replace e dis ri | isMockInstance mod e = replaceMock (exprType e) dis ri
          replace e dis ri | (tvs, e') <- collectTyBinders e, not (null tvs)
                           = do (e'', ri') <- replace e' dis ri
                                let (dis1, dis2) = partition (\(t,di) -> any (`elemVarSet` (tyVarsOfType t)) tvs) (dictInstances ri')
                                let dicts = filter (\(t,di) -> notElem t $ getAllPreds $ exprType e'') dis1
                                return (mkCoreLams (tvs ++ fmap snd dicts) e'', ri' {dictInstances = dis2})
          replace (Var x) dis ri | Just x' <- findReplaceBind mod x ri
                                 = do x'' <- addMockedInstancesExcept mod (getCommonPreds (varType x') (varType x)) (Var x')
                                      return (x'', ri)
          replace (App f x) dis ri = do (f', ri') <- replace f dis ri
                                        (x', ri'') <- replace x dis ri'
                                        e <- appWithMock mod f' x'
                                        return (e, ri'')
          replace (Lam x e) dis ri = do let dis' = if isPredicate mod $ varType x then (varType x, x) : dis else dis
                                        (e', ri') <- replace e dis' ri
                                        return (Lam x e', ri')
          replace (Let b e) dis ri = do (b', ri') <- replaceMocksByInstancesInBind mod (b, dis, ri)
                                        (e', ri'') <- replace e dis ri'
                                        return (Let b' e', ri'')
          replace (Case e x t as) dis ri = do (e', ri') <- replace e dis ri
                                              (as', ri'') <- replaceAlts as dis ri'
                                              return (Case e' x t as', ri'')
          replace (Cast e c) dis ri = do (e', ri') <- replace e dis ri
                                         let c' = updateCoercionType (exprType e') c
                                         return (Cast e' c', ri')
          replace (Tick t e) dis ri = do (e', ri') <- replace e dis ri
                                         return (Tick t e', ri')
          replace e dis ri = return (e, ri)
          replaceAlts (a:as) dis ri = do (as', ri') <- replaceAlts as dis ri
                                         (a', ri'') <- replaceAlt a dis ri'
                                         return (a':as', ri'')
          replaceAlts [] dis ri = return ([], ri)
          replaceAlt (con, xs, e) dis ri = do (e', ri') <- replace e dis ri
                                              return ((con, xs, e'), ri')
          replaceMock t dis ri
              | Just v <- lookup t dis = return (Var v, ri)
              | isPredicate mod t, Just v <- findDictInstance t ri = return (Var v, ri)
              | Just di <- dictInstance mod t = do di' <- addMockedInstancesExcept mod [] di
                                                   return (di', ri)
              | Just sc <- findSuperClass t (Var <$> snd <$> dis) = return (sc, ri)
              | isPredicate mod t = do v <- mkPredVar $ getClassPredTys t
                                       return (Var v, addDictInstance (t, v) ri)
              | otherwise = pprPanic "replaceMock - can't create class instance for:" (ppr t <+> ppr dis <+> ppr ri)

dictInstance :: ModInfo -> Type -> Maybe CoreExpr
dictInstance mod t
    | isNothing tMaybe = Just inst
    | Just (tc, ts') <- splitTyConApp_maybe t' = Just $ mkApps inst (Type <$> ts')
    | isJust (isNumLitTy t') || isJust (isStrLitTy t') = Just inst
    | isFunTy t' = Just inst
    | otherwise = Nothing
    where Just (cl, ts) = getClassPredTys_maybe t
          tMaybe = listToMaybe ts
          inst = getClassInstance mod cl tMaybe
          t' = fromMaybe (pprPanic "dictInstance" (ppr t)) tMaybe

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
showType (LitTy l) = text "LitTy(" <> ppr l <> text ")"

showTyCon :: TyCon -> SDoc
showTyCon = ppr
--showTyCon tc = showName (tyConName tc)
--    <> text "{"
--    <> ppr (tyConKind tc)
--    <> ppr (tyConArity tc)
--    <> ppr (tyConParent tc)
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
--showVar = ppr
showVar v = text "["
            <> showName (varName v)
            <+> ppr (varUnique v)
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
          show (TyConAppCo role tyCon cs) = text "TyConAppCo" <+> ppr role <+> ppr tyCon <+> vcat (showCoercion <$> cs)
          show (AppCo c1 c2) = text "AppCo" <+> showCoercion c1 <+> showCoercion c2
          show (ForAllCo tyVar c) = text "ForAllCo"
          show (CoVarCo coVar) = text "CoVarCo"
          show (AxiomInstCo coAxiom branchIndex cs) = text "AxiomInstCo" <+> showCoAxiom coAxiom <+> ppr branchIndex <+> vcat (fmap showCoercion cs)
          show (UnivCo fastString role type1 type2) = text "UnivCo"
          show (SymCo c) = text "SymCo" <+> showCoercion c
          show (TransCo c1 c2) = text "TransCo"
          show (AxiomRuleCo coAxiomRule types cs) = text "AxiomRuleCo"
          show (NthCo int c) = text "NthCo"
          show (LRCo leftOrRight c) = text "LRCo"
          show (InstCo c typ) = text "InstCo"
          show (SubCo c) = text "SubCo"

showCoAxiom :: CoAxiom br -> SDoc
showCoAxiom (CoAxiom u n r tc bs imp) =
    text "'Axiom "
    <+> ppr u
    <+> ppr n
    <+> ppr r
    <+> ppr tc
    <+> vcat (showCoAxiomBranch <$> fromBranchList bs)
    <+> ppr imp
    <+> text "'"

showCoAxiomBranch :: CoAxBranch -> SDoc
showCoAxiomBranch (CoAxBranch loc tvs roles lhs rhs incpoms) =
    text "'AxiomBranch "
    <+> ppr loc
    <+> ppr tvs
    <+> ppr roles
    <+> ppr lhs
    <+> ppr rhs
    <+> vcat (showCoAxiomBranch <$> incpoms)
    <+> text "'"

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

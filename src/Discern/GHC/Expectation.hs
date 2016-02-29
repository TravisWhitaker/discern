module Discern.GHC.Expectation where

import Control.Monad.Trans

import Data.List (find)

import qualified Data.Map.Strict as M

import Data.Maybe (fromJust)

import Discern.Expectation
import Discern.GHC.Type
import Discern.Report
import Discern.Type

import qualified Exception       as G
import qualified InstEnv         as G
import qualified Panic           as G
import qualified GHC             as G
import qualified GHC.Paths       as G
import qualified Name            as G

import Unsafe.Coerce

getTyThing :: String -> G.Ghc (Maybe G.TyThing)
getTyThing n = do
    ns <- G.parseName n
    case ns
        of []    -> return Nothing
           (n:_) -> G.lookupName n

getClsInsts :: String -> G.Ghc [G.ClsInst]
getClsInsts n = do
    ns <- G.parseName n
    case ns of []    -> return []
               (n:_) -> do i <- G.getInfo True n
                           case i of Nothing               -> return []
                                     (Just (_, _, cis, _)) -> return cis

tyConTyVarRep :: ExTyConTyVars -> [G.TyVar] -> TyConTyVarRep
tyConTyVarRep (ExTyConArity n) gtvs
    | n == length gtvs = TyConTyVarOK
    | otherwise        = TyConTyVarWrongArity n (length gtvs)
tyConTyVarRep (ExTyConVarNames ns) gtvs =
    let gns = map (G.occNameString . G.getOccName) gtvs
    in if ns == gns
       then TyConTyVarOK
       else TyConTyVarWrongNames ns gns

unDataCon :: G.DataCon -> (String, Either String Type)
unDataCon dc = ((G.occNameString (G.getOccName dc)), ghcDataConTypeToType dc)

-- | The 'ExDataCon' 'Type' must be normalized.
dataConRep :: [G.DataCon] -> ExDataCon -> DataConRep
dataConRep gdcs (ExDataCon n t) =
    let gdcs' = map unDataCon gdcs
        mnm   = find (\(gn, _) -> gn == n) gdcs'
    in case mnm
            of Nothing                -> DataConAbsent n
               (Just (_, (Left e)))   -> DataConWrongType n t (Left e)
               (Just (_, (Right gt))) -> if t == gt
                                         then DataConOK n
                                         else DataConWrongType n t (Right gt)

typeReport :: ExExport -> G.TyCon -> ExReport
typeReport (ExType n vs ds) gtc =
    let tvr  = tyConTyVarRep vs (G.tyConTyVars gtc)
        dcrs = map (dataConRep (G.tyConDataCons gtc)) ds
        stat = if tyConTyVarOK tvr && dataConsOK dcrs
               then ExportCorrect
               else ExportWrong
    in TypeReport n stat tvr dcrs
typeReport _ _ = error "typeReport can only be called on ExTypes"

classTyVarRep :: ExClassTyVars -> [G.TyVar] -> ClassTyVarRep
classTyVarRep (ExClassArity n) gtvs
    | n == length gtvs = ClassTyVarOK
    | otherwise        = ClassTyVarWrongArity n (length gtvs)
classTyVarRep (ExClassVarNames ns) gtvs =
    let gns = map (G.occNameString . G.getOccName) gtvs
    in if ns == gns
       then ClassTyVarOK
       else ClassTyVarWrongNames ns gns

unMeth :: G.Id -> (String, Either String Type)
unMeth i = ((G.occNameString (G.getOccName i)), ghcClassMethTypeToType i)

classMethRep :: [G.Id] -> ExClassMeth -> ClassMethRep
classMethRep gis (ExClassMeth n t) =
    let gms = map unMeth gis
        mmt = find (\(gn, _) -> gn == n) gms
    in case mmt
            of Nothing                -> ClassMethAbsent n
               (Just (_, (Left e)))   -> ClassMethWrongType n t (Left e)
               (Just (_, (Right gt))) -> if t == gt
                                         then ClassMethOK n
                                         else ClassMethWrongType n t (Right gt)

classReport :: ExExport -> G.TyCon -> ExReport
classReport (ExClass n vs ms) gtc
    | G.isClassTyCon gtc = let tvr  = classTyVarRep vs (G.tyConTyVars gtc)
                               cls  = fromJust (G.tyConClass_maybe gtc)
                               cmrs = map (classMethRep (G.classMethods cls)) ms
                               stat = if classTyVarOK tvr && classMethsOK cmrs
                                      then ExportCorrect
                                      else ExportWrong
                           in ClassReport n stat tvr cmrs
    | otherwise          = ClassReport n ExportAbsent undefined undefined
classReport _ _ = error "classReport can only be called on ExClasses"

checkInstance :: [Type] -> G.ClsInst -> Bool
checkInstance ts ci = let tvs = map ghcTyVarToTyVar (G.is_tvs ci)
                          cts = (map (normalizeScopedType tvs)) <$> (mapM ghcTypeToType (G.is_tys ci))
                      in case cts of (Left _)    -> False
                                     (Right gts) -> ts == gts

symbolTypeRep :: String -> Type -> G.Ghc SymbolTypeRep
symbolTypeRep n t = do
    et <- exprType n
    case et of (Left e)   -> return (SymbolTypeWrong t (Left e))
               (Right gt) -> return (if typeEq gt t
                                     then SymbolTypeOK
                                     else SymbolTypeWrong t (Right gt))

symbolCheck :: String -> SymbolTypeRep -> [TestRep] -> ExReport
symbolCheck n str trs = let stat
                              | symbolTypeOK str && testsOK trs = ExportCorrect
                              | otherwise                       = ExportWrong
                        in SymbolReport n stat str trs

symbolRunTests :: String -> G.Ghc [TestRep]
symbolRunTests n = (unsafeCoerce <$> G.compileExpr ("runTestTree" ++ n)) >>= liftIO

symbolReport :: ExExport -> G.Ghc ExReport
symbolReport (ExSymbol n t ts) = do
    ct <- symbolTypeRep n t
    if symbolTypeOK ct then symbolCheck n ct <$> symbolRunTests ts
                       else return (SymbolReport n ExportWrong ct undefined)
symbolReport _ = error "symbolReport can only be called on ExSymbols"

-- | Assumes the relevant modules and tests are in scope.
runExExport :: ExExport -> G.Ghc ExReport
runExExport e@(ExType n _ _)   = do
    tyt <- getTyThing n
    case tyt of (Just (G.ATyCon t)) -> return $ typeReport e t
                _                   -> return $ TypeReport n ExportAbsent undefined undefined
runExExport e@(ExClass n _ _)  = do
    tyt <- getTyThing n
    case tyt of (Just (G.ATyCon t)) -> return $ classReport e t
                _                   -> return $ ClassReport n ExportAbsent undefined undefined
runExExport (ExInstance cn ts) = do
    cls <- getClsInsts cn
    return $ if any (checkInstance ts) cls
             then InstanceReport ExportCorrect cn ts
             else InstanceReport ExportAbsent cn ts
runExExport e@(ExSymbol n t ts) = do
    tyt <- getTyThing n
    case tyt of (Just _) -> symbolReport e
                _        -> return $ SymbolReport n ExportAbsent undefined undefined

loadMod :: String -> G.Ghc Bool
loadMod n = do
    G.guessTarget n Nothing >>= G.addTarget
    G.load G.LoadAllTargets
    cx <- G.getContext
    G.handleGhcException (\_ -> return False)
                         (G.setContext ((G.IIModule (G.mkModuleName n)):cx) >> return True)

tryLoadMod :: String -> G.Ghc Bool
tryLoadMod n = do
    G.guessTarget n Nothing >>= G.addTarget
    G.load G.LoadAllTargets
    cx <- G.getContext
    G.handleGhcException (\_ -> return False)
                         (G.handleGhcException (\_ -> G.setContext ((G.IIModule (G.mkModuleName "Main")):cx) >> return True)
                                               ((G.setContext ((G.IIModule (G.mkModuleName n)):cx)) >> return True))

loadRunTests :: ExModule -> G.Ghc ModuleReport
loadRunTests (ExModule n ts xs) = do
    l <- loadMod ts
    if l then ModuleReport n <$> mapM runExExport xs
         else return (ModuleReport n [])

runExModule :: ExModule -> G.Ghc ModuleReport
runExModule e@(ExModule n ts xs) = do
    l <- tryLoadMod n
    if l then loadRunTests e
         else return (ModuleReport n [])

runExpectation :: Expectation -> IO Report
runExpectation (Expectation n init xs) = do
    is <- init
    case is of InitOK -> G.runGhc (Just G.libdir) (withDiscernEnv (Report (n ++ "_report.txt") is Nothing <$> mapM runExModule xs))
               _      -> return (Report (n ++ "_report.txt") is Nothing [])

module Discern.GHC.Expectation where

import Data.List (find)

import qualified Data.Map.Strict as M

import Data.Maybe (fromJust)

import Discern.Expectation
import Discern.GHC.Type
import Discern.Report
import Discern.Type

import qualified GHC             as G
import qualified GHC.Paths       as G
import qualified Name            as G

qual :: String -> String
qual = ("UT." ++)

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
               (n:_) -> case G.getInfo True n
                            of Nothing               -> return []
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
classTyVarRep (ExClassVarNames ns) gtve =
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
                                         else ClassMethWrongType n t gt

classReport :: ExExport -> G.TyCon -> ExReport
classReport (ExClass n vs ms) gtc =
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
                          cts = map (normalizeScopedType tvs) (G.is_tys)
                      in ts == cts

symbolReport :: ExExport -> G.Ghc ExReport
symbolReport = undefined

-- | Assumes the relevant modules are in scope and imported qualified with the
--   "UT." prefix.
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
                _        -> return $ SymbolReport n ExportMissing undefined undefined

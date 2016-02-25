module Discern.Report where

import Discern.Expectation
import Discern.Type

import Test.Tasty.Ingredients
import Test.Tasty.Runners

-- | A 'Report' is the result of executing an 'Expectation.'
data Report = Report {
    -- | Typically generated from the 'Expectation' name.
    rFilePath :: FilePath
  , rInitRep  :: InitStatus
  , rCompFail :: Maybe String
  , rModReps  :: [ModuleReport]
  }

data ModuleReport = ModuleReport {
    mrName    :: String
  , mrExports :: [ExReport]
  }

data ExReport = TypeReport {
                  trTyConName   :: String
                , trTyConStatus :: ExportStatus
                , trTyConTyVars :: TyConTyVarRep
                , trDataCons    :: [DataConRep]
                }
              | ClassReport {
                  crName    :: String
                , crStatus  :: ExportStatus
                , crTyVars  :: ClassTyVarRep
                , crMethods :: [ClassMethRep]
                }
              | InstanceReport {
                  irStatus    :: ExportStatus
                , irClassName :: String
                , irInstTy    :: [Type]
                }
              | SymbolReport {
                  srName   :: String
                , srStatus :: ExportStatus
                , srType   :: SymbolTypeRep
                , srTests  :: [TestRep]
                }

data ExportStatus = ExportCorrect
                  | ExportWrong
                  | ExportAbsent

data TyConTyVarRep = TyConTyVarOK
                   | TyConTyVarWrongArity {
                       tctvExArity :: Int
                     , tctvObArity :: Int
                     }
                   | TyConTyVarWrongNames {
                       tctvExNames :: [String]
                     , tctvObNames :: [String]
                     }

data DataConRep = DataConOK {
                    dcName :: String
                  }
                | DataConWrongType {
                    dcName   :: String
                  , dcExType :: Type
                  , dcObType :: Either String Type
                  }
                | DataConAbsent {
                    dcName :: String
                  }

data ClassTyVarRep = ClassTyVarOK
                   | ClassTyVarWrongArity {
                       ctvExArity :: Int
                     , ctvObArity :: Int
                     }
                   | ClassTyVarWrongNames {
                       ctvExNames :: [String]
                     , ctvObNames :: [String]
                     }

data ClassMethRep = ClassMethOK {
                      cmName :: String
                    }
                  | ClassMethWrongType {
                      cmName   :: String
                    , cmExType :: Type
                    , cmObType :: Either String Type
                    }
                  | ClassMethAbsent {
                      cmName :: String
                    }

data SymbolTypeRep = SymbolTypeOK
                   | SymbolTypeWrong {
                       stExType :: Type
                     , stObType :: Either String Type
                     }

data TestRep = TestRep {
    testName   :: String
  , testResult :: Result
  }

renderReport :: Report -> String
renderReport (Report _ (InitFail e) _) = unlines ["Expectation initialization failed: " ++ e]
renderReport (Report _ InitOK mrs) = unlines $ "Initialization OK." : map (unlines . renderModuleReport) mrs

renderModuleReport :: ModuleReport -> [String]
renderModuleReport (ModuleReport n es) = ("Reporting on module " ++ n ++ ":")
                                       : map (unlines . map ('\t':) . renderExReport) es

renderExReport :: ExReport -> [String]
renderExReport (TypeReport n ExportCorrect _ _)     = ["Exported type " ++ n ++ " OK."]
renderExReport (TypeReport n ExportAbsent _ _)      = ["Exported type " ++ n ++ " is absent."]
renderExReport (TypeReport n ExportWrong vs cs)     = ("Exported type " ++ n ++ " is incorrect.")
                                                    : ('\t' : renderTyConTyVarRep vs)
                                                    : map (('\t':) . renderDataConRep) cs
renderExReport (ClassReport n ExportCorrect _ _ )   = ["Exported class " ++ n ++ " OK."]
renderExReport (ClassReport n ExportAbsent _ _)     = ["Exported class " ++ n ++ " is absent."]
renderExReport (ClassReport n ExportWrong vs ms)    = ("Exported class " ++ n ++ " is incorrect.")
                                                    : ('\t' : renderClassTyVarRep vs)
                                                    : map (('\t':) . renderClassMethRep) ms
renderExReport (InstanceReport ExportCorrect cn ts) = ["Exported instance of " ++ cn ++ " for type(s) " ++ show ts ++ " OK."]
renderExReport (InstanceReport ExportAbsent cn ts)  = ["Exported instance of " ++ cn ++ "for type(s) " ++ show ts ++ " is absent."]
renderExReport (InstanceReport ExportWrong cn ts)   = error "Instances may only be present or absent; not wrong."
renderExReport (SymbolReport n ExportCorrect _ ts)  = ("Exported symbol " ++ n ++ " OK.")
                                                    : map (('\t':) . renderTestRep) ts
renderExReport (SymbolReport n ExportAbsent _ _)    = ["Exported symbol " ++ n ++ " is absent."]
renderExReport (SymbolReport n ExportWrong st ts)   = ("Exported symbol " ++ n ++ " is incorrect.")
                                                    : ('\t' : renderSymbolTypeRep st)
                                                    : map (('\t':) . renderTestRep) ts

renderTyConTyVarRep :: TyConTyVarRep -> String
renderTyConTyVarRep TyConTyVarOK               = "Type constructor type variables OK."
renderTyConTyVarRep (TyConTyVarWrongArity e o) = "Type constructor observed to have arity "
                                              ++ show o
                                              ++ " but "
                                              ++ show e
                                              ++ " was expected."
renderTyConTyVarRep (TyConTyVarWrongNames e o) = "Type constructor observed to have variable(s) "
                                              ++ show o
                                              ++ " but "
                                              ++ show e
                                              ++ " was/were expected."

renderDataConRep :: DataConRep -> String
renderDataConRep (DataConOK n)            = "Data constructor " ++ n ++ " OK."
renderDataConRep (DataConAbsent n)        = "Data constructor " ++ n ++ " is absent."
renderDataConRep (DataConWrongType n e o) = "Data constructor "
                                          ++ n
                                          ++ " observed to have type "
                                          ++ show o
                                          ++ " but "
                                          ++ show e
                                          ++ " was expected."

renderClassTyVarRep :: ClassTyVarRep -> String
renderClassTyVarRep ClassTyVarOK               = "Class type variables OK."
renderClassTyVarRep (ClassTyVarWrongArity e o) = "Class observed to have arity "
                                              ++ show o
                                              ++ " but "
                                              ++ show e
                                              ++ " was expected."
renderClassTyVarRep (ClassTyVarWrongNames e o) = "Class observed to have variable(s) "
                                              ++ show o
                                              ++ " but "
                                              ++ show e
                                              ++ " was/were expected."

renderClassMethRep :: ClassMethRep -> String
renderClassMethRep (ClassMethOK n)            = "Class method " ++ n ++ " OK."
renderClassMethRep (ClassMethAbsent n)        = "Class method " ++ n ++ " is absent."
renderClassMethRep (ClassMethWrongType n e o) = "Class method "
                                             ++ n
                                             ++ " observed to have type "
                                             ++ show o
                                             ++ " but "
                                             ++ show e
                                             ++ " was expected."

renderSymbolTypeRep :: SymbolTypeRep -> String
renderSymbolTypeRep SymbolTypeOK          = "Type OK."
renderSymbolTypeRep (SymbolTypeWrong e o) = "Type "
                                          ++ show o
                                          ++ " was observed but "
                                          ++ show e
                                          ++ " was expected."

renderTestRep :: TestRep -> String
renderTestRep (TestRep n (Result (Failure (TestTimedOut _)) d _ t)) =
    "Test " ++ n ++ " timed out in " ++ show t ++ " seconds with message: " ++ d
renderTestRep (TestRep n (Result (Failure _) d _ t))                =
    "Test " ++ n ++ " failed in " ++ show t ++ " seconds with message: " ++ d
renderTestRep (TestRep n (Result Success d _ t)) = 
    "Test " ++ n ++ " passed in " ++ show t ++ " seconds with message: " ++ d

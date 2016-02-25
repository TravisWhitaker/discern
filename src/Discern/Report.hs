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
                , irMethods   :: [InstMethRep]
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

data InstMethRep = InstMethOK {
                     imClassName :: String
                   , imInstTypes :: [Type]
                   }
                 | IntMethMissing {
                     imClassName :: String
                   , imInstTypes :: [Type]
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

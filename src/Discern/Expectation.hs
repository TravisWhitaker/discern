module Discern.Expectation where

import Discern.Type

-- | An 'Expectation' is a group of modules under test. Executing an
--   'Expectation' will yield a 'Report'.
data Expectation = Expectation {
    -- | A 'Report' will be named after its 'Expectation'.
    exName    :: String
    -- | An action required to initialize the environment, e.g. gunzipping.
  , exInit    :: IO InitStatus
    -- | A list of the modules under test.
  , exModules :: [ExModule]
  }

data ExModule = ExModule {
    emName    :: String
  , emExports :: [ExExport]
  }

data ExExport = ExType {
                  etTyConName   :: String
                , etTyConTyVars :: ExTyConTyVars
                , etDataCons    :: [ExDataCon]
                }
              | ExClass {
                  ecName    :: String
                , ecTyVars  :: ExClassTyVars
                , ecMethods :: [ExClassMeth]
                }
              | ExInstance {
                  eiClassName :: String
                , eiInstTy    :: [Type]
                }
              | ExSymbol {
                  esName  :: String
                , esType  :: Type
                , exTests :: [String]
                }

data ExTyConTyVars = ExTyConArity Int
                   | ExTyConVarNames [String]
                   deriving (Eq, Show)

data ExDataCon = ExDataCon {
    edName :: String
  , edType :: Type
  }
  deriving (Eq, Show)

data ExClassTyVars = ExClassArity Int
                   | ExClassVarNames [String]
                   deriving (Eq,  Show)

data ExClassMeth = ExClassMeth {
    ecmName :: String
  , ecmType :: Type
  } deriving (Eq, Show)

data InitStatus = InitOK
                | InitFail String
                deriving (Eq, Show)

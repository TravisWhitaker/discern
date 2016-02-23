module Discern.Types where

import Text.Show

import Data.List

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
    emName :: String
  , emTypes :: [ExExport]
  }

data ExExport = ExType {
                  etTyConName   :: String
                , etTyConTyVars :: ExTyConTyVars
                , etDataConss   :: [ExDataCon]
                }
              | ExpectedClass {
                  ecName    :: String
                , ecTyVars  :: ExClassTyVars
                , ecMethods :: [ExClassMeth]
                }
              | ExpectedInstance {
                  eiClassName :: String
                , eiInstTy    :: [Type]
                , eiMethNames :: [String]
                }
              | ExpectedSymbol {
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

data Type = TypeTyVar TyVar
          | TypeApp Type Type
          | TypeTyConApp TyCon [Type]
          | TypeUQuant [TyVar] Type
          | TypeConstraint Class [TyVar] Type
          deriving (Eq)

instance Show Type where
    showsPrec _ (TypeTyVar v)           = showString $ show v
    showsPrec p (TypeApp l r)           = showParen (p > 10) ((showsPrec 11 l) . showString " " . (showsPrec 11 r))
    showsPrec p (TypeTyConApp tc ts)    = showParen (p > 9) ((showsPrec 10 tc) . (foldl (\s f -> (f . (++ " ") . s)) id (map (showsPrec 10) ts)))
    showsPrec p (TypeUQuant ts t)       = showParen (p > 8) ((showString "forall ") . (foldl (\s f -> (f . (++ " ") . s)) id (map (showsPrec 9) ts)) . (showsPrec 9 t))
    showsPrec p (TypeConstraint c ts t) = showParen (p > 7) ((shows c) . (foldl (\s f -> (f . (++ " ") . s)) id (map (showsPrec 8) ts)) . (showsPrec 8 t))

-- | A type whose 'TyVar's indices have been normalized. Only 'AlphaType's are
--   alpha-equatable and are sensibly 'Show'-able.
newtype AlphaType = AlphaType Type
                  deriving (Eq, Show)

data TyVar = TyVar {
    tyVarName :: String
  , tyVarLI   :: Int
  }

instance Eq TyVar where
    (TyVar _ a) == (TyVar _ b) = a == b

instance Show TyVar where
    show (TyVar s _) = s

data TyCon = TyCon {
    tyConName  :: String
  , tyConArity :: Int
  } deriving (Eq)

instance Show TyCon where
    show (TyCon s _) = s

data Class = Class {
    className  :: String
  , classArity :: Int
  } deriving (Eq)

instance Show Class where
    show (Class s _) = s

--    -- | A 'Report' is the result of executing an 'Expectation.'
--    data Report = Report {
--        -- | Typically generated from the 'Expectation' name.
--        rFilePath :: FilePath
--      , rInitRep  :: InitReport
--      , rModReps  :: [ModuleReport]
--      }
--    
data InitStatus = InitOK
                | InitFail String
                deriving (Eq, Show)
--    
--    data ModuleReport = ModuleReport {

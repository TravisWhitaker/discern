module Discern.Report where

import Discern.Expectation
import Discern.Type

-- | A 'Report' is the result of executing an 'Expectation.'
data Report = Report {
    -- | Typically generated from the 'Expectation' name.
    rFilePath :: FilePath
  , rInitRep  :: InitStatus
--  , rModReps  :: [ModuleReport]
  }



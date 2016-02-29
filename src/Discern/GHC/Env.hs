module Discern.GHC.Env where

import qualified GHC       as G
import qualified GHC.Paths as G

-- | Sets DynFlags for interactive evaluation and imports the Prelude.
discernEnv :: G.Ghc ()
discernEnv = do
    df <- G.getSessionDynFlags
    G.setSessionDynFlags $ df { G.hscTarget = G.HscInterpreted
                              , G.ghcLink   = G.LinkInMemory
                              }
    G.setContext [ G.IIDecl (G.simpleImportDecl (G.mkModuleName "Prelude"))
                 , G.IIDecl (G.simpleImportDecl (G.mkModuleName "Discern.GHC.Test"))]

withDiscernEnv :: G.Ghc a -> G.Ghc a
withDiscernEnv = (discernEnv >>)

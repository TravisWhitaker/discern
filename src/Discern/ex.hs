import GHC
import GHC.Paths

import Outputable

import Type

outString :: Outputable o => o -> String
outString = showSDocUnsafe . ppr

grabType :: String -> IO Type
grabType s = runGhc (Just libdir) $ do
    df <- getSessionDynFlags
    setSessionDynFlags (df { hscTarget = HscInterpreted
                           , ghcLink   = LinkInMemory
                           })
    setContext [IIDecl (simpleImportDecl (mkModuleName "Prelude"))]
    exprType s

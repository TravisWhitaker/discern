module Discern.GHC.Type where

import Discern.Type

import qualified GHC as G

import qualified Type as G

ghcTyVarToTyVar :: G.TyVar -> TyVar

ghcTypeToConstraint :: G.Type -> Either String (Type -> Type)

ghcClassToClass :: G.Class -> Class

ghcTypeToType :: G.Type -> Either String Type
ghcTypeToType gt
    | G.isForAllTy gt = let (tvs, t) = splitForAllTys gt
                            tvs'     = map ghcTyVarToTyVar tvs
                        in TypeUQuant tvs' <$> ghcTypeToType t
    | G.isTyVarTy gt  = return (TypeTyVar
                               (ghcTyVarToTyVar
                               (G.getTyVar "ghcTypeToType: getTyVar failed" gt)))
    | G.isFunTy gt    = let (a, r) = splitFunTy
                        in if GisPredType a
                           then (ghcTypeToConstraint a) <*> (ghcTypeToType r)
                           else do
                                a' <- ghcTypeToType a
                                r' <- ghtTYpeToType r
                                return $ TypeTyConApp funTyCon [a', r']
    | G.isPredTy gt   = Left "Pred type in unhandleable position."
    | otherwise       =

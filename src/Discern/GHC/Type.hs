module Discern.GHC.Type where

import Discern.Type

import qualified GHC as G

import qualified Type as G

ghcTyVarToTyVar :: G.TyVar -> TyVar

ghcTypeToType :: G.Type -> Either String Type
ghcTypeToType gt
    | G.isForAllTy gt = let (tvs, t) = splitForAllTys gt
                            tvs'     = map ghcTyVarToTyVar tvs
                        in TypeUQuant tvs' <$> ghcTypeToType t
    | G.isTyVarTy gt  = return (TypeTyVar
                               (ghcTyVarToTyVar
                               (getTyVar "ghcTypeToType: getTyVar failed" gt)))
    | G.isFunTy gt    =
    | G.isPredTy gt   =
    | otherwise       =

module Discern.GHC.Type where

import Discern.Type

import qualified Class      as G

import qualified GHC        as G

import qualified Name       as G

import qualified Outputable as G

import qualified Type       as G

import qualified Var        as G

unTypeTyVar :: Type -> Either String TyVar
unTypeTyVar (TypeTyVar tv) = Right tv
unTypeTyVar _              = Left "unTypeTyCon: Type wasn't a TyVar."

ghcTyVarToTyVar :: G.TyVar -> TyVar
ghcTyVarToTyVar = mkLocalTyVar . G.getOccString

ghcTypeToConstraint :: G.Type -> Either String (Type -> Type)
ghcTypeToConstraint gt = let (c, ts) = G.getClassPredTys gt
                         in TypeConstraint (ghcClassToClass c) <$> mapM (\t -> ghcTypeToType t >>= unTypeTyVar) ts

ghcClassToClass :: G.Class -> Class
ghcClassToClass c = Class (G.getOccString c) (G.classArity c)

ghcTyConToTyCon :: G.TyCon -> TyCon
ghcTyConToTyCon tc = TyCon (G.getOccString tc) (length (G.tyConTyVars tc))

ghcTypeToType :: G.Type -> Either String Type
ghcTypeToType gt
    | G.isForAllTy gt = let (tvs, t) = G.splitForAllTys gt
                            tvs'     = map ghcTyVarToTyVar tvs
                        in TypeUQuant tvs' <$> ghcTypeToType t
    | G.isTyVarTy gt  = return (TypeTyVar
                               (ghcTyVarToTyVar
                               (G.getTyVar "ghcTypeToType: getTyVar failed" gt)))
    | G.isFunTy gt    = let (a, r) = G.splitFunTy gt
                        in if G.isPredTy a
                           then (ghcTypeToConstraint a) <*> (ghcTypeToType r)
                           else do
                                a' <- ghcTypeToType a
                                r' <- ghcTypeToType r
                                return $ TypeTyConApp funTyCon [a', r']
    | G.isPredTy gt   = Left "Pred type in unhandleable position."
    | G.isAlgType gt  = case G.splitTyConApp_maybe gt
                        of Nothing -> Left ("Weird algebraic type: " ++ (G.showSDocUnsafe (G.ppr gt)))
                           (Just (tc, ts)) -> TypeTyConApp (ghcTyConToTyCon tc) <$> mapM ghcTypeToType ts
    | otherwise       = case G.splitAppTy_maybe gt
                        of Nothing -> Left ("Weird unknown type: " ++ (G.showSDocUnsafe (G.ppr gt)))
                           (Just (tl, tr)) -> TypeApp <$> ghcTypeToType tl <*> ghcTypeToType tr

exprType :: String -> G.Ghc (Either String Type)
exprType = (ghcTypeToType <$>) . G.exprType

ghcDataConTypeToType :: G.DataCon -> Either String Type
ghcDataConTypeToType dc = normalizeScopedType tvs <$> ghcTypeToType (G.dataConType dc)
    where tvs = map ghcTyVarToTyVar (G.tyConTyVars (G.dataConTyCon dc))

ghcClassMethTypeToType :: G.Id -> Either String Type
ghcClassMethTypeToType i = classTyVars >>= (\tvs -> normalizeScopedType tvs <$> ghcTypeToType (G.varType i))
    where classTyVars = case G.isClassOpId_maybe i
                            of Nothing  -> Left "ghcClassMethTypeToType: Id wasn't a class method."
                               (Just c) -> Right (map ghcTyVarToTyVar (G.classTyVars c))

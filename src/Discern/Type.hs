module Discern.Type where

import           Data.List

import qualified Data.Map.Strict as M

import Text.Show

data Type = TypeTyVar TyVar
          | TypeApp Type Type
          | TypeTyConApp TyCon [Type]
          | TypeUQuant [TyVar] Type
          | TypeConstraint Class [TyVar] Type
          deriving (Eq, Show)

--instance Show Type where
--    showsPrec _ (TypeTyVar v)           = showString $ show v
--    showsPrec p (TypeApp l r)           = showParen (p > 10)
--                                                    ((showsPrec 11 l)
--                                                    . showString " " .
--                                                     (showsPrec 11 r))
--    showsPrec p (TypeTyConApp tc ts)    = showParen (p > 9)
--                                                    ((showsPrec 10 tc)
--                                                    . (foldl (\s f -> (f . (++ " ") . s)) id (map (showsPrec 10) ts)))
--    showsPrec p (TypeUQuant ts t)       = showParen (p > 8)
--                                                    ((showString "forall ")
--                                                    . (foldl (\s f -> (f . (++ " ") . s)) id (map (showsPrec 9) ts))
--                                                    . (showsPrec 9 t))
--    showsPrec p (TypeConstraint c ts t) = showParen (p > 7)
--                                                    ((shows c)
--                                                    . (foldl (\s f -> (f . (++ " ") . s)) id (map (showsPrec 8) ts))
--                                                    . (showsPrec 8 t))

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

mkLocalTyVar :: String -> TyVar
mkLocalTyVar a = TyVar a 0

tyVar :: TyVar -> Type
tyVar = TypeTyVar

typeApp :: Type -> Type -> Type
typeApp tl@(TypeTyVar tv) tr        = TypeApp tl tr
typeApp (TypeTyConApp tc ts) t      = tyConApp tc (ts ++ [t])
typeApp (TypeUQuant tv qt) t        = TypeUQuant tv (typeApp qt t)
typeApp (TypeConstraint c tvs qt) t = TypeConstraint c tvs (typeApp qt t)
typeApp tl tr                       = TypeApp tl tr

tyConApp :: TyCon -> [Type] -> Type
tyConApp tc@(TyCon _ a) ts
    | a < length ts = error ("tyConApp: type constructor "
                          ++ show tc
                          ++ " expects "
                          ++ show a
                          ++ " arguments but "
                          ++ show (length ts)
                          ++ " were applied.")
    | otherwise = TypeTyConApp tc ts

forall :: [TyVar] -> Type -> Type
forall = TypeUQuant

constraint :: Class -> [TyVar] -> Type -> Type
constraint = TypeConstraint

normalizeTyVar :: (Int, M.Map String Int) -> TyVar -> ((Int, M.Map String Int), TyVar)
normalizeTyVar (m, im) (TyVar n _)
    | M.member n im = ((m, im), (TyVar n (im M.! n)))
    | otherwise     = let m' = m + 1
                      in ((m', (M.insert n m' im)), (TyVar n m'))

normalizeType :: Type -> AlphaType
normalizeType t = AlphaType (snd (go ((0, M.empty), t)))
    where go ((m, im), (TypeTyVar tv))           = let ((m', im'), tv') = normalizeTyVar (m, im) tv
                                                   in ((m', im'), TypeTyVar tv')
          go ((m, im), (TypeApp tl tr))          = let ((m', im'), tl')   = go ((m, im), tl)
                                                       ((m'', im''), tr') = go ((m', im'), tr)
                                                   in ((m'', im''), (TypeApp tl' tr'))
          go ((m, im), (TypeTyConApp tc ts))     = let ((m', im'), ts') = goList [] ((m, im), ts)
                                                   in ((m', im'), (TypeTyConApp tc ts'))
          go ((m, im), (TypeUQuant tvs t))       = let ((m', im'), t') = go ((m, im), t)
                                                   in ((m', im'), (TypeUQuant tvs t'))
          go ((m, im), (TypeConstraint c tvs t)) = let ((m', im'), t') = go ((m, im), t)
                                                  in ((m', im'), (TypeConstraint c tvs t'))
          goList as ((m, im), [])                = ((m, im), reverse as)
          goList as ((m, im), (t:ts))            = let ((m', im'), t') = go ((m, im), t)
                                                   in goList (t':as) ((m', im'), ts)

typeEq :: Type -> Type -> Bool
typeEq tl tr = let (AlphaType tl') = normalizeType tl
                   (AlphaType tr') = normalizeType tr
               in tl' == tr'

-- * Common 'TyCon's

funTyCon :: TyCon
funTyCon = TyCon "(->)" 2

tuple2TyCon :: TyCon
tuple2TyCon = TyCon "(,)" 2

tuple3TyCon :: TyCon
tuple3TyCon = TyCon "(,,)" 3

tuple4TyCon :: TyCon
tuple4TyCon = TyCon "(,,,)" 4

tuple5TyCon :: TyCon
tuple5TyCon = TyCon "(,,,,)" 5

tuple6TyCon :: TyCon
tuple6TyCon = TyCon "(,,,,,)" 6

tuple7TyCon :: TyCon
tuple7TyCon = TyCon "(,,,,,,)" 7

tuple8TyCon :: TyCon
tuple8TyCon = TyCon "(,,,,,,,)" 8

tuple9TyCon :: TyCon
tuple9TyCon = TyCon "(,,,,,,,,)" 9

tuple10TyCon :: TyCon
tuple10TyCon = TyCon "(,,,,,,,,,)" 10

listTyCon :: TyCon
listTyCon = TyCon "([])" 1

maybeTyCon :: TyCon
maybeTyCon = TyCon "Maybe" 1

eitherTyCon :: TyCon
eitherTyCon = TyCon "Either" 2

boolTyCon :: TyCon
boolTyCon = TyCon "Bool" 0

intTyCon :: TyCon
intTyCon = TyCon "Int" 0

integerTyCon :: TyCon
integerTyCon = TyCon "Integer" 0

floatTyCon :: TyCon
floatTyCon = TyCon "Float" 0

doubleTyCon :: TyCon
doubleTyCon = TyCon "Double" 0

charTyCon :: TyCon
charTyCon = TyCon "Char" 0

-- * Common 'Class'es

eqClass :: Class
eqClass = Class "Eq" 1

ordClass :: Class
ordClass = Class "Ord" 1

readClass :: Class
readClass = Class "Read" 1

showClass :: Class
showClass = Class "Show" 1

functorClass :: Class
functorClass = Class "Functor" 1

applicativeClass :: Class
applicativeClass = Class "Applicative" 1

monadClass :: Class
monadClass = Class "Monad" 1

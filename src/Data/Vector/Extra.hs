{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Data.Vector.Extra (
  Vector,zipV,vector,toList
) where
import Data.AdditiveGroup ( AdditiveGroup(negateV, zeroV, (^+^)) )
import Data.AffineSpace ( AffineSpace(..) )
import Data.VectorSpace ( VectorSpace(..) )

data Vector a = Nil | !a :! !(Vector a) | Repeat !a

instance Foldable Vector where
  foldr f b (a :! va) = f a $ foldr f b va
  foldr _ b Nil = b
  foldr f b (Repeat a) = f a $ foldr f b (Repeat a)

vector :: [a] -> Vector a
vector = foldr (:!) Nil

toList :: Vector a -> [a]
toList = foldr (:) []

zipV :: Vector a -> Vector b -> Vector (a,b)
zipV (a :! va) (b :! vb) = (a, b) :! zipV va vb
zipV (Repeat a) (b :! vb) = (a, b) :! zipV (Repeat a) vb
zipV (a :! va) (Repeat b) = (a, b) :! zipV va (Repeat b)
zipV (Repeat a) (Repeat b) = Repeat (a, b)
zipV Nil _ = Nil
zipV _ Nil = Nil


instance Functor Vector where
  fmap f (a :! va) = f a :! fmap f va
  fmap f (Repeat a) = Repeat (f a)
  fmap _ Nil = Nil

zipWithV :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWithV f va vb = uncurry f <$> zipV va vb

instance Applicative Vector where
  pure a = Repeat a
  (<*>) vf va = zipWithV ($) vf va

instance (AdditiveGroup a) => AdditiveGroup (Vector a) where
  zeroV = pure zeroV
  (^+^) va vb = (^+^) <$> va <*> vb
  negateV va = negateV <$> va

instance (VectorSpace a) => VectorSpace (Vector a) where
  type Scalar (Vector a) = Scalar a
  (*^) s va = (s *^) <$> va

instance (AffineSpace a) => AffineSpace (Vector a) where
  type Diff (Vector a) = Vector (Diff a)
  (.+^) va dva = (.+^) <$> va <*> dva
  (.-.) va vb = (.-.) <$> va <*> vb

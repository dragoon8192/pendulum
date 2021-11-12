module Data.Vector.Extra (
  Vector(Vector)
) where
import Data.AdditiveGroup
import Data.AffineSpace
import Data.VectorSpace

newtype Vector a = Vector [a]

vector :: [a] -> Vector a
vector = Vector

instance Functor Vector where
  fmap f (Vector as) = Vector $ map f as

instance Applicative Vector where
  pure a = Vector [a]
  (<*>) (Vector fs) (Vector as) = Vector $ zipWith ($) fs as

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

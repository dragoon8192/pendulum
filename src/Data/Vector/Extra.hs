module Data.Vector.Extra (
) where
import Data.AdditiveGroup
import Data.VectorSpace

newtype Vector a = Vector [a]

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


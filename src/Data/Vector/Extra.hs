{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Data.Vector.Extra (
  sumV,zipV,zipV3,vector,innerList,
  Vector(Vector)
) where
import Data.AdditiveGroup ( AdditiveGroup(negateV, zeroV, (^+^)) )
import Data.AffineSpace ( AffineSpace(..) )
import Data.VectorSpace ( VectorSpace(..) )

data Vector a = Vector { innerList :: ![a] }

vector :: [a] -> Vector a
vector = Vector

sumV :: (Num a) => Vector a -> a
sumV (Vector as) = sum as

zipV :: Vector a -> Vector b -> Vector (a,b)
zipV (Vector as) (Vector bs) = Vector $ zip as bs

zipV3 :: Vector a -> Vector b -> Vector c -> Vector (a,b,c)
zipV3 (Vector as) (Vector bs) (Vector cs) = Vector $ zip3 as bs cs

instance Functor Vector where
  fmap f (Vector as) = Vector $ map f as

instance Applicative Vector where
  pure a = Vector . repeat $ a
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

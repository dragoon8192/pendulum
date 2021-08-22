module PendPhysModel () where
import Data.AffineSpace
import Data.VectorSpace
import Data.Functor

type DTime = Double
data Pendulum q p = Pendulum {
                mass    :: Double,
                length  :: Double,
                theta   :: q,
                pTheta  :: p
                  }

instance Functor (Pendulum a) where
  fmap f (Pendulum m l q p) = Pendulum m l q (f p)
instance Functor2 Pendulum where
  fmap2 f (Pendulum m l q p) = Pendulum m l (f q) p

instance HamiltonianModel Pendulum Double Double where
  hamiltonian (Pendulum m l q p) = p * p / (2.0 * m * l) - m * 9.8 * l * cos q
  dqdt (Pendulum m l q p) = p / (m * l * l)
  dpdt (Pendulum m l q p) = - m * 9.8 * l * sin q
  addQ dq (Pendulum m l q p) = Pendulum m l (q .+^ dq) p
  addP dp (Pendulum m l q p) = Pendulum m l q (p .+^ dp)

class Functor2 f where
  fmap2 :: (a -> a') -> f a b -> f a' b

class (AffineSpace q, AffineSpace p) => HamiltonianModel a q p where
  hamiltonian :: a q p -> Double
  dqdt  :: a q p -> Diff q
  dpdt  :: a q p-> Diff p
  addQ  :: Diff q -> a q p -> a q p
  addP  :: Diff p -> a q p -> a q p
  qEvol :: (v ~ Diff q, VectorSpace v) => Scalar v -> a q p -> a q p
  qEvol dt apq = addQ (dt *^ dqdt apq) apq
  pEvol :: (v ~ Diff p, VectorSpace v) => Scalar v -> a q p -> a q p
  pEvol dt apq = addP (dt *^ dpdt apq) apq
  symplecticEvol :: (v ~ Diff p, v ~ Diff q, VectorSpace v) => Scalar v -> a q p -> a q p
  symplecticEvol dt = qEvol dt . pEvol dt


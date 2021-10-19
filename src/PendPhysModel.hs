module PendPhysModel (
  Pendulum(Pendulum,mass,length),
  symplecticEvol,
)where
import Data.AffineSpace
import Data.VectorSpace
import Control.Arrow
import Data.Functor
import Control.Monad
import Control.Monad.Reader

type DTime = Double
data Pendulum a = Pendulum {
                mass    :: Double,
                length  :: Double,
                datas    :: a
                }
                deriving (Show,Eq)

--newtype Pendulum a = Reader (Double,Double) a
--mass :: Pendulum Double
--mass = fst =< ask
--length = snd =< ask

instance Functor Pendulum where
  fmap f (Pendulum m l a) = Pendulum m l (f a)

instance HamiltonianModel Pendulum Double Double where
  hamiltonian (Pendulum m l (q,p)) = p * p / (2.0 * m * l) - m * 9.8 * l * cos q
  dqdt (Pendulum m l (q,p)) = p / (m * l * l)
  dpdt (Pendulum m l (q,p)) = - m * 9.8 * l * sin q

class (Functor a, AffineSpace q, AffineSpace p) => HamiltonianModel a q p where
  hamiltonian :: a (q,p) -> Double
  dqdt  :: a (q,p) -> Diff q
  dpdt  :: a (q,p)-> Diff p
  addQ  :: Diff q -> a (q,p) -> a (q,p)
  addQ dq = fmap $ first (.+^ dq)
  addP  :: Diff p -> a (q,p) -> a (q,p)
  addP dp = fmap $ second (.+^ dp)
  qEvol :: (v ~ Diff q, VectorSpace v) => Scalar v -> a (q,p) -> a (q,p)
  qEvol dt aqp = addQ (dt *^ dqdt aqp) aqp
  pEvol :: (v ~ Diff p, VectorSpace v) => Scalar v -> a (q,p) -> a (q,p)
  pEvol dt aqp = addP (dt *^ dpdt aqp) aqp
  symplecticEvol :: (v ~ Diff p, v ~ Diff q, VectorSpace v) => Scalar v -> a (q,p) -> a (q,p)
  symplecticEvol dt = qEvol dt . pEvol dt


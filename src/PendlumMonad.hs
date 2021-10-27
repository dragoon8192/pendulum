module PendlumMonad (
  Pendulum,
)where
import Data.AffineSpace
import Data.VectorSpace
import Control.Arrow
import Data.Functor
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

class (Monad s, AffineSpace (Q s), AffineSpace (P s), VectorSpace (Diff (Q s)), VectorSpace (Diff (P s)), DTime s ~ Scalar (Diff (P s)), DTime s ~ Scalar (Diff (Q s))) => PhysSystem s where
  type DTime s :: *
  type Q s :: *
  type P s :: *

  getPhase :: s (Q s, P s)
  getQ :: s (Q s)
  getQ = fmap fst getPhase
  getP :: s (P s)
  getP = fmap snd getPhase

  getDiffPhase :: s (Diff (Q s), Diff (P s))
  getDqDt :: s (Diff (Q s))
  getDqDt = fmap fst getDiffPhase
  getDpDt :: s (Diff (P s))
  getDpDt = fmap snd getDiffPhase

  evolQ :: DTime s -> s ()
  evolP :: DTime s -> s ()
  symplecticEvol1 :: DTime s -> s ()
  symplecticEvol1 dt = do
    evolQ dt
    evolP dt

newtype PhysicalSystem d q p x = PhysicalSystem (ReaderT (d, (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p)) (State (q, p)) x)
  deriving (Functor, Applicative, Monad, MonadState (q,p))
  -- MonadReader (d, (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p))
runPhysicalSystem :: PhysicalSystem d q p x -> d -> (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p) -> (q, p) -> (x, (q, p))
runPhysicalSystem (PhysicalSystem system) d (dqdtFunc, dpdtFunc) (q, p)
  = runState (runReaderT system (d, (dqdtFunc, dpdtFunc))) (q, p)
execPhysicalSystem :: PhysicalSystem d q p x -> d -> (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p) -> (q, p) -> (q, p)
execPhysicalSystem (PhysicalSystem system) d (dqdtFunc, dpdtFunc) (q, p)
  = execState (runReaderT system (d, (dqdtFunc, dpdtFunc))) (q, p)
askData :: (AffineSpace q, AffineSpace p, VectorSpace (Diff q), VectorSpace (Diff p)) => PhysicalSystem d q p d
askData = PhysicalSystem $ asks fst
askDiffFunc :: (AffineSpace q, AffineSpace p, VectorSpace (Diff q), VectorSpace (Diff p))
  => PhysicalSystem d q p (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p)
askDiffFunc = PhysicalSystem $ asks snd

instance (AffineSpace q, AffineSpace p, VectorSpace (Diff q), VectorSpace (Diff p), Scalar (Diff q) ~ Scalar (Diff p))
  => PhysSystem (PhysicalSystem d q p) where
  type DTime (PhysicalSystem d q p) = Scalar (Diff q)
  type Q (PhysicalSystem d q p) = q
  type P (PhysicalSystem d q p) = p

  getPhase = get
  getDiffPhase = eval2 <$> askDiffFunc <*> askData <*> getPhase
    where
      eval2 (f,g) d pq = (f d pq, g d pq)

  evolQ dt = do
    dqdt <- getDqDt
    modify $ first (.+^ dt *^ dqdt)
    --modify . first . flip (.+^) . (dt *^) =<< getDqDt
  evolP dt = do
    dpdt <- getDpDt
    modify $ second (.+^ dt *^ dpdt)

type Pendulum = PhysicalSystem (Double,Double) Double Double
runPendulum = runPhysicalSystem system

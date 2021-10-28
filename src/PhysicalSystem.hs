module PhysicalSystem (
  PhysicalSystem,
  runPhysicalSystem,
  askData,
  getPhase,
  getQ,
  getP,
  getDiffPhase,
  symplecticEvol1
) where
import Data.AffineSpace ( AffineSpace((.+^), Diff) )
import Data.VectorSpace ( VectorSpace(..) )
import Control.Arrow ( Arrow(second, first) )
import Control.Monad.Reader
    ( asks, ReaderT(..), MonadReader(local, ask) )
import Control.Monad.State
    ( MonadState(get), State, modify, evalState )

class (MonadState (Q s,P s) s, MonadReader ((Data s -> (Q s, P s) -> Diff (Q s), Data s -> (Q s, P s) -> Diff (P s)),Data s) s, AffineSpace (Q s), AffineSpace (P s), VectorSpace (Diff (Q s)), VectorSpace (Diff (P s)), DTime s ~ Scalar (Diff (P s)), DTime s ~ Scalar (Diff (Q s)))
  => PhysicalSystemClass s where
  type DTime s :: *
  type Data s :: *
  type Q s :: *
  type P s :: *
  -- MonadReader
  askDiffFunc :: s (Data s -> (Q s, P s) -> Diff (Q s), Data s -> (Q s, P s) -> Diff (P s))
  askDiffFunc = asks fst
  askData :: s (Data s)
  askData = asks snd
  -- MonadState
  getPhase :: s (Q s, P s)
  getPhase = get
  getQ :: s (Q s)
  getQ = fmap fst getPhase
  getP :: s (P s)
  getP = fmap snd getPhase
  -- Differential
  getDiffPhase :: s (Diff (Q s), Diff (P s))
  getDiffPhase = eval2 <$> askDiffFunc <*> askData <*> getPhase
    where
      eval2 (f,g) d pq = (f d pq, g d pq)
  getDqDt :: s (Diff (Q s))
  getDqDt = fmap fst getDiffPhase
  getDpDt :: s (Diff (P s))
  getDpDt = fmap snd getDiffPhase
  -- evolution
  evolQ :: DTime s -> s ()
  evolQ dt = do
    dqdt <- getDqDt
    modify $ first (.+^ dt *^ dqdt)
    --modify . first . flip (.+^) . (dt *^) =<< getDqDt
  evolP :: DTime s -> s ()
  evolP dt = do
    dpdt <- getDpDt
    modify $ second (.+^ dt *^ dpdt)
  -- symplectic evolution
  symplecticEvol1 :: DTime s -> s ()
  symplecticEvol1 dt = do
    evolP dt
    evolQ dt

newtype PhysicalSystem d q p x = PhysicalSystem (ReaderT ((d -> (q, p) -> Diff q, d -> (q, p) -> Diff p), d) (State (q, p)) x)
  deriving (Functor, Applicative, Monad, MonadState (q,p))

instance (AffineSpace q, AffineSpace p, dq ~ Diff q, dp ~ Diff p)
  => MonadReader ((d -> (q, p) -> dq, d -> (q, p) -> dp), d) (PhysicalSystem d q p) where
  ask = PhysicalSystem ask
  local f (PhysicalSystem x) = PhysicalSystem $ local f x

instance (AffineSpace q, AffineSpace p, VectorSpace (Diff q), VectorSpace (Diff p), Scalar (Diff q) ~ Scalar (Diff p))
  => PhysicalSystemClass (PhysicalSystem d q p) where
  type DTime (PhysicalSystem d q p) = Scalar (Diff q)
  type Data (PhysicalSystem d q p) = d
  type Q (PhysicalSystem d q p) = q
  type P (PhysicalSystem d q p) = p

runPhysicalSystem :: (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p) -> d -> (q, p) -> PhysicalSystem d q p x -> x
runPhysicalSystem (dqdtFunc, dpdtFunc) d (q, p) (PhysicalSystem system)
  = evalState (runReaderT system ((dqdtFunc, dpdtFunc), d)) (q, p)
--execPhysicalSystem :: PhysicalSystem d q p x -> (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p) -> d -> (q, p) -> (q, p)
--execPhysicalSystem (PhysicalSystem system) (dqdtFunc, dpdtFunc) d (q, p)
--  = execState (runReaderT system ((dqdtFunc, dpdtFunc), d)) (q, p)

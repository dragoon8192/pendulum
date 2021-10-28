module PhysicalSystem (
  PhysicalSystem,
  runPhysicalSystem,
  flipRunPhysicalSystem,
  PhysicalSystemT,
  lift,
  physicalSystem,
  runPhysicalSystemT,
  flipRunPhysicalSystemT,
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
import Control.Monad.State
import Data.Functor.Identity

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

newtype PhysicalSystemT d q p m x = PhysicalSystemT (ReaderT ((d -> (q, p) -> Diff q, d -> (q, p) -> Diff p), d) (StateT (q, p) m) x)
  deriving (Functor, Applicative, Monad, MonadState (q,p))

instance (Monad m, AffineSpace q, AffineSpace p, dq ~ Diff q, dp ~ Diff p)
  => MonadReader ((d -> (q, p) -> dq, d -> (q, p) -> dp), d) (PhysicalSystemT d q p m) where
  ask = PhysicalSystemT ask
  local f (PhysicalSystemT x) = PhysicalSystemT $ local f x

instance MonadTrans (PhysicalSystemT d q p) where
  lift mx = PhysicalSystemT . lift . lift $ mx

instance (Monad m, AffineSpace q, AffineSpace p, VectorSpace (Diff q), VectorSpace (Diff p), Scalar (Diff q) ~ Scalar (Diff p))
  => PhysicalSystemClass (PhysicalSystemT d q p m) where
  type DTime (PhysicalSystemT d q p m) = Scalar (Diff q)
  type Data (PhysicalSystemT d q p m) = d
  type Q (PhysicalSystemT d q p m) = q
  type P (PhysicalSystemT d q p m) = p

physicalSystem :: (Monad m, AffineSpace q, AffineSpace p)
  => ((d -> (q, p) -> Diff q, d -> (q, p) -> Diff p) -> d -> (q, p) -> x) -> PhysicalSystemT d q p m x
physicalSystem f = PhysicalSystemT . ReaderT $ f2
  where
    f2 (dFunc , d) = state $ \qp -> (f dFunc d qp, qp)

runPhysicalSystemT :: (Monad m)
  => PhysicalSystemT d q p m x -> (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p) -> d -> (q, p) -> m x
runPhysicalSystemT (PhysicalSystemT system) (dqdtFunc, dpdtFunc) d (q, p)
  = evalStateT (runReaderT system ((dqdtFunc, dpdtFunc), d)) (q, p)

flipRunPhysicalSystemT :: (Monad m)
  => (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p) -> d -> (q, p) -> PhysicalSystemT d q p m x -> m x
flipRunPhysicalSystemT = (flip .) . flip . flip runPhysicalSystemT


type PhysicalSystem d q p x = PhysicalSystemT d q p Identity x

runPhysicalSystem :: PhysicalSystem d q p x -> (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p) -> d -> (q, p) -> x
runPhysicalSystem system (dqdtFunc, dpdtFunc) d (q, p)
  = runIdentity $ runPhysicalSystemT system (dqdtFunc, dpdtFunc) d (q, p)

flipRunPhysicalSystem :: (d -> (q, p) -> Diff q, d -> (q, p) -> Diff p) -> d -> (q, p) -> PhysicalSystem d q p x -> x
flipRunPhysicalSystem = (flip .) . flip . flip runPhysicalSystem

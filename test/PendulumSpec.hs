module PendulumSpec (spec) where
import Test.Hspec
import Pendulum

spec :: Spec
spec = do
  let pend = flipRunPendulum (1.0, 1.0) (pi/6.0, 0) getPhase 
  let pend2 = flipRunPendulum  (1.0, 1.0) (pi/6.0, 0) (symplecticEvol1 0.1 >> getPhase)
  describe "runPendulum getPhase (1.0, 1.0) (pi/6.0, 0)" $ do
    it "is equal to itself" $
      pend `shouldBe` (pi/6.0, 0)
--    it "'s diffPhase = (0, -0.49)" $
--      diff `shouldBe` ((0, -0.49), (pi/6.0, 0))
    it "deberops with 'symplecticEvol1 0.1' " $
      pend2 `shouldBe` (pi/6.0 - 0.49 * 0.1, -0.49)

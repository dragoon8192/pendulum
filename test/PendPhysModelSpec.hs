module PendPhysModelSpec (spec) where
import Test.Hspec
import PendPhysModel

spec :: Spec
spec = do
  let pend = Pendulum 1.0 1.0 (pi/6,0) :: Pendulum (Double,Double)
  describe "Pendulum 1.0 1.0 (pi/6,0)" $ do
    it "is equal to itself" $
      pend `shouldBe` Pendulum 1.0 1.0 (pi/6,0)
    it "deberops with 0.1 sec" $
      symplecticEvol 0.1 pend `shouldBe` Pendulum 1.0 1.0 (pi/6 - 0.49 * 0.1, -0.49)

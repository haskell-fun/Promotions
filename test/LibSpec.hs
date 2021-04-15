module LibSpec where

import Lib (application)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Wai (get, shouldRespondWith, with)
import Test.QuickCheck.Property ()

spec :: Spec
spec = describe "Simple test" $ do
  it "example-based unit test" $
    1 `shouldBe` 1

  prop "property-based unit test" $
    \l -> reverse (reverse l) == (l :: [Int])

  with (return application) $ do
    describe "GET /status" $ do
      it "responds with 200" $ do
        get "/status" `shouldRespondWith` 200

      it "responds with ok status" $ do
        let response = "{\"healthy\":\"ok\"}"
        get "/status" `shouldRespondWith` response

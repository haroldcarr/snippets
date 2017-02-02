{-# LANGUAGE OverloadedStrings #-}

import           App        as A
import           Sys        as S
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "test" test

-- the test simulates system operation
test :: Spec
test = do
  it "works" $
     do
       s <- A.appState
       r <- S.run s "1st"
       r `shouldBe` "initialValue response"
       -------------------------
       r <- S.run s "2nd"
       r `shouldBe` "1st response"
       -------------------------
       r <- S.run s "3rd"
       r `shouldBe` "2nd response"

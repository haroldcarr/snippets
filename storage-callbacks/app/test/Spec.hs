{-# LANGUAGE OverloadedStrings #-}

import           App        as A
import           Lib        as L
import           Test.Hspec as S

main :: IO ()
main = hspec $ do
  describe "test" test

test :: Spec
test = do
  it "works" $
     do
       s <- A.appState
       r <- L.run s (L.CmdRequest "1st")
       r `shouldBe` (L.CmdResponse "initialValue response")
       -------------------------
       r <- L.run s (L.CmdRequest "2nd")
       r `shouldBe` (L.CmdResponse "1st response")
       -------------------------
       r <- L.run s (L.CmdRequest "3rd")
       r `shouldBe` (L.CmdResponse "2nd response")

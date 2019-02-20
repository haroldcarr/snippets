{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE OverloadedLists #-}

module PendingRequestsSpec where

import           Lib
------------------------------------------------------------------------------
import           Data.Foldable   (foldr')
import qualified Data.Map.Strict as Map
import           Test.Hspec

c220 = Command 2 (RequestId (Nonce 2) 0)
c221 = Command 2 (RequestId (Nonce 2) 1) -- different shard
c230 = Command 2 (RequestId (Nonce 3) 0)
c310 = Command 3 (RequestId (Nonce 1) 0)

spec :: Spec
spec  = describe "pending requests" $ do
  addTo
  rmFrom
  rmAllBelow
  shuffle

------------------------------------------------------------------------------
addTo :: Spec
addTo  = do
  it "addToPR empty/empty" $
    addToPR c220 []
    `shouldBe`
    [(2, [(Nonce 2, c220)])]

  it "addToPR inner non-empty/non-empty" $
    addToPR c230 [(2, [(Nonce 2, c220)])]
    `shouldBe`
    [(2, [(Nonce 2, c220)
         ,(Nonce 3, c230)])]

  it "addToPR outer to non-empty/non-empty" $
    addToPR c310 [(2, [(Nonce 2, c220)])]
    `shouldBe`
    [(2, [(Nonce 2, c220)])
    ,(3, [(Nonce 1, c310)])]

  -- overwrite functionality which will not be used
  it "addToPR non-empty/non-empty overwrite" $
    addToPR c221 [(2, [(Nonce 2, c220)])]
    `shouldBe`
    [(2, [(Nonce 2, c221)])]

------------------------------------------------------------------------------
rmFrom :: Spec
rmFrom  = do
  it "rmFromPR empty/empty" $
    rmFromPR c220 []
    `shouldBe`
    []

  it "rmFromPR non-existent from non-empty/non-empty" $
    rmFromPR c220 [(2, [(Nonce 3, c230)])]
    `shouldBe`
    [(2, [(Nonce 3, c230)])]

  it "rmFromPR only member from non-empty/non-empty" $
    rmFromPR c220 [(2, [(Nonce 2, c220)])]
    `shouldBe`
    []

  it "rmFromPR inner member from non-empty/non-empty" $
    rmFromPR c230 [(2, [(Nonce 2, c220)
                       ,(Nonce 3, c230)])]
    `shouldBe`
    [(2, [(Nonce 2, c220)])]

  it "rmFromPR outer member from non-empty/non-empty" $
    rmFromPR c220 [(2, [(Nonce 2, c220)])
                  ,(3, [(Nonce 1, c310)])]
    `shouldBe`
    [(3, [(Nonce 1, c310)])]

------------------------------------------------------------------------------
rmAllBelow :: Spec
rmAllBelow  = do
  it "rmAllBelowPR empty/empty" $
    rmAllBelowPR 2 2 []
    `shouldBe`
    []

  it "rmAllBelowPR non-existent in non-empty/non-empty" $
    rmAllBelowPR 4 2 [(2, [(Nonce 2, c220)])
                     ,(3, [(Nonce 1, c310)])]
    `shouldBe`
    [(2, [(Nonce 2, c220)])
    ,(3, [(Nonce 1, c310)])]

  it "rmAllBelowPR non-empty/non-empty 1" $
    rmAllBelowPR 2 4 [(2, [(Nonce 2, c220)
                          ,(Nonce 3, c230)])
                     ,(3, [(Nonce 1, c310)])]
    `shouldBe`
    [(3, [(Nonce 1, c310)])]

  it "rmAllBelowPR non-empty/non-empty 2" $
    rmAllBelowPR 2 3 [(2, [(Nonce 2, c220)
                          ,(Nonce 3, c230)])
                     ,(3, [(Nonce 1, c310)])]
    `shouldBe`
    [(2, [(Nonce 3, c230)])
    ,(3, [(Nonce 1, c310)])]

  it "rmAllBelowPR non-empty/non-empty -> empty/empty" $
    rmAllBelowPR 2 4 [(2, [(Nonce 2, c220)
                          ,(Nonce 3, c230)])]
    `shouldBe`
    []

------------------------------------------------------------------------------
shuffle :: Spec
shuffle  = do
  s1 <- runIO (shufflePR [])
  it "shuffle empty/empty" $
    s1
    `shouldBe`
    []

  let pr55 = mkPR 5 5
  s2 <- runIO (shufflePR pr55)
  it "shuffle empty/empty" $
    s2
    `shouldBe`
    [] -- intentionally wrong to see the random output when running test

mkPR :: Int -> Int -> PendingRequests
mkPR numNodes numCmdsPerNode =
  foldr' goOuter Map.empty ([0 .. numNodes] :: [Int])
 where
  goOuter nid acc = foldr' (goInner nid) acc ([0 .. numCmdsPerNode] :: [Int])
  goInner nid nonce = addToPR (Command nid (RequestId (Nonce nonce) 0))

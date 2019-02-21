{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE OverloadedLists #-}

module PendingRequestsSpec where

import           Lib
------------------------------------------------------------------------------
import           Data.Foldable   (foldr')
import qualified Data.Map.Strict as Map
import           Test.Hspec

mkCmd :: NodeId -> Int -> ShardId -> Command
mkCmd nid n s = Command nid (RequestId (Nonce n) s)

c220 = mkCmd 2 2 0
c221 = mkCmd 2 2 1 -- different shard
c230 = mkCmd 2 3 0
c310 = mkCmd 3 1 0

mkPR :: [Command] -> PendingRequests
mkPR  = foldr' addToPR Map.empty

mkPR' :: Int -> Int -> PendingRequests
mkPR' numNodes numCmdsPerNode =
  foldr' addToPR Map.empty mkPR''
 where
  mkPR'' = do
    nid   <- [1 .. numNodes] :: [Int]
    nonce <- [1 .. numCmdsPerNode]
    pure (Command nid (RequestId (Nonce nonce) 0))

------------------------------------------------------------------------------
spec :: Spec
spec  = describe "pending requests" $ do
  addTo
  rmFrom
  rmAllBelow
  roundRobin

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
roundRobin :: Spec
roundRobin  = do
  it "roundRobinPR empty/empty" $
    roundRobinPR 99 []
    `shouldBe`
    []

  it "roundRobinPR empty/empty" $
    roundRobinPR 99 (addToPR (mkCmd 3 0 0) (mkPR' 2 2))
    `shouldBe`
    [ mkCmd 1 1 0
    , mkCmd 2 1 0
    , mkCmd 3 0 0
    , mkCmd 1 2 0
    , mkCmd 2 2 0
    ]


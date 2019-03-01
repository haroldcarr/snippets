{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE OverloadedLists #-}

module AERQSpec where

import           Lib
------------------------------------------------------------------------------
import qualified Data.Map.Strict as Map
import           Test.Hspec

fAER :: Int -> Int -> Int -> String -> AppendEntriesResponse
fAER t li n h = fakeAER (Term t) (NodeId n) (LogIndex li) (fakeCumulativeHash h) True

mkAERs :: Int -> Int -> [AppendEntriesResponse]
mkAERs t li = [ fAER t li n "1" | n <- [ 0.. ] ]

aer1,aer1',aer2 :: AppendEntriesResponse
aer1  = fAER 1 1 1 "aer1"
aer1' = aer1 { _aerHash = fakeCumulativeHash "bad" }
aer2  = fAER 1 1 2 "aer2"

spec :: Spec
spec  = do
  honestInsert
  dishonestInsert
  getQuorum

getQuorum :: Spec
getQuorum  = describe "getQuorum" $ do
  let xT1LI1 = mkAERs 1 1
      xT1LI2 = mkAERs 1 2
      xT2LI1 = mkAERs 2 1
      xT2LI2 = mkAERs 2 2

  it "no quorum" $
    aerqGetQuorum 3 <$> aerqInsertMany
                        (take 1 xT1LI1 ++ take 1 xT1LI2 ++ take 2 xT2LI1 ++ take 2 xT2LI2)
                        Map.empty
    `shouldBe`
    Right Nothing

  it "quorum" $
    aerqGetQuorum 3 <$> aerqInsertMany
                        (take 1 xT1LI1 ++ take 1 xT1LI2 ++ take 2 xT2LI1 ++ take 3 xT2LI2)
                        Map.empty
    `shouldBe`
    Right (Just (LogIndex 2,[(Term 2, take 3 xT2LI2)]))

  it "quorum >" $
    aerqGetQuorum 5 <$> aerqInsertMany (take 4 xT1LI1) Map.empty
    `shouldBe`
    Right Nothing

  it "quorum --" $
    aerqGetQuorum 4 <$> aerqInsertMany (take 4 xT1LI1) Map.empty
    `shouldBe`
    Right (Just (LogIndex 1,[(Term 1, take 4 xT1LI1)]))

  it "quorum <" $
    aerqGetQuorum 3 <$> aerqInsertMany (take 4 xT1LI1) Map.empty
    `shouldBe`
    Right (Just (LogIndex 1,[(Term 1, take 4 xT1LI1)]))

  it "two quorums for same LogIndex but different Terms" $
    aerqGetQuorum 3 <$> aerqInsertMany (take 4 xT1LI1 ++ take 4 xT2LI1) Map.empty
    `shouldBe`
    Right (Just (LogIndex 1,[(Term 2, take 4 xT2LI1)
                            ,(Term 1, take 4 xT1LI1)]))

  it "two quorums, ignore non-quorum" $
    aerqGetQuorum 3 <$> aerqInsertMany (take 4 xT1LI1 ++ take 4 xT2LI1 ++ take 2 xT2LI2) Map.empty
    `shouldBe`
    Right (Just (LogIndex 1,[(Term 2, take 4 xT2LI1)
                            ,(Term 1, take 4 xT1LI1)]))

honestInsert :: Spec
honestInsert  = describe "honestInsert" $
  it "aerqInsert same AER twice" $
    (aerqInsert aer1 Map.empty >>= aerqInsert aer1)
    `shouldBe`
    aerqInsert aer1 Map.empty

-- if someone lies
-- - they should have no influence (so they should be completely removed)
-- - they should not be able to interfere with honest ones
dishonestInsert :: Spec
dishonestInsert  = describe "dishonestInsert" $ do
  it "insert good after: aerqInsert same AER twice - but with different CumulativeHash-s" $
    (aerqInsert aer1 Map.empty >>= aerqInsert aer1' >>= aerqInsert aer2)
    `shouldBe`
    Left (mkAerqInsertError aer1' aer1, Map.empty)

  it "insert good middle: aerqInsert same AER twice - but with different CumulativeHash-s" $
    let Right xx = aerqInsert aer2 Map.empty
     in (aerqInsert aer1 Map.empty >>= aerqInsert aer2 >>= aerqInsert aer1')
        `shouldBe`
        Left (mkAerqInsertError aer1' aer1, xx)

  it "insert good before: aerqInsert same AER twice - but with different CumulativeHash-s" $
    let Right xx = aerqInsert aer2 Map.empty
     in (aerqInsert aer2 Map.empty >>= aerqInsert aer1 >>= aerqInsert aer1')
        `shouldBe`
        Left (mkAerqInsertError aer1' aer1, xx)


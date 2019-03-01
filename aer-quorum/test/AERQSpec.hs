{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE OverloadedLists #-}

module AERQSpec where

import           Lib
------------------------------------------------------------------------------
import qualified Data.Map.Strict as Map
import           Test.Hspec

nid1,nid2 :: NodeId
nid1  = NodeId 1
nid2  = NodeId 2
aer1,aer1',aer2 :: AppendEntriesResponse
aer1  = fakeAER (Term 1) nid1 (LogIndex 1) (fakeCumulativeHash "aer1") True
aer1' = aer1                   { _aerHash = fakeCumulativeHash "bad" }
aer2  = fakeAER (Term 1) nid2 (LogIndex 1) (fakeCumulativeHash "aer2") True

spec :: Spec
spec = do
  liars
  describe "AERQuorum" $ do
    it "aerqmInsert same AER twice" $ do
      aerqmInsert aer1 Map.empty >>= aerqmInsert aer1
      `shouldBe`
      aerqmInsert aer1 Map.empty

-- if someone lies
-- - they should have no influence (so they should be completely removed)
-- - they should not be able to interfere with honest ones
liars :: Spec
liars = describe "liars" $ do
  it "insert good after: aerqmInsert same AER twice - but with different CumulativeHash-s" $ do
    aerqmInsert aer1 Map.empty >>= aerqmInsert aer1' >>= aerqmInsert aer2
    `shouldBe`
    Left (mkAerqmInsertError aer1' aer1)

  it "insert good middle: aerqmInsert same AER twice - but with different CumulativeHash-s" $ do
    aerqmInsert aer1 Map.empty >>= aerqmInsert aer2  >>= aerqmInsert aer1'
    `shouldBe`
    Left (mkAerqmInsertError aer1' aer1)

  it "insert good before: aerqmInsert same AER twice - but with different CumulativeHash-s" $ do
    aerqmInsert aer2 Map.empty >>= aerqmInsert aer1  >>= aerqmInsert aer1'
    `shouldBe`
    Left (mkAerqmInsertError aer1' aer1)


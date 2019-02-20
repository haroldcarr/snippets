{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Lib where

import           Control.Lens
import           Control.Monad.Random.Class
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import           System.Random
import           System.Random.Shuffle

type NodeId  = Int
type ShardId = Int
data Command = Command
  { _cmdClientId  :: !NodeId
  , _cmdRequestId :: !RequestId
  } deriving (Show, Eq)

newtype Nonce = Nonce Int
  deriving (Show, Read, Eq, Ord, Enum, Num, Real, Integral)

data RequestId = RequestId
  { _ridNonce :: !Nonce
  , _ridShard :: !ShardId
  } deriving (Show, Read, Eq, Ord)

type PendingRequests = Map.Map NodeId (Map.Map Nonce Command)

-- https://hackage.haskell.org/package/lens-4.16.1/docs/Control-Lens-Iso.html#v:non
addToPR :: Command -> PendingRequests -> PendingRequests
addToPR c@Command {..} pr =
  pr & at _cmdClientId . non Map.empty . at (_ridNonce _cmdRequestId) ?~ c

rmFromPR :: Command -> PendingRequests -> PendingRequests
rmFromPR c@Command {..} pr =
  pr & at _cmdClientId . non Map.empty . at (_ridNonce _cmdRequestId) .~ Nothing

rmAllBelowPR :: NodeId -> Int -> PendingRequests -> PendingRequests
rmAllBelowPR nid i pr =
  case Map.lookup nid pr of
    Nothing    -> pr
    Just inner ->
      let inner' = Map.dropWhileAntitone (\(Nonce k) -> k < i) inner
       in if Map.null inner'
            then Map.delete nid pr
            else Map.insert nid inner' pr

-- TODO : this should shuffle by NodeId, but keep Nonce for each NodeId monotonically increasing.
shufflePR :: MonadRandom m => PendingRequests -> m [Command]
shufflePR pr =
  shuffleM (concatMap Map.elems (Map.elems pr))




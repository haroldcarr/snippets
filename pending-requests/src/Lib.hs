{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Lib where

import           Control.Lens
import           Data.List       (sortOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)
import           Data.Ord        (comparing)
import qualified Data.ByteString as BS

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

rmAllBelowPR :: NodeId -> Nonce -> PendingRequests -> PendingRequests
rmAllBelowPR nid i pr =
  case Map.lookup nid pr of
    Nothing    -> pr
    Just inner ->
      let inner' = Map.dropWhileAntitone (< i) inner
       in if Map.null inner'
            then Map.delete nid pr
            else Map.insert nid inner' pr

-- TODO : make more efficient version
roundRobinPR :: Int -> PendingRequests -> [Command]
roundRobinPR i pr =
  rr [] i (map (sortOn (_ridNonce . _cmdRequestId) . Map.elems)
               (Map.elems pr))
 where
  rr :: [[Command]] -> Int -> [[Command]] -> [Command]
  rr acc 0  _     = concat (reverse acc)
  rr acc _ []     = concat (reverse acc)
  rr acc n xs     = rr (mapMaybe safeHead xs : acc) (n - 1) (mapMaybe safeTail xs)
  safeHead    []  = Nothing
  safeHead (x: _) = Just x
  safeTail    []  = Nothing
  safeTail (_:xs) = Just xs


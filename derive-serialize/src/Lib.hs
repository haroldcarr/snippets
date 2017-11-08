{-# LANGUAGE DeriveGeneric #-}

module Lib where

import           Data.ByteString
import           Data.Serialize
import           Data.Set
import           GHC.Generics

data UserTree a
  = Leaf
  | Node (Set a) (UserTree a) (UserTree a)
  deriving (Show, Generic)
instance (Ord a, Serialize a) => Serialize (UserTree a)

run :: IO (Bool         , ByteString, Either String           Bool,
           UserTree Bool, ByteString, Either String (UserTree Bool),
           UserTree Bool, ByteString, Either String (UserTree Bool))
run = do
  let xs = True
  let ys = Leaf :: UserTree Bool
  let zs = Node (Data.Set.singleton False) Leaf Leaf :: UserTree Bool
  let exs = encode xs
  let eys = encode ys
  let ezs = encode zs
  let dxs = decode exs
  let dys = decode eys
  let dzs = decode ezs
  return (xs, exs, dxs,
          ys, eys, dys,
          zs, ezs, dzs)


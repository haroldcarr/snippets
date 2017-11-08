{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Code where

import           Data.ByteString
import           Data.IntervalMap.Generic.Strict
import           Data.Serialize
import           Data.Set
import           GHC.Generics

------------------------------------------------------------------------------

newtype LogIndex = LogIndex Int deriving (Eq, Ord, Generic)
instance Serialize LogIndex

newtype LogIndexInterval a = LogIndexInterval LogIndex deriving (Eq, Ord, Generic)
instance Serialize (LogIndexInterval LogIndex)

newtype ActiveAssignment = ActiveAssignment Int deriving (Eq, Ord, Generic)
instance Serialize ActiveAssignment

data ActiveAssignments
  = IntervalMap (LogIndexInterval LogIndex) (Set ActiveAssignment)
  deriving (Eq, Ord, Generic)
instance Serialize ActiveAssignments

------------------------------------------------------------------------------

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
  let zs = Node (Data.Set.singleton False) Leaf Leaf
  let exs = encode xs
  let eys = encode ys
  let ezs = encode zs
  let dxs = decode exs
  let dys = decode eys
  let dzs = decode ezs
  return (xs, exs, dxs,
          ys, eys, dys,
          zs, ezs, dzs)


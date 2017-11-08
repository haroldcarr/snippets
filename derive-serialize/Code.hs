{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Code where

import           Data.ByteString
import           Data.IntervalMap.Generic.Strict
import           Data.Serialize
import           Data.Set
import           Data.Word                       (Word64)
import           GHC.Generics

------------------------------------------------------------------------------

newtype LogIndex = LogIndex Int
  deriving (Show, Read, Eq, Ord, Enum, Num, Real, Integral, Generic, Serialize)

data LogIndexInterval a
  = LogIndexInterval a a
  deriving (Eq , Ord , Show , Generic)
instance (Serialize a) => Serialize (LogIndexInterval a)

data NodeID = NodeID { _host :: !String, _port :: !Word64, _fullAddr :: !String }
  deriving (Eq,Ord,Read,Generic)
instance Show NodeID where
  show = ("NodeID " ++) . _fullAddr
instance Serialize NodeID

data ActiveAssignment = ActiveAssignment
  { _activeAssignmentActiveInterval :: !(LogIndexInterval LogIndex)
  , _activeAssignmentNodeID         :: !NodeID
  }
  deriving (Eq, Show, Ord, Generic)
instance Serialize ActiveAssignment

type ActiveAssignments = IntervalMap (LogIndexInterval LogIndex) (Set ActiveAssignment)

{-
instance (Generic a, Generic b) => Generic (IntervalMap a b)
instance (Generic a, Generic b, Serialize a, Serialize b) => Serialize (IntervalMap a b)
instance Generic   ActiveAssignments
instance Serialize ActiveAssignments
-}

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


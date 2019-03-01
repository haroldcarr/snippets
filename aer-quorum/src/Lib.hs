{-# LANGUAGE RecordWildCards #-}

module Lib where

import           Control.Lens
import qualified Data.Map.Strict as Map

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}


newtype LogIndex              = LogIndex       Int       deriving (Show, Eq, Ord)
newtype Term                  = Term           Int       deriving (Show, Eq, Ord)
newtype NodeId                = NodeId         Int       deriving (Show, Eq, Ord)
newtype CumulativeHash        = CumulativeHash String    deriving (Show, Eq, Ord)
data    AERSuccess            = Success   | Failure      deriving (Show, Eq, Ord)
data    AERConvinced          = Convinced | NotConvinced deriving (Show, Eq, Ord)
data    AppendEntriesResponse = AppendEntriesResponse
  { _aerTerm        :: !Term
  , _aerIndex       :: !LogIndex
  , _aerNodeId      :: !NodeId
  , _aerSuccess     :: !AERSuccess
  , _aerConvinced   :: !AERConvinced
  , _aerHash        :: !CumulativeHash
  , _aerWasVerified :: !Bool
  } deriving (Show, Eq)

type AERQuorum = Map.Map (LogIndex, Term)
                         (Map.Map NodeId AppendEntriesResponse)

fakeAER :: Term -> NodeId -> LogIndex -> CumulativeHash -> Bool -> AppendEntriesResponse
fakeAER t n l c b = AppendEntriesResponse t l n Success Convinced c b

fakeCumulativeHash :: String -> CumulativeHash
fakeCumulativeHash = CumulativeHash

aerqmInsert :: AppendEntriesResponse -> AERQuorum -> Either String AERQuorum
aerqmInsert aer aerq =
  case aerqmLookup aer aerq of
    Nothing ->
      return $ aerqmInsertUnconditional aer aerq
    Just e  ->
      if _aerHash e == _aerHash aer
      then return aerq
      else Left (mkAerqmInsertError aer e)

mkAerqmInsertError :: AppendEntriesResponse -> AppendEntriesResponse -> String
mkAerqmInsertError a1 a2 =
  "aerqmInsert: AER with different hash; " ++ show a1 ++ "; " ++ show a2

-- https://hackage.haskell.org/package/lens-4.16.1/docs/Control-Lens-Iso.html#v:non
aerqmInsertUnconditional :: AppendEntriesResponse -> AERQuorum -> AERQuorum
aerqmInsertUnconditional aer@AppendEntriesResponse {..} aerq =
  aerq &  at (_aerIndex, _aerTerm) . non Map.empty
       .  at _aerNodeId
       ?~ aer

aerqmLookup :: AppendEntriesResponse -> AERQuorum -> Maybe AppendEntriesResponse
aerqmLookup AppendEntriesResponse {..} aerq =
      Map.lookup (_aerIndex, _aerTerm) aerq
  >>= Map.lookup _aerNodeId

aerqmDeleteAllForNodeId :: NodeId -> AERQuorum -> AERQuorum
aerqmDeleteAllForNodeId nid aerq =
  Map.foldrWithKey go aerq aerq
 where
  go k m acc = let xx = Map.filterWithKey (\n _ -> n /= nid) m
                in if null xx then Map.delete k    acc
                              else Map.insert k xx acc

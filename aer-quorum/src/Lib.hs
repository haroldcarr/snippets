{-# LANGUAGE RecordWildCards #-}

module Lib where

import           Control.Lens    hiding (transform)
import qualified Data.List       as L
import qualified Data.Map.Strict as Map
import           Data.Ord        (Down (..))

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
                         (Map.Map NodeId
                                  AppendEntriesResponse)

fakeAER :: Term -> NodeId -> LogIndex -> CumulativeHash -> Bool -> AppendEntriesResponse
fakeAER t n l c b = AppendEntriesResponse t l n Success Convinced c b

fakeCumulativeHash :: String -> CumulativeHash
fakeCumulativeHash  = CumulativeHash

aerqInsertMany :: [AppendEntriesResponse] -> AERQuorum -> Either (String, AERQuorum) AERQuorum
aerqInsertMany aers m =
  L.foldl' (\mm aer -> mm >>= aerqInsert aer) (Right m) aers

-- | Returns an error if the given AER is already present but with a different hash.
-- The error returns the given quorum structure, but with all AERs for the
-- associated NodeId removed.
aerqInsert :: AppendEntriesResponse -> AERQuorum -> Either (String, AERQuorum) AERQuorum
aerqInsert aer aerq =
  if _aerSuccess aer /= Success || _aerConvinced aer /= Convinced
  then Left (mkAerqInsertSucConError aer, aerq)
  else
    case aerqLookup aer aerq of
      Nothing ->
        return $ aerqInsertUnconditional aer aerq
      Just e  ->
        if _aerHash e == _aerHash aer
        then return aerq
        else Left ( mkAerqInsertHashDiffError aer e
                  , aerqDeleteAllForNodeId (_aerNodeId e) aerq )

-- https://hackage.haskell.org/package/lens-4.16.1/docs/Control-Lens-Iso.html#v:non
aerqInsertUnconditional :: AppendEntriesResponse -> AERQuorum -> AERQuorum
aerqInsertUnconditional aer@AppendEntriesResponse {..} aerq =
  aerq &  at (_aerIndex, _aerTerm) . non Map.empty
       .  at _aerNodeId
       ?~ aer

mkAerqInsertSucConError :: AppendEntriesResponse -> String
mkAerqInsertSucConError a =
  "aerqmInsert: AER with unsuccessful or not convinced; " ++ show a

mkAerqInsertHashDiffError :: AppendEntriesResponse -> AppendEntriesResponse -> String
mkAerqInsertHashDiffError a1 a2 =
  "aerqmInsert: AER with different hash; " ++ show a1 ++ "; " ++ show a2

aerqLookup :: AppendEntriesResponse -> AERQuorum -> Maybe AppendEntriesResponse
aerqLookup AppendEntriesResponse {..} aerq =
      Map.lookup (_aerIndex, _aerTerm) aerq
  >>= Map.lookup _aerNodeId

aerqDeleteAllForNodeId :: NodeId -> AERQuorum -> AERQuorum
aerqDeleteAllForNodeId nid aerq =
  Map.foldrWithKey go aerq aerq
 where
  go k m acc = let xx = Map.filterWithKey (\n _ -> n /= nid) m
                in if null xx then Map.delete k    acc
                              else Map.insert k xx acc

-- | Returns a list of 'AppendEntriesResponse' for the highest possible 'LogIndex'
-- that has at least @qsize@ aer.
-- If this returns more than one element, that means there are
-- different quorums for the same LogIndex different Terms.
-- That is dealt with by the caller.
-- If no quorum exists, returns an empty list.
aerqGetQuorum
  :: Int
  -> AERQuorum
  -> Maybe (LogIndex, [(Term, [AppendEntriesResponse])])
aerqGetQuorum qsize
  = transform                            -- transform to output signature
  . head'                                -- get the largest quorums
  . L.groupBy cmpLI                      -- group quorums of the same size
  . L.sortOn (Down . getLI)              -- largest quorums first
  . filter ((>= qsize) . Map.size . snd) -- get quorums
  . Map.toList
 where
  head'   []  = []
  head' (x:_) = x
  getLI       = fst . fst
  cmpLI x y   = getLI x == getLI y
  transform ::      [((LogIndex,   Term), Map.Map NodeId AppendEntriesResponse)]
            -> Maybe  (LogIndex, [(Term,                [AppendEntriesResponse])])

  transform []               = Nothing
  transform a@(((li,_),_):_) = Just (li, L.foldl' (go li) [] a)
  go :: LogIndex
     -> [(Term, [AppendEntriesResponse])]
     -> ((LogIndex, Term), Map.Map NodeId AppendEntriesResponse)
     -> [(Term, [AppendEntriesResponse])]
  go li0 acc ((li, t), m) =
    if li /= li0 then error "LogIndex do not match" -- TODO : prove not necessary and remove
    else (t, Map.elems m) : acc

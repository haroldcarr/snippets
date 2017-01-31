{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module TheSpec where

import           Control.Lens           hiding (Index, (|>))
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import           Data.ByteString.Char8  (putStrLn)
import           GHC.Generics           hiding (from)
import           Prelude                hiding (putStrLn)

data Spec m a = Spec
  {
    _applyLogEntry   :: a -> Command -> m (a, CommandResult)
  }

newtype CommandEntry = CommandEntry { unCommandEntry :: ByteString }
  deriving (Show, Eq, Ord, Generic)

newtype CommandResult = CommandResult { unCommandResult :: ByteString }
  deriving (Show, Eq, Ord, Generic)

data Command = Command
  { _cmdEntry      :: !CommandEntry
  }
  deriving (Show, Eq, Generic)
makeLenses ''Command

spec :: MonadIO m
     => (a -> Command -> m (a, CommandResult))
     -> Spec m a
spec applyFn = Spec
    {
      _applyLogEntry   = applyFn
    }

mkSpec :: MonadIO m
    => ((String -> IO ()) -> a -> Command -> IO (a, CommandResult))
    -> Spec m a
mkSpec applyFn =
  spec (liftIO2 (applyFn (\_ -> return ())))

run :: MonadIO m
    => Spec m a
    -> a -> Command -> m (a, CommandResult)
run spec@Spec{..}s c = _applyLogEntry s c

liftIO2 :: MonadIO m => (a -> b -> IO c) -> a -> b -> m c
liftIO2 f a = liftIO . f a

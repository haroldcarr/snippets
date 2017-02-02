{-# LANGUAGE OverloadedStrings #-}

module App where

import           Control.Monad.IO.Class
import           Data.ByteString        as BS hiding (putStrLn)
import           Data.ByteString.Char8  (putStrLn)
import           Lib
import           Prelude                hiding (putStrLn)

appApplyFn :: (String -> IO ()) -> ByteString -> CmdRequest -> IO (ByteString, CmdResponse)
appApplyFn _ s c@(CmdRequest bs) = do
  putStrLn (unCmdRequest c)
  return (bs, CmdResponse (BS.append s " response"))

appState :: IO (State IO ByteString)
appState = mkState "initialValue" appApplyFn


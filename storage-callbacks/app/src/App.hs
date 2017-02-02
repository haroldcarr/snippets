{-# LANGUAGE OverloadedStrings #-}

module App where

import           Control.Monad.IO.Class
import           Data.ByteString        as BS hiding (putStrLn)
import           Data.ByteString.Char8  (putStrLn)
import           Prelude                hiding (putStrLn)
import           Sys

appApplyFn :: (String -> IO ()) -> ByteString -> CmdRequest -> IO (ByteString, CmdResponse)
appApplyFn _ v c = do
  putStrLn v
  putStrLn c
  -- c will be stored to be used next time in the example
  -- v is from initialization or the previous call
  return (c, BS.append v " response")

appState :: IO (State IO ByteString)
appState = mkState "initialValue" appApplyFn


{-# LANGUAGE OverloadedStrings #-}

module WriteBuffer.Wai where

import           WriteBuffer

import           Control.Concurrent.STM
import           Data.Aeson
import qualified Data.ByteString.Lazy   as BS
import           Data.Monoid
import qualified Network.HTTP.Types     as H
import           Network.Wai
import           System.IO              (stderr)

writeBufferApplication :: FromJSON rec => Int -> IO (TBQueue rec, Application)
writeBufferApplication qSize= do
    q <- atomically $ newTBQueue qSize
    pure (q, app q)
  where
    app queue req respond
        | requestMethod req == "POST" = do
            body <- lazyRequestBody req
            case decode body of
                Nothing -> do
                    BS.hPutStr stderr $ "Failed to parse " <> body
                    respond $ responseLBS H.status400 [] ""
                Just rec -> do
                    atomically $ (writeTBQueue queue $! rec)
                    respond $ responseLBS H.status201 [] ""
        | otherwise = do
            respond $ responseLBS H.status405 [] ""



{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Monad.Logger
import           WriteBuffer
import           WriteBuffer.Persistent
import           WriteBuffer.Wai

import           Control.Concurrent
import           Control.Concurrent.STM.TBQueue
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Database.Persist
import           Database.Persist.MySQL
import           Network.Wai.Handler.Warp

import           Models

main :: IO ()
main = do
    (q, app) <- writeBufferApplication 10000
    runStdoutLoggingT $ withMySQLPool connectInfo 8 $ \pool -> do
        liftIO $ putStrLn "Starting the write buffer"
        runWriteBuffer $ persistToDatabase @TimeSeries q (flip runSqlPool pool)
        liftIO $ do
            putStrLn "Starting web app"
            run 8012 app

connectInfo = defaultConnectInfo
    { connectUser = "write_buffer_example"
    , connectPassword = "write_buffer_example"
    , connectDatabase = "write_buffer_example"
    , connectHost = "localhost"
    }

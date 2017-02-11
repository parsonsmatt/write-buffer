{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module WriteBuffer where

import           Control.Concurrent.Lifted
import           Control.Concurrent.STM
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.ByteString             (ByteString)
import           Data.DList
import           Data.Int
import           Data.IORef.Lifted
import           System.Clock
import           System.IO
import           System.Timeout

data WriteBufferOpts rec m
    = WriteBufferOpts
    { saveRecords      :: [rec] -> m ()
    , onError          :: SomeException -> m ()
    , maxBufferSize    :: Int
    , maxTimeToWait    :: Integer
    , bufferInputQueue :: TBQueue rec
    }

makeBufferOpts :: MonadIO m => TBQueue rec -> ([rec] -> m ()) -> WriteBufferOpts rec m
makeBufferOpts bufferInputQueue saveRecords = WriteBufferOpts {..}
  where
    maxBufferSize = 1000
    maxTimeToWait = 5 * 10^9
    onError = liftIO . print

runWriteBuffer :: (MonadCatch m, MonadBaseControl IO m, MonadIO m) => WriteBufferOpts rec m -> m ThreadId
runWriteBuffer WriteBufferOpts{..} = fork $ do
    timeRef <- newIORef =<< liftIO (getTime Monotonic)
    forever $ loop empty 0 timeRef
  where
    loop inputs len timeRef
        | len >= maxBufferSize = do
            saveRecords (toList inputs)
            loop empty 0 timeRef
        | otherwise = do
            earlier <- liftIO $ readIORef timeRef
            now <- liftIO $ getTime Monotonic
            let diff = diffTimeSpec now earlier
            if toNanoSecs diff >= maxTimeToWait
                then do
                    liftIO $ writeIORef timeRef now
                    saveRecords (toList inputs) `catch` onError
                    loop empty 0 timeRef
                else do
                    minput <- liftIO $ timeout (fromIntegral maxTimeToWait `div` 1000) $ atomically $ readTBQueue bufferInputQueue
                    loop (maybe id (flip snoc) minput inputs) (len + 1) timeRef

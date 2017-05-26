{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module WriteBuffer
    ( module WriteBuffer
    , module X
    ) where

import           Control.Concurrent.Lifted
import           Control.Concurrent.STM          as X
import           Control.Concurrent.STM.TBMQueue as X
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.ByteString                 (ByteString)
import           Data.DList
import           Data.Int
import           Data.IORef.Lifted
import           System.Clock
import           System.IO
import           System.Timeout

data WriteBufferOpts rec m
    = WriteBufferOpts
    { maxBufferSize    :: Int
    , maxTimeToWait    :: Integer
    , bufferInputQueue :: TBMQueue rec
    , onError          :: SomeException -> m ()
    , saveRecords      :: [rec] -> m ()
    }

makeBufferOpts :: MonadIO m => TBMQueue rec -> ([rec] -> m ()) -> WriteBufferOpts rec m
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
            if toNanoSecs diff >= maxTimeToWait then do
                liftIO $ writeIORef timeRef now
                saveRecords (toList inputs) `catch` onError
                loop empty 0 timeRef
            else do
                mminput <- liftIO
                    $ timeout (fromIntegral maxTimeToWait `div` 1000)
                    $ atomically
                    $ readTBMQueue bufferInputQueue
                case mminput of
                    Nothing ->  -- timeout
                        loop inputs len timeRef
                    Just minput ->
                        case minput of
                            Nothing -> -- closed queue
                                pure ()
                            Just input ->
                                loop (inputs `snoc` input) (len + 1) timeRef

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module WriteBuffer.Persistent where

import           WriteBuffer

import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Database.Persist.Class
import           Database.Persist.Sql

persistToDatabase
    ::
    ( PersistRecordBackend rec backend
    , PersistStoreWrite backend
    , MonadIO m
    , backend ~ SqlBackend
    , MonadBaseControl IO m
    , Show rec
    )
    => TBQueue rec
    -> (forall a. ReaderT backend m a -> m a)
    -> WriteBufferOpts rec m
persistToDatabase q k = makeBufferOpts q $ \xs -> do
    liftIO (print xs)
    e <- try $ k $ insertMany_ xs
    liftIO $ case e of
        Left (err :: SomeException) -> do
            print err
        Right _ -> pure ()

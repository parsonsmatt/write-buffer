module WriteBuffer.STM.TBMQueue
    ( module WriteBuffer.STM.TBMQueue
    , module X
    ) where

import           WriteBuffer                     as X

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMQueue
import           Control.Monad.IO.Class

-- | This function takes a @'TBMQueue' a@ and yields a new @'TBMQueue' [a]@
-- which yields lists of the original inputs. The first parameter is the
-- bound of the output queue size -- the threads will block if this size is
-- reached and nothing is drawing from it. The second parameter is the
-- chunk size -- at most, this many elements will be in each output list.
-- The third parameter is the maximum amount of time to wait before
-- yielding a list.
writeBufferTBMQueue
    :: Int -- ^ The output queue bound
    -> Int -- ^ The chunk size for the output lists.
    -> Integer -- ^ The amount of time to wait in microseconds.
    -> TBMQueue a -- ^ The input queue.
    -> IO (TBMQueue [a])
writeBufferTBMQueue bound chunkSize i input = do
    output <- newTBMQueueIO bound
    let opts = (makeBufferOpts input (liftIO . atomically . writeTBMQueue output))
                { maxTimeToWait = i
                , maxBufferSize = chunkSize
                }
    runWriteBuffer opts
    pure output


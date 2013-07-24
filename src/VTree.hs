module VTree
    ( Addr(..)
    , module VTree
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Serialize
import Data.Word
import System.Serial
import System.Posix.Terminal
import Network.Protocol.XBee
import Network.Protocol.XBee.Series1

data VTree = VTree !XBee !ThreadId !(TVar (Word16, Word16, Word16))
type Level = Word16

data Response = OK | Fail !String
    deriving (Eq, Ord, Show)

sender :: XBee -> TVar (Word16, Word16, Word16) -> IO ()
sender h t = go =<< atomically (readTVar t)
    where
        go prev = do
            next <- atomically $ do
                next <- readTVar t
                when (next == prev) retry
                return next
            
            send h (Addr16 2) next
            threadDelay (round (1e6 / 30 :: Double))
            go next

-- note: use the 'cu' device on Mac OS, not the 'tty'
openVTree :: FilePath -> IO VTree
openVTree port = do
    x <- runXBee True =<< openSerial port B38400 8 One NoParity NoFlowControl
    
    t <- newTVarIO (0,0,0)
    tid <- forkIO (sender x t)
    
    return (VTree x tid t)

closeVTree :: VTree -> IO ()
closeVTree (VTree x t _) = do
    killThread t
    closeXBee x

setRGB :: VTree -> Addr -> Double -> Double -> Double -> IO ()
setRGB h addr r g b = setRGB16 h addr (f r) (f g) (f b)
    where
        clip lo hi = max lo . min hi
        f x = truncate (clip 0 0xFFFF (0x10000 * x))

-- TODO: handle addr, currently it's ignored
setRGB16 :: VTree -> Addr -> Level -> Level -> Level -> IO ()
setRGB16 (VTree _ _ t) _addr r g b = atomically (writeTVar t (r, g, b))

send :: XBee -> Addr -> (Word16, Word16, Word16) -> IO ()
send x addr (r, g, b) = writeXBee x $
    TxFrame (Tx 0 (Just txDisableAck) addr msg)
    where msg = runPut (mapM_ putWord16be [b,g,r])

module VTree where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Serialize
import Data.Word
import System.Serial
import System.Posix.Terminal
import System.IO
import Network.Protocol.XBee
import Network.Protocol.XBee.Series1

data VTree = VTree !(XBee Series1) !ThreadId !(TVar (Word16, Word16, Word16))
type Level = Word16

data Response = OK | Fail !String
    deriving (Eq, Ord, Show)

-- type Addr = Word64

prototype :: Addr
prototype =
    Addr16 2
    -- 0x13a20040b087b0

sender :: XBee Series1 -> TVar (Word16, Word16, Word16) -> IO ()
sender h t = go =<< atomically (readTVar t)
    where
        go prev = do
            next <- atomically $ do
                next <- readTVar t
                when (next == prev) retry
                return next
            
            send h prototype next
            threadDelay (round (1e6 / 30 :: Double))
            go next

foo port = do
    p <- openSerial port B9600  8 One NoParity NoFlowControl
    
    hClose p

-- note: use the 'cu' device on Mac OS, not the 'tty'
openVTree :: FilePath -> IO VTree
openVTree port = do
    foo port
    x <- runXBee True =<< openSerial port B38400 8 One NoParity NoFlowControl
    
    t <- newTVarIO (0,0,0)
    tid <- forkIO (sender x t)
    
    return (VTree x tid t)

bar :: Num a => a
bar = 42

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

send :: XBee Series1 -> Addr -> (Word16, Word16, Word16) -> IO ()
send x addr (r, g, b) = writeXBee x $
    --TxFrame (Tx 0 addr 0xFFFE 0 Nothing msg)
        TxFrame (Tx 0 (Just txDisableAck) addr msg)

    where msg = runPut (mapM_ putWord16be [r,g,b])

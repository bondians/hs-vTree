module VTree where

import Control.Applicative
import Control.Concurrent
import Data.Word
import System.IO
import System.Serial
import System.Posix.Terminal

data VTree = VTree !Handle !(MVar Word8)
type Level = Word8

data Response = OK | Fail !String
    deriving (Eq, Ord, Show)

-- note: use the 'cu' device on Mac OS, not the 'tty'
openVTreeLed port addr = VTree
    <$> openSerial port B38400 8 One NoParity NoFlowControl
    <*> newMVar addr

closeVTree (VTree h _) = hClose h

parseResponse "Ok" = OK
parseResponse other = Fail other
readResponse (VTree h _) = parseResponse <$> hGetLine h

sendCommand' (VTree h _) = hPutStrLn h . unwords
sendCommand vTree cmd = do
    sendCommand' vTree cmd
    readResponse vTree

setCmd :: String -> VTree -> Level -> IO ()
setCmd cmd vTree@(VTree _ a) level = do
    addr <- readMVar a
    sendCommand' vTree [cmd, show addr, show level]

data Color = Red | Yellow | Green | Blue
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

set Red    = setCmd "setRed"
set Yellow = setCmd "setYellow"
set Green  = setCmd "setGreen"
set Blue   = setCmd "setBlue"

setAll vt level = sequence_
    [ set color vt level
    | color <- [minBound .. maxBound]
    ]

setRYGB :: VTree -> Word8 -> Word8 -> Word8 -> Word8 -> IO ()
setRYGB vt@(VTree _ a) r y g b = do
    addr <- readMVar a
    sendCommand' vt ["setRYGB", show addr, show r, show y, show g, show b]
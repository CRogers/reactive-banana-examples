module YouPressedKey where

import Control.Monad (forever)
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin)
import Reactive.Banana
import Reactive.Banana.Frameworks

main :: IO ()
main = do
    (addKeyEvent, fireKey) <- newAddHandler
    network <- compile $ makeNetworkDescription addKeyEvent
    actuate network
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    forever (getChar >>= fireKey)

makeNetworkDescription :: Frameworks t => AddHandler Char -> Moment t ()
makeNetworkDescription addKeyEvent = do
    eKey <- fromAddHandler addKeyEvent
    reactimate $ (\n -> putStrLn $ "You pressed: " ++ show n) <$> eKey
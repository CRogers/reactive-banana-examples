module Counter where

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
    eNumEventsChanged <- changes $ eventCounter eKey
    reactimate' $ fmap (\n -> putStrLn $ "Counter is at: " ++ show n) <$> eNumEventsChanged

eventCounter :: Event t a -> Behavior t Int
eventCounter = accumB 0 . fmap (const (+1))
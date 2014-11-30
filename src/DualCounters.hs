module DualCounters where

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
    keys <- fromAddHandler addKeyEvent
    let aKeys = filterE (== 'a') keys
    let bKeys = filterE (== 'b') keys
    let numAKeys = eventCounter aKeys
    let numBKeys = eventCounter bKeys
    let numABKeys = (,) <$> numAKeys <*> numBKeys
    numABKeysChanged <- changes numABKeys
    reactimate' $ fmap (\(a, b) -> putStrLn $ "A keys: " ++ show a ++ ", B keys: " ++ show b) <$> numABKeysChanged

eventCounter :: Event t a -> Behavior t Int
eventCounter = accumB 0 . fmap (const (+1))
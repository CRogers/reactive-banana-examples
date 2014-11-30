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
    let numAKeys = keyCounter 'a' keys
    let numBKeys = keyCounter 'b' keys
    let numABKeys = (,) <$> numAKeys <*> numBKeys
    numABKeysChanged <- changes numABKeys
    reactimate' $ fmap (\(a, b) -> putStrLn $ "A keys: " ++ show a ++ ", B keys: " ++ show b) <$> numABKeysChanged

keyCounter :: Char -> Event t Char -> Behavior t Int
keyCounter c = eventCounter . filterE (== c)

eventCounter :: Event t a -> Behavior t Int
eventCounter = accumB 0 . fmap (const (+1))
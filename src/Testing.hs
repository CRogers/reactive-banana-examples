{-# LANGUAGE Rank2Types #-}

module Testing where

import Data.IORef

import Control.Monad (forever, forM)
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

interpretFrameworks' :: (forall t. Event t a -> Behavior t b) -> [a] -> IO [[b]]
interpretFrameworks' f xs = do
    output <- newIORef []
    (addHandler, runHandlers) <- newAddHandler
    network <- compile $ do
        e <- fromAddHandler addHandler
        o <- changes $ f e
        reactimate' $ (fmap . fmap) (\b -> modifyIORef output (++[b])) o

    actuate network
    bs <- forM xs $ \x -> do
        runHandlers x
        bs <- readIORef output
        writeIORef output []
        return bs
    return bs

test :: IO Bool
test = do
    states <- interpretFrameworks' eventCounter [(), ()]
    return $ last states == [2]
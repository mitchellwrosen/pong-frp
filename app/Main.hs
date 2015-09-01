{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Break (Break)
import qualified Control.Break as Break
import Control.Monad.Trans.Class (lift)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.Function (fix)
import Foreign.C.Types (CInt)
import Linear.Affine
import Linear.V2
import Linear.V4
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified SDL

data Pong = Pong
    { pongPaddle1 :: Paddle
    , pongPaddle2 :: Paddle
    } deriving Show

data InputEvent
    = UpPressed
    | UpReleased
    | DownPressed
    | DownReleased
    | WPressed
    | WReleased
    | SPressed
    | SReleased
    deriving Show

fps :: RealFrac a => a -> Int
fps x = floor (1000000/x)

main :: IO ()
main = do
    SDL.initialize [SDL.InitEverything]
    window <- SDL.createWindow "My app" SDL.defaultWindow
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    (eventAddHandler, fireEvent)  <- newAddHandler
    (frameAddHandler, frameEvent) <- newAddHandler
    network <- compile (makeNetwork renderer frameAddHandler eventAddHandler)
    actuate network

    framesChan <- newChan

    forkIO . forever $ do
        writeChan framesChan ()
        threadDelay (fps 60)

    Break.loop $ do
        let f :: SDL.Event -> Break () IO ()
            f e =
                case SDL.eventPayload e of
                    SDL.KeyboardEvent (SDL.KeyboardEventData _ pressed False (SDL.Keysym _ keycode _)) ->
                         case (keycode, pressed) of
                             (SDL.KeycodeUp,   SDL.Pressed)  -> lift $ fireEvent UpPressed
                             (SDL.KeycodeDown, SDL.Pressed)  -> lift $ fireEvent DownPressed
                             (SDL.KeycodeW,    SDL.Pressed)  -> lift $ fireEvent WPressed
                             (SDL.KeycodeS,    SDL.Pressed)  -> lift $ fireEvent SPressed
                             (SDL.KeycodeUp,   SDL.Released) -> lift $ fireEvent UpReleased
                             (SDL.KeycodeDown, SDL.Released) -> lift $ fireEvent DownReleased
                             (SDL.KeycodeW,    SDL.Released) -> lift $ fireEvent WReleased
                             (SDL.KeycodeS,    SDL.Released) -> lift $ fireEvent SReleased
                             (SDL.KeycodeQ,    SDL.Pressed)  -> Break.break ()
                    _ -> pure ()

        SDL.mapEvents f
        lift $ readChan framesChan >>= frameEvent

makeNetwork :: forall t. Frameworks t => SDL.Renderer -> AddHandler () -> AddHandler InputEvent -> Moment t ()
makeNetwork renderer frameAddHandler eventAddHandler = do
    frames :: Event t ()         <- fromAddHandler frameAddHandler
    events :: Event t InputEvent <- fromAddHandler eventAddHandler

    let bpaddle1 :: Behavior t Paddle
        bpaddle1 = stepper id (f <$> events) <*> pure initPaddle1 -- accumB initPaddle1 (f <$> events)
          where
            f :: InputEvent -> (Paddle -> Paddle)
            f UpPressed   = paddleUp
            f DownPressed = paddleDown
            f _           = id

        bpaddle2 :: Behavior t Paddle
        bpaddle2 = stepper id (f <$> events) <*> pure initPaddle2 -- accumB initPaddle2 (f <$> events)
          where
            f :: InputEvent -> Paddle -> Paddle
            f WPressed = paddleUp
            f SPressed = paddleDown
            f _        = id

        bpong :: Behavior t Pong
        bpong = Pong <$> bpaddle1 <*> bpaddle2

        epong :: Event t Pong
        epong = bpong <@ frames

    reactimate (print <$> epong)
    -- reactimate (renderGame renderer <$> epong)

type Paddle = SDL.Rectangle CInt

initPaddle1 :: Paddle
initPaddle1 = SDL.Rectangle (P (V2 100 100)) (V2 10 60)

initPaddle2 :: Paddle
initPaddle2 = SDL.Rectangle (P (V2 200 200)) (V2 10 60)

paddleDown :: Paddle -> Paddle
paddleDown (SDL.Rectangle x y) = SDL.Rectangle (x + (P (V2 0 1))) y

paddleUp :: Paddle -> Paddle
paddleUp (SDL.Rectangle x y) = SDL.Rectangle (x - (P (V2 0 1))) y

renderGame :: SDL.Renderer -> Pong -> IO ()
renderGame renderer (Pong p1 p2) = do
    SDL.renderDrawColor renderer SDL.$= V4 0 0 0 255
    SDL.renderClear renderer
    renderPaddle renderer p1
    renderPaddle renderer p2
    SDL.renderPresent renderer

renderPaddle :: SDL.Renderer -> Paddle -> IO ()
renderPaddle renderer p = do
    SDL.renderDrawColor renderer SDL.$= V4 0 0 255 255
    SDL.renderFillRect renderer (Just p)

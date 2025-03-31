module EventHelper
  ( MyEvents
  , eQuit
  , eKeyDown
  , eKeyUp
  , parseEvents
  , keycodeStatusLookupFromEvents
  , eventIsQuit
  ) where

import SDL  (Keycode, Keysym (keysymKeycode))
import SDL.Event

data MyEvents = MyEvents    { eQuit :: Bool
                            , eKeyDown :: Keycode -> Bool
                            , eKeyUp :: Keycode -> Bool
                            }

parseEvents :: [Event] -> MyEvents
parseEvents events = MyEvents {
    eQuit = any eventIsQuit events,
    eKeyDown = keycodeStatusLookupFromEvents events Pressed,
    eKeyUp = keycodeStatusLookupFromEvents events Released
}

keycodeStatusLookupFromEvents :: [Event] -> InputMotion -> Keycode -> Bool
keycodeStatusLookupFromEvents events motion keycode =
    any (\event -> case eventPayload event of
                    KeyboardEvent keyboardEvent ->
                        keyboardEventKeyMotion keyboardEvent == motion &&
                        keysymKeycode (keyboardEventKeysym keyboardEvent) == keycode
                    _ -> False
        ) events

eventIsQuit :: Event -> Bool
eventIsQuit event = case eventPayload event of
    QuitEvent -> True
    _ -> False
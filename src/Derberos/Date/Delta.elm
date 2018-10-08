module Derberos.Date.Delta exposing (..)

{-| Utils for working with date deltas.


# Deltas

@docs addSeconds, addMinutes, addHours

-}

import Time exposing (Posix, millisToPosix, posixToMillis)


{-| Add seconds to the time
-}
addSeconds : Int -> Posix -> Posix
addSeconds delta time =
    time
        |> posixToMillis
        |> (+) (delta * 1000)
        |> millisToPosix


{-| Add minutes to the time
-}
addMinutes : Int -> Posix -> Posix
addMinutes delta time =
    time
        |> posixToMillis
        |> (+) (delta * 1000 * 60)
        |> millisToPosix


{-| Add hours to the time
-}
addHours : Int -> Posix -> Posix
addHours delta time =
    time
        |> posixToMillis
        |> (+) (delta * 1000 * 60 * 60)
        |> millisToPosix


{-| Add days to the time
-}
addDays : Int -> Posix -> Posix
addDays delta time =
    time
        |> posixToMillis
        |> (+) (delta * 1000 * 60 * 60 * 24)
        |> millisToPosix

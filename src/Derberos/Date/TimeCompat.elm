module Derberos.Date.TimeCompat
    exposing
        ( Zone(..)
        , convertZoneFromTime
        , utc
        )

{-| This module contains the needed parts for make this library compatible with [elm/time](https://github.com/elm/time).

The library needs access to Era and Zone, there is an open issue opened for it: <https://github.com/elm/time/issues/8>

Maybe in the future this module could be fully removed.

Some functions have beeen copied here from [elm/time](https://github.com/elm/time).

@docs convertZoneFromTime

-}

import Time exposing (Posix, Zone(..), posixToMillis)


-- HELPERS FROM elm/time
-- Some of these functions are useful for this library, but are not exposed in elm/time
-- so they have been copied here.


type Zone
    = Zone Int (List Era)


{-| Currently the public API only needs:

  - `start` is the beginning of this `Era` in "minutes since the Unix Epoch"
  - `offset` is the UTC offset of this `Era` in minutes

-}
type alias Era =
    { start : Int
    , offset : Int
    }


{-| The time zone for Coordinated Universal Time ([UTC])
The `utc` zone has no time adjustments. It never observes daylight-saving
time and it never shifts around based on political restructuring.
[UTC]: <https://en.wikipedia.org/wiki/Coordinated_Universal_Time>
-}
utc : Zone
utc =
    Zone 0 []


convertZoneFromTime : Time.Zone -> Zone
convertZoneFromTime data =
    Zone 0 []


toAdjustedMinutes : Zone -> Posix -> Int
toAdjustedMinutes (Zone defaultOffset eras) time =
    toAdjustedMinutesHelp defaultOffset (flooredDiv (posixToMillis time) 60000) eras


toAdjustedMinutesHelp : Int -> Int -> List Era -> Int
toAdjustedMinutesHelp defaultOffset posixMinutes eras =
    case eras of
        [] ->
            posixMinutes + defaultOffset

        era :: olderEras ->
            if era.start < posixMinutes then
                posixMinutes + era.offset
            else
                toAdjustedMinutesHelp defaultOffset posixMinutes olderEras


flooredDiv : Int -> Float -> Int
flooredDiv numerator denominator =
    floor (toFloat numerator / denominator)

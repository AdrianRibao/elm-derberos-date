module Derberos.Date.TimeCompat
    exposing
        ( Era
        , Zone(..)
        , convertToTimeNativeZone
        , utc
        )

{-| This module contains the needed parts for make this library compatible with [elm/time](https://github.com/elm/time).

The library needs access to Era and Zone, there is an open issue opened for it: <https://github.com/elm/time/issues/8>

Maybe in the future this module could be fully removed.

Some functions have beeen copied here from [elm/time](https://github.com/elm/time).

@docs Zone
@docs Era
@docs utc
@docs convertToTimeNativeZone

-}

import Time exposing (Posix, Zone(..), customZone, posixToMillis)


-- HELPERS FROM elm/time
-- Some of these functions are useful for this library, but are not exposed in elm/time
-- so they have been copied here.


{-| Information about a particular time zone.
The [IANA Time Zone Database][iana] tracks things like UTC offsets and
daylight-saving rules so that you can turn a `Posix` time into local times
within a time zone.

See [`utc`](#utc), [`here`](#here), and [`Browser.Env`][env] to learn how to
obtain `Zone` values.

[iana]: https://www.iana.org/time-zones
[env]: /packages/elm/browser/latest/Browser#Env

-}
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


{-| Convert the internal representation of the Zone to the [Time Zone](https://package.elm-lang.org/packages/elm/time/latest/Time#Zone)
-}
convertToTimeNativeZone : Zone -> Time.Zone
convertToTimeNativeZone (Zone offset eras) =
    customZone offset eras


{-| The time zone for Coordinated Universal Time ([UTC])
The `utc` zone has no time adjustments. It never observes daylight-saving
time and it never shifts around based on political restructuring.
[UTC]: <https://en.wikipedia.org/wiki/Coordinated_Universal_Time>
-}
utc : Zone
utc =
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

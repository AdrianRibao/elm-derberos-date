module Derberos.Date.Calendar exposing (..)

{-| Utils for working with ranges of dates.


# Calendar

@docs getFirstDayOfMonth
@docs getCurrentWeekDates, getCurrentMonthDates, getWeekDates, getMonthDates

-}

import Derberos.Date.Core exposing (DateRecord, civilToPosix, posixToCivil)
import Time exposing (Posix)


{-| Get the first day of the month for a given time.
-}
getFirstDayOfMonth : Posix -> Posix
getFirstDayOfMonth time =
    time
        |> posixToCivil
        |> setDay1OfMonth
        |> civilToPosix


setDay1OfMonth : DateRecord -> DateRecord
setDay1OfMonth civilTime =
    { civilTime
        | day = 1
        , hour = 0
        , minute = 0
        , second = 0
        , millis = 0
    }


getCurrentWeekDates =
    False


getCurrentMonthDates =
    False


getWeekDates =
    False


getMonthDates =
    False

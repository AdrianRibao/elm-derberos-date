module Derberos.Date.Delta exposing (..)

{-| Utils for working with date deltas.


# Deltas

@docs addSeconds, addMinutes, addHours
@docs prevWeekdayFromTime

-}

import Derberos.Date.Core exposing (civilToPosix, getWeekday, monthToNumber, newDateRecord, numberToMonth, posixToCivil)
import Derberos.Date.Utils exposing (getPrevMonth, numberOfDaysInMonth, weekdayDiff, weekdayDiffBack)
import Time exposing (Posix, Weekday, millisToPosix, posixToMillis, toDay, toMonth, toYear, utc)


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


{-| Add years to the time
-}
addYears : Int -> Posix -> Posix
addYears delta time =
    let
        millis =
            posixToMillis time

        yearMod =
            modBy 4 (toYear utc time)

        delta_years =
            millis + (delta * 1000 * 60 * 60 * 24 * 365)

        drift_4_years =
            delta_years
                + (((delta + yearMod) // 4) * 1000 * 60 * 60 * 24)

        drift_100_years =
            drift_4_years
                - (((delta + yearMod) // 100) * 1000 * 60 * 60 * 24)

        drift_400_years =
            drift_100_years
                + (((delta + yearMod) // 400) * 1000 * 60 * 60 * 24)
    in
    millisToPosix drift_400_years


{-| Add months to the time.
-}
addMonths : Int -> Posix -> Posix
addMonths delta time =
    let
        -- First get the civil datetime. Because adding months is something humans do the human way, increasing months.
        civilDateTime =
            time
                |> posixToCivil

        _ =
            1451606400000
                -- This is 1/1/205
                |> millisToPosix
                |> posixToCivil

        _ =
            civilToPosix <| newDateRecord 2016 1 1 0 0 0 0

        newYear =
            civilDateTime.year + (delta // 12)

        newMonth =
            (civilDateTime.month + delta)
                |> modBy 12

        newCivil =
            { civilDateTime
                | month = newMonth
                , year = newYear
            }
    in
    civilToPosix <| newDateRecord newCivil.year newCivil.month newCivil.day newCivil.hour newCivil.minute newCivil.second newCivil.millis


{-| Given a time and a weekday, get the date of the previous weekday
-}
prevWeekdayFromTime : Weekday -> Posix -> Posix
prevWeekdayFromTime weekday time =
    let
        timeWeekday =
            getWeekday time

        diffDays =
            weekdayDiffBack timeWeekday weekday
                |> (*) -1
    in
    addDays diffDays time


{-| Given a time and a weekday, get the date of the next weekday
-}
nextWeekdayFromTime : Weekday -> Posix -> Posix
nextWeekdayFromTime weekday time =
    let
        timeWeekday =
            getWeekday time

        diffDays =
            weekdayDiff timeWeekday weekday
    in
    addDays diffDays time

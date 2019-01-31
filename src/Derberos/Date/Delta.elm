module Derberos.Date.Delta
    exposing
        ( addDays
        , addHours
        , addMinutes
        , addMonths
        , addSeconds
        , addYears
        , nextWeekdayFromTime
        , prevWeekdayFromTime
        )

{-| Utils for working with date deltas.

@docs addSeconds, addMinutes, addHours, addDays, addMonths, addYears
@docs prevWeekdayFromTime, nextWeekdayFromTime

-}

import Derberos.Date.Core exposing (addTimezoneMilliseconds, adjustMilliseconds, civilToPosix, newDateRecord, posixToCivil)
import Derberos.Date.Utils exposing (getPrevMonth, getWeekday, monthToNumber, numberOfDaysInMonth, numberToMonth, weekdayDiff, weekdayDiffBack)
import Time exposing (Month(..), Posix, Weekday, Zone, customZone, millisToPosix, posixToMillis, toDay, toMonth, toYear, utc)


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

        tz =
            customZone 0 []

        yearMod =
            modBy 4 (toYear tz time)

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
addMonths : Int -> Zone -> Posix -> Posix
addMonths delta zone time =
    let
        adjustedTime =
            addTimezoneMilliseconds zone time

        monthNumber =
            monthToNumber <|
                Time.toMonth zone adjustedTime

        finalMonth =
            monthNumber
                + delta

        civilDateTime =
            adjustedTime
                |> posixToCivil

        newYear =
            civilDateTime.year
                + (floor <| (toFloat finalMonth / 12))

        newDay =
            min civilDateTime.day (numberOfDaysInMonth newYear <| Maybe.withDefault Jan <| numberToMonth finalMonth)

        newMonth =
            modBy 12 finalMonth

        newCivil =
            { civilDateTime
                | day = newDay
                , month = newMonth + 1
                , year = newYear
            }
    in
    (civilToPosix <| newDateRecord newCivil.year newCivil.month newCivil.day newCivil.hour newCivil.minute newCivil.second newCivil.millis utc)
        |> adjustMilliseconds zone


{-| Given a time and a weekday, get the date of the previous weekday
-}
prevWeekdayFromTime : Weekday -> Zone -> Posix -> Posix
prevWeekdayFromTime weekday zone time =
    let
        timeWeekday =
            getWeekday zone time

        diffDays =
            weekdayDiffBack timeWeekday weekday
                |> (*) -1
    in
    addDays diffDays time


{-| Given a time and a weekday, get the date of the next weekday
-}
nextWeekdayFromTime : Weekday -> Zone -> Posix -> Posix
nextWeekdayFromTime weekday zone time =
    let
        timeWeekday =
            getWeekday zone time

        diffDays =
            weekdayDiff timeWeekday weekday
    in
    addDays diffDays time

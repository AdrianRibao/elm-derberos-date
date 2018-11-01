module Derberos.Date.Calendar
    exposing
        ( getCurrentMonthDates
        , getCurrentMonthDatesFullWeeks
        , getCurrentWeekDates
        , getFirstDayOfMonth
        , getFirstDayOfYear
        , getLastDayOfYear
        )

{-| Utils for working with ranges of dates.


# Calendar

@docs getFirstDayOfMonth
@docs getCurrentWeekDates
@docs getCurrentMonthDates, getCurrentMonthDatesFullWeeks
@docs getFirstDayOfYear, getLastDayOfYear

-}

import Derberos.Date.Core exposing (DateRecord, addTimezoneMilliseconds, adjustMilliseconds, civilToPosix, posixToCivil)
import Derberos.Date.Delta exposing (addDays, nextWeekdayFromTime, prevWeekdayFromTime)
import Derberos.Date.Utils exposing (numberOfDaysInMonth, numberToMonth, resetTime)
import Time exposing (Month(..), Posix, Weekday(..), Zone, millisToPosix, posixToMillis)


{-| Get the first day of the month for a given time.
-}
getFirstDayOfMonth : Zone -> Posix -> Posix
getFirstDayOfMonth zone time =
    time
        |> addTimezoneMilliseconds zone
        |> posixToCivil
        |> setDay1OfMonth
        |> civilToPosix
        |> adjustMilliseconds zone


setDay1OfMonth : DateRecord -> DateRecord
setDay1OfMonth civilTime =
    { civilTime
        | day = 1
        , hour = 0
        , minute = 0
        , second = 0
        , millis = 0
    }


{-| Get the week dates for a given time. It returns the week from Monday to Sunday
-}
getCurrentWeekDates : Zone -> Posix -> List Posix
getCurrentWeekDates zone time =
    let
        weekMonday =
            time
                |> addTimezoneMilliseconds zone
                |> resetTime
                |> adjustMilliseconds zone
                |> prevWeekdayFromTime Mon zone
    in
    List.range 0 6
        |> List.map (\delta -> addDays delta weekMonday)


{-| Return a list of dates for the month where the time belongs.

This returns the days from `1` to `last day of the month`.

-}
getCurrentMonthDates : Zone -> Posix -> List Posix
getCurrentMonthDates zone time =
    let
        firstDayOfMonth =
            getFirstDayOfMonth zone time

        dateRecord =
            posixToCivil time

        year =
            dateRecord.year

        month =
            (dateRecord.month - 1)
                |> numberToMonth
                |> Maybe.withDefault Jan

        numberDaysInMonth =
            numberOfDaysInMonth dateRecord.year month
    in
    List.range 0 (numberDaysInMonth - 1)
        |> List.map (\delta -> addDays delta firstDayOfMonth)


{-| Return a list of dates for the month starting on Monday and ending on Sunday
-}
getCurrentMonthDatesFullWeeks : Zone -> Posix -> List Posix
getCurrentMonthDatesFullWeeks zone time =
    let
        firstDayOfMonth =
            time
                |> getFirstDayOfMonth zone
                |> prevWeekdayFromTime Mon zone

        lastDayOfMonth =
            time
                |> getLastDayOfMonth zone
                |> nextWeekdayFromTime Sun zone

        numberDaysInMonth =
            (posixToMillis lastDayOfMonth - posixToMillis firstDayOfMonth) // (1000 * 60 * 60 * 24)
    in
    List.range 0 numberDaysInMonth
        |> List.map (\delta -> addDays delta firstDayOfMonth)


{-| Get the last day of the month at time 00:00:00
-}
getLastDayOfMonth : Zone -> Posix -> Posix
getLastDayOfMonth zone time =
    let
        dateRecord =
            time
                |> addTimezoneMilliseconds zone
                |> posixToCivil

        year =
            dateRecord.year

        month =
            (dateRecord.month - 1)
                |> numberToMonth
                |> Maybe.withDefault Jan

        lastDayInMonth =
            numberOfDaysInMonth year month

        newRecord =
            { dateRecord
                | day = lastDayInMonth
                , hour = 0
                , minute = 0
                , second = 0
                , millis = 0
            }
    in
    newRecord
        |> civilToPosix


{-| Get the first day of the year
-}
getFirstDayOfYear : Zone -> Posix -> Posix
getFirstDayOfYear zone time =
    let
        dateRecord =
            time
                |> addTimezoneMilliseconds zone
                |> posixToCivil

        newRecord =
            { dateRecord
                | month = 1
                , day = 1
                , hour = 0
                , minute = 0
                , second = 0
                , millis = 0
            }
    in
    newRecord
        |> civilToPosix
        |> adjustMilliseconds zone


{-| Get the last day of the year
-}
getLastDayOfYear : Zone -> Posix -> Posix
getLastDayOfYear zone time =
    let
        dateRecord =
            time
                |> addTimezoneMilliseconds zone
                |> posixToCivil

        newRecord =
            { dateRecord
                | month = 12
                , day = 31
                , hour = 0
                , minute = 0
                , second = 0
                , millis = 0
            }
    in
    newRecord
        |> civilToPosix
        |> adjustMilliseconds zone

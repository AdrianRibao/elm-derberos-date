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

import Derberos.Date.Core exposing (DateRecord, civilToPosix, posixToCivil)
import Derberos.Date.Delta exposing (addDays, nextWeekdayFromTime, prevWeekdayFromTime)
import Derberos.Date.Utils exposing (numberOfDaysInMonth, numberToMonth)
import Time exposing (Month(..), Posix, Weekday(..), Zone, millisToPosix, posixToMillis)


{-| Get the first day of the month for a given time.
-}
getFirstDayOfMonth : Zone -> Posix -> Posix
getFirstDayOfMonth zone time =
    time
        |> posixToCivil zone
        |> setDay1OfMonth zone
        |> civilToPosix


setDay1OfMonth : Zone -> DateRecord -> DateRecord
setDay1OfMonth zone civilTime =
    { civilTime
        | day = 1
        , hour = 0
        , minute = 0
        , second = 0
        , millis = 0
        , zone = zone
    }


{-| Get the week dates for a given time. It returns the week from Monday to Sunday
-}
getCurrentWeekDates : Posix -> List Posix
getCurrentWeekDates time =
    let
        weekMonday =
            prevWeekdayFromTime Mon time
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
            posixToCivil zone time

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
        firstDay =
            time
                |> getFirstDayOfMonth zone
                |> prevWeekdayFromTime Mon

        lastDay =
            time
                |> getLastDayOfMonth zone
                |> nextWeekdayFromTime Sun

        numberDaysInMonth =
            (posixToMillis lastDay - posixToMillis firstDay) // (1000 * 60 * 60 * 24)
    in
    List.range 0 numberDaysInMonth
        |> List.map (\delta -> addDays delta firstDay)


{-| Get the last day of the month at time 00:00:00
-}
getLastDayOfMonth : Zone -> Posix -> Posix
getLastDayOfMonth zone time =
    let
        dateRecord =
            time
                |> posixToCivil zone

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
                , zone = zone
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
                |> posixToCivil zone

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


{-| Get the last day of the year
-}
getLastDayOfYear : Zone -> Posix -> Posix
getLastDayOfYear zone time =
    let
        dateRecord =
            time
                |> posixToCivil zone

        newRecord =
            { dateRecord
                | month = 12
                , day = 31
                , hour = 0
                , minute = 0
                , second = 0
                , millis = 0
                , zone = zone
            }
    in
    newRecord
        |> civilToPosix

module Derberos.Date.Calendar
    exposing
        ( getCurrentMonthDates
        , getCurrentMonthDatesFullWeeks
        , getCurrentWeekDates
        , getFirstDayOfMonth
        )

{-| Utils for working with ranges of dates.


# Calendar

@docs getFirstDayOfMonth
@docs getCurrentWeekDates
@docs getCurrentMonthDates, getCurrentMonthDatesFullWeeks

-}

import Derberos.Date.Core exposing (DateRecord, civilToPosix, posixToCivil)
import Derberos.Date.Delta exposing (addDays, nextWeekdayFromTime, prevWeekdayFromTime)
import Derberos.Date.Utils exposing (numberOfDaysInMonth, numberToMonth)
import Time exposing (Month(..), Posix, Weekday(..), millisToPosix, posixToMillis)


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
getCurrentMonthDates : Posix -> List Posix
getCurrentMonthDates time =
    let
        firstDayOfMonth =
            getFirstDayOfMonth time

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
getCurrentMonthDatesFullWeeks : Posix -> List Posix
getCurrentMonthDatesFullWeeks time =
    let
        firstDay =
            time
                |> getFirstDayOfMonth
                |> prevWeekdayFromTime Mon
                |> Debug.log "First day"

        lastDay =
            time
                |> getLastDayOfMonth
                |> nextWeekdayFromTime Sun
                |> Debug.log "Last day"

        numberDaysInMonth =
            (posixToMillis lastDay - posixToMillis firstDay)
                // (1000 * 60 * 60 * 24)
                |> Debug.log "Number of days"
    in
    List.range 0 numberDaysInMonth
        |> List.map (\delta -> addDays delta firstDay)


{-| Get the last day of the month at time 00:00:00
-}
getLastDayOfMonth : Posix -> Posix
getLastDayOfMonth time =
    let
        dateRecord =
            time
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

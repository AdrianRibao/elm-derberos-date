module Derberos.Date.Core exposing (..)

{-| Core functions for working with dates

@docs monthToNumber, numberToMonth
@docs civilToPosix, posixToCivil

-}

import Derberos.Date.Utils exposing (weekdayFromNumber)
import Time exposing (Month(..), Posix, Weekday(..), millisToPosix, posixToMillis)


{-| Convert the month to a number in the range [0, 11]
-}
monthToNumber : Month -> Int
monthToNumber month =
    case month of
        Jan ->
            0

        Feb ->
            1

        Mar ->
            2

        Apr ->
            3

        May ->
            4

        Jun ->
            5

        Jul ->
            6

        Aug ->
            7

        Sep ->
            8

        Oct ->
            9

        Nov ->
            10

        Dec ->
            11


{-| Given a number from 0 to 11, convert it to the corresponding month.
-}
numberToMonth : Int -> Maybe Month
numberToMonth monthNumber =
    case monthNumber of
        0 ->
            Just Jan

        1 ->
            Just Feb

        2 ->
            Just Mar

        3 ->
            Just Apr

        4 ->
            Just May

        5 ->
            Just Jun

        6 ->
            Just Jul

        7 ->
            Just Aug

        8 ->
            Just Sep

        9 ->
            Just Oct

        10 ->
            Just Nov

        11 ->
            Just Dec

        _ ->
            Nothing


{-| Given a datetime, get the posix time
-}
civilToPosix : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Posix
civilToPosix year month day hour minute second millis =
    let
        y =
            year
                - (if month <= 2 then
                    1
                   else
                    0
                  )

        era =
            floor (toFloat y / 400)

        yoe =
            y - era * 400

        mp =
            modBy 12 (month + 9)

        doy =
            (153 * mp + 2) // 5 + day - 1

        doe =
            yoe
                * 365
                + yoe
                // 4
                - yoe
                // 100
                + doy

        days =
            era * 146097 + doe - 719468

        time =
            hour * 3600 * 1000 + minute * 60 * 1000 + second * 1000 + millis

        resultInMilliseconds =
            days * 24 * 3600 * 1000 + time
    in
    resultInMilliseconds
        |> millisToPosix


{-| Given a Posix time, get the human datetime.
-}
posixToCivil : Posix -> { year : Int, month : Int, day : Int, hour : Int, minute : Int, second : Int, millis : Int }
posixToCivil time =
    let
        milliseconds =
            posixToMillis time

        millis =
            modBy 1000 milliseconds

        second =
            (toFloat milliseconds / 1000)
                |> floor
                |> modBy 60

        minute =
            (toFloat milliseconds / (60 * 1000))
                |> floor
                |> modBy 60

        hour =
            (toFloat milliseconds / (60 * 60 * 1000))
                |> floor
                |> modBy 24

        minutes =
            (toFloat milliseconds / (60 * 1000))
                |> floor

        rawDay =
            (toFloat minutes / (60 * 24) + 719468)
                |> floor

        era =
            (if rawDay >= 0 then
                rawDay
             else
                rawDay - 146096
            )
                // 146097

        dayOfEra =
            rawDay
                - era
                * 146097

        -- [0, 146096]
        yearOfEra =
            (dayOfEra - dayOfEra // 1460 + dayOfEra // 36524 - dayOfEra // 146096)
                // 365

        -- [0, 399]
        year =
            yearOfEra + era * 400

        dayOfYear =
            dayOfEra - (365 * yearOfEra + yearOfEra // 4 - yearOfEra // 100)

        -- [0, 365]
        mp =
            (5 * dayOfYear + 2) // 153

        -- [0, 11]
        month =
            mp
                + (if mp < 10 then
                    3
                   else
                    -9
                  )

        -- [1, 12]
    in
    { year =
        year
            + (if month <= 2 then
                1
               else
                0
              )
    , month = month
    , day = dayOfYear - (153 * mp + 2) // 5 + 1 -- [1, 31]
    , hour = hour
    , minute = minute
    , second = second
    , millis = millis
    }


getWeekday : Posix -> Weekday
getWeekday time =
    let
        milliseconds =
            posixToMillis time

        days =
            (toFloat milliseconds / (24 * 60 * 60 * 1000))
                |> floor

        weekdayNumber =
            modBy 7 (days + 3)
    in
    weekdayFromNumber weekdayNumber
        |> Maybe.withDefault Mon

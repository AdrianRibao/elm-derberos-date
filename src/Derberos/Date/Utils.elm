module Derberos.Date.Utils
    exposing
        ( getIsoFormat
        , getNextMonth
        , getNextWeekday
        , getPrevMonth
        , getPrevWeekday
        , getWeekday
        , isLeapYear
        , monthToNumber
        , monthToNumber1
        , numberOfDaysInMonth
        , numberToMonth
        , resetTime
        , weekdayDiff
        , weekdayDiffBack
        , weekdayFromNumber
        , weekdayToNumber
        )

{-| Utils for working with dates.


# Functions


## Related to years

@docs isLeapYear


## Related to months

@docs getNextMonth, getPrevMonth
@docs monthToNumber, monthToNumber1, numberToMonth
@docs numberOfDaysInMonth


## Related to weekdays

@docs getWeekday
@docs getNextWeekday, getPrevWeekday
@docs weekdayToNumber, weekdayFromNumber
@docs weekdayDiff, weekdayDiffBack


## Related to time

@docs resetTime


## Formatting

@docs getIsoFormat

-}

import Derberos.Date.TimeCompat exposing (Zone, convertToTimeNativeZone)
import Time exposing (Month(..), Posix, Weekday(..), Zone(..), millisToPosix, posixToMillis)


{-| Given a month, return the next month.

    getNextMonth Apr == May

-}
getNextMonth : Month -> Month
getNextMonth month =
    case month of
        Jan ->
            Feb

        Feb ->
            Mar

        Mar ->
            Apr

        Apr ->
            May

        May ->
            Jun

        Jun ->
            Jul

        Jul ->
            Aug

        Aug ->
            Sep

        Sep ->
            Oct

        Oct ->
            Nov

        Nov ->
            Dec

        Dec ->
            Jan


{-| Given a month, return the previous month.

    getPrevMonth Apr == May

-}
getPrevMonth : Month -> Month
getPrevMonth month =
    case month of
        Jan ->
            Dec

        Feb ->
            Jan

        Mar ->
            Feb

        Apr ->
            Mar

        May ->
            Apr

        Jun ->
            May

        Jul ->
            Jun

        Aug ->
            Jul

        Sep ->
            Aug

        Oct ->
            Sep

        Nov ->
            Oct

        Dec ->
            Nov


{-| Given a `Weekday`, get the next weekday.
getNextWeekday Sun == Mon
-}
getNextWeekday : Weekday -> Weekday
getNextWeekday weekday =
    case weekday of
        Mon ->
            Tue

        Tue ->
            Wed

        Wed ->
            Thu

        Thu ->
            Fri

        Fri ->
            Sat

        Sat ->
            Sun

        Sun ->
            Mon


{-| Given a `Weekday`, get the previous weekday.
getPrevWeekday Sun == Sat
-}
getPrevWeekday : Weekday -> Weekday
getPrevWeekday weekday =
    case weekday of
        Mon ->
            Sun

        Tue ->
            Mon

        Wed ->
            Tue

        Thu ->
            Wed

        Fri ->
            Thu

        Sat ->
            Fri

        Sun ->
            Sat


{-| If the year is a leap year, return True. False otherwise.

    isLeapYear 2018 == False
    isLeapYear 2016 == True

-}
isLeapYear : Int -> Bool
isLeapYear year =
    if ((modBy 4 year == 0) && (modBy 100 year /= 0)) || (modBy 400 year == 0) then
        True
    else
        False


{-| Return the number of days in a month.

    numberOfDaysInMonth 2018 Feb == 28
    numberOfDaysInMonth 2016 Feb == 29

-}
numberOfDaysInMonth : Int -> Month -> Int
numberOfDaysInMonth year month =
    case month of
        Jan ->
            31

        Feb ->
            case isLeapYear year of
                True ->
                    29

                False ->
                    28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31


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


{-| Convert the month to a number in the range [1, 12]
-}
monthToNumber1 : Month -> Int
monthToNumber1 month =
    monthToNumber month + 1


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


{-| Convert the Weekday to a number representation. Starts with 0 on Monday.

    weekdayToNumber Mon == 0
    weekdayToNumber Sun == 6

-}
weekdayToNumber : Weekday -> Int
weekdayToNumber weekday =
    case weekday of
        Mon ->
            0

        Tue ->
            1

        Wed ->
            2

        Thu ->
            3

        Fri ->
            4

        Sat ->
            5

        Sun ->
            6


{-| Convert a number to a weekday. 0 is for Monday.

    weekdayFromNumber 0 == Just Mon
    weekdayFromNumber 6 == Just Sun
    weekdayFromNumber 8 == Nothing

-}
weekdayFromNumber : Int -> Maybe Weekday
weekdayFromNumber weekdayNumber =
    case weekdayNumber of
        0 ->
            Just Mon

        1 ->
            Just Tue

        2 ->
            Just Wed

        3 ->
            Just Thu

        4 ->
            Just Fri

        5 ->
            Just Sat

        6 ->
            Just Sun

        _ ->
            Nothing


{-| Given a Time, return the `Weekday`
-}
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


{-| Get the difference in days between two weekdays. Assume always forward direction.
For example, the returned value from Friday to Wednesday is 5.

    weekdayDiff Fri Wed == 5
    weekdayDiff Mon Wed == 2

-}
weekdayDiff : Weekday -> Weekday -> Int
weekdayDiff day1 day2 =
    let
        day1Number =
            weekdayToNumber day1

        day2Number =
            weekdayToNumber day2
    in
    day2Number
        - day1Number
        + (if day1Number <= day2Number then
            0
           else
            7
          )


{-| Get the difference in days between two weekdays. It works in the backwards direction.
For example, the returned value from Friday to Wednesday is 2.

    weekdayDiffBack Fri Wed == 2
    weekdayDiffBack Mon Wed == 5

-}
weekdayDiffBack : Weekday -> Weekday -> Int
weekdayDiffBack day1 day2 =
    let
        day1Number =
            weekdayToNumber day1

        day2Number =
            weekdayToNumber day2
    in
    (if day1Number >= day2Number then
        0
     else
        7
    )
        - (day2Number
            - day1Number
          )


{-| Given a datetime, set the time to 00:00:00.000
-}
resetTime : Posix -> Posix
resetTime time =
    time
        |> posixToMillis
        |> (\millis -> millis // (1000 * 60 * 60 * 24))
        |> (*) (1000 * 60 * 60 * 24)
        |> millisToPosix


{-| Get the iso format of the date
-}
getIsoFormat : String -> Derberos.Date.TimeCompat.Zone -> Posix -> String
getIsoFormat separator tz time =
    let
        tzNative =
            tz
                |> convertToTimeNativeZone

        year =
            Time.toYear tzNative time
                |> String.fromInt

        month =
            Time.toMonth tzNative time
                |> monthToNumber1
                |> String.fromInt
                |> String.padLeft 2 '0'

        day =
            Time.toDay tzNative time
                |> String.fromInt
                |> String.padLeft 2 '0'

        hour =
            Time.toHour tzNative time
                |> String.fromInt
                |> String.padLeft 2 '0'

        minute =
            Time.toMinute tzNative time
                |> String.fromInt
                |> String.padLeft 2 '0'

        second =
            Time.toSecond tzNative time
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    year ++ separator ++ month ++ separator ++ day ++ "T" ++ hour ++ ":" ++ minute ++ ":" ++ second

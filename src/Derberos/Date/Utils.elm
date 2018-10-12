module Derberos.Date.Utils exposing (..)

{-| Utils for working with dates.


# Utils

@docs getNextMonth, getPrevMonth
@docs getNextWeekday, getPrevWeekday
@docs isLeapYear
@docs numberOfDaysInMonth

-}

import Time exposing (Month(..), Posix, Weekday(..))


{-| Get the next month.
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


{-| Get the previous month.
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


{-| Get the next weekday.
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


{-| Get the prev weekday.
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


{-| If the year is a leap year, return True, or False otherwise.
-}
isLeapYear : Int -> Bool
isLeapYear year =
    if ((modBy 4 year == 0) && (modBy 100 year /= 0)) || (modBy 400 year == 0) then
        True
    else
        False


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

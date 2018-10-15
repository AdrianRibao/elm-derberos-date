module Derberos.Date.Utils exposing (..)

{-| Utils for working with dates.


# Utils

@docs getNextMonth, getPrevMonth
@docs getNextWeekday, getPrevWeekday
@docs isLeapYear
@docs numberOfDaysInMonth
@docs weekdayToNumber, weekdayFromNumber
@docs weekdayDiff, weekdayDiffBack

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


{-| Return the number of days in a month.
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


{-| Convert the Weekday to a number representation. Starts with 0 on Monday.
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


{-| Get the difference in days between two weekdays. Assume always forward direction.
For example, the returned value from Friday to Wednesday is 5.
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

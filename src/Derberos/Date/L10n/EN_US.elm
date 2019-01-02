module Derberos.Date.L10n.EN_US exposing (config)

{-| Functions for localized values in en_US.


# en_US

@docs config

-}

import Derberos.Date.Core exposing (Config)
import Derberos.Date.Utils exposing (monthToNumber1)
import Time exposing (Month(..), Posix, Weekday(..), Zone, utc)


{-| Configuration for `es_ES`
-}
config : Config
config =
    { getMonthName = getMonthName
    , getWeekName = getWeekdayName
    , getCommonFormatDate = getCommonFormatDate
    , getCommonFormatTime = getCommonFormatTime
    , getCommonFormatDateTime = getCommonFormatDateTime
    }


getMonthName : Month -> String
getMonthName month =
    case month of
        Jan ->
            "January"

        Feb ->
            "Februrary"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


getWeekdayName : Weekday -> String
getWeekdayName weekday =
    case weekday of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"


getCommonFormatDate : String -> Zone -> Posix -> String
getCommonFormatDate separator tz time =
    let
        year =
            Time.toYear tz time
                |> String.fromInt

        month =
            Time.toMonth tz time
                |> monthToNumber1
                |> String.fromInt
                |> String.padLeft 2 '0'

        day =
            Time.toDay tz time
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    month ++ separator ++ day ++ separator ++ year


getCommonFormatTime : Zone -> Posix -> String
getCommonFormatTime tz time =
    let
        hour =
            Time.toHour tz time
                |> String.fromInt
                |> String.padLeft 2 '0'

        minute =
            Time.toMinute tz time
                |> String.fromInt
                |> String.padLeft 2 '0'

        second =
            Time.toSecond tz time
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    hour ++ ":" ++ minute ++ ":" ++ second


getCommonFormatDateTime : String -> Zone -> Posix -> String
getCommonFormatDateTime separator tz time =
    let
        year =
            Time.toYear tz time
                |> String.fromInt

        month =
            Time.toMonth tz time
                |> monthToNumber1
                |> String.fromInt
                |> String.padLeft 2 '0'

        day =
            Time.toDay tz time
                |> String.fromInt
                |> String.padLeft 2 '0'

        hour =
            Time.toHour tz time
                |> String.fromInt
                |> String.padLeft 2 '0'

        minute =
            Time.toMinute tz time
                |> String.fromInt
                |> String.padLeft 2 '0'

        second =
            Time.toSecond tz time
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    month ++ separator ++ day ++ separator ++ year ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second

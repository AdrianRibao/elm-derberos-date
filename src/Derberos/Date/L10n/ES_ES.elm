module Derberos.Date.L10n.ES_ES exposing (config)

{-| Functions for localized values in ES_es.


# Es_es

@docs config

-}

import Derberos.Date.Core exposing (Config)
import Derberos.Date.TimeCompat exposing (Zone, convertToTimeNativeZone, utc)
import Derberos.Date.Utils exposing (monthToNumber1)
import Time exposing (Month(..), Posix, Weekday(..))


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
            "Enero"

        Feb ->
            "Febrero"

        Mar ->
            "Marzo"

        Apr ->
            "Abril"

        May ->
            "Mayo"

        Jun ->
            "Junio"

        Jul ->
            "Julio"

        Aug ->
            "Agosto"

        Sep ->
            "Septiembre"

        Oct ->
            "Octubre"

        Nov ->
            "Noviembre"

        Dec ->
            "Diciembre"


getWeekdayName : Weekday -> String
getWeekdayName weekday =
    case weekday of
        Mon ->
            "Lunes"

        Tue ->
            "Martes"

        Wed ->
            "Miércoles"

        Thu ->
            "Jueves"

        Fri ->
            "Viernes"

        Sat ->
            "Sábado"

        Sun ->
            "Domingo"


getCommonFormatDate : String -> Zone -> Posix -> String
getCommonFormatDate separator tz time =
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
    in
    day ++ separator ++ month ++ separator ++ year


getCommonFormatTime : Zone -> Posix -> String
getCommonFormatTime tz time =
    let
        tzNative =
            tz
                |> convertToTimeNativeZone

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
    hour ++ ":" ++ minute ++ ":" ++ second


getCommonFormatDateTime : String -> Zone -> Posix -> String
getCommonFormatDateTime separator tz time =
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
    day ++ separator ++ month ++ separator ++ year ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second

module Derberos.Date.L10n.ES_ES exposing (config)

{-| Functions for localized values in ES_es.


# Es_es

@docs config

-}

import Derberos.Date.Core exposing (Config)
import Derberos.Date.TimeCompat exposing (Zone, convertToTimeNativeZone, utc)
import Time exposing (Month(..), Posix, Weekday(..))


{-| Configuration for `es_ES`
-}
config : Config
config =
    { getMonthName = getMonthName
    , getWeekName = getWeekdayName
    , getIsoFormat = getIsoFormat
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


getIsoFormat : Zone -> Posix -> String
getIsoFormat tz time =
    let
        tzNative =
            tz
                |> convertToTimeNativeZone

        year =
            Time.toYear tzNative time
                |> String.fromInt

        month =
            Time.toMonth tzNative time
                |> getMonthName

        day =
            Time.toDay tzNative time
                |> String.fromInt

        hour =
            Time.toHour tzNative time
                |> String.fromInt

        minute =
            Time.toMinute tzNative time
                |> String.fromInt

        second =
            Time.toSecond tzNative time
                |> String.fromInt
    in
    year ++ " " ++ month ++ " " ++ " " ++ day ++ " " ++ hour ++ " " ++ minute ++ " " ++ second

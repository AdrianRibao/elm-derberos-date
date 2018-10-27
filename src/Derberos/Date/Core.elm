module Derberos.Date.Core
    exposing
        ( Config
        , DateRecord
        , civilToPosix
        , getTzOffset
        , newDateRecord
        , posixToCivil
        )

{-| Core functions for working with dates

@docs DateRecord, newDateRecord
@docs civilToPosix, posixToCivil
@docs Config
@docs getTzOffset

-}

import Derberos.Date.Utils exposing (monthToNumber1, weekdayFromNumber)
import Time
    exposing
        ( Month(..)
        , Posix
        , Weekday(..)
        , Zone
        , customZone
        , millisToPosix
        , posixToMillis
        )


{-| Store the date in a record.
-}
type alias DateRecord =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    , millis : Int
    , zone : Zone
    }


{-| Generate a new DateRecord

    newDateRecord 2018 2 13 19 45 0 0 == {
        year = 2018
        , month = 2
        , day = 13
        , hour = 19
        , minute = 45
        , second = 0
        , millis = 0
        }

-}
newDateRecord : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Zone -> DateRecord
newDateRecord year month day hour minute second millis zone =
    { year = year
    , month = month
    , day = day
    , hour = hour
    , minute = minute
    , second = second
    , millis = millis
    , zone = zone
    }


{-| Given a datetime, get the posix time without taking care of the
timezone offset
-}
civilToPosixUnadjusted : DateRecord -> Posix
civilToPosixUnadjusted dateRecord =
    let
        y =
            dateRecord.year
                - (if dateRecord.month <= 2 then
                    1
                   else
                    0
                  )

        era =
            floor (toFloat y / 400)

        yoe =
            y - era * 400

        mp =
            modBy 12 (dateRecord.month + 9)

        doy =
            (153 * mp + 2) // 5 + dateRecord.day - 1

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
            dateRecord.hour * 3600 * 1000 + dateRecord.minute * 60 * 1000 + dateRecord.second * 1000 + dateRecord.millis

        resultInMilliseconds =
            days * 24 * 3600 * 1000 + time
    in
    resultInMilliseconds
        |> millisToPosix


civilToPosix : DateRecord -> Posix
civilToPosix dateRecord =
    dateRecord
        |> civilToPosixUnadjusted
        -- Now we have the milliseconds as if the dateRecord was in UTC
        |> adjustMilliseconds dateRecord.zone


adjustMilliseconds : Zone -> Posix -> Posix
adjustMilliseconds zone time =
    let
        -- The time has the milliseconds as if it was using UTC
        -- but it actually has a tz to be applied. So if we had
        -- 2018/10/28T01:30:00 UTC we want to convert it to
        -- 2018/10/28T01:30:00 zone  so we have to substract
        -- the offset.
        offset =
            time
                |> getTzOffset zone

        millis =
            time
                |> posixToMillis
    in
    (millis - (offset * 60000))
        |> millisToPosix


{-| Given a Posix time, get the human datetime.
-}
posixToCivil : Zone -> Posix -> DateRecord
posixToCivil zone time =
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
    , zone = zone
    }


{-| Store the configuration for getting i18n and l10n formats.
-}
type alias Config =
    { getMonthName : Month -> String
    , getWeekName : Weekday -> String
    , getCommonFormatDate : String -> Zone -> Posix -> String
    , getCommonFormatTime : Zone -> Posix -> String
    , getCommonFormatDateTime : String -> Zone -> Posix -> String
    }


{-| Gets the difference in minutes given the current [Time Zone](https://package.elm-lang.org/packages/elm/time/latest/Time#Zone)
and the utc time.
-}
getTzOffset : Time.Zone -> Posix -> Int
getTzOffset zone time =
    let
        utcMillis =
            time
                |> posixToMillis

        localMillis =
            civilFromPosixWithTimezone zone time
                |> civilToPosixUnadjusted
                |> posixToMillis
    in
    (localMillis - utcMillis) // 60000


{-| Convert a posix time with a zone to a DateRecord. Adjusted with timezone.

This functions should be used only to calculate the offset, because uses the timezone
utc for specifying a 0 offset from utc.

-}
civilFromPosixWithTimezone : Time.Zone -> Posix -> DateRecord
civilFromPosixWithTimezone tz time =
    let
        zeroOffset =
            customZone 0 []

        year =
            Time.toYear tz time

        month =
            Time.toMonth tz time
                |> monthToNumber1

        day =
            Time.toDay tz time

        hour =
            Time.toHour tz time

        minute =
            Time.toMinute tz time

        second =
            Time.toSecond tz time

        millis =
            Time.toMillis tz time
    in
    newDateRecord year month day hour minute second millis zeroOffset

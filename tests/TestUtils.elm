module TestUtils exposing (..)

import Derberos.Date.Utils
    exposing
        ( getIsoFormat
        , getNextMonth
        , getNextWeekday
        , getPrevMonth
        , getPrevWeekday
        , getWeekday
        , isLeapYear
        , numberOfDaysInMonth
        , resetTime
        , weekdayDiff
        )
import Expect
import Test exposing (..)
import Time exposing (Month(..), Weekday(..), customZone, millisToPosix, utc)


all : Test
all =
    describe "Test date utils"
        [ describe "Test next month"
            [ test "Get next month Jan" <| \() -> Expect.equal (getNextMonth Jan) Feb
            , test "Test next month Feb" <| \() -> Expect.equal (getNextMonth Feb) Mar
            , test "Test next month Mar" <| \() -> Expect.equal (getNextMonth Mar) Apr
            , test "Test next month Apr" <| \() -> Expect.equal (getNextMonth Apr) May
            , test "Test next month May" <| \() -> Expect.equal (getNextMonth May) Jun
            , test "Test next month Jun" <| \() -> Expect.equal (getNextMonth Jun) Jul
            , test "Test next month Jul" <| \() -> Expect.equal (getNextMonth Jul) Aug
            , test "Test next month Aug" <| \() -> Expect.equal (getNextMonth Aug) Sep
            , test "Test next month Sep" <| \() -> Expect.equal (getNextMonth Sep) Oct
            , test "Test next month Oct" <| \() -> Expect.equal (getNextMonth Oct) Nov
            , test "Test next month Nov" <| \() -> Expect.equal (getNextMonth Nov) Dec
            , test "Test next month Dec" <| \() -> Expect.equal (getNextMonth Dec) Jan
            ]
        , describe "Tests prev month"
            [ test "Get prev month Jan" <| \() -> Expect.equal (getPrevMonth Jan) Dec
            , test "Get prev month Feb" <| \() -> Expect.equal (getPrevMonth Feb) Jan
            , test "Get prev month Mar" <| \() -> Expect.equal (getPrevMonth Mar) Feb
            , test "Get prev month Apr" <| \() -> Expect.equal (getPrevMonth Apr) Mar
            , test "Get prev month May" <| \() -> Expect.equal (getPrevMonth May) Apr
            , test "Get prev month Jun" <| \() -> Expect.equal (getPrevMonth Jun) May
            , test "Get prev month Jul" <| \() -> Expect.equal (getPrevMonth Jul) Jun
            , test "Get prev month Aug" <| \() -> Expect.equal (getPrevMonth Aug) Jul
            , test "Get prev month Sept" <| \() -> Expect.equal (getPrevMonth Sep) Aug
            , test "Get prev month Oct" <| \() -> Expect.equal (getPrevMonth Oct) Sep
            , test "Get prev month Nov" <| \() -> Expect.equal (getPrevMonth Nov) Oct
            , test "Get prev month Dec" <| \() -> Expect.equal (getPrevMonth Dec) Nov
            ]
        , describe "Tests next weekday"
            [ test "Get next weekday Mon" <| \() -> Expect.equal (getNextWeekday Mon) Tue
            , test "Get next weekday Tue" <| \() -> Expect.equal (getNextWeekday Tue) Wed
            , test "Get next weekday Wed" <| \() -> Expect.equal (getNextWeekday Wed) Thu
            , test "Get next weekday Thu" <| \() -> Expect.equal (getNextWeekday Thu) Fri
            , test "Get next weekday Fri" <| \() -> Expect.equal (getNextWeekday Fri) Sat
            , test "Get next weekday Sat" <| \() -> Expect.equal (getNextWeekday Sat) Sun
            , test "Get next weekday Sun" <| \() -> Expect.equal (getNextWeekday Sun) Mon
            ]
        , describe "Tests prev weekday"
            [ test "Get prev weekday Mon" <| \() -> Expect.equal (getPrevWeekday Mon) Sun
            , test "Get prev weekday Tue" <| \() -> Expect.equal (getPrevWeekday Tue) Mon
            , test "Get prev weekday Wed" <| \() -> Expect.equal (getPrevWeekday Wed) Tue
            , test "Get prev weekday Thu" <| \() -> Expect.equal (getPrevWeekday Thu) Wed
            , test "Get prev weekday Fri" <| \() -> Expect.equal (getPrevWeekday Fri) Thu
            , test "Get prev weekday Sat" <| \() -> Expect.equal (getPrevWeekday Sat) Fri
            , test "Get prev weekday Sun" <| \() -> Expect.equal (getPrevWeekday Sun) Sat
            ]
        , describe "Tests is leap year"
            [ test "Is leap year 1980" <| \() -> Expect.equal (isLeapYear 1980) True
            , test "Is leap year 1981" <| \() -> Expect.equal (isLeapYear 1981) False
            , test "Is leap year 1982" <| \() -> Expect.equal (isLeapYear 1982) False
            , test "Is leap year 1983" <| \() -> Expect.equal (isLeapYear 1983) False
            , test "Is leap year 1984" <| \() -> Expect.equal (isLeapYear 1984) True
            , test "Is leap year 2000" <| \() -> Expect.equal (isLeapYear 2000) True
            , test "Is leap year 2100" <| \() -> Expect.equal (isLeapYear 2100) False
            , test "Is leap year 2400" <| \() -> Expect.equal (isLeapYear 2400) True
            ]
        , describe "Tests number of days in month"
            [ test "Number of days in 1980 month Jan" <| \() -> Expect.equal (numberOfDaysInMonth 1980 Jan) 31
            , test "Number of days in 1980 month Feb" <| \() -> Expect.equal (numberOfDaysInMonth 1980 Feb) 29
            , test "Number of days in 1980 month Mar" <| \() -> Expect.equal (numberOfDaysInMonth 1980 Mar) 31
            , test "Number of days in 1980 month Apr" <| \() -> Expect.equal (numberOfDaysInMonth 1980 Apr) 30
            , test "Number of days in 1980 month May" <| \() -> Expect.equal (numberOfDaysInMonth 1980 May) 31
            , test "Number of days in 1980 month Jun" <| \() -> Expect.equal (numberOfDaysInMonth 1980 Jun) 30
            , test "Number of days in 1980 month Jul" <| \() -> Expect.equal (numberOfDaysInMonth 1980 Jul) 31
            , test "Number of days in 1980 month Aug" <| \() -> Expect.equal (numberOfDaysInMonth 1980 Aug) 31
            , test "Number of days in 1980 month Sept" <| \() -> Expect.equal (numberOfDaysInMonth 1980 Sep) 30
            , test "Number of days in 1980 month Oct" <| \() -> Expect.equal (numberOfDaysInMonth 1980 Oct) 31
            , test "Number of days in 1980 month Nov" <| \() -> Expect.equal (numberOfDaysInMonth 1980 Nov) 30
            , test "Number of days in 1980 month Dec" <| \() -> Expect.equal (numberOfDaysInMonth 1980 Dec) 31
            , test "Number of days in 1981 month Jan" <| \() -> Expect.equal (numberOfDaysInMonth 1981 Jan) 31
            , test "Number of days in 1981 month Feb" <| \() -> Expect.equal (numberOfDaysInMonth 1981 Feb) 28
            , test "Number of days in 1981 month Mar" <| \() -> Expect.equal (numberOfDaysInMonth 1981 Mar) 31
            , test "Number of days in 1981 month Apr" <| \() -> Expect.equal (numberOfDaysInMonth 1981 Apr) 30
            , test "Number of days in 1981 month May" <| \() -> Expect.equal (numberOfDaysInMonth 1981 May) 31
            , test "Number of days in 1981 month Jun" <| \() -> Expect.equal (numberOfDaysInMonth 1981 Jun) 30
            , test "Number of days in 1981 month Jul" <| \() -> Expect.equal (numberOfDaysInMonth 1981 Jul) 31
            , test "Number of days in 1981 month Aug" <| \() -> Expect.equal (numberOfDaysInMonth 1981 Aug) 31
            , test "Number of days in 1981 month Sept" <| \() -> Expect.equal (numberOfDaysInMonth 1981 Sep) 30
            , test "Number of days in 1981 month Oct" <| \() -> Expect.equal (numberOfDaysInMonth 1981 Oct) 31
            , test "Number of days in 1981 month Nov" <| \() -> Expect.equal (numberOfDaysInMonth 1981 Nov) 30
            , test "Number of days in 1981 month Dec" <| \() -> Expect.equal (numberOfDaysInMonth 1981 Dec) 31
            ]
        , describe "Tests Weekdays"
            [ test "From Tue(1) to Wed(2)" <| \() -> Expect.equal (weekdayDiff Tue Wed) 1
            , test "From Tue(1) to Mon(0)" <| \() -> Expect.equal (weekdayDiff Tue Mon) 6
            , test "From Fri(4) to Tue(1)" <| \() -> Expect.equal (weekdayDiff Fri Tue) 4
            , test "From Sun(6) to Tue(1)" <| \() -> Expect.equal (weekdayDiff Sun Tue) 2
            , test "From Wed(2) to Sun(6)" <| \() -> Expect.equal (weekdayDiff Wed Sun) 4
            , test "From Fri(4) to Wed(2)" <| \() -> Expect.equal (weekdayDiff Fri Wed) 5
            , test "From Sat(5) to Sat(5)" <| \() -> Expect.equal (weekdayDiff Sat Sat) 0
            ]
        , describe "Test get weekday from days"
            [ test "Get weekday for 24/4/1980" <|
                \() ->
                    let
                        weekday =
                            325467900000
                                |> millisToPosix
                                |> getWeekday
                    in
                    Expect.equal weekday Thu
            , test "Get weekday for 13/2/2018" <|
                \() ->
                    let
                        weekday =
                            1518551100000
                                |> millisToPosix
                                |> getWeekday
                    in
                    Expect.equal weekday Tue
            , test "Get weekday for 15/1/1984" <|
                \() ->
                    let
                        weekday =
                            443043900000
                                |> millisToPosix
                                |> getWeekday
                    in
                    Expect.equal weekday Sun
            ]
        , describe "Test reset time"
            [ test "Test 2018/10/19 11:05:45.1232 to 2018/10/19 00:00:00.000" <| \() -> Expect.equal (resetTime <| millisToPosix 1539939885000) (millisToPosix 1539907200000)
            ]
        , describe "Test converting to ISO format"
            [ test "Test for 26/10/2018T10:52 UTC" <|
                \() ->
                    let
                        time =
                            millisToPosix 1540543940000

                        isoFormat =
                            time
                                |> getIsoFormat "-" utc
                    in
                    Expect.equal isoFormat "2018-10-26T08:52:20"
            , test "Test for 26/10/2018T10:52 for CEST" <|
                \() ->
                    let
                        time =
                            millisToPosix 1540543940000

                        cest =
                            customZone 120 []

                        isoFormat =
                            time
                                |> getIsoFormat "-" cest
                    in
                    Expect.equal isoFormat "2018-10-26T10:52:20"
            ]
        ]

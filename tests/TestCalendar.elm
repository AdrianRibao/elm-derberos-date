module TestCalendar exposing (..)

import Derberos.Date.Calendar exposing (getCurrentMonthDates, getCurrentMonthDatesFullWeeks, getCurrentWeekDates, getFirstDayOfMonth, getFirstDayOfYear, getLastDayOfYear)
import Derberos.Date.TimeCompat exposing (utc)
import Expect
import Test exposing (..)
import Time exposing (Posix, Weekday(..), millisToPosix)


all : Test
all =
    describe "Test calendar functions"
        [ describe "Test get first day of month"
            [ test "First day of month for date 16/10/2018" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1539005079000

                        -- 1/10/2018 00:00:00
                        expectedTime =
                            millisToPosix 1538352000000
                    in
                    Expect.equal (getFirstDayOfMonth utc posixTime) expectedTime
            ]
        , describe "Test get week for date"
            [ test "Week for day Wed 17/10/2018 12:00:00" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1539777600000

                        -- From Mon 15/10/18 to 21/10/18
                        expectedTimes =
                            [ millisToPosix 1539604800000
                            , millisToPosix 1539691200000
                            , millisToPosix 1539777600000
                            , millisToPosix 1539864000000
                            , millisToPosix 1539950400000
                            , millisToPosix 1540036800000
                            , millisToPosix 1540123200000
                            ]
                    in
                    Expect.equal (getCurrentWeekDates posixTime) expectedTimes
            , test "Week for day Wed 13/2/2018 12:00:00" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1518523200000

                        -- From Mon 12/10/18 to 18/10/18
                        expectedTimes =
                            [ millisToPosix 1518436800000
                            , millisToPosix 1518523200000
                            , millisToPosix 1518609600000
                            , millisToPosix 1518696000000
                            , millisToPosix 1518782400000
                            , millisToPosix 1518868800000
                            , millisToPosix 1518955200000
                            ]
                    in
                    Expect.equal (getCurrentWeekDates posixTime) expectedTimes
            ]
        , describe "Test get month for date"
            [ test "Month for day Wed 17/10/2018 12:00:00" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1539777600000

                        -- From Mon 1/10/18 to 31/10/18
                        expectedTimes =
                            [ millisToPosix 1538352000000
                            , millisToPosix 1538438400000
                            , millisToPosix 1538524800000
                            , millisToPosix 1538611200000
                            , millisToPosix 1538697600000
                            , millisToPosix 1538784000000
                            , millisToPosix 1538870400000
                            , millisToPosix 1538956800000
                            , millisToPosix 1539043200000
                            , millisToPosix 1539129600000
                            , millisToPosix 1539216000000
                            , millisToPosix 1539302400000
                            , millisToPosix 1539388800000
                            , millisToPosix 1539475200000
                            , millisToPosix 1539561600000
                            , millisToPosix 1539648000000
                            , millisToPosix 1539734400000
                            , millisToPosix 1539820800000
                            , millisToPosix 1539907200000
                            , millisToPosix 1539993600000
                            , millisToPosix 1540080000000
                            , millisToPosix 1540166400000
                            , millisToPosix 1540252800000
                            , millisToPosix 1540339200000
                            , millisToPosix 1540425600000
                            , millisToPosix 1540512000000
                            , millisToPosix 1540598400000
                            , millisToPosix 1540684800000
                            , millisToPosix 1540771200000
                            , millisToPosix 1540857600000
                            , millisToPosix 1540944000000
                            ]
                    in
                    Expect.equal (getCurrentMonthDates utc posixTime) expectedTimes
            ]
        , describe "Test get full month for date"
            [ test "Month for day Wed 17/10/2018 12:00:00" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1539777600000

                        -- From Mon 1/10/18 to 31/10/18
                        expectedTimes =
                            [ millisToPosix 1538352000000
                            , millisToPosix 1538438400000
                            , millisToPosix 1538524800000
                            , millisToPosix 1538611200000
                            , millisToPosix 1538697600000
                            , millisToPosix 1538784000000
                            , millisToPosix 1538870400000
                            , millisToPosix 1538956800000
                            , millisToPosix 1539043200000
                            , millisToPosix 1539129600000
                            , millisToPosix 1539216000000
                            , millisToPosix 1539302400000
                            , millisToPosix 1539388800000
                            , millisToPosix 1539475200000
                            , millisToPosix 1539561600000
                            , millisToPosix 1539648000000
                            , millisToPosix 1539734400000
                            , millisToPosix 1539820800000
                            , millisToPosix 1539907200000
                            , millisToPosix 1539993600000
                            , millisToPosix 1540080000000
                            , millisToPosix 1540166400000
                            , millisToPosix 1540252800000
                            , millisToPosix 1540339200000
                            , millisToPosix 1540425600000
                            , millisToPosix 1540512000000
                            , millisToPosix 1540598400000
                            , millisToPosix 1540684800000
                            , millisToPosix 1540771200000
                            , millisToPosix 1540857600000
                            , millisToPosix 1540944000000
                            , millisToPosix 1541030400000
                            , millisToPosix 1541116800000
                            , millisToPosix 1541203200000
                            , millisToPosix 1541289600000
                            ]
                    in
                    Expect.equal (getCurrentMonthDatesFullWeeks utc posixTime) expectedTimes
            , test "Month for day Dec 15/12/2018 12:00:00" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1544867097000

                        -- From Mon 26/12/18 to 6/1/19
                        expectedTimes =
                            [ millisToPosix 1543190400000
                            , millisToPosix 1543276800000
                            , millisToPosix 1543363200000
                            , millisToPosix 1543449600000
                            , millisToPosix 1543536000000
                            , millisToPosix 1543622400000
                            , millisToPosix 1543708800000
                            , millisToPosix 1543795200000
                            , millisToPosix 1543881600000
                            , millisToPosix 1543968000000
                            , millisToPosix 1544054400000
                            , millisToPosix 1544140800000
                            , millisToPosix 1544227200000
                            , millisToPosix 1544313600000
                            , millisToPosix 1544400000000
                            , millisToPosix 1544486400000
                            , millisToPosix 1544572800000
                            , millisToPosix 1544659200000
                            , millisToPosix 1544745600000
                            , millisToPosix 1544832000000
                            , millisToPosix 1544918400000
                            , millisToPosix 1545004800000
                            , millisToPosix 1545091200000
                            , millisToPosix 1545177600000
                            , millisToPosix 1545264000000
                            , millisToPosix 1545350400000
                            , millisToPosix 1545436800000
                            , millisToPosix 1545523200000
                            , millisToPosix 1545609600000
                            , millisToPosix 1545696000000
                            , millisToPosix 1545782400000
                            , millisToPosix 1545868800000
                            , millisToPosix 1545955200000
                            , millisToPosix 1546041600000
                            , millisToPosix 1546128000000
                            , millisToPosix 1546214400000
                            , millisToPosix 1546300800000
                            , millisToPosix 1546387200000
                            , millisToPosix 1546473600000
                            , millisToPosix 1546560000000
                            , millisToPosix 1546646400000
                            , millisToPosix 1546732800000
                            ]
                    in
                    Expect.equal (getCurrentMonthDatesFullWeeks utc posixTime) expectedTimes
            ]
        , describe "Test getFirstDayOfYear"
            [ test "First day of year for date 2018/10/19" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1539940555000

                        -- Expect 2018/1/1 00:00:00
                        expectedTime =
                            millisToPosix 1514764800000
                    in
                    Expect.equal (getFirstDayOfYear utc posixTime) expectedTime
            ]
        , describe "Test getLastDayOfYear"
            [ test "Last day of year for date 2018/10/19" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1539940555000

                        -- Expect 2018/1/1 00:00:00
                        expectedTime =
                            millisToPosix 1546214400000
                    in
                    Expect.equal (getLastDayOfYear utc posixTime) expectedTime
            ]
        ]

module TestCalendar exposing (..)

import Derberos.Date.Calendar exposing (getCurrentMonthDates, getCurrentWeekDates, getFirstDayOfMonth)
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
                    Expect.equal (getFirstDayOfMonth posixTime) expectedTime
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
                    Expect.equal (getCurrentMonthDates posixTime) expectedTimes
            ]
        ]

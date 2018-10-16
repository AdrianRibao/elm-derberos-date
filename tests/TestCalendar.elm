module TestCalendar exposing (..)

import Derberos.Date.Calendar exposing (getFirstDayOfMonth)
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
        ]

module TestDelta exposing (..)

import Derberos.Date.Delta exposing (addDays, addHours, addMinutes, addSeconds)
import Expect
import Test exposing (..)
import Time exposing (Posix, millisToPosix)


all : Test
all =
    describe "Test delta functions"
        [ describe "Test add seconds"
            [ test "Add 60 seconds" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1539005079000

                        expectedTime =
                            millisToPosix 1539005139000
                    in
                    Expect.equal (addSeconds 60 posixTime) expectedTime
            , test "Add 60 minutes" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1539005079000

                        expectedTime =
                            millisToPosix 1539008679000
                    in
                    Expect.equal (addMinutes 60 posixTime) expectedTime
            , test "Add 5 hours" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1539005079000

                        expectedTime =
                            millisToPosix 1539023079000
                    in
                    Expect.equal (addHours 5 posixTime) expectedTime
            , test "Add 7 days" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1539005079000

                        expectedTime =
                            millisToPosix 1539609879000
                    in
                    Expect.equal (addDays 7 posixTime) expectedTime
            ]
        ]

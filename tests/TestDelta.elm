module TestDelta exposing (..)

import Derberos.Date.Delta exposing (addDays, addHours, addMinutes, addSeconds, addYears)
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
            ]
        , describe "Test add minutes"
            [ test "Add 60 minutes" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1539005079000

                        expectedTime =
                            millisToPosix 1539008679000
                    in
                    Expect.equal (addMinutes 60 posixTime) expectedTime
            ]
        , describe "Test add hours"
            [ test "Add 5 hours" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1539005079000

                        expectedTime =
                            millisToPosix 1539023079000
                    in
                    Expect.equal (addHours 5 posixTime) expectedTime
            ]
        , describe "Test add days"
            [ test "Add 7 days" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1539005079000

                        expectedTime =
                            millisToPosix 1539609879000
                    in
                    Expect.equal (addDays 7 posixTime) expectedTime
            ]
        , describe "Test add years"
            [ test "Add 1 years to 14/1/2014 (1393632000000)" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1393632000000

                        expectedTime =
                            millisToPosix 1425168000000
                    in
                    Expect.equal (addYears 1 posixTime) expectedTime
            , test "Add 2 years to 14/1/2014 (1393632000000)" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1393632000000

                        expectedTime =
                            millisToPosix 1456790400000
                    in
                    Expect.equal (addYears 2 posixTime) expectedTime
            , test "Add 3 years to 14/1/2014 (1393632000000)" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1393632000000

                        expectedTime =
                            millisToPosix 1488326400000
                    in
                    Expect.equal (addYears 3 posixTime) expectedTime
            , test "Add 200 years to 14/1/2014 (1393632000000)" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1393632000000

                        expectedTime =
                            millisToPosix 7704979200000
                    in
                    Expect.equal (addYears 200 posixTime) expectedTime
            , test "Add 500 years to 14/1/2014 (1393632000000)" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1393632000000

                        expectedTime =
                            millisToPosix 17172086400000
                    in
                    Expect.equal (addYears 500 posixTime) expectedTime
            ]
        ]

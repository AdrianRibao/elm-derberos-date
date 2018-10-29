module TestDelta exposing (..)

import Derberos.Date.Core exposing (posixToCivil)
import Derberos.Date.Delta exposing (addDays, addHours, addMinutes, addMonths, addSeconds, addYears, nextWeekdayFromTime, prevWeekdayFromTime)
import Expect
import Test exposing (..)
import Time exposing (Posix, Weekday(..), customZone, millisToPosix, utc)


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
            , test "Add 41 days From 26/12/2018 to 6/1/2019" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1543190400000

                        expectedTime =
                            millisToPosix 1546732800000
                    in
                    Expect.equal (addDays 41 posixTime) expectedTime
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
        , describe "Test add months"
            [ test "Add 12 months to 1/1/2015 (1420070400000)" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1420070400000

                        expectedTime =
                            millisToPosix 1451606400000
                    in
                    Expect.equal (addMonths 12 utc posixTime) expectedTime
            , test "Add 1 months to 1/1/2015 (1420070400000)" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1420070400000

                        expectedTime =
                            millisToPosix 1422748800000
                    in
                    Expect.equal (addMonths 1 utc posixTime) expectedTime
            , test "Add 1 months to 20181031T23:45:00 testing CET" <|
                \() ->
                    let
                        cet =
                            customZone 60 []

                        -- This is 20181031T23:45:00 UTC
                        -- CET:20181101T00:45:00+01:00
                        posixTime =
                            millisToPosix 1541029500000

                        -- Expect UTC: 20181131T23:45:00Z
                        -- Expect CET: 20181201T00:45:00+01:00
                        expectedTime =
                            millisToPosix 1543621500000
                    in
                    Expect.equal (addMonths 1 cet posixTime) expectedTime
            , test "Add 24 months to 1/1/2015 (1420070400000)" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1420070400000

                        expectedTime =
                            millisToPosix 1483228800000
                    in
                    Expect.equal (addMonths 24 utc posixTime) expectedTime
            , test "Add 12 months to 29/2/2016 (1456704000000)" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1456704000000

                        expectedTime =
                            millisToPosix 1488326400000
                    in
                    Expect.equal (addMonths 12 utc posixTime) expectedTime
            , test "Substract 12 months to 24/4/2016 (1461456000000)" <|
                \() ->
                    let
                        posixTime =
                            millisToPosix 1461456000000

                        expectedTime =
                            millisToPosix 1429833600000
                    in
                    Expect.equal (addMonths -12 utc posixTime) expectedTime
            , test "Substract 6 months to 24/4/2016 (1461456000000)" <|
                \() ->
                    let
                        -- 20160424
                        posixTime =
                            millisToPosix 1461456000000

                        -- Expect 20151024
                        expectedTime =
                            millisToPosix 1445644800000
                    in
                    Expect.equal (addMonths -6 utc posixTime) expectedTime
            , test "Add 1 months to 20181224T00:00:00" <|
                \() ->
                    let
                        -- This is UTC: 20181224T00:00:00Z
                        -- This is CET: 20181123T23:00:00+01:00
                        posixTime =
                            millisToPosix 1545609600000

                        -- EXPECT UTC: 20190124T00:00:00Z
                        -- EXPECT CET: 20190123T23:00:00+01:00
                        expectedTime =
                            millisToPosix 1548288000000
                    in
                    Expect.equal (addMonths 1 utc posixTime) expectedTime
            ]
        , describe "Test get prev weekday from times"
            [ test "From Monday to Friday" <|
                \() ->
                    let
                        -- Monday 15/10/2018
                        posixTime =
                            millisToPosix 1539613796000

                        -- Friday 12/10/2018
                        expectedTime =
                            millisToPosix 1539354596000
                    in
                    Expect.equal (prevWeekdayFromTime Fri utc posixTime) expectedTime
            , test "From Monday to Tuesday" <|
                \() ->
                    let
                        -- Monday 15/10/2018
                        posixTime =
                            millisToPosix 1539613796000

                        -- Tuesday 9/10/2018
                        expectedTime =
                            millisToPosix 1539095396000
                    in
                    Expect.equal (prevWeekdayFromTime Tue utc posixTime) expectedTime
            , test "From Saturday to Tuesday" <|
                \() ->
                    let
                        -- Monday 12/10/2018
                        posixTime =
                            millisToPosix 1539432000000

                        -- Tuesday 9/10/2018
                        expectedTime =
                            millisToPosix 1539086400000
                    in
                    Expect.equal (prevWeekdayFromTime Tue utc posixTime) expectedTime
            , test "From Monday to Monday" <|
                \() ->
                    let
                        -- Monday 15/10/2018
                        posixTime =
                            millisToPosix 1539613796000

                        -- Monday 15/10/2018
                        expectedTime =
                            millisToPosix 1539613796000
                    in
                    Expect.equal (prevWeekdayFromTime Mon utc posixTime) expectedTime
            , test "From Monday to Tuesday using CET" <|
                \() ->
                    let
                        cet =
                            customZone 60 []

                        -- Monday 20181015T23:45:00Z
                        -- CET is Tuesday 20181016T00:45:00Z
                        posixTime =
                            millisToPosix 1539647100000

                        -- Monday 20181015T23:45:00Z
                        -- CET Tuesday 20181016T00:45:00Z
                        expectedTime =
                            millisToPosix 1539647100000
                    in
                    Expect.equal (prevWeekdayFromTime Tue cet posixTime) expectedTime
            ]
        , describe "Test get next weekday from times"
            [ test "From Monday to Friday" <|
                \() ->
                    let
                        -- Monday 15/10/2018
                        posixTime =
                            millisToPosix 1539561600000

                        -- Friday 19/10/2018
                        expectedTime =
                            millisToPosix 1539907200000
                    in
                    Expect.equal (nextWeekdayFromTime Fri utc posixTime) expectedTime
            , test "From Monday to Tuesday" <|
                \() ->
                    let
                        -- Monday 15/10/2018
                        posixTime =
                            millisToPosix 1539561600000

                        -- Tuesday 16/10/2018
                        expectedTime =
                            millisToPosix 1539648000000
                    in
                    Expect.equal (nextWeekdayFromTime Tue utc posixTime) expectedTime
            , test "From Saturday to Tuesday" <|
                \() ->
                    let
                        -- Saturday 13/10/2018
                        posixTime =
                            millisToPosix 1539388800000

                        -- Tuesday 16/10/2018
                        expectedTime =
                            millisToPosix 1539648000000
                    in
                    Expect.equal (nextWeekdayFromTime Tue utc posixTime) expectedTime
            , test "From Monday to Monday" <|
                \() ->
                    let
                        -- Monday 15/10/2018
                        posixTime =
                            millisToPosix 1539613796000

                        -- Monday 15/10/2018
                        expectedTime =
                            millisToPosix 1539613796000
                    in
                    Expect.equal (nextWeekdayFromTime Mon utc posixTime) expectedTime
            , test "Next Tuesday from Tuesday using CET" <|
                \() ->
                    let
                        cet =
                            customZone 60 []

                        -- Monday 20181015T23:45:00Z
                        -- CET is Tuesday 20181016T00:45:00Z
                        posixTime =
                            millisToPosix 1539647100000

                        -- Monday 20181015T23:45:00Z
                        -- CET Tuesday 20181016T00:45:00Z
                        expectedTime =
                            millisToPosix 1539647100000
                    in
                    Expect.equal (nextWeekdayFromTime Tue cet posixTime) expectedTime
            ]
        ]

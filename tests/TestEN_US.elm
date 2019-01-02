module TestEN_US exposing (..)

import Derberos.Date.L10n.EN_US exposing (config)
import Expect
import Test exposing (..)
import Time exposing (Month(..), Weekday(..), customZone, millisToPosix, utc)


all : Test
all =
    describe "Test date utils for EN_US"
        [ describe "Test converting to normal format date"
            [ test "Test for 26/10/2018T08:52 with - separator" <|
                \() ->
                    let
                        time =
                            millisToPosix 1540543940000

                        commonFormatDate =
                            time
                                |> config.getCommonFormatDate "-" utc
                    in
                    Expect.equal commonFormatDate "10-26-2018"
            , test "Test for 26/10/2018T08:52 with / separator" <|
                \() ->
                    let
                        time =
                            millisToPosix 1540543940000

                        commonFormatDate =
                            time
                                |> config.getCommonFormatDate "/" utc
                    in
                    Expect.equal commonFormatDate "10/26/2018"
            ]
        , describe "Test converting to normal format time"
            [ test "Test for 26/10/2018T08:52" <|
                \() ->
                    let
                        time =
                            millisToPosix 1540543940000

                        commonFormatTime =
                            time
                                |> config.getCommonFormatTime utc
                    in
                    Expect.equal commonFormatTime "08:52:20"
            , test "Test for 26/10/2018T08:52 with TZ CEST" <|
                \() ->
                    let
                        time =
                            millisToPosix 1540543940000

                        cest =
                            customZone 120 []

                        commonFormatTime =
                            time
                                |> config.getCommonFormatTime cest
                    in
                    Expect.equal commonFormatTime "10:52:20"
            ]
        , describe "Test converting to normal format datetime"
            [ test "Test for datetime 26/10/2018T08:52 with - separator" <|
                \() ->
                    let
                        time =
                            millisToPosix 1540543940000

                        commonFormatDate =
                            time
                                |> config.getCommonFormatDateTime "-" utc
                    in
                    Expect.equal commonFormatDate "10-26-2018 08:52:20"
            , test "Test for datetime 26/10/2018T08:52 with / separator" <|
                \() ->
                    let
                        time =
                            millisToPosix 1540543940000

                        commonFormatDate =
                            time
                                |> config.getCommonFormatDateTime "/" utc
                    in
                    Expect.equal commonFormatDate "10/26/2018 08:52:20"
            , test "Test for datetime 26/10/2018T08:52 with / separator and madrid tz" <|
                \() ->
                    let
                        time =
                            millisToPosix 1540543940000

                        europeMadrid =
                            customZone 120 []

                        commonFormatDate =
                            time
                                |> config.getCommonFormatDateTime "/" europeMadrid
                    in
                    Expect.equal commonFormatDate "10/26/2018 10:52:20"
            ]
        ]

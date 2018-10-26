module TestES_ES exposing (..)

import Derberos.Date.L10n.ES_ES exposing (config)
import Expect
import Test exposing (..)
import Time exposing (Month(..), Weekday(..), millisToPosix, utc)


all : Test
all =
    describe "Test date utils"
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
                    Expect.equal commonFormatDate "26-10-2018"
            , test "Test for 26/10/2018T08:52 with / separator" <|
                \() ->
                    let
                        time =
                            millisToPosix 1540543940000

                        commonFormatDate =
                            time
                                |> config.getCommonFormatDate "/" utc
                    in
                    Expect.equal commonFormatDate "26/10/2018"
            ]
        , describe "Test converting to normal format tim"
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
                    Expect.equal commonFormatDate "26-10-2018 08:52:20"
            , test "Test for datetime 26/10/2018T08:52 with / separator" <|
                \() ->
                    let
                        time =
                            millisToPosix 1540543940000

                        commonFormatDate =
                            time
                                |> config.getCommonFormatDateTime "/" utc
                    in
                    Expect.equal commonFormatDate "26/10/2018 08:52:20"
            ]
        ]

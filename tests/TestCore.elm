module TestCore exposing (..)

import Derberos.Date.Core exposing (civilToPosix, newDateRecord, posixToCivil)
import Expect
import Test exposing (..)
import Time exposing (Posix, Weekday(..), millisToPosix)


all : Test
all =
    describe "Test core functions"
        [ describe "Test civil to posix"
            [ test "Convert 12/10/2018 12:34:56 to posix" <|
                \() ->
                    let
                        calculatedTime =
                            civilToPosix <| newDateRecord 2018 10 12 12 34 56 123

                        expectedTime =
                            millisToPosix 1539347696123
                    in
                    Expect.equal calculatedTime expectedTime
            , test "Convert 13/2/2018 19:45:00 to posix" <|
                \() ->
                    let
                        calculatedTime =
                            civilToPosix <| newDateRecord 2018 2 13 19 45 0 0

                        expectedTime =
                            millisToPosix 1518551100000
                    in
                    Expect.equal calculatedTime expectedTime
            , test "Prev Epoch Convert 12/10/2018 12:34:56 to posix" <|
                \() ->
                    let
                        calculatedTime =
                            civilToPosix <| newDateRecord 1490 4 24 23 0 0 0

                        expectedTime =
                            millisToPosix -15137456400000
                    in
                    Expect.equal calculatedTime expectedTime
            , test "Test epoch" <|
                \() ->
                    let
                        calculatedTime =
                            civilToPosix <| newDateRecord 1970 1 1 0 0 0 0

                        expectedTime =
                            millisToPosix 0
                    in
                    Expect.equal calculatedTime expectedTime
            ]
        , describe "Test posix to civil"
            [ test "Convert posix date 12/10/2018 12:34:56" <|
                \() ->
                    let
                        calculatedTime =
                            1539347696123
                                |> millisToPosix
                                |> posixToCivil

                        expectedTime =
                            { year = 2018
                            , month = 10
                            , day = 12
                            , hour = 12
                            , minute = 34
                            , second = 56
                            , millis = 123
                            }
                    in
                    Expect.equal calculatedTime expectedTime
            , test "Convert calculatedTime to 13/2/2018 19:45:00" <|
                \() ->
                    let
                        calculatedTime =
                            1518551100000
                                |> millisToPosix
                                |> posixToCivil

                        expectedTime =
                            { year = 2018
                            , month = 2
                            , day = 13
                            , hour = 19
                            , minute = 45
                            , second = 0
                            , millis = 0
                            }
                    in
                    Expect.equal calculatedTime expectedTime
            , test "Prev Epoch Convert calculatedTime to 24/4/1490 23:00:00" <|
                \() ->
                    let
                        calculatedTime =
                            -15137453644000
                                |> millisToPosix
                                |> posixToCivil

                        expectedTime =
                            { year = 1490
                            , month = 4
                            , day = 24
                            , hour = 23
                            , minute = 45
                            , second = 56
                            , millis = 0
                            }
                    in
                    Expect.equal calculatedTime expectedTime
            , test "Prev Epoch Convert calculatedTime to 24/4/-1 23:00:00" <|
                \() ->
                    let
                        calculatedTime =
                            -62188909200000
                                |> millisToPosix
                                |> posixToCivil

                        expectedTime =
                            { year = -1
                            , month = 4
                            , day = 24
                            , hour = 23
                            , minute = 0
                            , second = 0
                            , millis = 0
                            }
                    in
                    Expect.equal calculatedTime expectedTime
            , test "Prev Epoch Convert calculatedTime to 24/4/1 23:45:56" <|
                \() ->
                    let
                        calculatedTime =
                            -62125748044000
                                |> millisToPosix
                                |> posixToCivil

                        expectedTime =
                            { year = 1
                            , month = 4
                            , day = 24
                            , hour = 23
                            , minute = 45
                            , second = 56
                            , millis = 0
                            }
                    in
                    Expect.equal calculatedTime expectedTime
            , test "Test epoch" <|
                \() ->
                    let
                        calculatedTime =
                            0
                                |> millisToPosix
                                |> posixToCivil

                        expectedTime =
                            { year = 1970
                            , month = 1
                            , day = 1
                            , hour = 0
                            , minute = 0
                            , second = 0
                            , millis = 0
                            }
                    in
                    Expect.equal calculatedTime expectedTime
            ]
        ]

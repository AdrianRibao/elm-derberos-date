module TestTimeCompat exposing (..)

import Derberos.Date.TimeCompat exposing (Zone(..))
import Derberos.Date.Utils exposing (convertZoneFromTime)
import Expect
import Test exposing (..)
import Time exposing (utc)


all : Test
all =
    describe "Test TimeCompat."
        [ describe "Test converting timezones to internal"
            [ test "Test the offset is 0" <|
                \() ->
                    case convertZoneFromTime utc of
                        Zone int _ ->
                            Expect.equal int 0
            , test "Test the eras is empty" <|
                \() ->
                    case convertZoneFromTime utc of
                        Zone _ eras ->
                            Expect.equal eras []
            ]
        ]

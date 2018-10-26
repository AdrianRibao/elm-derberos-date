module TestTimeCompat exposing (..)

import Derberos.Date.TimeCompat exposing (Zone(..))
import Expect
import Test exposing (..)
import Time exposing (Zone, customZone, utc)


europe_madrid : Time.Zone
europe_madrid =
    customZone 120 []



-- No tests yet


all : Test
all =
    describe "Test TimeCompat."
        [ describe "Placeholder tests"
            [ test "Placeholder test" <|
                \() ->
                    Expect.equal True True
            ]
        ]

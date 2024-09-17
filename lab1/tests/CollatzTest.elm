module CollatzTest exposing (suite)

import CollatzMap exposing (longestCollatzMap)
import CollatzModule exposing (longestCollatzModule)
import CollatzRec exposing (longestCollatzRec)
import CollatzTail exposing (longestCollatzTail)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Collatz Length Tests"
        [ test "The longest Collatz sequence under 1000 is 871 (tail)" <|
            \() ->
                Expect.equal 871 (longestCollatzTail 1000)
        , test "The longest Collatz sequence under 1000 is 871 (rec)" <|
            \() ->
                Expect.equal 871 (longestCollatzRec 1000)
        , test "The longest Collatz sequence under 1000 is 871 (module)" <|
            \() ->
                Expect.equal 871 (longestCollatzModule 1000)
        , test "The longest Collatz sequence under 1000 is 871 (map)" <|
            \() ->
                Expect.equal 871 (longestCollatzMap 1000)
        ]

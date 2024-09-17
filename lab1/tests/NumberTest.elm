module NumberTest exposing (..)

import Expect
import NumberLetterMap exposing (numberLetterMap)
import NumberLetterModule exposing (numberLetterModule)
import NumberLetterRec exposing (numberLetterRec)
import NumberLetterTail exposing (numberLetterTail)
import Test exposing (..)


suite : Test
suite =
    describe "Number Letter Count Tests"
        [ test "The number of letters in the words for the numbers 1 to 1000 is 21124 (rec)" <|
            \() ->
                Expect.equal 21124 (numberLetterRec 1000)
        , test "The number of letters in the words for the numbers 1 to 1000 is 21124 (tail)" <|
            \() ->
                Expect.equal 21124 (numberLetterTail 1000)
        , test "The number of letters in the words for the numbers 1 to 1000 is 21124 (module)" <|
            \() ->
                Expect.equal 21124 (numberLetterModule 1000)
        , test "The number of letters in the words for the numbers 1 to 1000 is 21124 (map)" <|
            \() ->
                Expect.equal 21124 (numberLetterMap 1000)
        ]

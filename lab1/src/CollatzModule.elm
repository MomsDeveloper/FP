module CollatzModule exposing (longestCollatzModule)


collatzSeq : Int -> List Int
collatzSeq n =
    if n == 1 then
        [ 1 ]

    else if remainderBy 2 n == 0 then
        n :: collatzSeq (n // 2)

    else
        n :: collatzSeq (3 * n + 1)


collatzSeqsStartAndLength : Int -> List ( Int, Int )
collatzSeqsStartAndLength limit =
    if limit == 1 then
        [ ( 1, 1 ) ]

    else
        ( limit, List.length (collatzSeq limit) ) :: collatzSeqsStartAndLength (limit - 1)


compareSeqLengths : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
compareSeqLengths ( s0, l0 ) ( s1, l1 ) =
    if l0 > l1 then
        ( s0, l0 )

    else
        ( s1, l1 )


maxCollatzSeq : List ( Int, Int ) -> Int
maxCollatzSeq xs =
    List.foldl compareSeqLengths ( 0, 0 ) xs
        |> Tuple.first


longestCollatzModule : Int -> Int
longestCollatzModule n =
    maxCollatzSeq (collatzSeqsStartAndLength n)

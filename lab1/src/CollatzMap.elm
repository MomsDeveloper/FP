module CollatzMap exposing (longestCollatzMap)


collatzSeq : Int -> List Int
collatzSeq n =
    if n == 1 then
        [ 1 ]

    else if remainderBy 2 n == 0 then
        n :: collatzSeq (n // 2)

    else
        n :: collatzSeq (3 * n + 1)


compareSeqLengths : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
compareSeqLengths ( s0, l0 ) ( s1, l1 ) =
    if l0 > l1 then
        ( s0, l0 )

    else
        ( s1, l1 )


longestCollatzMap : Int -> Int
longestCollatzMap limit =
    List.range 1 limit
        |> List.map (\x -> ( x, collatzSeq x ))
        |> List.map (\( x, xs ) -> ( x, List.length xs ))
        |> List.foldl compareSeqLengths ( 0, 0 )
        |> Tuple.first

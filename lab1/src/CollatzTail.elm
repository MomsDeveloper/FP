module CollatzTail exposing (longestCollatzTail)


collatzLength : Int -> Int
collatzLength n =
    let
        collatzHelper count value =
            if value == 1 then
                count

            else if remainderBy 2 value == 0 then
                collatzHelper (count + 1) (value // 2)

            else
                collatzHelper (count + 1) (3 * value + 1)
    in
    collatzHelper 1 n


compareSeqLengths : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
compareSeqLengths ( s0, l0 ) ( s1, l1 ) =
    if l0 > l1 then
        ( s0, l0 )

    else
        ( s1, l1 )


longestCollatzTail : Int -> Int
longestCollatzTail limit =
    let
        foldHelper currentLimit currentBest =
            let
                collatzLen =
                    collatzLength currentLimit

                newBest =
                    compareSeqLengths ( currentLimit, collatzLen ) currentBest
            in
            if currentLimit == 1 then
                Tuple.first newBest

            else
                foldHelper (currentLimit - 1) newBest
    in
    foldHelper limit ( 0, 0 )

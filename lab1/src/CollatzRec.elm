module CollatzRec exposing (longestCollatzRec)


collatzLengthRec : Int -> Int
collatzLengthRec n =
    if n == 1 then
        1

    else if remainderBy 2 n == 0 then
        1 + collatzLengthRec (n // 2)

    else
        1 + collatzLengthRec (3 * n + 1)


compareSeqLengths : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
compareSeqLengths ( s0, l0 ) ( s1, l1 ) =
    if l0 > l1 then
        ( s0, l0 )

    else
        ( s1, l1 )


longestCollatzRec : Int -> Int
longestCollatzRec limit =
    let
        foldHelper currentLimit currentBest =
            let
                collatzLen =
                    collatzLengthRec currentLimit

                newBest =
                    compareSeqLengths ( currentLimit, collatzLen ) currentBest
            in
            if currentLimit == 1 then
                Tuple.first newBest

            else
                foldHelper (currentLimit - 1) newBest
    in
    foldHelper limit ( 0, 0 )

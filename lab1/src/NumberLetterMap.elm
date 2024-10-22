module NumberLetterMap exposing (numberLetterMap)

import Array exposing (Array)


numberToWords : Int -> Int
numberToWords n =
    if n > 1000 || n < 1 then
        0

    else if n == 1000 then
        11

    else if n < 20 then
        ones n

    else if n < 100 then
        tens n

    else
        hundreds n


ones : Int -> Int
ones n =
    let
        len_ar_1_19 =
            Array.fromList [ 0, 3, 3, 5, 4, 4, 3, 5, 5, 4, 3, 6, 6, 8, 8, 7, 7, 9, 8, 8 ]
    in
    getOrZero n len_ar_1_19


tens : Int -> Int
tens n =
    let
        len_ar_20_90 =
            Array.fromList [ 0, 0, 6, 6, 5, 5, 5, 7, 6, 6 ]
    in
    getOrZero (n // 10) len_ar_20_90 + ones (remainderBy 10 n)


hundreds : Int -> Int
hundreds n =
    let
        hundredsDigit =
            n // 100

        remainder =
            remainderBy 100 n
    in
    ones hundredsDigit
        + 7
        + (if remainder == 0 then
            0

           else
            3 + numberToWords remainder
          )


getOrZero : Int -> Array Int -> Int
getOrZero index array =
    Array.get index array |> Maybe.withDefault 0


numberLetterMap : Int -> Int
numberLetterMap n =
    List.range 1 n
        |> List.map numberToWords
        |> List.sum

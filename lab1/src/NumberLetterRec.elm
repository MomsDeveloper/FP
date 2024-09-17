module NumberLetterRec exposing (numberLetterRec)

import Array

numberToWords : Int -> String
numberToWords n =
    let
        ones =
            Array.fromList [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ]

        teens =
            Array.fromList [ "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" ]

        tens =
            Array.fromList [ "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety" ]

        getOrEmpty index array =
            Array.get index array |> Maybe.withDefault ""
    in
    if n == 0 then
        ""

    else if n < 10 then
        getOrEmpty (n - 1) ones

    else if n < 20 then
        getOrEmpty (n - 10) teens

    else if n < 100 then
        let
            tensDigit =
                n // 10

            onesDigit =
                remainderBy 10 n
        in
        getOrEmpty (tensDigit - 2) tens
            ++ (if onesDigit > 0 then
                    numberToWords onesDigit

                else
                    ""
               )

    else if n < 1000 then
        let
            hundredsDigit =
                n // 100

            remainder =
                remainderBy 100 n
        in
        if remainder == 0 then
            getOrEmpty (hundredsDigit - 1) ones ++ "hundred"

        else
            getOrEmpty (hundredsDigit - 1) ones ++ "hundredand" ++ numberToWords remainder

    else if n == 1000 then
        "onethousand"

    else
        ""


numberLetterRec : Int -> Int
numberLetterRec n =
    if n == 0 then
        0

    else
        String.length (numberToWords n)
            + numberLetterRec (n - 1)

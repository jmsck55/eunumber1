
include my.e

with trace

constant
    n1 = {
           {-7},
           2,
           70,
           10,
           0
         }


trace(1)

useLongDivision = FALSE

? EunMultiplicativeInverse(n1)
? lastIterCount

useLongDivision = TRUE

? EunMultiplicativeInverse(n1)
? lastIterCount

? LongDivision(1, 1, -7, 3, 70, 10)

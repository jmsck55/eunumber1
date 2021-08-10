
include my.e

with trace

constant
    n1 = {
           {-3},
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

useLongDivision = FALSE


-- with trace

include my.e

-- trace(1)

object a, b, c, d

a = ToEun(10)
b = ToEun(100)

c = EunPower(a, b, 2) -- the "2" is to make it more accurate.

puts(1, ToString(c, TRUE) & "\n")

d = EunGeneralRoot(c, a, 2) -- the "2" is to make it more accurate.

puts(1, ToString(d, TRUE) & "\n")

abort(0)

object
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

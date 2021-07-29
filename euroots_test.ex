-- Copyright (c) 2016-2021 James Cook

-- Test:
with trace
include my.e
trace(1)

puts(1, "Calculating, please wait.\n")

defaultTargetLength = 35
calculationSpeed = defaultTargetLength
defaultRadix = 10
adjustRound = 0

function Func1Exp(sequence n1, integer exp1, integer targetLength, integer radix)
     return CosExp(n1,exp1,targetLength,radix)
end function
-- ? Func1Exp({1},-1,100,10)
integer myfunc1 = routine_id("Func1Exp")
sequence ja, jb
ja = {{1},0}
jb = {{2},0}
? FindRootExp(myfunc1,ja[1],ja[2],jb[1],jb[2],defaultTargetLength,defaultRadix)
puts(1,"Should be: ")
puts(1, "\n")
? {"15707963267948966192313216916398" - '0', 0}


function Func2Exp(sequence n1, integer exp1, integer maxlength, integer radix)
     return SinExp(n1,exp1,maxlength,radix)
end function
integer myfunc2 = routine_id("Func2Exp")
--sequence ja, jb
ja = {{2},0}
jb = {{4},0}
puts(1, "Calculating Pi...This could take a few minutes...")
puts(1, "\n")
? FindRootExp(myfunc2,ja[1],ja[2],jb[1],jb[2],defaultTargetLength,defaultRadix)
puts(1,"Should be: Pi\n")
? GetPI()

-- end of file.

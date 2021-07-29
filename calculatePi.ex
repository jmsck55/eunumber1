-- Copyright (c) 2016-2021 James Cook

-- How to calculate PI with Eunumber:

-- 1) Pick an infinite series that goes to zero quickly:

-- pi = 3 + 4/(2*3*4) - 4/(4*5*6) + 4/(6*7*8) - 4/(8*9*10) + 4/(10*11*12)
--      - 4/(12*13*14) ...

-- For this formula, take three and start alternating between Adding and 
-- Subtracting fractions with numerators of 4 and denominators that are the 
-- product of three consecutive integers which increase with every new 
-- iteration. Each subsequent fraction begins its set of integers with the 
-- highest one used in the previous fraction. Carry this out even a few times 
-- and the results get fairly close to pi.

-- See also: https://www.wikihow.com/Calculate-Pi

-- 2) Code it:

include my.e

with trace
trace(1)

defaultRadix = 10
defaultTargetLength = 10

sequence mypi, Sum, numerator, denom, ans

integer count

count = 3

Sum = ToEun(3)

numerator = ToEun(4)

mypi = ToEun(0)

while EunCompare(mypi, Sum) do

	trace(1)
	mypi = Sum
	
	denom = EunMultiply(EunMultiply(ToEun(count - 1), ToEun(count)), ToEun(count + 1))
	
	ans = EunDivide(numerator, denom)
	
	if IsIntegerOdd(floor(count/2)) then
		Sum = EunAdd(Sum, ans)
	else
		Sum = EunSubtract(Sum, ans)
	end if
	
	count += 2
	
end while

puts(1, "Answers are close to PI, but not exactly PI.\n")

printf(1, "Iterations: %d\n", {floor(count / 2)})

puts(1, ToString(mypi) & "\n")
? length(mypi[1])

puts(1, "Another calculation of PI using: 4 * arctan(1)\n")

mypi = EunMultiply(ToEun(4), EunArcTan(ToEun(1)))
puts(1, ToString(mypi) & "\n")
? length(mypi[1])

puts(1, "A more accurate pre-calculation of PI to 1000 decimal places:\n")

defaultTargetLength = 2000

mypi = ToEun(
"3.14159265358979323846264338327950288419716939937510582097494459230781640628" &
"620899862803482534211706798214808651328230664709384460955058223172535940812" &
"848111745028410270193852110555964462294895493038196442881097566593344612847" &
"564823378678316527120190914564856692346034861045432664821339360726024914127" &
"372458700660631558817488152092096282925409171536436789259036001133053054882" &
"046652138414695194151160943305727036575959195309218611738193261179310511854" &
"807446237996274956735188575272489122793818301194912983367336244065664308602" &
"139494639522473719070217986094370277053921717629317675238467481846766940513" &
"200056812714526356082778577134275778960917363717872146844090122495343014654" &
"958537105079227968925892354201995611212902196086403441815981362977477130996" &
"051870721134999999837297804995105973173281609631859502445945534690830264252" &
"230825334468503526193118817101000313783875288658753320838142061717766914730" &
"359825349042875546873115956286388235378759375195778185778053217122680661300" &
"19278766111959092164201989"
)

puts(1, ToString(mypi) & "\n")
? length(mypi[1])


-- Copyright (c) 2016-2021 James Cook

include my.e

public function TestMultiplicativeInverse(Eun val)
-- This function can be adapted to other functions that use "moreAccuracy" type variables.
	sequence range, n1, n2
	n1 = EunMultiplicativeInverse(val)
	val[3] *= 2
	n2 = EunMultiplicativeInverse(val, n1[1])
	range = EunTest(n1, n2)
	if range[1] < length(n1[1]) then
		adjustRound = n1[3] - range[1] -- sets adjustRound
		n1 = EunAdjustRound(n1)
	end if
	return {n1, n2}
end function


object A, st

adjustRound = 0
multInvMoreAccuracy = 0 -- or it could be -1, to use calculationSpeed

while 1 do
	st = prompt_string("Enter a number for MultiplicativeInverse, or enter to exit: ")
	
	if length(st) = 0 then
		exit
	end if
	
	A = ToEun(st)
	
	? TestMultiplicativeInverse(A)
	
	? adjustRound
	
end while

puts(1, "done.\n")


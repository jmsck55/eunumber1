-- Copyright (c) 2016-2021 James Cook
-- Eunumber, advanced sequence based arithmetic with exponents

--FILES: (All as one file.)
--namespace myeunumber
--public include myeunumber.e
--public include numio.e
--public include nthroot.e
--public include mymath.e
--public include triangulation.e
--public include myeuroots.e
--public include mycomplex.e
--public include quadraticequation.e
--additions at end of file.

namespace myeunumber

-- with trace

public include minieun.e

integer Z = 4

public function GetAdjustPrecision()
	return Z
end function

public procedure SetAdjustPrecision(PositiveInteger i)
	Z = i
end procedure

--nthroot.e

-- NthRoot algorithm

-- Find the nth root of any number

public Bool realMode = TRUE

public procedure SetRealMode(Bool i)
	realMode = i
end procedure
public function GetRealMode()
	return realMode
end function

public function IntPowerExp(PositiveInteger toPower, sequence n1, integer exp1, 
			      TargetLength targetLength, AtomRadix radix)
-- b^x = e^(x * ln(b))
	sequence p
	if toPower = 0 then
		return {{1}, 0, targetLength, radix, 0}
	end if
	p = {n1, exp1}
	for i = 2 to toPower do
		p = MultiplyExp(p[1], p[2], n1, exp1, targetLength, radix)
		sleep(nanosleep)
	end for
	return p
end function

-- function NthRoot(object x, object guess, object n)
--      object quotient, average
--      quotient = x / power(guess, n-1)
--      average = (quotient + ((n-1) * guess)) / n
--      return average
-- end function

public function NthRootProtoExp(PositiveScalar n, sequence x1, integer x1Exp,
				   sequence guess, integer guessExp, 
				   TargetLength targetLength, AtomRadix radix)
	sequence p, quot, average
	p = IntPowerExp(n - 1, guess, guessExp, targetLength, radix)
	quot = DivideExp(x1, x1Exp, p[1], p[2], targetLength, radix)
	p = MultiplyExp({n - 1}, 0, guess, guessExp, targetLength, radix)
	p = AddExp(p[1], p[2], quot[1], quot[2], targetLength, radix)
	average = DivideExp(p[1], p[2], {n}, 0, targetLength, radix)
	return average
end function

public PositiveOption nthRootMoreAccuracy = -1 -- if -1, then use calculationSpeed

public procedure SetNthRootMoreAccuracy(PositiveOption i)
	nthRootMoreAccuracy = i
end procedure
public function GetNthRootMoreAccuracy()
	return nthRootMoreAccuracy
end function

public integer nthRootIter = 1000000000
public integer lastNthRootIter = 0

public sequence nthRootHowComplete = {-1, 0}

public function NthRootExp(PositiveScalar n, sequence x1, integer x1Exp, sequence guess, 
			integer guessExp, TargetLength targetLength, AtomRadix radix)
	sequence tmp, lookat, ret
	integer protoTargetLength, moreAccuracy
	nthRootHowComplete = {-1, 0}
	if length(x1) = 0 then
		nthRootHowComplete = {0, 0}
		lastNthRootIter = 1
		return {x1, x1Exp, targetLength, radix, 0}
	end if
	if length(x1) = 1 then
		if x1[1] = 1 or x1[1] = -1 then
			nthRootHowComplete = {1, 1}
			lastNthRootIter = 1
			return {x1, x1Exp, targetLength, radix, 0}
		end if
	end if
	if nthRootMoreAccuracy >= 0 then
		moreAccuracy = nthRootMoreAccuracy
	elsif calculationSpeed then
		moreAccuracy = Ceil(targetLength / calculationSpeed)
	else
		moreAccuracy = 0 -- changed to 0
	end if
	-- targetLength += Z
	protoTargetLength = targetLength + moreAccuracy
	ret = AdjustRound(guess, guessExp, targetLength, radix, FALSE)
	lastNthRootIter = nthRootIter
	for i = 1 to nthRootIter do
		tmp = NthRootProtoExp(n, x1, x1Exp, guess, guessExp, protoTargetLength, radix)
		guess = tmp[1]
		guessExp = tmp[2]
		lookat = ret
		ret = AdjustRound(guess, guessExp, targetLength, radix, NO_SUBTRACT_ADJUST)
		if ret[2] = lookat[2] then
			nthRootHowComplete = Equaln(ret[1], lookat[1])
			if nthRootHowComplete[1] = nthRootHowComplete[2] then
			-- if equal(ret[1], lookat[1]) then
				lastNthRootIter = i
				exit
			end if
		end if
		sleep(nanosleep)
	end for
	if lastNthRootIter = nthRootIter then
		call_proc(divideCallBackId, {3})
		return {}
	end if
	-- ret = AdjustRound(ret[1], ret[2], targetLength - Z, radix, NO_SUBTRACT_ADJUST)
	return ret
end function

public function EunNthRoot(PositiveScalar n, Eun n1, object guess = 0)
	integer isImag, exp1, f
	sequence ret
	atom a
	exp1 = 0
	if atom(guess) then
		-- Latest code:
		exp1 = n1[2]
		f = remainder(exp1, n)
		if f then
			exp1 -= f
			if exp1 <= 0 then
				exp1 += n
			end if
		end if
		n1[2] -= exp1
		guess = ToAtom(n1)
		a = guess
		f = 0
		if a < 0 then
			-- factor out sqrt(-1), an imaginary number, on even roots
			a = -a -- atom
			f = IsIntegerOdd(n)
		end if
		a = power(a, 1 / n)
		if f then
			a = -a -- atom
		end if
		guess = ToEun(a, n1[4], n1[3])
	end if
	if n1[4] != guess[4] then
		guess = EunConvert(guess, n1[4], n1[3])
	end if
	if IsIntegerEven(n) then
		if length(n1[1]) and n1[1][1] < 0 then
			if realMode then
				call_proc(divideCallBackId, {4})
			end if
			-- factor out sqrt(-1)
			isImag = 1
			n1[1] = Negate(n1[1])
		else
			isImag = 0
		end if
		if IsNegative(guess[1]) then
			guess[1] = Negate(guess[1])
		end if
	end if
	ret = NthRootExp(n, n1[1], n1[2], guess[1], guess[2], n1[3], n1[4])
	exp1 = floor(exp1 / n)
	ret[2] += exp1
	if IsIntegerOdd(n) then
		return ret
	else
		return {isImag, ret, EunNegate(ret)}
	end if
end function

public function EunSquareRoot(Eun n1, object guess = {})
-- Set "realMode" variable to TRUE (or 1), if you want it to crash if supplied a negative number.
-- Use "isImag" to determine if the result is complex, 
-- which will happen if a negative number is passed to this function.
	return EunNthRoot(2, n1, guess)
end function

public function EunCubeRoot(Eun n1, object guess = {})
	return EunNthRoot(3, n1, guess)
end function

public function EunSqrt(Eun n1)
	object tmp
	sequence guess, ret
	integer exp
	atom a
	exp = n1[2]
	-- factor out a perfect square, of a power of radix, an even number
	if IsIntegerOdd(exp) then
		if exp > 0 then
			exp -= 1
		else
			exp += 1
		end if
	end if
	n1[2] -= exp
	tmp = ToAtom(n1)
	a = tmp
	if a < 0 then
		-- factor out sqrt(-1), an imaginary number
		a = -a -- atom
	end if
	a = sqrt(a)
	tmp = ToEun(a, n1[4], n1[3])
	guess = tmp
	ret = EunSquareRoot(n1, guess)
	exp = floor(exp / 2)
	ret[2][2] += exp
	ret[3][2] += exp
	-- returns isImag for if imaginary, and two answers: {one positive, one negative}
	return ret
end function

--mymath.e

-- MyMath: My Additions to myEunumber.

-- Experimental

-- this number should be calculated by the program:

-- Use GetPI() and GetE() instead.

-- public Eun EunPI = {"31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679" - '0', 0, 100, 10, 0}
-- public Eun EunE  = {"2718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427" - '0', 0, 100, 10, 0}

-- Begin ArcTan():

public PositiveOption arcTanMoreAccuracy = -1 -- if -1, then use calculationSpeed

public procedure SetArcTanMoreAccuracy(PositiveOption i)
	arcTanMoreAccuracy = i
end procedure
public function GetArcTanMoreAccuracy()
	return arcTanMoreAccuracy
end function

public integer arcTanIter = 1000000000
public integer arcTanCount = 0

public sequence arcTanHowComplete = {-1, 0}

public function ArcTanExp(sequence n1, integer exp1, TargetLength targetLength, AtomRadix radix)
	sequence sum, a, b, c, d, e, f, tmp, count, xSquared, xSquaredPlusOne, lookat, ret
	integer protoTargetLength, moreAccuracy
	arcTanHowComplete = {-1, 0}
	if arcTanMoreAccuracy >= 0 then
		moreAccuracy = arcTanMoreAccuracy
	elsif calculationSpeed then
		moreAccuracy = Ceil(targetLength / calculationSpeed)
	else
		moreAccuracy = 0 -- changed to 0
	end if
	-- targetLength += Z
	protoTargetLength = targetLength + moreAccuracy
	-- First iteration:
	-- x*x + 1
	e = MultiplyExp(n1, exp1, n1, exp1, protoTargetLength, radix)
	e = AddExp(e[1], e[2], {1}, 0, protoTargetLength, radix)
	-- x/e
	sum = DivideExp(n1, exp1, e[1], e[2], protoTargetLength, radix)
	a = {{1}, 0}
	b = {{1}, 0}
	c = {{1}, 0}
	count = {{1}, 0}
	d = {n1,exp1}
	xSquared = MultiplyExp(n1, exp1, n1, exp1, protoTargetLength, radix)
	xSquaredPlusOne = AddExp(xSquared[1], xSquared[2], {1}, 0, protoTargetLength, radix)
	e = xSquaredPlusOne
	-- Second iteration(s):
	ret = AdjustRound(sum[1], sum[2], targetLength, radix, FALSE)
	arcTanCount = arcTanIter
	for n = 1 to arcTanIter do
		a = MultiplyExp(a[1], a[2], {4}, 0, protoTargetLength, radix)
		tmp = AdjustRound({n}, 0, protoTargetLength, radix, FALSE)
		b = MultiplyExp(b[1], b[2], tmp[1], tmp[2], protoTargetLength, radix)
		tmp = MultiplyExp(b[1], b[2], b[1], b[2], protoTargetLength, radix)
		-- it needs these statements:
		count = AddExp(count[1], count[2], {1}, 0, protoTargetLength, radix)
		c = MultiplyExp(c[1], c[2], count[1], count[2], protoTargetLength, radix)
		count = AddExp(count[1], count[2], {1}, 0, protoTargetLength, radix)
		c = MultiplyExp(c[1], c[2], count[1], count[2], protoTargetLength, radix)
		d = MultiplyExp(d[1], d[2], xSquared[1], xSquared[2], protoTargetLength, radix)
		e = MultiplyExp(e[1], e[2], xSquaredPlusOne[1], xSquaredPlusOne[2], protoTargetLength, radix)
		f = MultiplyExp(a[1], a[2], tmp[1], tmp[2], protoTargetLength, radix)
		f = DivideExp(f[1], f[2], c[1], c[2], protoTargetLength, radix)
		f = MultiplyExp(f[1], f[2], d[1], d[2], protoTargetLength, radix)
		f = DivideExp(f[1], f[2], e[1], e[2], protoTargetLength, radix)
		sum = AddExp(sum[1], sum[2], f[1], f[2], protoTargetLength, radix)
		lookat = ret
		ret = AdjustRound(sum[1], sum[2], targetLength, radix, NO_SUBTRACT_ADJUST)
		if ret[2] = lookat[2] then
			arcTanHowComplete = Equaln(ret[1], lookat[1])
			if arcTanHowComplete[1] = arcTanHowComplete[2] then
			-- if equal(ret[1], lookat[1]) then
				arcTanCount = n
				exit
			end if
		end if
		sleep(nanosleep)
	end for
	if arcTanCount = arcTanIter then
		call_proc(divideCallBackId, {3})
		return {}
	end if
	-- ret = AdjustRound(ret[1], ret[2], targetLength - Z, radix, NO_SUBTRACT_ADJUST)
	return ret
end function

public function EunArcTan(Eun a)
	return ArcTanExp(a[1], a[2], a[3], a[4])
end function

-- End ArcTan().

sequence quarterPI
quarterPI = repeat(0, 4)
public function GetQuarterPI(TargetLength targetLength = defaultTargetLength, AtomRadix radix = defaultRadix, PositiveInteger multBy = 1)
	targetLength += Z
	if quarterPI[3] != targetLength or quarterPI[4] != radix then
		quarterPI = ArcTanExp({1}, 0, targetLength, radix)
	end if
	if multBy != 1 then
		return EunMultiply(quarterPI, NewEun({multBy}, 0, targetLength, radix))
	end if
	return quarterPI
end function
public function GetHalfPI(TargetLength targetLength = defaultTargetLength, integer radix = defaultRadix)
	return GetQuarterPI(targetLength, radix, 2)
end function
public function GetPI(TargetLength targetLength = defaultTargetLength, integer radix = defaultRadix)
	return GetQuarterPI(targetLength, radix, 4)
end function


public integer expExp1Iter = 1000
public sequence exp1HowComplete = {-1, 0}

public function ExpExp1(sequence n1, integer exp1, TargetLength targetLength, AtomRadix radix)
-- not quite accurate enough for large numbers.

-- it doesn't like large numbers.

-- does work for negative numbers.
-- using taylor series
--https://en.wikipedia.org/wiki/TaylorSeries
--My algorithm:
--      integer maxIter = 100
--      atom x, sum, tmp
--      x = 1
--      sum = 1
--      for i = maxIter to 1 by -1 do
--              tmp = (x/i)
--              sum *= tmp
--              sum += 1
--      end for
--      return sum
--end My algorithm.
	sequence sum, tmp, den
	exp1HowComplete = {-1, 0}
	sum = {{1}, 0}
	den = {{expExp1Iter + 1}, 0}
	for i = expExp1Iter to 1 by -1 do
		den = AddExp(den[1], den[2], {-1}, 0, targetLength, radix)
		tmp = DivideExp(n1, exp1, den[1], den[2], targetLength, radix)
		sum = MultiplyExp(sum[1], sum[2], tmp[1], tmp[2], targetLength, radix)
		sum = AddExp(sum[1], sum[2], {1}, 0, targetLength, radix)
		exp1HowComplete = {i, expExp1Iter}
		sleep(nanosleep)
	end for
	return sum
end function

public function EunExp1(Eun a)
	return ExpExp1(a[1], a[2], a[3], a[4])
end function

public function Exponentiation(atom u, integer m)
	atom q, prod, current
	q = m
	prod = 1
	current = u
	if q > 0 then
		while q > 0 do
			if remainder(q, 2) = 1 then
				prod *= current
				q -= 1
			end if
			current *= current
			q /= 2
			sleep(nanosleep)
		end while
	else
		while q < 0 do
			if remainder(q, 2) = -1 then
				prod /= current
				q += 1
			end if
			current *= current
			q /= 2
			sleep(nanosleep)
		end while
	end if
	return prod
end function

-- atom E = 2.7182818284590452353602874713527
-- ? Exponentiation(E, 20) -- answer is 485165195.4
-- ? Exponentiation(E, -21) -- answer is 7.582560428e-10

public function Remainder2Exp(sequence n1, integer exp1)
-- returns 1 or 0
	integer n
	n = exp1 + 1 -- reminder.
	if n < 1 then
		return 0
	end if
	if n > length(n1) then
		return 0
	end if
	return IsIntegerOdd(n1[n])
end function

public sequence expWholeHowComplete = {0, -1}

public function EunExpWhole(Eun u, Eun m)
-- exp function for whole numbers
	sequence q, prod, current
	integer targetLength, radix
	expWholeHowComplete = {0, -1}
	current = u
	targetLength = current[3]
	radix = current[4]
	if m[4] != radix then
		q = EunConvert(m, radix, targetLength)
	else
		q = m
		q[3] = targetLength
	end if
	prod = {{1}, 0, targetLength, radix, 0}
	if CompareExp(q[1], q[2], {}, 0) = 1 then
		while CompareExp(q[1], q[2], {}, 0) = 1 do
			expWholeHowComplete = {q[2], -1}
			if Remainder2Exp(q[1], q[2]) = 1 then
				prod = EunMultiply(prod, current)
				q = AddExp({-1}, 0, q[1], q[2], targetLength, radix)
			end if
			current = EunMultiply(current, current)
			q = DivideExp(q[1], q[2], {2}, 0, targetLength, radix)
			sleep(nanosleep)
		end while
	else
		while CompareExp(q[1], q[2], {}, 0) = -1 do
			expWholeHowComplete = {q[2], -1}
			if Remainder2Exp(q[1], q[2]) = -1 then
				prod = EunDivide(prod, current)
				q = AddExp({1}, 0, q[1], q[2], targetLength, radix)
			end if
			current = EunMultiply(current, current)
			q = DivideExp(q[1], q[2], {2}, 0, targetLength, radix)
			sleep(nanosleep)
		end while
	end if
	prod = AdjustRound(prod[1], prod[2], m[3], prod[4], NO_SUBTRACT_ADJUST)
	return prod
end function

public PositiveOption expMoreAccuracy = -1 -- if -1, then use calculationSpeed

public procedure SetExpMoreAccuracy(PositiveOption i)
	expMoreAccuracy = i
end procedure
public function GetExpMoreAccuracy()
	return expMoreAccuracy
end function

public integer expExpIter = 1000000000
public integer expExpCount = 0

public sequence expHowComplete = {-1, 0}

public function ExpExp(sequence n1, integer exp1, TargetLength targetLength, AtomRadix radix)
-- it doesn't like large numbers.
-- so, factor
-- 
--      e^(a+b) <=> e^(a) * e^(b)
--      e^(whole+frac) <=> EunExpWhole(E,whole) * EunExp(fract)
-- 
-- using taylor series
--https://en.wikipedia.org/wiki/TaylorSeries
--
-- -- exp(1) = sum of k=0 to inf (1/k!)
-- 
-- atom x
-- x = 1
-- 
-- atom sum, num, den
-- num = 1
-- den = 1
-- sum = 1
-- for i = 1 to 100 do
--      num *= x
--      den *= i
--      sum += ( num / den )
-- end for
-- 
-- ? sum
	sequence num, den, sum, tmp, lookat, ret
	integer protoTargetLength, moreAccuracy
	expHowComplete = {-1, 0}
	if expMoreAccuracy >= 0 then
		moreAccuracy = expMoreAccuracy
	elsif calculationSpeed then
		moreAccuracy = Ceil(targetLength / calculationSpeed)
	else
		moreAccuracy = 0 -- changed to 0
	end if
	-- targetLength += Z
	protoTargetLength = targetLength + moreAccuracy
	num = {{1}, 0}
	den = {{1}, 0}
	sum = {{1}, 0}
	ret = NewEun({1}, 0, targetLength, radix)
	expExpCount = expExpIter
	for i = 1 to expExpIter do
		num = MultiplyExp(num[1], num[2], n1, exp1, protoTargetLength, radix)
		den = MultiplyExp(den[1], den[2], {i}, 0, protoTargetLength, radix)
		tmp = DivideExp(num[1], num[2], den[1], den[2], protoTargetLength, radix)
		sum = AddExp(sum[1], sum[2], tmp[1], tmp[2], protoTargetLength, radix)
		lookat = ret
		ret = AdjustRound(sum[1], sum[2], targetLength, radix, NO_SUBTRACT_ADJUST)
		if ret[2] = lookat[2] then
			expHowComplete = Equaln(ret[1], lookat[1])
			if expHowComplete[1] = expHowComplete[2] then -- how equal are they? Use tasks to report on how close we are to the answer.
			-- if equal(ret[1], lookat[1]) then
				expExpCount = i
				exit
			end if
		end if
		sleep(nanosleep)
	end for
	if expExpCount = expExpIter then
		call_proc(divideCallBackId, {3})
		return {}
	end if
	-- ret = AdjustRound(ret[1], ret[2], targetLength - Z, radix, NO_SUBTRACT_ADJUST)
	return ret
end function

sequence eunE
eunE = repeat(0, 4)
public function GetE(TargetLength targetLength = defaultTargetLength, AtomRadix radix = defaultRadix)
	targetLength += Z
	if eunE[3] != targetLength or eunE[4] != radix then
		eunE = ExpExp({1}, 0, targetLength, radix)
	end if
	return eunE
end function


public function EunExp(Eun n1)
-- 
-- ExpExp() doesn't like large numbers.
-- so, factor
-- 
--      e^(a+b) <=> e^(a) * e^(b)
--      e^(whole+frac) <=> EunExpWhole(E,whole) * EunExp(fract)
-- 
	-- get the whole and fractional parts of the number:
	sequence num, frac
	integer exp1, size, isNeg
	num = n1
	exp1 = num[2]
	size = exp1 + 1
	-- factor out (-1), use MultiplicativeInverse (1/x) later
	isNeg = IsNegative(num[1])
	if isNeg then
		num[1] = Negate(num[1])
	end if
	frac = num
	if size > 0 then
		if size < length(num[1]) then
			num[1] = num[1][1..size]
			frac[1] = frac[1][size + 1..$]
			frac[2] = -1
		else
			frac = {}
		end if
		num = EunExpWhole(GetE(num[3], num[4]), num)
	else
		num = {}
	end if
	if length(frac) then
		frac = ExpExp(frac[1], frac[2], frac[3], frac[4])
		if length(num) then
			num = EunMultiply(num, frac)
		else
			num = frac
		end if
	end if
	num = AdjustRound(num[1], num[2], num[3], num[4], NO_SUBTRACT_ADJUST)
	if isNeg then
		num = EunMultiplicativeInverse(num)
	end if
	return num
end function

public integer ExpExpFastIter = 1 -- try to keep this number small.

public function GetExpFastIter()
	return ExpExpFastIter
end function

public procedure SetExpFastIter(integer i)
	ExpExpFastIter = i
end procedure

public sequence expFastHowComplete = {-1, 0}

public function ExpExpFast(sequence x1, integer exp1, sequence y2, integer exp2, TargetLength targetLength, AtomRadix radix)
-- e^(x/y) = 1 + 2x/(2y-x+x^2/(6y+x^2/(10y+x^2/(14y+x^2/(18y+x^2/(22y+...
	-- precalculate:
	-- 1, 2x, x, x^2, 4y, (2 + 4i)y
	-- i = targetLength to 0.
	-- Subtract 4y
	sequence xSquared, fourY, targetLengthY, tmp
	expFastHowComplete = {-1, 0}
	xSquared = MultiplyExp(x1, exp1, x1, exp1, targetLength, radix)
	fourY = MultiplyExp({-4}, 0, y2, exp2, targetLength, radix)
	targetLengthY = MultiplyExp({2 + 4 * ExpExpFastIter}, 0, y2, exp2, targetLength, radix)
	tmp = targetLengthY
	for i = ExpExpFastIter to 2 by -1 do
		tmp = DivideExp(xSquared[1], xSquared[2], tmp[1], tmp[2], targetLength, radix)
		targetLengthY = AddExp(targetLengthY[1], targetLengthY[2], fourY[1], fourY[2], targetLength, radix)
		tmp = AddExp(tmp[1], tmp[2], targetLengthY[1], targetLengthY[2], targetLength, radix)
		expFastHowComplete = {i, 0}
		sleep(nanosleep)
	end for
	tmp = DivideExp(xSquared[1], xSquared[2], tmp[1], tmp[2], targetLength, radix)
	tmp = AddExp(tmp[1], tmp[2], -x1, exp1, targetLength, radix)
	tmp = AddExp(tmp[1], tmp[2], y2 * 2, exp2, targetLength, radix)
	tmp = DivideExp(x1 * 2, exp1, tmp[1], tmp[2], targetLength, radix)
	tmp = AddExp({1}, 0, tmp[1], tmp[2], targetLength, radix)
	expFastHowComplete = {1, 0}
	return tmp
end function

public function EunExpFast(Eun numerator, Eun denominator)
-- e^(x/y) = 1 + 2x/(2y-x+x^2/(6y+x^2/(10y+x^2/(14y+x^2/(18y+x^2/(22y+...
	TargetLength targetLength
	if numerator[4] != denominator[4] then
		call_proc(divideCallBackId, {5})
		return {}
	end if
	if numerator[3] > denominator[3] then
		targetLength = numerator[3]
	else
		targetLength = denominator[3]
	end if
	object tmp0, tmp1
	tmp1 = ExpExpFast(numerator[1], numerator[2], denominator[1], denominator[2], targetLength, numerator[4])
	while 1 do
		tmp0 = tmp1
		ExpExpFastIter *= 2
		tmp1 = ExpExpFast(numerator[1], numerator[2], denominator[1], denominator[2], targetLength, numerator[4])
		if tmp1[2] = tmp0[2] then
			expFastHowComplete = Equaln(tmp1[1], tmp0[1])
			if expFastHowComplete[1] = expFastHowComplete[2] then
				exit
			end if
		end if
		sleep(nanosleep)
	end while
	ExpExpFastIter /= 2
	return tmp1
end function

-- Logarithms:

public PositiveOption logMoreAccuracy = -1 -- if -1, then use calculationSpeed

public procedure SetLogMoreAccuracy(PositiveOption i)
	logMoreAccuracy = i
end procedure
public function GetLogMoreAccuracy()
	return logMoreAccuracy
end function

public integer logIter = 1000000000 -- 50
public integer logIterCount = 0

public sequence logHowComplete = {-1, 0}

public function LogExp(sequence n1, integer exp1, sequence guess, integer exp0, TargetLength targetLength, AtomRadix radix)
	-- ln(x) = y[n] = y[n - 1] + 2 * (x - exp(y[n - 1]))/(x + exp(y[n - 1]))
	sequence expY, xMinus, xPlus, tmp, lookat, ret, one
	integer protoTargetLength, moreAccuracy
	logHowComplete = {-1, 0}
	one = NewEun({1}, 0, protoTargetLength, radix)
	if logMoreAccuracy >= 0 then
		moreAccuracy = logMoreAccuracy
	elsif calculationSpeed then
		moreAccuracy = Ceil(targetLength / calculationSpeed)
	else
		moreAccuracy = 0 -- changed to 0
	end if
	-- targetLength += Z
	protoTargetLength = targetLength + moreAccuracy
	guess = NewEun(guess, exp0, protoTargetLength, radix)
	ret = guess
	logIterCount = logIter
	for i = 1 to logIter do
		-- guess = guess + 2 * (num1 - exp(guess))/(num1 + exp(guess))
		expY = EunExpFast(guess, one)
		--expY = EunExp({guess[1], guess[2], protoTargetLength, radix, 0})
		xPlus = AddExp(n1, exp1, expY[1], expY[2], protoTargetLength, radix)
		xMinus = AddExp(n1, exp1, Negate(expY[1]), expY[2], protoTargetLength, radix)
		tmp = DivideExp(xMinus[1], xMinus[2], xPlus[1], xPlus[2], protoTargetLength, radix)
		tmp = MultiplyExp({2}, 0, tmp[1], tmp[2], protoTargetLength, radix)
		guess = AddExp(guess[1], guess[2], tmp[1], tmp[2], protoTargetLength, radix)
		lookat = ret
		ret = AdjustRound(guess[1], guess[2], targetLength, radix, NO_SUBTRACT_ADJUST)
		if ret[2] = lookat[2] then
			logHowComplete = Equaln(ret[1], lookat[1])
			if logHowComplete[1] = logHowComplete[2] then
			-- if equal(ret[1], lookat[1]) then
				logIterCount = i
				exit
			end if
		end if
		sleep(nanosleep)
	end for
	if logIterCount = logIter then
		call_proc(divideCallBackId, {3})
		return {}
	end if
	-- ret = AdjustRound(ret[1], ret[2], targetLength - Z, radix, NO_SUBTRACT_ADJUST)
	return ret
end function

public function EunLog(Eun n1, object guess = 0)
	sequence ret
	integer isImag
	atom a
	if atom(guess) then
		guess = ToAtom(n1)
		a = guess
		isImag = (a < 0)
		if isImag then
			-- result would be an imaginary number (imag == i)
			-- ln(-1) = PI * i
			-- ln(-a) = ln(a) + (PI * i)
			a = -a -- atom
			n1[1] = Negate(n1[1])
		end if
		a = log(a) -- it makes a guess
		guess = ToEun(a)
	end if
	if n1[4] != guess[4] then
		guess = EunConvert(guess, n1[4], n1[3])
	end if
	ret = LogExp(n1[1], n1[2], guess[1], guess[2], n1[3], n1[4])
	if isImag then
		return { ret, GetPI(ret[3] - Z, ret[4]) }
	else
		return ret
	end if
end function

-- Powers: a number (base), raised to the power of another number (raisedTo)

public function EunPower(Eun base, Eun raisedTo)
	return EunExp(EunMultiply(EunLog(base), raisedTo))
end function

--BEGIN TRIG FUNCTIONS:

--function RadiansToDegrees(r)
--      return r * 90 / halfPI
--end function
--function DegreesToRadians(d)
--      return d * halfPI / 90
--end function

public function EunRadiansToDegrees(Eun r)
	sequence ninety
	Eun d = r
	d[3] += Z
	ninety = NewEun({9}, 1, d[3], 10)
	if d[4] != 10 then
		ninety = EunConvert(ninety, d[4], d[3])
	end if
	d = EunMultiply(EunDivide(d, GetHalfPI(r[3], d[4])), ninety)
	return AdjustRound(d[1], d[2], r[3], d[4], NO_SUBTRACT_ADJUST)
end function
public function EunDegreesToRadians(Eun d)
	sequence ninety
	Eun r = d
	r[3] += Z
	ninety = NewEun({9}, 1, r[3], 10)
	if r[4] != 10 then
		ninety = EunConvert(ninety, r[4], r[3])
	end if
	r = EunMultiply(EunDivide(r, ninety), GetHalfPI(d[3], r[4]))
	return AdjustRound(r[1], r[2], d[3], r[4], NO_SUBTRACT_ADJUST)
end function

-- Using Newton's method:
-- 
-- "sin"
-- sine(x) = x - ((x^3)/(3!)) + ((x^5)/(5!)) - ((x^7)/(7!)) + ((x^9)/(9!)) - ...
-- 
-- cos(x)  = 1 - ((x^2)/(2!)) + ((x^4)/(4!)) - ((x^6)/(6!)) + ((x^8)/(8!)) - ...
-- 
-- 
-- 
-- tan(x) = sine(x) / cos(x)
-- 
-- csc(x) = 1 / sine(x)
-- sec(x) = 1 / cos(x)
-- cot(x) = cos(x) / sine(x)
-- 
-- "atan"
-- arctan(x) = x - ((x^3)/3) + ((x^5)/5) - ((x^7)/7) + ..., where abs(x) < 1
-- 
-- 
-- 
-- use: cos() for calculating
-- 
-- 
-- what about tan()?
-- 
-- tan(x) = sine(x) / cos(x)
-- 
-- End comments.


-- !!! Remember to use Radians (Rad) on these functions !!!

public PositiveOption sinMoreAccuracy = -1 -- if -1, then use calculationSpeed

public procedure SetSinMoreAccuracy(PositiveOption i)
	sinMoreAccuracy = i
end procedure
public function GetSinMoreAccuracy()
	return sinMoreAccuracy
end function

public integer sinIter = 1000000000 -- 500
public integer sinIterCount = 0

public sequence trigHowComplete = {-1, 0} -- for sin() and cos()

public function SinExp(sequence n1, integer exp1, TargetLength targetLength, AtomRadix radix)
-- sine(x) = x - ((x^3)/(3!)) + ((x^5)/(5!)) - ((x^7)/(7!)) + ((x^9)/(9!)) - ...
	-- Cases: 0 equals zero (0)
	-- Range: -PI/2 to PI/2, inclusive
	sequence ans, a, b, tmp, xSquared, lookat, ret
	integer step, protoTargetLength, moreAccuracy
	if length(n1) = 0 then
		trigHowComplete = {0, 0}
		return NewEun({}, exp1, targetLength, radix)
	end if
	trigHowComplete = {-1, 0}
	if sinMoreAccuracy >= 0 then
		moreAccuracy = sinMoreAccuracy
	elsif calculationSpeed then
		moreAccuracy = Ceil(targetLength / calculationSpeed)
	else
		moreAccuracy = 0 -- changed to 0
	end if
	-- targetLength += Z
	protoTargetLength = targetLength + moreAccuracy
	step = 1 -- SinExp() uses 1
	xSquared = MultiplyExp(n1, exp1, n1, exp1, protoTargetLength, radix)
	a = {n1, exp1} -- a is the numerator, SinExp() starts with x.
	b = {{1}, 0} -- b is the denominator.
	-- copy x to ans:
	ans = a -- in SinExp(), ans starts with x.
	ret = AdjustRound(ans[1], ans[2], targetLength, radix, FALSE)
	sinIterCount = sinIter
	for i = 1 to sinIter do -- start at 1 for all computer languages.
		-- first step is 3, for SinExp()
		step += 2
		tmp = MultiplyExp({step - 1}, 0, {step}, 0, protoTargetLength, radix)
		b = MultiplyExp(b[1], b[2], tmp[1], tmp[2], protoTargetLength, radix)
		a = MultiplyExp(a[1], a[2], xSquared[1], xSquared[2], protoTargetLength, radix)
		tmp = DivideExp(a[1], a[2], b[1], b[2], protoTargetLength, radix)
		if IsPositiveOdd(i) then
			-- Subtract
			tmp[1] = Negate(tmp[1])
		end if
		ans = AddExp(ans[1], ans[2], tmp[1], tmp[2], protoTargetLength, radix)
		lookat = ret
		ret = AdjustRound(ans[1], ans[2], targetLength, radix, NO_SUBTRACT_ADJUST)
		if ret[2] = lookat[2] then
			trigHowComplete = Equaln(ret[1], lookat[1])
			if trigHowComplete[1] = trigHowComplete[2] then
			-- if equal(ret[1], lookat[1]) then
				sinIterCount = i
				exit
			end if
		end if
		sleep(nanosleep)
	end for
	if sinIterCount = sinIter then
		call_proc(divideCallBackId, {3})
		return {}
	end if
	-- ret = AdjustRound(ret[1], ret[2], targetLength - Z, radix, NO_SUBTRACT_ADJUST)
	return ans
end function

-- !!! Remember to use Radians (Rad) on these functions !!!

public PositiveOption cosMoreAccuracy = -1 -- if -1, then use calculationSpeed

public procedure SetCosMoreAccuracy(PositiveOption i)
	cosMoreAccuracy = i
end procedure
public function GetCosMoreAccuracy()
	return cosMoreAccuracy
end function

public integer cosIter = 1000000000 -- 500
public integer cosIterCount = 0

public function CosExp(sequence n1, integer exp1, TargetLength targetLength, AtomRadix radix)
-- cos(x) = 1 - ((x^2)/(2!)) + ((x^4)/(4!)) - ((x^6)/(6!)) + ((x^8)/(8!)) - ...
	-- Range: -PI/2 to PI/2, exclusive
	sequence ans, a, b, tmp, xSquared, lookat, ret
	integer step, protoTargetLength, moreAccuracy
	trigHowComplete = {-1, 0}
	if cosMoreAccuracy >= 0 then
		moreAccuracy = cosMoreAccuracy
	elsif calculationSpeed then
		moreAccuracy = Ceil(targetLength / calculationSpeed)
	else
		moreAccuracy = 0 -- changed to 0
	end if
	-- targetLength += Z
	protoTargetLength = targetLength + moreAccuracy
	step = 0 -- CosExp() uses 0
	xSquared = MultiplyExp(n1, exp1, n1, exp1, protoTargetLength, radix)
	a = {{1}, 0} -- a is the numerator, CosExp() starts with 1.
	b = {{1}, 0} -- b is the denominator.
	-- copy "1" to ans:
	ans = a -- in CosExp(), ans starts with 1.
	ret = AdjustRound(ans[1], ans[2], targetLength, radix, FALSE)
	cosIterCount = cosIter
	for i = 1 to cosIter do -- start at 1 for all computer languages.
		-- first step is 2, for CosExp()
		step += 2
		tmp = MultiplyExp({step - 1}, 0, {step}, 0, protoTargetLength, radix)
		b = MultiplyExp(b[1], b[2], tmp[1], tmp[2], protoTargetLength, radix)
		a = MultiplyExp(a[1], a[2], xSquared[1], xSquared[2], protoTargetLength, radix)
		tmp = DivideExp(a[1], a[2], b[1], b[2], protoTargetLength, radix)
		if IsPositiveOdd(i) then
			-- Subtract
			tmp[1] = Negate(tmp[1])
		end if
		ans = AddExp(ans[1], ans[2], tmp[1], tmp[2], protoTargetLength, radix)
		lookat = ret
		ret = AdjustRound(ans[1], ans[2], targetLength, radix, NO_SUBTRACT_ADJUST)
		if ret[2] = lookat[2] then
			trigHowComplete = Equaln(ret[1], lookat[1])
			if trigHowComplete[1] = trigHowComplete[2] then
			-- if equal(ret[1], lookat[1]) then
				cosIterCount = i
				exit
			end if
		end if
		sleep(nanosleep)
	end for
	if cosIterCount = cosIter then
		call_proc(divideCallBackId, {3})
		return {}
	end if
	-- ret = AdjustRound(ret[1], ret[2], targetLength - Z, radix, NO_SUBTRACT_ADJUST)
	return ans
end function

-- EunSin:

public function EunSin(Eun x)
-- To find sin(x):
-- y = x mod PI
-- if y >= -PI/2 and y <= PI/2 then
--  r = sin(y)
-- else
--  if y < 0 then
--   r = -cos(y + PI/2)
--  else
--   r = cos(y - PI/2)
--  end if
-- end if
-- return r
	sequence y, half_pi, one_pi
	-- targetLength = x[3] -- + Z
	half_pi = GetHalfPI(x[3], x[4])
	one_pi = GetPI(x[3], x[4])
	y = EunfMod(x, one_pi)
	if EunCompare(y, half_pi) <= 0 then
		if EunCompare(y, EunNegate(half_pi)) >= 0 then
			y = SinExp(y[1], y[2], y[3], y[4])
		end if
	else
		if IsNegative(y[1]) then
			y = EunAdd(y, half_pi)
			y = CosExp(y[1], y[2], y[3], y[4])
			y = EunNegate(y)
		else
			y = EunSubtract(y, half_pi)
			y = CosExp(y[1], y[2], y[3], y[4])
		end if
	end if
	-- y = AdjustRound(y[1], y[2], targetLength - Z, radix, NO_SUBTRACT_ADJUST)
	return y
end function

-- EunCos:

public function EunCos(Eun x)
-- To find cos(x):
-- y = abs(x) mod (2*PI)
-- if y < PI then
--  r = cos(y)
-- else
--  r = sin(y - (3/2)*PI)
-- end if
-- return r
	sequence y, half_pi, var_pi
	-- targetLength = x[3] -- + Z
	half_pi = GetHalfPI(x[3], x[4])
	var_pi = EunMultiply(half_pi, NewEun({4}, 0, x[3], x[4]))
	y = x
	y[1] = AbsoluteValue(y[1])
	y = EunfMod(y, var_pi) -- y = abs(x) mod 2*pi
	var_pi = EunMultiply(half_pi, NewEun({2}, 0, x[3], y[4]))
	if EunCompare(y, var_pi) < 0 then -- if (y < pi) then
		y = CosExp(y[1], y[2], y[3], y[4])
	else
		-- return sin(y - (3/2)*pi)
		var_pi = EunMultiply(half_pi, NewEun({3}, 0, y[3], y[4]))
		y = EunSubtract(y, var_pi)
		y = SinExp(y[1], y[2], y[3], y[4])
	end if
	-- y = AdjustRound(y[1], y[2], targetLength - Z, radix, NO_SUBTRACT_ADJUST)
	return y
end function

-- Tangent

public function EunTan(Eun a)
	return EunDivide(EunSin(a), EunCos(a))
end function

-- !!! Remember to use Radians (Rad) on these functions !!!

--https://en.wikipedia.org/wiki/Inverse_trigonometric_functions
--INCOMPLETE: arcSin and arcTan are experimental for now.

--                  1*z^3     1*3*z^5     1*3*5*z^7
-- arcsin(z) = z + (-----) + (-------) + (---------) + ...
--                   2* 3      2*4* 5      2*4*6* 7

-- Pattern: (1,2,3) ; (1,2,3,4,5) ; (1,2,3,4,5,6,7) ; (1,2,3,4,5,6,7,8,9)
-- odds on top, evens on bottom, except for the latest odd value.

--Note: Too slow?

public PositiveOption arcSinMoreAccuracy = -1 -- if -1, then use calculationSpeed

public procedure SetArcSinMoreAccuracy(PositiveOption i)
	arcSinMoreAccuracy = i
end procedure
public function GetArcSinMoreAccuracy()
	return arcSinMoreAccuracy
end function

public integer arcSinIter = 1000000000
public integer arcSinIterCount = 0

public sequence arcSinHowComplete = {-1, 0}

public function ArcSinExp(sequence n1, integer exp1, TargetLength targetLength, AtomRadix radix)
--something wrong with arcsin()?
-- arcsin(z) = z + (1/2)(z^3/3) + (1*3/(2*4))(z^5/5) + (1*3*5/(2*4*6))(z^7/7) + ...
	sequence sum, xSquared, top, bottom, odd, even, x, tmp, lookat, ret
	integer protoTargetLength, moreAccuracy
	arcSinHowComplete = {-1, 0}
	if arcSinMoreAccuracy >= 0 then
		moreAccuracy = arcSinMoreAccuracy
	elsif calculationSpeed then
		moreAccuracy = Ceil(targetLength / calculationSpeed)
	else
		moreAccuracy = 0 -- changed to 0
	end if
	-- targetLength += Z
	protoTargetLength = targetLength + moreAccuracy
	sum = {n1, exp1}
	x = {n1, exp1}
	xSquared = MultiplyExp(n1, exp1, n1, exp1, targetLength, radix)
	bottom = {{2}, 0}
	odd = {{3}, 0}
	-- First iteration:
	tmp = MultiplyExp(bottom[1], bottom[2], odd[1], odd[2], targetLength, radix)
	x = MultiplyExp(x[1], x[2], xSquared[1], xSquared[2], targetLength, radix)
	tmp = DivideExp(x[1], x[2], tmp[1], tmp[2], targetLength, radix)
	sum = AddExp(sum[1], sum[2], tmp[1], tmp[2], targetLength, radix)
	-- Second iteration(s):
	top = {{1}, 0}
	even = {{2}, 0}
	ret = sum
	arcSinIterCount = arcSinIter
	for n = 1 to arcSinIter do
		even = AddExp(even[1], even[2], {2}, 0, protoTargetLength, radix)
		bottom = MultiplyExp(bottom[1], bottom[2], even[1], even[2], protoTargetLength, radix)
		top  = MultiplyExp(top[1], top[2], odd[1], odd[2], protoTargetLength, radix)
		odd = AddExp(odd[1], odd[2], {2}, 0, protoTargetLength, radix)
		tmp = MultiplyExp(bottom[1], bottom[2], odd[1], odd[2], protoTargetLength, radix)
		x = MultiplyExp(x[1], x[2], xSquared[1], xSquared[2], protoTargetLength, radix)
		tmp = DivideExp(x[1], x[2], tmp[1], tmp[2], protoTargetLength, radix)
		tmp = MultiplyExp(tmp[1], tmp[2], top[1], top[2], protoTargetLength, radix)
		sum = AddExp(sum[1], sum[2], tmp[1], tmp[2], protoTargetLength, radix)
		lookat = ret
		ret = AdjustRound(sum[1], sum[2], targetLength, radix, NO_SUBTRACT_ADJUST)
		if ret[2] = lookat[2] then
			arcSinHowComplete = Equaln(ret[1], lookat[1])
			if arcSinHowComplete[1] = arcSinHowComplete[2] then
			-- if equal(ret[1], lookat[1]) then
				arcSinIterCount = n
				exit
			end if
		end if
		sleep(nanosleep)
	end for
	if arcSinIterCount = arcSinIter then
		call_proc(divideCallBackId, {3})
		return {}
	end if
	-- ret = AdjustRound(ret[1], ret[2], targetLength - Z, radix, NO_SUBTRACT_ADJUST)
	return ret
end function

public function EunArcSin(Eun a)
-- arcsin(z) = z + (1/2)(z^3/3) + (1*3/(2*4))(z^5/5) + (1*3*5/(2*4*6))(z^7/7) + ...
	return ArcSinExp(a[1], a[2], a[3], a[4])
end function

public function EunArcCos(Eun a)
-- arccos(x) = arcsin(1) - arcsin(x)
-- arccos(x) = (EunPi / 2) - arcsin(x)
	return EunSubtract(GetHalfPI(a[3] - Z, a[4]), EunArcSin(a))
end function

-- EunArcTan, function is coded above.

-- Method 1:
-- 
--             +inf      (-1)^n * z^(2*n+1)
-- arctan(z) = sumation( ------------------- )
--             n=0       (2*n+1)
-- 
-- for: [abs(z) <= 1, z != i, z != -i]

--HERE, code method 1, TODO!

-- 
-- 
-- ArctanExp funtion: (Method 2)
-- 
--             +inf      (2^(2*n)) * ((n!)^2) * (z^(2*n+1))
-- arctan(z) = sumation( ---------------------------------- )
--             n=0       ((2*n+1)!) * ((1+z^2)^(n+1))
-- 
-- function Myfactorial(integer f)
--      atom y
--      y = 1
--      --for i = 1 to f do
--      for i = 2 to f do
--              y *= i
--      end for
--      return y
-- end function
-- integer MyarctanIter = 80
-- function Myarctan(atom x)
--      atom sum, a, b, c, d, e, f
--      sum = x / (1 + x*x)
--      for n = 1 to MyarctanIter do
--              --a = power(2,2*n) -- [0]=1, [1]=4, [2]=16, [3]=64, [4]=256, [5]=1024, [6]=4096, [7]=16384, [8]=65536, [9]=262144, [10]=1048576,
--              a = power(4,n)
--              b = Myfactorial(n)
--              b *= b
--              c = Myfactorial(2*n + 1) -- [0]=1, [1]=6, [2]=120, [3]=5040, [4]=362880, [5]=39916800,
--              d = power(x, 2*n + 1) -- equals: x * power(x,2*n)
--              e = power(1 + x*x, n + 1) -- precalculate: (1 + x*x)
--              f = a * b
--              f = f / c
--              f = f * d
--              f = f / e
--              sum += f
--      end for
--      return sum
-- end function

-- Comments: Slow, and inaccurate.

--Next one to work on:

-- ArctanExp, using continued fractions:

-- Status: Not done yet.

-- This method for arctan function is Too slow:

-- public integer arctanIter = 1000000000 -- 500
-- public integer lastIterCountArctan = 0
-- 
-- public function ArctanExp(sequence n1, integer exp1, TargetLength targetLength, integer radix)
-- -- works best with small numbers.
-- -- arctan(x) = x - ((x^3)/3) + ((x^5)/5) - ((x^7)/7) + ..., where abs(x) < 1
-- -- sine(x) = x - ((x^3)/(3!)) + ((x^5)/(5!)) - ((x^7)/(7!)) + ((x^9)/(9!)) - ...
--      sequence ans, a, b, tmp, xSquared, lookat
--      --integer step
--      --step = 1 -- SinExp() uses 1
--      xSquared = MultiplyExp(n1, exp1, n1, exp1, targetLength, radix)
--      
--      a = {n1, exp1} -- a is the numerator, SinExp() starts with x.
--      b = {{1}, 0} -- b is the denominator.
--      
--      -- copy x to ans:
--      ans = a -- in SinExp(), ans starts with x.
--      for i = 1 to arctanIter do -- start at 1 for all computer languages.
--              lookat = ans
--              -- first step is 3, for SinExp()
--              --step += 2
--              --tmp = MultiplyExp({step-1}, 0, {step}, 0, targetLength, radix)
--              --b = MultiplyExp(b[1], b[2], tmp[1], tmp[2], targetLength, radix)
--              b = AddExp(b[1], b[2], {2}, 0, targetLength, radix)
--              --b = {{step}, 0} -- "b" is "step" in arctan()
--              
--              a = MultiplyExp(a[1], a[2], xSquared[1], xSquared[2], targetLength, radix)
--              tmp = DivideExp(a[1], a[2], b[1], b[2], targetLength, radix)
--              
--              if IsPositiveOdd(i) then
--                      -- Subtract
--                      tmp[1] = Negate(tmp[1])
--              end if
--              
--              ans = AddExp(ans[1], ans[2], tmp[1], tmp[2], targetLength, radix)
--              if length(ans[1]) > targetLength then
--                      ans[1] = ans[1][1..targetLength]
--              end if
--              if equal(ans, lookat) then
--                      lastIterCountArctan = i
--                      exit -- break
--              end if
--      end for
--      
--      return ans
-- end function


-- More Trig functions:

-- sine = opp/hyp
-- cos = adj/hyp
-- tan = opp/adj
-- 
-- csc = hyp/opp
-- sec = hyp/adj
-- cot = adj/opp

public function EunCsc(Eun a)
	return EunMultiplicativeInverse(EunSin(a))
end function

public function EunSec(Eun a)
	return EunMultiplicativeInverse(EunCos(a))
end function

public function EunCot(Eun a)
	return EunMultiplicativeInverse(EunTan(a))
end function


public function EunArcCsc(Eun a)
	return EunArcSin(EunMultiplicativeInverse(a))
end function

public function EunArcSec(Eun a)
	return EunArcCos(EunMultiplicativeInverse(a))
end function

public function EunArcCot(Eun a)
	integer f
	sequence tmp
	f = CompareExp(a[1], a[2], {}, 0)
	if f = 0 then
		return GetHalfPI(a[3] - Z, a[4])
	end if
	tmp = EunArcTan(EunMultiplicativeInverse(a))
	if f < 0 then
		tmp = EunAdd(tmp, GetPI(a[3] - Z, a[4]))
	end if
	return tmp
end function

-- Hyperbolic functions:

public function EunSinh(Eun a)
-- sinh(x) = (e^(x) - e^(-x)) / 2
	return EunDivide(EunSubtract(EunExp(a), EunExp(EunNegate(a))), NewEun({2}, 0, a[3], a[4]))
end function

public function EunCosh(Eun a)
-- cosh(x) = (e^(x) + e^(-x)) / 2
	return EunDivide(EunAdd(EunExp(a), EunExp(EunNegate(a))), NewEun({2}, 0, a[3], a[4]))
end function

public function EunTanh(Eun a)
-- tanh(x) = e^(2*x) => a; (a - 1) / (a + 1)
	sequence tmp, local
	local = NewEun({2}, 0, a[3], a[4])
	tmp = EunExp(EunMultiply(a, local))
	local[1] = {1}
	return EunDivide(EunSubtract(tmp, local), EunAdd(tmp, local))
end function

public function EunCoth(Eun a)
-- coth(x) = x != 0; 1 / tanh(x)
	if not CompareExp(a[1], a[2], {}, 0) then
		puts(1, "Error(7):  In MyEuNumber, trig functions: Invalid number passed to\n \"EunCoth()\", cannot be zero (0).\n  See file: ex.err\n")
		abort(1/0)
	end if
	return EunMultiplicativeInverse(EunTanh(a))
end function

public function EunSech(Eun a)
-- sech(x) = 1 / cosh(x)
	return EunMultiplicativeInverse(EunCosh(a))
end function

public function EunCsch(Eun a)
-- csch(x) = x != 0; 1 / sinh(x)
	if not CompareExp(a[1], a[2], {}, 0) then
		puts(1, "Error(7):  In MyEuNumber, trig functions: Invalid number passed to\n \"EunCsch()\", cannot be zero (0).\n  See file: ex.err\n")
		abort(1/0)
	end if
	return EunMultiplicativeInverse(EunSinh(a))
end function

-- See also:
-- https://en.wikipedia.org/wiki/Inverse_hyperbolic_functions

public function EunArcSinh(Eun a)
-- arcsinh(x) = ln(x + sqrt(x^2 + 1))
	sequence tmp
	tmp = EunSqrt(EunAdd(EunMultiply(a, NewEun({2}, 0, a[3], a[4])), NewEun({1}, 0, a[3], a[4])))
	if tmp[1] then
		puts(1, "Error(7):  In MyEuNumber, EunArcSinh(): error, encountered imaginary number,\n something went wrong internally.\n  See file: ex.err\n")
		abort(1/0)
	end if
	return EunLog(EunAdd(a, tmp[2]))
end function

public function EunArcCosh(Eun a)
-- arccosh(x) = x >= 1; ln(x + sqrt(x^2 - 1))
	sequence tmp
	if CompareExp(a[1], a[2], {1}, 0) = -1 then
		puts(1, "Error(7):  In MyEuNumber, trig functions: Invalid number passed to\n \"EunArcCosh()\", cannot be zero (0).\n  See file: ex.err\n")
		abort(1/0)
	end if
	tmp = EunSqrt(EunSubtract(EunMultiply(a, NewEun({2}, 0, a[3], a[4])), NewEun({1}, 0, a[3], a[4])))
	if tmp[1] then
		puts(1, "Error(7):  In MyEuNumber, EunArcCosh(): error, encountered imaginary number,\n something went wrong internally.\n  See file: ex.err\n")
		abort(1/0)
	end if
	return EunLog(EunAdd(a, tmp[2]))
end function

public function EunArcTanh(Eun a)
-- arctanh(x) = abs(x) < 1; ln((1 + x)/(1 - x)) / 2
	sequence tmp, local
	if CompareExp(AbsoluteValue(a[1]), a[2], {1}, 0) >= 0 then
		puts(1, "Error(7):  In MyEuNumber, EunArcTanh(): supplied number is out of domain/range\n  See file: ex.err\n")
		abort(1/0)
	end if
	local = NewEun({1}, 0, a[3], a[4])
	tmp = EunDivide(EunAdd(local, a), EunSubtract(local, a))
	local[1] = {2}
	return EunDivide(EunLog(tmp), local)
end function

public function EunArcCoth(Eun a)
-- arccoth(x) = abs(x) > 1; ln((x + 1)/(x - 1)) / 2
	sequence tmp, local
	if CompareExp(AbsoluteValue(a[1]), a[2], {1}, 0) <= 0 then
		puts(1, "Error(7):  In MyEuNumber, EunArcCoth(): supplied number is out of domain/range\n  See file: ex.err\n")
		abort(1/0)
	end if
	local = NewEun({1}, 0, a[3], a[4])
	tmp = EunDivide(EunAdd(a, local), EunSubtract(a, local))
	local[1] = {2}
	return EunDivide(EunLog(tmp), local)
end function

public function EunArcSech(Eun a)
-- arcsech(x) = 0 < x <= 1; 1 / x => a; ln(a + sqrt(a^2 - 1)) :: ln((1 + sqrt(1 - x^2)) / x)
	sequence tmp, s
	if CompareExp(a[1], a[2], {}, 0) <= 0 or CompareExp(a[1], a[2], {1}, 0) = 1 then
		puts(1, "Error(7):  In MyEuNumber, EunArcSech(): supplied number is out of domain/range\n  See file: ex.err\n")
		abort(1/0)
	end if
	tmp = EunMultiplicativeInverse(a)
	s = EunSqrt(EunSubtract(EunMultiply(tmp, tmp), NewEun({1}, 0, a[3], a[4])))
	return EunLog(EunAdd(tmp, s[2]))
end function

public function EunArcCsch(Eun a)
-- arccsch(x) = x != 0; 1 / x => a; ln(a + sqrt(a^2 + 1))
	sequence tmp, s
	if not length(a[1]) then
		puts(1, "Error(7):  In MyEuNumber, EunArcCsch(): supplied number is out of domain/range\n  See file: ex.err\n")
		abort(1/0)
	end if
	tmp = EunMultiplicativeInverse(a)
	s = EunSqrt(EunAdd(EunMultiply(tmp, tmp), NewEun({1}, 0, a[3], a[4])))
	return EunLog(EunAdd(tmp, s[2]))
end function


--END TRIG FUNCTIONS.

--triangulation.e

-- Triangulation using two (2) points

-- Given: angle A, angle B, distance D (distance between angles A and B)
-- Find distance E and F, (from angle A and B), to intersect point.
-- C is a temporary value.

-- Proof:
-- NOTE: uses all positive numbers
--
-- define:
-- observer at point A, angle A from distance D
-- observer at point B, angle B from distance D
-- distance D between point A and point B
-- distance E coming from angle A
-- distance F coming from angle B
-- height G at right angles (tangent) to distance D
-- X^2 <=> X*X
--
-- G <=> E * sin(A) <=> F * sin(B)
-- divide one by the other, equalling value of one (1)
-- ratio: F / E == sin(A) / sin(B)
-- F == E * sin(A) / sin(B)
-- Pythagorean Theorem:
-- D^2 = E^2 + F^2
-- E == sqrt(D^2 - F^2)
-- D^2 == E^2 + (E * sin(A) / sin(B))^2
-- D^2 == E^2 + E^2 * (sin(A) / sin(B))^2
-- D^2 == E^2 * (1 + (sin(A) / sin(B))^2)
-- E == sqrt(D^2 / (1 + (sin(A) / sin(B))^2))
-- ratio inverted for "F":
-- F == sqrt(D^2 / (1 + (sin(B) / sin(A))^2))
--
-- End of Proof

type WhichOnes(integer i)
	return i >= 1 and i <= 3
end type

public function EunTriangulation(Eun angleA, Eun angleB, Eun distance, WhichOnes whichOnes = 3)
	Eun dsquared, sa, sb
	sequence s, tmp
	integer mode
	if IsNegative(angleA[1]) or IsNegative(angleB[1]) or IsNegative(distance[1]) then
		call_proc(divideCallBackId, {6})
		return {}
	end if
	mode = realMode
	realMode = TRUE
	sa = EunSin(angleA)
	sb = EunSin(angleB)
	dsquared = EunSquare(distance)
	s = {0, 0}
	if and_bits(whichOnes, 1) then
		tmp = EunSqrt(
			EunDivide(
				dsquared,
				EunAdd({{1}, 0, angleA[3], angleA[4]}, EunSquare(EunDivide(sa, sb)))
			)
		)
		s[1] = tmp[2]
	end if
	if and_bits(whichOnes, 2) then
		tmp = EunSqrt(
			EunDivide(
				dsquared,
				EunAdd({{1}, 0, angleA[3], angleA[4]}, EunSquare(EunDivide(sb, sa)))
			)
		)
		s[2] = tmp[2]
	end if
	realMode = mode
	return s
end function


--myeuroots.e

-- "FindRoot" means: Find the roots (or zeros) of an equation.

public function MyCompareExp(sequence n1, integer exp1, sequence n2, integer exp2)
	return CompareExp(n1, exp1, n2, exp2)
end function

public sequence delta = {{1},-10}
--public sequence delta = {{1},-80}
--0.0000000001 -- must be a positive number

public procedure SetDelta(integer exp1)
	delta[2] = exp1
end procedure

public function GetDelta()
	return delta[2]
end function

public PositiveInteger eurootsAdjustRound = 4

public procedure SetEurootsAdjustRound(PositiveInteger i)
	eurootsAdjustRound = i
end procedure

public function GetEurootsAdjustRound()
	return eurootsAdjustRound
end function


-- findRoot private variables:
sequence a, b, c, d, fa, fb, fc, s, fs, tmp, tmp1, tmp2
integer mflag, lookatIter
integer comp1, comp2

function Condition1Through_5(PositiveScalar len, AtomRadix radix)
	sequence sb, bc, cd
	
	--tmp1 = ((3 * a) + b) / 4
	tmp1 = MultiplyExp({3}, 0, a[1], a[2], len, radix)
	tmp1 = AddExp(tmp1[1], tmp1[2], b[1], b[2], len, radix)
	tmp1 = DivideExp(tmp1[1], tmp1[2], {4}, 0, len, radix)
	
	comp1 = MyCompareExp(s[1], s[2], tmp1[1], tmp1[2])
	comp2 = MyCompareExp(s[1], s[2], b[1], b[2])
	
	-- condition 1:
	if (not (comp1 = -1 and comp2 = 1)) and (not (comp2 = -1 and comp1 = 1)) then
		return 1
	end if
	
	sb = AddExp(s[1], s[2], Negate(b[1]), b[2], len, radix)
	sb[1] = AbsoluteValue(sb[1])
	
	if mflag = 1 then
		bc = AddExp(b[1], b[2], Negate(c[1]), c[2], len, radix)
		bc[1] = AbsoluteValue(bc[1])
		-- condition 2:
		tmp = DivideExp(bc[1], bc[2], {2}, 0, len, radix)
		comp1 = MyCompareExp(sb[1], sb[2], tmp[1], tmp[2])
		if comp1 >= 0 then
			return 1
		end if
		-- condition 4:
		comp1 = MyCompareExp(bc[1], bc[2], delta[1], delta[2])
		if comp1 = -1 then
			return 1
		end if
	else
		cd = AddExp(c[1], c[2], Negate(d[1]), d[2], len, radix)
		cd[1] = AbsoluteValue(cd[1])
		-- condition 3:
		tmp = DivideExp(cd[1], cd[2], {2}, 0, len, radix)
		comp1 = MyCompareExp(sb[1], sb[2], tmp[1], tmp[2])
		if comp1 >= 0 then
			return 1
		end if
		-- condition 5:
		comp1 = MyCompareExp(cd[1], cd[2], delta[1], delta[2])
		if comp1 = -1 then
			return 1
		end if
	end if
	return 0
end function

public function FindRootExp(integer rid, sequence n1, integer exp1, 
		sequence n2, integer exp2, PositiveScalar len, AtomRadix radix, integer littleEndian = 0,
		object passToFunc1 = {})
	
	len += 3
	delta = {{1}, floor((exp1 + exp2) / 2) - (len) + 2}
	
	a = {n1, exp1}
	b = {n2, exp2}
	if littleEndian then
		fa = call_func(rid, {reverse(n1), exp1, len, radix, passToFunc1})
		fa[1] = reverse(fa[1])
		fb = call_func(rid, {reverse(n2), exp2, len, radix, passToFunc1})
		fb[1] = reverse(fb[1])
	else
		fa = call_func(rid, {n1, exp1, len, radix, passToFunc1})
		fb = call_func(rid, {n2, exp2, len, radix, passToFunc1})
	end if
	
	if fa[1][1] * fb[1][1] >= 0 then
		return 1 -- error
	end if
	
	comp1 = MyCompareExp(AbsoluteValue(fa[1]), fa[2], AbsoluteValue(fb[1]), fb[2])
	if comp1 = -1 then
		-- swap, and set c=a
		c = b
		b = a
		a = c
	else
		c = a
	end if
	fc = fa
	fs = {{1}, 0}
	mflag = 1
	lookatIter = 0
	
	while 1 do
		lookatIter += 1
		
		comp1 = MyCompareExp(fa[1], fa[2], fc[1], fc[2])
		comp2 = MyCompareExp(fb[1], fb[2], fc[1], fc[2])
		if comp1 != 0 and comp2 != 0 then
			-- calculate "s" (inverse quadratic interpolation)
			--s = (a*fb*fc) / ((fa-fb)*(fa-fc))
			tmp1 = AddExp(fa[1], fa[2], Negate(fb[1]), fb[2], len, radix)
			tmp2 = AddExp(fa[1], fa[2], Negate(fc[1]), fc[2], len, radix)
			tmp1 = MultiplyExp(tmp1[1], tmp1[2], tmp2[1], tmp2[2], len, radix)
			tmp2 = MultiplyExp(fb[1], fb[2], fc[1], fc[2], len, radix)
			tmp2 = MultiplyExp(tmp2[1], tmp2[2], a[1], a[2], len, radix)
			tmp1 = DivideExp(tmp2[1], tmp2[2], tmp1[1], tmp1[2], len, radix)
			s = tmp1
			
			--s += (b*fa*fc) / ((fb-fa)*(fb-fc))
			tmp1 = AddExp(fb[1], fb[2], Negate(fa[1]), fa[2], len, radix)
			tmp2 = AddExp(fb[1], fb[2], Negate(fc[1]), fc[2], len, radix)
			tmp1 = MultiplyExp(tmp1[1], tmp1[2], tmp2[1], tmp2[2], len, radix)
			tmp2 = MultiplyExp(fa[1], fa[2], fc[1], fc[2], len, radix)
			tmp2 = MultiplyExp(tmp2[1], tmp2[2], b[1], b[2], len, radix)
			tmp1 = DivideExp(tmp2[1], tmp2[2], tmp1[1], tmp1[2], len, radix)
			s = AddExp(s[1], s[2], tmp1[1], tmp1[2], len, radix)
			
			--s += (c*fa*fb) / ((fc-fa)*(fc-fb))
			tmp1 = AddExp(fc[1], fc[2], Negate(fa[1]), fa[2], len, radix)
			tmp2 = AddExp(fc[1], fc[2], Negate(fb[1]), fb[2], len, radix)
			tmp1 = MultiplyExp(tmp1[1], tmp1[2], tmp2[1], tmp2[2], len, radix)
			tmp2 = MultiplyExp(fa[1], fa[2], fb[1], fb[2], len, radix)
			tmp2 = MultiplyExp(tmp2[1], tmp2[2], c[1], c[2], len, radix)
			tmp1 = DivideExp(tmp2[1], tmp2[2], tmp1[1], tmp1[2], len, radix)
			s = AddExp(s[1], s[2], tmp1[1], tmp1[2], len, radix)
		else
			-- calculate "s" (secant rule)
			--s = b - (fb * (b-a)/(fb-fa))
			tmp1 = AddExp(b[1], b[2], Negate(a[1]), a[2], len, radix)
			tmp2 = AddExp(fb[1], fb[2], Negate(fa[1]), fa[2], len, radix)
			tmp1 = MultiplyExp(tmp1[1], tmp1[2], fb[1], fb[2], len, radix)
			tmp1 = DivideExp(tmp1[1], tmp1[2], tmp2[1], tmp2[2], len, radix)
			s = AddExp(b[1], b[2], Negate(tmp1[1]), tmp1[2], len, radix)
		end if
		
		if Condition1Through_5(len, radix) then
			s = AddExp(a[1], a[2], b[1], b[2], len, radix)
			s = DivideExp(s[1], s[2], {2}, 0, len, radix)
			mflag = 1
		else
			mflag = 0
		end if
		
		if littleEndian then
			fs = call_func(rid, {reverse(s[1]), s[2], len, radix, passToFunc1})
			fs[1] = reverse(fs[1])
		else
			fs = call_func(rid, {s[1], s[2], len, radix, passToFunc1})
		end if
		
		d = c -- (d is assigned for the first time here, it won't be used above on the first iteration because mflag is set)
		c = b
		if fa[1][1] * fs[1][1] < 0 then
			b = s
		else
			a = s
		end if
		
		comp1 = MyCompareExp(AbsoluteValue(fa[1]), fa[2], AbsoluteValue(fb[1]), fb[2])
		if comp1 = -1 then
			-- swap(a, b)
			tmp = b
			b = a
			a = tmp
		end if
		
		tmp1 = AddExp(b[1], b[2], Negate(a[1]), a[2], len, radix)
		tmp1[1] = AbsoluteValue(tmp1[1])
		comp1 = MyCompareExp(tmp1[1], tmp1[2], delta[1], delta[2])
		if length(fb[1]) = 0 or length(fs[1]) = 0 or comp1 = -1 then
			exit
		end if
		sleep(nanosleep)
	end while
	
	len -= eurootsAdjustRound
	b = AdjustRound(b[1], b[2], len, radix)
	s = AdjustRound(s[1], s[2], len, radix)
	
	return {b, s, lookatIter}
end function

--mycomplex.e

-- Complex number functions

public constant REAL = 1, IMAG = 2

public type Complex(object x)
	if sequence(x) then
		if length(x) = 2 then
			if Eun(x[1]) then
				if Eun(x[2]) then
					return 1
				end if
			end if
		end if
	end if
	return 0
end type

public function NewComplex(Eun real = NewEun(), Eun imag = NewEun())
	return {real, imag}
end function

public function ComplexCompare(Complex c1, Complex c2)
	integer ret
	ret = EunCompare(c1[REAL], c2[REAL])
	if ret then
		return ret
	end if
	ret = EunCompare(c1[IMAG], c2[IMAG])
	return ret
end function

public function ComplexAdjustRound(Complex c1, integer adjustBy = 0)
	return {EunAdjustRound(c1[1], adjustBy), EunAdjustRound(c1[2], adjustBy)}
end function

-- Negate the imaginary part of a Complex number
public function NegateImag(Complex a)
	a[IMAG][1] = Negate(a[IMAG][1])
	return a
end function

public function ComplexAdd(Complex a, Complex b)
	return {EunAdd(a[1], b[1]), EunAdd(a[2], b[2])}
end function

public function ComplexNegate(Complex b)
	return {EunNegate(b[1]), EunNegate(b[2])}
end function

public function ComplexSubtract(Complex a, Complex b)
	return {EunAdd(a[1], EunNegate(b[1])), EunAdd(a[2], EunNegate(b[2]))}
end function

public function ComplexMultiply(Complex n1, Complex n2)
	-- n1 = (a+bi)
	-- n2 = (c+di)
	-- (a+bi)(c+di) <=> ac + adi + bci + bdii
	-- <=> (ac - bd) + (ad + bc)i
	Eun real, imag
	real = EunSubtract(EunMultiply(n1[1], n2[1]), EunMultiply(n1[2], n2[2]))
	imag = EunAdd(EunMultiply(n1[1], n2[2]), EunMultiply(n1[2], n2[1]))
	return {real, imag}
end function

public function ComplexMultiplicativeInverse(Complex n2)
	-- Eun a, b, c
	-- (a+bi)(a-bi) <=> a*a + b*b
	-- n2 = (a+bi)
	-- a = n2[1]
	-- b = n2[2]
	-- c = (a*a + b*b)
	-- 1 / n2 <=> (a-bi) / (a*a + b*b)
	-- <=> (a / (a*a + b*b)) - (b / (a*a + b*b))i
	-- <=> (a / c) - (b / c)i
	Eun a, b, c, real, imag
	a = n2[1]
	b = n2[2]
	c = EunMultiplicativeInverse(EunAdd(EunMultiply(a, a), EunMultiply(b, b)))
	real = EunMultiply(a, c)
	imag = EunNegate(EunMultiply(b, c))
	return {real, imag}
end function

public function ComplexDivide(Complex n1, Complex n2)
	return ComplexMultiply(n1, ComplexMultiplicativeInverse(n2))
end function

public PositiveInteger complexSqrtAdjustRound = 4

public procedure SetComplexSqrtAdjustRound(PositiveInteger i)
	complexSqrtAdjustRound = i
end procedure

public function GetComplexSqrtAdjustRound()
	return complexSqrtAdjustRound
end function

public function ComplexSqrt(Complex a)
	--
	-- This equation takes REAL numbers as input to "x" and "y"
	-- So, they use the NON-complex functions to calculate them.
	-- sqrt(x + iy) <=> (1/2) * sqrt(2) * [ sqrt( sqrt(x*x + y*y) + x )  +  i*sign(y) * sqrt( sqrt(x*x + y*y) - x ) ]
	--
	-- NOTE: results are both positive and negative. Remember i (the imaginary part) is always both positive and negative in mathematics.
	-- NOTE: So, you will need to factor that information into your equations.
	Eun x, y, n1, n2, tmp, tmptwo
	Complex cret -- complex return value
	sequence s
	x = a[1]
	y = a[2]
	x[3] += 2
	y[3] += 2
	n1 = EunMultiply(x, x) -- a.real * a.real, x^2
	n2 = EunMultiply(y, y) -- a.imag * a.imag, y^2
	tmp = EunAdd(n1, n2) -- x^2 + y^2
	s = EunSqrt(tmp) -- should not return an imaginary number
	tmp = s[2] -- data member, get the postive answer
	n1 = EunAdd(tmp, x) -- a.real, (sqrt(x^2 + y^2) + x)
	n2 = EunSubtract(tmp, x) -- a.real, (sqrt(x^2 + y^2) - x), round down
	s = EunSqrt(n1) -- could check "isImaginaryFlag", but will always return real number
	if s[1] then
		puts(1, "Error(6):  In MyEuNumber, something went wrong in ComplexSqrt().\n  See file: ex.err\n")
		abort(1/0)
	end if
	n1 = s[2] -- sqrt(sqrt(x^2 + y^2) + x)
	s = EunSqrt(n2) -- could check "isImaginaryFlag", but will always return real number
	if s[1] then
		puts(1, "Error(6):  In MyEuNumber, something went wrong in ComplexSqrt().\n  See file: ex.err\n")
		abort(1/0)
	end if
	n2 = s[2] -- sqrt(sqrt(x^2 + y^2) - x)
	if length(y[1]) then -- a.imag
		if y[1][1] < 0 then
			n2[1] = Negate(n2[1])
		end if
	end if
	tmptwo = NewEun({2}, 0, x[3], x[4])
	s = EunSqrt(tmptwo)
	tmp = s[2]
	tmp = EunDivide(tmp, tmptwo)
	n1 = EunMultiply(n1, tmp)
	n2 = EunMultiply(n2, tmp)
	n1[3] -= complexSqrtAdjustRound
	n2[3] -= complexSqrtAdjustRound
	n1 = AdjustRound(n1[1], n1[2], n1[3], n1[4], NO_SUBTRACT_ADJUST)
	n2 = AdjustRound(n2[1], n2[2], n2[3], n2[4], NO_SUBTRACT_ADJUST)
	cret = NewComplex(n1, n2)
	return {cret, ComplexNegate(cret)}
end function

public function ComplexQuadraticEquation(Complex a, Complex b, Complex c)
	--done.
	-- About the Quadratic Equation:
	--
	-- The quadratic equation produces two answers (the answers may be the same)
	-- ax^2 + bx + c
	-- f(a,b,c) = (-b +-sqrt(b*b - 4*a*c)) / (2*a)
	-- answer[0] = ((-b + sqrt(b*b - 4*a*c)) / (2*a))
	-- answer[1] = ((-b - sqrt(b*b - 4*a*c)) / (2*a))
	--
	-- The "Complex" quadratic equation produces about 2 results
	--
	Complex ans, tmp
	sequence s
	ans = ComplexMultiply(a, c) -- a * c
	tmp = NewComplex(NewEun({4}, 0, a[1][3], a[1][4]), NewEun({}, 0, a[2][3], a[2][4])) -- 4
	ans = ComplexMultiply(tmp, ans) -- 4 * a * c
	tmp = ComplexMultiply(b, b) -- b * b
	ans = ComplexSubtract(tmp, ans) -- b * b - 4 * a * c
	s = ComplexSqrt(ans) -- sqrt(b * b - 4 * a * c)
	tmp = ComplexNegate(b)
	s[1] = ComplexAdd(s[1], tmp) -- (-b + sqrt(b * b - 4 * a * c))
	s[2] = ComplexAdd(s[2], tmp) -- s[2] is already negative
	ans = ComplexAdd(a, a) -- 2 * a
	ans = ComplexMultiplicativeInverse(ans) -- (1 / (2 * a))
	s[1] = ComplexMultiply(s[1], ans)
	s[2] = ComplexMultiply(s[2], ans)
	return s
end function

--quadraticEquation.e

-- The Quadratic Equation
-- 
-- ax^2 + bx + c = 0
-- 
-- x = (-b +-sqrt(b^2 - 4ac)) / (2a)
-- 
-- x1 = (-b + sqrt(b^2 - 4ac)) / (2a)
-- x2 = (-b - sqrt(b^2 - 4ac)) / (2a)
-- 
-- 4*a*c
-- Negate
-- b*b
-- Subtract
-- sqrt
-- b
-- Negate
-- Add/Subtract
-- 2a
-- divide
-- 
-- two answers

public function EunQuadraticEquation(Eun a, Eun b, Eun c)
	Eun n1, n2, n3, ans, tmp
	Complex c1, c2, c3
	sequence s
	if a[4] != b[4] or a[4] != c[4] then
		return 0
	end if
	if a[3] != b[3] or a[3] != c[3] then
		return 0
	end if
	ans = EunMultiply(a, c)
	ans = EunMultiply({{4}, 0, a[3], a[4]}, ans)
	ans = EunNegate(ans)
	tmp = EunMultiply(b, b)
	ans = EunAdd(tmp, ans)
	s = EunSqrt(ans)
	tmp = EunNegate(b)
	if s[1] then -- isImaginary, treat is as a Complex number
		-- Complex
		c1 = NewComplex(tmp, s[2]) -- (-b) + ans * i
		c2 = NewComplex(tmp, s[3]) -- (-b) - ans * i
		tmp = EunMultiply({{2}, 0, a[3], a[4]}, a)
		n3 = EunMultiplicativeInverse(tmp)
		c3 = NewComplex(n3, {{}, 0, a[3], a[4]})
		c1 = ComplexMultiply(c1, c3)
		c2 = ComplexMultiply(c2, c3)
		return {c1, c2}
	else
		n1 = EunAdd(tmp, s[2])
		n2 = EunAdd(tmp, s[3])
		tmp = EunMultiply({{2}, 0, a[3], a[4]}, a)
		tmp = EunMultiplicativeInverse(tmp)
		n1 = EunMultiply(n1, tmp)
		n2 = EunMultiply(n2, tmp)
		return {n1, n2}
	end if
end function

--end of file.

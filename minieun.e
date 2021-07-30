-- Copyright (c) 2016-2021 James Cook
-- mini eunumber, included in myeunumber.e

namespace mini

include get.e

ifdef BITS64 then
	public include std/machine.e
	public include std/convert.e
	public include std/sequence.e
	public include std/os.e
elsedef
	public include allocate.e
end ifdef

public include eunversion.e

-- GetVersion() is included from eunversion.e
--public function GetVersion() -- revision number

ifdef WINDOWS then
public atom nanosleep = -1 -- Windows doesn't need to use sleep(nanosleep)
elsedef
public atom nanosleep = 2/1000000000
end ifdef

public constant TRUE = 1, FALSE = 0

public type Bool(integer i)
	return i = FALSE or i = TRUE
end type

ifdef USE_TASK_YIELD then
public Bool useTaskYield = FALSE -- or TRUE
end ifdef

-- sleep(nanosleep)

public procedure SetNanoSleep(atom a)
	nanosleep = a
end procedure

public function GetNanoSleep()
	return nanosleep
end function

ifdef USE_SMALL_RADIX then
	puts(1, "\nWarning: Using small radix.\nThis program is experimental.\nPress enter to continue, or any other key to exit.\n")
	sleep(1) -- sleep for one (1) second.
	while get_key() != -1 do
		sleep(nanosleep)
	end while
	if wait_key() != 13 then
		abort(1)
	end if
end ifdef

-- with trace

-- NOTE: Negated integer named variables should be in parenthesis.

-- MyEunumber

-- Big endian. Sequence math. With exponents.

-- #Big endian functions:

-- GetDivideByZeroFlag()
-- SetDefaultTargetLength(integer i)
-- GetDefaultTargetLength()
-- SetDefaultRadix(integer i)
-- GetDefaultRadix()
-- SetAdjustRound(integer i)
-- GetAdjustRound()
-- SetCalcSpeed(atom speed)
-- GetCalcSpeed()
-- SetMoreAccuracy(integer i)
-- GetMoreAccuracy()
-- SetRound(integer i)
-- GetRound()
-- SetRoundToNearestOption(integer boolean_value_num)
-- GetRoundToNearestOption()

-- RoundTowardsZero
-- Equaln
-- IsIntegerOdd(integer i)
-- IsIntegerEven(integer i)

-- Borrow
-- Carry
-- Add
-- Sum
-- ConvertRadix
-- Multiply
-- Square
-- IsNegative
-- Negate
-- AbsoluteValue
-- Subtract
-- TrimLeadingZeros
-- TrimTrailingZeros
-- AdjustRound
-- MultiplyExp
-- AddExp

-- ProtoMultiplicativeInverseExp
-- IntToDigits
-- ExpToAtom
-- GetGuess
-- MultiplicativeInverseExp
-- DivideExp
-- ConvertExp

-- Eun (type)
-- NewEun
-- EunAdjustRound(Eun n1, integer adjustBy = 0)
-- EunMultiply
-- EunSquare
-- EunAdd
-- EunNegate
-- EunAbsoluteValue
-- EunSubtract
-- EunMultiplicativeInverse
-- EunDivide
-- EunConvert

-- CompareExp
-- GetEqualLength
-- EunCompare
-- EunReverse -- reverse endian
-- EunFracPart -- returns the fraction part (no Rounding)
-- EunIntPart -- returns the integer part (no Rounding)
-- EunRoundSig -- Round to number of significant digits
-- EunRoundSignificantDigits -- same as EunRoundSig
-- EunRoundToInt -- Round to nearest integer
-- EunCombInt(Eun n1, integer adjustBy = 0)
-- EunModf(Eun fp) -- similar to C's "modf()"
-- EunfDiv(Eun num, Eun den) -- similar to C's "div()"
-- EunfMod(Eun num, Eun den) -- similar to C's "fmod()", just the "mod" or remainder

public constant DOUBLE_LARGEST = 1.0e+300
public constant DOUBLE_SMALLEST = 1.0e-300
ifdef BITS64 then
public constant DOUBLE_INT_MAX = 18446744073709551615 -- (power(2, 64) - 1)
public constant DOUBLE_INT_MIN = - (DOUBLE_INT_MAX)
public constant INT_MAX = power(2, 62) - 1 -- value: 4611686018427387903
public constant INT_MAX10 = power(10, 18) -- value: 1000000000000000000
public constant MAX_RADIX10 = power(10, 6) -- value: 1000000
public constant MAX_RADIX = 1048576 -- power(2, floor(62/2)-4) -- value: 134217728
public constant DOUBLE_RADIX = MAX_RADIX10 -- floor(sqrt(DOUBLE_INT_MAX)) + 1 -- 4294967296
public constant DOUBLE_RADIX10 = MAX_RADIX10 -- 1000000000
elsedef
public constant DOUBLE_INT_MAX = 9007199254740991 -- (power(2, 53) - 1)
public constant DOUBLE_INT_MIN = - (DOUBLE_INT_MAX)
public constant INT_MAX = power(2, 30) - 1 -- value: 1073741823
public constant INT_MAX10 = power(10, 9) -- value: 1000000000
public constant MAX_RADIX10 = power(10, 3) -- value: 1000
public constant MAX_RADIX = 1024 -- MAX_RADIX10 -- power(2, floor(30/2)-4) -- value: 2048
public constant DOUBLE_RADIX = MAX_RADIX10 -- floor(sqrt(DOUBLE_INT_MAX)) + 1 -- 94906266
public constant DOUBLE_RADIX10 = MAX_RADIX10 -- 10000000
end ifdef

export function iff(integer condition, object iftrue, object iffalse)
	if condition then
		return iftrue
	else
		return iffalse
	end if
end function

export function abs(atom a)
	if a >= 0 then
		return a
	else
		return - (a)
	end if
end function

public function Ceil(atom a)
	return -(floor(-a))
end function

public type PositiveInteger(integer i)
	return i >= 0
end type

public type NegativeInteger(integer i)
	return i < 0
end type

public type PositiveScalar(integer i)
	return i >= 2
end type

public type NegativeScalar(integer i)
	return i <= -2
end type

public type PositiveOption(integer i)
	return i >= -1
end type

public type PositiveAtom(atom a)
	return a >= 0.0
end type

public type NegativeAtom(atom a)
	return a < 0.0
end type

public type AtomRadix(atom a)
ifdef USE_SMALL_RADIX then
	return a >= 1.001 and a <= DOUBLE_RADIX -- must be larger than 1.0
elsedef
	return a >= 2 and a <= DOUBLE_RADIX -- must be 2.0 or larger
end ifdef
end type

public Bool divideByZeroFlag = FALSE

public function GetDivideByZeroFlag()
	return divideByZeroFlag
end function
public procedure SetDivideByZeroFlag(Bool i)
	divideByZeroFlag = i
end procedure

public Bool zeroDividedByZeroFlag = TRUE -- if true, zero divided by zero returns one (0/0 = 1)

public function GetZeroDividedByZeroFlag()
	return zeroDividedByZeroFlag
end function
public procedure SetZeroDividedByZeroFlag(Bool i)
	zeroDividedByZeroFlag = i
end procedure

public type TargetLength(integer i)
	return i >= 1
end type

public TargetLength defaultTargetLength = 70 -- 70 * 3 = 210 (I tried to keep it under 212)
public AtomRadix defaultRadix = 10 -- 10 is good for everything from 16-bit shorts, to 32-bit ints, to 64-bit long longs.
public Bool isRoundToZero = FALSE -- make TRUE to allow rounding small numbers to zero.
public PositiveInteger adjustRound = 5 -- 3 -- can be 0 to a small integer, removes digits of inaccuracy, or adds digits of accuracy under ROUND_TO_NEAREST_OPTION
public PositiveAtom calculationSpeed = floor(defaultTargetLength / 3) -- can be 0 or from 1 to targetLength
public PositiveOption multInvMoreAccuracy = -1 -- 15, if -1, then use calculationSpeed

public procedure SetIsRoundToZero(Bool i)
	isRoundToZero = i
end procedure
public function GetIsRoundToZero()
	return isRoundToZero
end function

public procedure SetMultiplicativeInverseMoreAccuracy(PositiveOption i)
	multInvMoreAccuracy = i
end procedure
public function GetMultiplicativeInverseMoreAccuracy()
	return multInvMoreAccuracy
end function

public procedure SetDefaultTargetLength(PositiveScalar i)
	defaultTargetLength = i
end procedure
public function GetDefaultTargetLength()
	return defaultTargetLength
end function

public procedure SetDefaultRadix(AtomRadix i)
	defaultRadix = i
end procedure
public function GetDefaultRadix()
	return defaultRadix
end function

public procedure SetAdjustRound(PositiveInteger i)
	adjustRound = i
end procedure
public function GetAdjustRound()
	return adjustRound
end function

public type CalcSpeedType(atom speed)
	return speed = 0.0 or speed >= 1.0
end type

public procedure SetCalcSpeed(CalcSpeedType speed)
	calculationSpeed = speed
end procedure
public function GetCalcSpeed()
	return calculationSpeed
end function

public integer iter = 1000000000 -- max number of iterations before returning
public integer lastIterCount = 0 -- MultiplicativeInverseExp has not been called yet, so the value is -1

-- public constant ATOM_EPSILON = 2.22044604925031308085e-16 -- DBL_EPSILON 64-bit
public constant LOG_ATOM_SMALLEST = log(1.0e-300)
public constant LOG_ATOM_LARGEST = log(1.0e+300)

public constant ROUND_INF = 1 -- Round towards +infinity or -infinity, (positive or negative infinity)
public constant ROUND_ZERO = 2 -- Round towards zero
public constant ROUND_TRUNCATE = 3 -- Don't round, truncate
public constant ROUND_POS_INF = 4 -- Round towards positive +infinity
public constant ROUND_NEG_INF = 5 -- Round towards negative -infinity
-- round even:
public constant ROUND_EVEN = 6 -- Round making number even on halfRadix
-- round odd:
public constant ROUND_ODD = 7 -- Round making number odd on halfRadix
public Bool ROUND_TO_NEAREST_OPTION = FALSE -- Round to nearest whole number (Eun integer), true or false

public procedure SetRoundToNearestOption(Bool boolean_value_num)
	ROUND_TO_NEAREST_OPTION = boolean_value_num
end procedure
public function GetRoundToNearestOption()
	return ROUND_TO_NEAREST_OPTION
end function

public procedure IntegerModeOn()
	ROUND_TO_NEAREST_OPTION = 1
end procedure
public procedure IntegerModeOff()
	ROUND_TO_NEAREST_OPTION = 0
end procedure

type Round2(integer i)
	return i >= 1 and i <= 7
end type

public constant ROUND_AWAY_FROM_ZERO = ROUND_INF
public constant ROUND_TOWARDS_ZERO = ROUND_ZERO
public constant ROUND_TOWARDS_NEGATIVE_INFINITY = ROUND_NEG_INF
public constant ROUND_TOWARDS_POSITIVE_INFINITY = ROUND_POS_INF

public constant ROUND_DOWN = ROUND_NEG_INF -- Round downward.
public constant ROUND_UP = ROUND_POS_INF -- Round upward.

-- public for "doFile.ex":
public Round2 ROUND = ROUND_INF -- or you could try: ROUND_INF or any other ROUND method

public procedure SetRound(Round2 i)
	ROUND = i
end procedure
public function GetRound()
	return ROUND
end function

public function RoundTowardsZero(atom x)
	if x < 0 then
		return Ceil(x)
	else
		return floor(x)
	end if
end function

public function Round(object a, object precision = 1)
	return floor(0.5 + (a * precision )) / precision
end function

public function RangeEqual(sequence a, sequence b, PositiveInteger start, PositiveInteger stop)
	if length(a) >= stop and length(b) >= stop then
		for i = stop to start by -1 do
			if a[i] != b[i] then
				return 0
			end if
			sleep(nanosleep)
		end for
		return 1
	end if
	return 0
end function

public function Equaln(sequence a, sequence b)
	integer minlen, maxlen
ifdef USE_TASK_YIELD then
	if useTaskYield then
		task_yield()
	end if
end ifdef
	if length(a) > length(b) then
		maxlen = length(a)
		minlen = length(b)
	else
		maxlen = length(b)
		minlen = length(a)
	end if
	for i = 1 to minlen do
		if a[i] != b[i] then
			return {i - 1, maxlen}
		end if
		sleep(nanosleep)
	end for
	return {minlen, maxlen}
end function

public function IsIntegerOdd(integer i)
	return remainder(i, 2) and 1
end function

public function IsIntegerEven(integer i)
	return not IsIntegerOdd(i)
end function

public function IsPositiveOdd(integer i)
	return and_bits(i, 1)
end function

public function IsPositiveEven(integer i)
	return and_bits(i, 0)
end function

-- Function definition
public function Borrow(sequence numArray, AtomRadix radix)
	for i = length(numArray) to 2 by -1 do
		if numArray[i] < 0 then
			numArray[i] += radix
			numArray[i - 1] -= 1
		end if
		sleep(nanosleep)
	end for
	return numArray
end function

public function NegativeBorrow(sequence numArray, AtomRadix radix)
	for i = length(numArray) to 2 by -1 do
		if numArray[i] > 0 then
			numArray[i] -= radix
			numArray[i - 1] += 1
		end if
		sleep(nanosleep)
	end for
	return numArray
end function

public function Carry(sequence numArray, AtomRadix radix)
	atom q, r, b
	integer i
	i = length(numArray)
	while i > 0 do
		b = numArray[i]
		if b >= radix then
			q = floor(b / radix)
			r = remainder(b, radix)
			numArray[i] = r
			if i = 1 then
				numArray = prepend(numArray, q)
			else
				i -= 1
				-- q += numArray[i] -- test for integer overflow
				numArray[i] += q
			end if
		else
			i -= 1
		end if
		sleep(nanosleep)
	end while
	return numArray
end function

public function NegativeCarry(sequence numArray, AtomRadix radix)
	atom q, r, b, negativeRadix
	integer i
	negativeRadix = -radix
	i = length(numArray)
	while i > 0 do
		b = numArray[i]
		if b <= negativeRadix then
			q = -(floor(b / negativeRadix)) -- bug fix
			r = remainder(b, radix)
			numArray[i] = r
			if i = 1 then
				numArray = prepend(numArray, q)
			else
				i -= 1
				-- q += numArray[i] -- test for integer overflow
				numArray[i] += q
			end if
		else
			i -= 1
		end if
		sleep(nanosleep)
	end while
	return numArray
end function

public function Add(sequence n1, sequence n2)
	integer c, len
	sequence numArray, tmp
	if length(n1) >= length(n2) then
		len = length(n2)
		c = length(n1) - (len)
		-- copy n1 to numArray:
		numArray = n1
		tmp = n2
	else
		len = length(n1)
		c = length(n2) - (len)
		-- copy n2 to numArray:
		numArray = n2
		tmp = n1
	end if
	for i = 1 to len do
		c += 1
		numArray[c] += tmp[i]
		sleep(nanosleep)
	end for
	return numArray
end function

public function Subtr(sequence n1, sequence n2)
	integer c, len
	sequence numArray, tmp
	if length(n1) >= length(n2) then
		len = length(n2)
		c = length(n1) - (len)
		-- copy n1 to numArray:
		numArray = n1
		tmp = n2
		for i = 1 to len do
			c += 1
			numArray[c] -= tmp[i]
			sleep(nanosleep)
		end for
	else
		len = length(n1)
		c = length(n2) - (len)
		-- numArray = n2[1..c+1] & (n1 - n2[c+2..$])
		-- numArray = repeat(0, length(n2))
		-- numArray[1..c+1] = n2[1..c+1]
		-- numArray[c+2..$] = n1 - n2[c+2..$]
		-- copy n2 to numArray:
		numArray = n2
		tmp = n1
		for i = 1 to len do
			c += 1
			numArray[c] = tmp[i] - numArray[c]
			sleep(nanosleep)
		end for
	end if
	return numArray
end function

public function Sum(sequence dst, sequence srcs)
	for i = 1 to length(srcs) do
		dst = Add(dst, srcs[i])
		sleep(nanosleep)
	end for
	return dst
end function

public function ConvertRadix(sequence number, AtomRadix fromRadix, AtomRadix toRadix)
	sequence target, base, tmp
	atom digit
	integer isNeg
	target = {} -- same as: {0}
	if length(number) then
		base = {1}
		isNeg = number[1] < 0
		for i = length(number) to 1 by -1 do
			tmp = base
			digit = number[i]
			for j = 1 to length(tmp) do
				tmp[j] *= digit
				sleep(nanosleep)
			end for
			target = Add(target, tmp)
			if isNeg then
				target = NegativeCarry(target, toRadix)
			else
				target = Carry(target, toRadix)
			end if
			for j = 1 to length(base) do
				base[j] *= fromRadix
				sleep(nanosleep)
			end for
			base = Carry(base, toRadix)
			sleep(nanosleep)
		end for
	end if
	return target
end function

public function Multiply(sequence n1, sequence n2, integer len = length(n1) + length(n2) - 1)
	integer f, j
	atom g
	sequence numArray
	if length(n1) = 0 or length(n2) = 0 then
		return {}
	end if
	-- len = length(n1) + length(n2) - 1
	numArray = repeat(0, len)
ifdef EUN_MINI_MULT then
-- This method may be faster:
	f = length(n2)
	for i = 1 to length(n1) do
		g = n1[i]
		j = 1
		for h = i to iff(len < f, len, f) do
			numArray[h] += g * n2[j]
			j += 1
			sleep(nanosleep)
		end for
		f += 1
		sleep(nanosleep)
	end for
elsedef
	f = 1
	if len > length(n2) then
		for k = length(n2) to len do
		-- for i = 1 to length(n1) do
			g = n1[f]
			j = 1
			-- k = iff(len < f1, len, f1)
			for h = f to k do
				numArray[h] += g * n2[j]
				j += 1
				sleep(nanosleep)
			end for
			f += 1
			sleep(nanosleep)
		end for
	end if
	for i = f to length(n1) do
		g = n1[i]
		j = 1
		-- k = iff(len < f1, len, f1)
		for h = i to len do
			numArray[h] += g * n2[j]
			j += 1
			sleep(nanosleep)
		end for
		f += 1
		sleep(nanosleep)
	end for
end ifdef
	return numArray
end function

public function Square(sequence n1)
	return Multiply(n1, n1) -- multiply it by its self, once
end function

public function IsNegative(sequence numArray)
	if length(numArray) then
		return numArray[1] < 0
	end if
	return 0
end function

public function Negate(sequence numArray)
	for i = 1 to length(numArray) do
		numArray[i] = - (numArray[i])
		sleep(nanosleep)
	end for
	return numArray
end function

public function AbsoluteValue(sequence numArray)
	if length(numArray) then
		if numArray[1] < 0 then
			numArray = Negate(numArray)
		end if
	end if
	return numArray
end function

public function Subtract(sequence numArray, AtomRadix radix, Bool isMixed = TRUE)
	if length(numArray) then
		if numArray[1] < 0 then
			numArray = NegativeCarry(numArray, radix)
			if isMixed then
				numArray = NegativeBorrow(numArray, radix)
			end if
		else
			numArray = Carry(numArray, radix)
			if isMixed then
				numArray = Borrow(numArray, radix)
			end if
		end if
	end if
	return numArray
end function


-- Rounding functions:

public function TrimLeadingZeros(sequence numArray)
	for i = 1 to length(numArray) do
		if numArray[i] != 0 then
			if i = 1 then
				return numArray
			else
				return numArray[i..$]
			end if
		end if
		sleep(nanosleep)
	end for
	return {}
end function

-- public function TrimLeadingZeros2(sequence numArray)
-- 	while length(numArray) and numArray[1] = 0 do
-- 		numArray = numArray[2..$]
-- 		sleep(nanosleep)
-- 	end while
-- 	return numArray
-- end function

public function TrimTrailingZeros(sequence numArray)
	for i = length(numArray) to 1 by -1 do
		if numArray[i] != 0 then
			if i = length(numArray) then
				return numArray
			else
				return numArray[1..i]
			end if
		end if
		sleep(nanosleep)
	end for
	return {}
end function

-- public function TrimTrailingZeros2(sequence numArray)
-- 	while length(numArray) and numArray[$] = 0 do
-- 		numArray = numArray[1..$-1]
-- 		sleep(nanosleep)
-- 	end while
-- 	return numArray
-- end function

--public function CarryRadixOnlyEx(sequence numArray, atom radix, integer way = 1)
--	atom b
--	integer i
--	i = length(numArray)
--	while i > 0 do
--		b = numArray[i]
--		if b != radix then
--			return numArray
--		end if
--		numArray[i] = 0 -- modulus, or remainder
--		if i = 1 then
--			numArray = prepend(numArray, way)
--			return numArray
--		else
--			i -= 1
--			numArray[i] += way
--		end if
--		sleep(nanosleep)
--	end while
--	return numArray
--end function

type ThreeOptions(integer i)
	return i >= 0 and i <= 2
end type

public constant NO_SUBTRACT_ADJUST = 2

public function AdjustRound(sequence num, integer exponent, TargetLength targetLength, AtomRadix radix, ThreeOptions isMixed = 1)
	integer oldlen, roundTargetLength, rounded
	atom halfRadix, negHalfRadix, f
	sequence ret
ifdef USE_TASK_YIELD then
	if useTaskYield then
		task_yield()
	end if
end ifdef
if isMixed != NO_SUBTRACT_ADJUST then
	oldlen = length(num)
	num = TrimLeadingZeros(num)
	exponent += (length(num) - (oldlen))
	--adjustExponent()
	oldlen = length(num)
	-- in Subtract, the first element of num cannot be a zero.
	num = Subtract(num, radix, isMixed)
	-- use Subtract() when there are both negative and positive numbers.
	-- otherwise, you can use Carry().
	num = TrimLeadingZeros(num)
	exponent += (length(num) - (oldlen))
end if
	rounded = 0
	if length(num) = 0 then
		ret = {{}, exponent, targetLength, radix, rounded}
		return ret
	end if
	if isRoundToZero and exponent < -targetLength then
		ret = {{}, exponent, targetLength, radix, (num[1] < 0) - (num[1] > 0)} -- "rounded"
		return ret
	end if
	-- Round2: num, exponent, targetLength, radix
	if ROUND_TO_NEAREST_OPTION then
		roundTargetLength = exponent + 1 + adjustRound
		if targetLength < roundTargetLength then
			targetLength = roundTargetLength
		end if
		if roundTargetLength <= 1 then
			if exponent <= -1 then
				if exponent = -1 then
					num = {0} & num
				else
					num = {0, 0}
				end if
				exponent = 0 -- zero because it rounds to nearest integer
			end if
			roundTargetLength = 1
		end if
	else
		roundTargetLength = targetLength - (adjustRound)
		if roundTargetLength <= 0 then
			if roundTargetLength = 0 then
				num = {0} & num
				exponent += 1
			else
				num = {0, 0}
				-- exponent = 0 -- don't reset exponent
			end if
			roundTargetLength = 1
		end if
	end if
	if length(num) > roundTargetLength then
		if ROUND = ROUND_TRUNCATE then
			num = num[1..roundTargetLength]
			rounded = (num[1] < 0) - (num[1] > 0) -- give the opposite of the sign
		else
			halfRadix = floor(radix / 2)
			negHalfRadix = - (halfRadix)
			f = num[roundTargetLength + 1]
			if integer(radix) and IsIntegerOdd(radix) then
				-- feature: support for odd radixes
				for i = roundTargetLength + 2 to length(num) do
					if f != halfRadix and f != negHalfRadix then
						exit
					end if
					f = num[i]
					sleep(nanosleep)
				end for
			end if
			if f = halfRadix or f = negHalfRadix then
				if ROUND = ROUND_EVEN then
					halfRadix -= IsIntegerOdd(num[roundTargetLength])
				elsif ROUND = ROUND_ODD then
					halfRadix -= IsIntegerEven(num[roundTargetLength])
				elsif ROUND = ROUND_ZERO then
					f = 0
				end if
			elsif ROUND = ROUND_INF then -- round towards plus(+) and minus(-) infinity
				halfRadix -= 1
			elsif ROUND = ROUND_POS_INF then -- round towards plus(+) infinity
				f += 1
			elsif ROUND = ROUND_NEG_INF then -- round towards minus(-) infinity
				f -= 1
			end if
			num = num[1..roundTargetLength]
			rounded = (f > halfRadix) - (f < negHalfRadix) -- 1 for round up, -1 for round down.
			if rounded then
				num[roundTargetLength] += rounded
				if rounded > 0 then
					num = Carry(num, radix)
				else
					num = NegativeCarry(num, radix)
				end if
				exponent += (length(num) - (roundTargetLength))
			else
				rounded = (num[1] < 0) - (num[1] > 0) -- give the opposite of the sign
			end if
		end if
	end if
	num = TrimTrailingZeros(num)
	oldlen = length(num)
	num = TrimLeadingZeros(num)
	exponent += (length(num) - (oldlen))
	ret = {num, exponent, targetLength, radix, rounded}
	return ret
end function

integer multLastLen = 0
atom multLastRadix = 0
integer multLen
atom multlogt, multlogr

public function MultiplyExp(sequence n1, integer exp1, sequence n2, integer exp2, TargetLength targetLength, AtomRadix radix)
	sequence numArray, ret
	integer len, flag = 0
	if multLastLen != targetLength then
		multLastLen = targetLength
		multlogt = log(targetLength)
		flag = 1
	end if
	if multLastRadix != radix then
		multLastRadix = radix
		multlogr = log(radix)
		flag = 1
	end if
	if flag then
		multLen = targetLength -- iff(length(n1) > length(n2), length(n1), length(n2))
		multLen += Ceil(multlogt / multlogr)
		multLen += 1
	end if
	len = length(n1) + length(n2) - 1
	if multLen < len then
		len = multLen
	end if
	numArray = Multiply(n1, n2, len)
	ret = AdjustRound(numArray, exp1 + exp2, targetLength, radix, FALSE) -- TRUE for backwards compatability
	return ret
end function

public function SquareExp(sequence n1, integer exp1, TargetLength targetLength, AtomRadix radix)
	return MultiplyExp(n1, exp1, n1, exp1, targetLength, radix)
end function


public function AddExp(sequence n1, integer exp1, sequence n2, integer exp2, TargetLength targetLength, AtomRadix radix)
	sequence numArray, ret
	integer size
	size = (length(n1) - (exp1)) - (length(n2) - (exp2))
	if size < 0 then
		size = - (size)
		n1 = n1 & repeat(0, size)
	elsif 0 < size then
		n2 = n2 & repeat(0, size)
	end if
	if exp1 < exp2 then
		exp1 = exp2
	end if
	numArray = Add(n1, n2)
	ret = AdjustRound(numArray, exp1, targetLength, radix, IsNegative(n1) xor IsNegative(n2))
	return ret
end function

public function SubtractExp(sequence n1, integer exp1, sequence n2, integer exp2, TargetLength targetLength, AtomRadix radix)
	sequence numArray, ret
	integer size
	size = (length(n1) - (exp1)) - (length(n2) - (exp2))
	if size < 0 then
		size = - (size)
		n1 = n1 & repeat(0, size)
	elsif 0 < size then
		n2 = n2 & repeat(0, size)
	end if
	if exp1 < exp2 then
		exp1 = exp2
	end if
	numArray = Subtr(n1, n2)
	ret = AdjustRound(numArray, exp1, targetLength, radix, IsNegative(n1) xor (not IsNegative(n2)))
	return ret
end function


-- Division and Multiply Inverse:
-- https://en.wikipedia.org/wiki/Newton%27s_method#Multiplyiplicative_inverses_of_numbers_and_power_series

constant two = {2}
PositiveInteger forSmallRadix = 0 -- this number can be 0 or greater

public procedure SetForSmallRadix(PositiveInteger i)
	forSmallRadix = i -- increase this number for smaller radixes
end procedure

public function GetForSmallRadix()
	return forSmallRadix
end function

public function ProtoMultiplicativeInverseExp(sequence guess, integer exp0, sequence den1, integer exp1, TargetLength targetLength, AtomRadix radix)
	-- a = guess
	-- n1 = den1
	-- f(a) = a * (2 - n1 * a)
	--
	-- Proof: for f(x) = 1/x
	-- f(a) = 2a - n1*a^2
	-- a = 2a - n1*a^2
	-- 0 = a - n1*a^2
	-- x = a
	-- ax^2 + bx + c = 0
	-- a=(- n1), b=1, c=0
	-- x = (-b +-sqrt(b^2 - 4ac)) / (2a)
	-- x = (-1 +-1) / (-2*n1)
	-- x = 0, 1/n1
	sequence tmp, numArray, ret
	integer exp2
	tmp = MultiplyExp(guess, exp0, den1, exp1, targetLength, radix) -- den1 * a
-- tmp -- turns to one
	numArray = tmp[1]
	exp2 = tmp[2]
	tmp = SubtractExp(two, 0, numArray, exp2, targetLength - (forSmallRadix), radix) -- 2 - tmp
-- tmp -- turns to one
	numArray = tmp[1]
	exp2 = tmp[2]
	if length(numArray) = 1 then
		if numArray[1] = 1 then
			if exp2 = 0 then
				-- signal_solution_found = 1
				return {guess, exp0}
			end if
		end if
	end if
	ret = MultiplyExp(guess, exp0, numArray, exp2, targetLength, radix) -- a * tmp
-- ret -- turns to ans
	return ret
end function


public function IntToDigits(atom x, AtomRadix radix)
	sequence numArray
	atom a
	numArray = {}
	while x != 0 do
		a = remainder(x, radix)
		numArray = prepend(numArray, a)
		x = RoundTowardsZero(x / radix) -- must be Round() to work on negative numbers
		sleep(nanosleep)
	end while
	return numArray
end function

constant logTen = log(10)
integer sigDigits = 0
integer minSigDigits = 0
integer maxSigDigits = 0
atom static_multInvRadix = 0
atom static_logRadix = 0

procedure set_div_static_vars(atom radix)
	static_multInvRadix = radix
	static_logRadix = log(radix)
	sigDigits = Ceil(15 / (log(radix) / logTen))
	--ifdef BITS64 then
	--	sigDigits = Ceil(18 / (log(radix) / log(10)))
	--elsedef
	--	sigDigits = Ceil(15 / (log(radix) / log(10)))
	--end ifdef
	minSigDigits = floor(LOG_ATOM_SMALLEST / static_logRadix)
	maxSigDigits = floor(LOG_ATOM_LARGEST / static_logRadix)
end procedure

public function LongDivision(atom num, integer exp1, atom denom, integer exp2, TargetLength protoTargetLength, AtomRadix radix)
	integer exp0, optionNegOne
	atom quot
	sequence guess
	if static_multInvRadix != radix then
		set_div_static_vars(radix)
	end if
	optionNegOne = 1
	if num < 0 then
		num = - (num)
		optionNegOne *= -1
	end if
	if denom < 0 then
		denom = - (denom)
		optionNegOne *= -1
	end if
	if num <= denom then
		exp0 = 0
		while 1 do
			quot = floor(num / denom) * optionNegOne
			num = remainder(num, denom)
			num *= radix
			exp0 -= 1
			if quot != 0 then
				exit
			end if
		end while
		guess = {quot}
	else
		quot = floor(num / denom) * optionNegOne
		num = remainder(num, denom)
		num *= radix
		exp0 = floor(log(quot) / static_logRadix)
		guess = IntToDigits(quot, radix)
	end if
	while num != 0 and length(guess) < protoTargetLength do
		quot = floor(num / denom) * optionNegOne
		num = remainder(num, denom)
		num *= radix
-- 		if length(guess) = 0 and quot = 0 then
-- 			exp0 -= 1
-- 		else
			guess = guess & {quot}
-- 		end if
		sleep(nanosleep)
	end while
-- 	oldlen = length(guess)
-- 	guess = TrimLeadingZeros(guess)
-- 	exp0 += length(guess) - oldlen
	exp0 += exp1 - exp2
	return {guess, exp0, protoTargetLength, radix}
end function

public function ExpToAtom(sequence n1, integer exp1, PositiveInteger targetLen, AtomRadix radix)
	atom p, ans, lookat, ele
	integer overflowBy
	if length(n1) = 0 then
		return 0 -- tried to divide by zero
	end if
	if static_multInvRadix != radix then
		set_div_static_vars(radix)
	end if
	-- what if exp1 is too large?
	overflowBy = exp1 - maxSigDigits + 2 -- +2 may need to be bigger
			-- overflowBy = exp1 - floor(LOG_ATOM_MAX / p) + 2 -- +2 may need to be bigger
	if overflowBy > 0 then
		-- overflow warning in "power()" function
		-- reduce size
		exp1 -= overflowBy
	else
		-- what if exp1 is too small?
		overflowBy = exp1 - minSigDigits - 2 -- -2 may need to be bigger
				-- overflowBy = exp1 - floor(LOG_ATOM_MIN / p) - 2 -- -2 may need to be bigger
		if overflowBy < 0 then
			exp1 -= overflowBy
		else
			overflowBy = 0
		end if
	end if
	exp1 -= targetLen
	p = power(radix, exp1)
	ans = n1[1] * p
	for i = 2 to length(n1) do
		p = p / radix
		ele = n1[i]
		if ele != 0 then
			lookat = ans
			ans += ele * p
			if ans = lookat then
				exit
			end if
		end if
		sleep(nanosleep)
	end for
	-- if overflowBy is positive, then there was an overflow
	-- overflowBy is an offset of that overflow in the given radix
	return {ans, overflowBy}
end function

public function GetGuessExp(sequence den, integer exp1, integer protoTargetLength, AtomRadix radix)
	sequence guess, tmp
	atom denom, one, ans
	integer raised, mySigDigits, exp2
	object val
	if static_multInvRadix != radix then
		set_div_static_vars(radix)
	end if
	if protoTargetLength < sigDigits then
		mySigDigits = protoTargetLength
	else
		mySigDigits = sigDigits
	end if
	val = ToAtom(NewEun(den, exp1, protoTargetLength, radix))
	if atom(val) and val != 0 then
		denom = val
		val = floor(log(denom) / static_logRadix)
		if val <= INT_MAX then
			exp2 = val
			denom = denom / power(radix, exp2)
			tmp = LongDivision(1, 0, denom, exp2, protoTargetLength, radix)
			return tmp
		end if
	end if
	raised = length(den) - 1
	tmp = ExpToAtom(den, raised, mySigDigits, radix)
	denom = tmp[1]
	raised -= tmp[2]
	one = power(radix, raised)
	ans = RoundTowardsZero(one / denom)
	guess = IntToDigits(ans, radix) -- works on negative numbers
	-- tmp = AdjustRound(guess, exp1, mySigDigits - 1, radix, FALSE)
	-- tmp[3] = protoTargetLength
	return NewEun(guess, - (exp1) - 1, protoTargetLength, radix)
end function


public procedure DefaultDivideCallBack(integer i)
	if i = 1 then
		puts(1, "Error(1):  In MyEuNumber, tried to divide by zero (1/0).  See file: ex.err\n")
	elsif i = 2 then
		printf(1, "Error:  In MyEuNumber, forSmallRadix is %d, try increasing\n SetForSmallRadix() to a larger integer.  See file: ex.err\n", {forSmallRadix})
	elsif i = 3 then
		puts(1, "Error(4):  In MyEuNumber, too many iterations.  Unable to calculate number.\n  See file: ex.err\n")
	elsif i = 4 then
		puts(1, "Error(2):  In MyEuNumber, in real mode, even root of -1, i.e. sqrt(-1).\n  See file: ex.err\n")
	elsif i = 5 then
		puts(1, "Error(5):  In MyEuNumber, radixes are not equal.  See file: ex.err\n")
	elsif i = 6 then
		puts(1, "Error(3):  In MyEuNumber, negative argument(s) in \"EunTriangulation()\".\n  See file: ex.err\n")
	end if
	abort(1/0)
end procedure

public integer divideCallBackId = routine_id("DefaultDivideCallBack")

public sequence howComplete = {-1, 0}

public function MultiplicativeInverseExp(sequence den1, integer exp1, TargetLength targetLength, AtomRadix radix, sequence guess = {})
	sequence lookat, ret
	integer exp0, protoTargetLength, protoMoreAccuracy
	howComplete = {-1, 0}
	den1 = TrimTrailingZeros(den1)
	if length(den1) = 0 then
		lastIterCount = 1
		divideByZeroFlag = 1
		call_proc(divideCallBackId, {1})
		return {}
	end if
	if length(den1) = 1 then
		if den1[1] = 1 or den1[1] = -1 then -- optimization
			howComplete = {1, 1}
			lastIterCount = 1
			return NewEun(den1, -(exp1), targetLength, radix)
		end if
	end if
	if multInvMoreAccuracy >= 0 then
		protoMoreAccuracy = multInvMoreAccuracy
	elsif calculationSpeed then
		protoMoreAccuracy = Ceil(targetLength / calculationSpeed)
	else
		protoMoreAccuracy = 0 -- changed to 0
	end if
	protoTargetLength = targetLength + protoMoreAccuracy
	if length(guess) = 0 then
		-- factor out a power of radix, from both the numerator and the denominator,
		-- then multiply them later.
		ret = GetGuessExp(den1, exp1, protoTargetLength, radix)
		guess = ret[1]
		exp0 = ret[2]
-- 	else
-- 		exp0 = - (exp1) - 1
	end if
	exp0 = - (exp1) - 1
	ret = AdjustRound(guess, exp0, targetLength, radix, FALSE)
	lastIterCount = iter
	for i = 1 to iter do
		lookat = ret
		ret = ProtoMultiplicativeInverseExp(guess, exp0, den1, exp1, protoTargetLength, radix)
		guess = ret[1]
		-- ? {length(guess), protoTargetLength}
		exp0 = ret[2]
		ret = AdjustRound(guess, exp0, targetLength, radix, NO_SUBTRACT_ADJUST)
		if length(ret) = 2 then
			-- solution found
			howComplete = repeat(length(ret[1]), 2)
			lastIterCount = i
			exit
		end if
		if ret[2] = lookat[2] then
			howComplete = Equaln(ret[1], lookat[1])
			if howComplete[1] = howComplete[2] then
			-- if equal(ret[1], lookat[1]) then
				lastIterCount = i
				exit
			end if
		end if
		sleep(nanosleep)
	end for
	if lastIterCount = iter then
		call_proc(divideCallBackId, {2})
		return {}
	end if
	return ret
end function


public function DivideExp(sequence num1, integer exp1, sequence den2, integer exp2, TargetLength targetLength, AtomRadix radix)
	sequence tmp
	if zeroDividedByZeroFlag and length(num1) = 0 and length(den2) = 0 then
		return NewEun({1}, 0, targetLength, radix)
	end if
	tmp = MultiplicativeInverseExp(den2, exp2, targetLength, radix)
	if length(tmp) then
		tmp = MultiplyExp(num1, exp1, tmp[1], tmp[2], targetLength, radix)
		return tmp
	else
		return {}
	end if
end function


public function ConvertExp(sequence n1, integer exp1, TargetLength targetLength, AtomRadix fromRadix, AtomRadix toRadix)
	sequence n2, n3, result
	integer exp2, exp3
	n1 = TrimTrailingZeros(n1)
	if length(n1) = 0 then
		result = {{}, 0, targetLength, toRadix, 0}
		return result
	end if
	if length(n1) <= exp1 then
		n1 = n1 & repeat(0, exp1 - length(n1) + 1)
	end if
	n2 = ConvertRadix(n1, fromRadix, toRadix)
	exp2 = length(n2) - 1
	n3 = ConvertRadix({1} & repeat(0, length(n1) - (exp1) - 1), fromRadix, toRadix)
	exp3 = length(n3) - 1
	result = DivideExp(n2, exp2, n3, exp3, targetLength, toRadix)
	return result
end function

--here
public function IsProperLengthAndRadix(TargetLength targetLength = defaultTargetLength, AtomRadix radix = defaultRadix)
	return (targetLength * power(radix - 1, 3) <= DOUBLE_INT_MAX)
--here
-- On 64-bit systems, long double has significand precision of 64 bits: DOUBLE_MAX = (power(2, 64) - 1) -- value: 18446744073709551615
-- On 32-bit systems, double has significand precision of 53 bits: DOUBLE_MAX = (power(2, 53) - 1) -- value: 9007199254740991
end function


-- Eun (type)
public type Eun(object x)
	if sequence(x) then
	if length(x) = 5 then
	if sequence(x[1]) then
	if integer(x[2]) then
	if integer(x[5]) then
	if integer(x[3]) then
	if atom(x[4]) then
		return IsProperLengthAndRadix(x[3], x[4])
	end if
	end if
	end if
	end if
	end if
	end if
	end if
	return 0
end type

public function NewEun(sequence num = {}, integer exp = 0, TargetLength targetLength = defaultTargetLength, AtomRadix radix = defaultRadix, integer flags = 0)
	return {num, exp, targetLength, radix, flags}
end function

public function EunAdjustRound(Eun n1, integer adjustBy = 0)
	if length(n1[1]) = 0 then
		return n1
	end if
	if adjustBy > 0 then
		integer tmp
		sequence s
		tmp = adjustRound
		adjustRound = adjustBy - 1
		s = AdjustRound(n1[1], n1[2], n1[3], n1[4])
		adjustRound = tmp
		return s
	end if
	return AdjustRound(n1[1], n1[2], n1[3], n1[4])
end function

public function RemoveLastDigits(Eun n1, PositiveInteger digits = 1)
	n1[1] = n1[1][1..$-digits]
	return n1
end function

-- EunMultiply
public function EunMultiply(Eun n1, Eun n2)
	TargetLength targetLength
	if n1[4] != n2[4] then
		call_proc(divideCallBackId, {5})
		return {}
	end if
	if n1[3] > n2[3] then
		targetLength = n1[3]
	else
		targetLength = n2[3]
	end if
	return MultiplyExp(n1[1], n1[2], n2[1], n2[2], targetLength, n1[4])
end function

public function EunSquare(Eun n1)
	return SquareExp(n1[1], n1[2], n1[3], n1[4])
end function

-- EunAdd
public function EunAdd(Eun n1, Eun n2)
	TargetLength targetLength
	if n1[4] != n2[4] then
		call_proc(divideCallBackId, {5})
		return {}
	end if
	if n1[3] > n2[3] then
		targetLength = n1[3]
	else
		targetLength = n2[3]
	end if
	return AddExp(n1[1], n1[2], n2[1], n2[2], targetLength, n1[4])
end function
-- EunSum
public function EunSum(sequence data)
	Eun sum
	sum = NewEun()
	for i = 1 to length(data) do
		sum = EunAdd(sum, data[i])
		sleep(nanosleep)
	end for
	return sum
end function
-- EunNegate
public function EunNegate(Eun n1)
	n1[1] = Negate(n1[1])
	return n1
end function
-- EunAbsoluteValue
public function EunAbsoluteValue(Eun n1)
	n1[1] = AbsoluteValue(n1[1])
	return n1
end function
-- EunSubtract
public function EunSubtract(Eun n1, Eun n2)
	TargetLength targetLength
	if n1[4] != n2[4] then
		call_proc(divideCallBackId, {5})
		return {}
	end if
	if n1[3] > n2[3] then
		targetLength = n1[3]
	else
		targetLength = n2[3]
	end if
	return SubtractExp(n1[1], n1[2], n2[1], n2[2], targetLength, n1[4])
end function
-- EunMultiplicativeInverse
public function EunMultiplicativeInverse(Eun n1, sequence guess = {})
	return MultiplicativeInverseExp(n1[1], n1[2], n1[3], n1[4], guess)
end function
-- EunDivide
public function EunDivide(Eun n1, Eun n2)
	TargetLength targetLength
	if n1[4] != n2[4] then
		call_proc(divideCallBackId, {5})
		return {}
	end if
	if n1[3] > n2[3] then
		targetLength = n1[3]
	else
		targetLength = n2[3]
	end if
	return DivideExp(n1[1], n1[2], n2[1], n2[2], targetLength, n1[4])
end function
-- EunConvert
public function EunConvert(Eun n1, atom toRadix, TargetLength targetLength)
	return ConvertExp(n1[1], n1[2], targetLength, n1[4], toRadix)
end function

integer equalLength = 0

public function CompareExp(sequence n1, integer exp1, sequence n2, integer exp2)
-- It doesn't look at targetLength or radix, so they should be the same.
-- still fixing, Fixed for negative values.
	integer minlen, f
	-- Case of zero (0)
	equalLength = 0
	if length(n1) = 0 then
		if length(n2) = 0 then
			return 0
		end if
		if n2[1] > 0 then
			return -1
		else
			return 1
		end if
		-- return iff(n2[1] > 0, -1, 1)
	end if
	if length(n2) = 0 then
		if n1[1] > 0 then
			return 1
		else
			return -1
		end if
		-- return iff(n1[1] > 0, 1, -1)
	end if
	-- Case of unequal signs (mismatch of signs, sign(n1) xor sign(n2))
	if n1[1] > 0 then
		if n2[1] < 0 then
			return 1
		end if
		-- both positive
		if exp1 != exp2 then
			return (exp1 > exp2) - (exp1 < exp2)
		end if
	else
		if n2[1] > 0 then
			return -1
		end if
		-- both negative
		if exp1 != exp2 then
			return (exp1 < exp2) - (exp1 > exp2)
		end if
	end if
	-- exponents and signs are the same:
	if length(n1) > length(n2) then
		n2 = n2 & {0} -- use zero as "sentinel" last digit
		minlen = length(n2)
	elsif length(n1) < length(n2) then
		n1 = n1 & {0} -- use zero as "sentinel" last digit
		minlen = length(n1)
	else
		minlen = length(n1)
	end if
	for i = 1 to minlen do
		f = (n1[i] > n2[i]) - (n1[i] < n2[i])
		if f then
			return f
		end if
		equalLength += 1
		sleep(nanosleep)
	end for
	return 0 -- numbers are equal
end function

public function GetEqualLength()
	return equalLength
end function

public function EunCompare(Eun n1, Eun n2)
	if n1[4] != n2[4] then
		return {}
	end if
	return CompareExp(n1[1], n1[2], n2[1], n2[2])
end function

public function EunReverse(Eun n1) -- reverse endian
	n1[1] = reverse(n1[1])
	return n1
end function

public function EunFracPart(Eun n1)
	integer len
	if n1[2] >= 0 then
		len = n1[2] + 1
		if len >= length(n1[1]) then
			n1[1] = {}
			n1[2] = 0
		else
			n1[1] = n1[1][len + 1..$]
			n1[2] = -1
		end if
	end if
	return n1
end function

public function EunIntPart(Eun n1)
	integer len
	len = n1[2] + 1
	if len < length(n1[1]) then
		if n1[2] < 0 then
			n1[1] = {}
			n1[2] = 0
		else
			n1[1] = n1[1][1..len]
		end if
	end if
	return n1
end function

public function EunRoundSig(Eun n1, PositiveScalar sigDigits = defaultTargetLength)
	TargetLength targetLength
	targetLength = n1[3]
	n1 = AdjustRound(n1[1], n1[2], sigDigits, n1[4])
	n1[3] = targetLength
	return n1
end function

public function EunRoundSignificantDigits(Eun n1, PositiveScalar sigDigits = defaultTargetLength)
	return EunRoundSig(n1, sigDigits)
end function

public function EunRoundToInt(Eun n1) -- Round to nearest integer
	if n1[2] < -1 then
		n1[1] = {}
		n1[2] = 0
	else
		n1 = EunRoundSig(n1, n1[2] + 1)
	end if
	return n1
end function

type UpOneRange(integer i)
	return i <= 1 and i >= -1
end type

public function EunCombInt(Eun n1, integer adjustBy = 0, UpOneRange upOne = 0) -- upOne should be: 1, 0, or -1
-- if there is any fraction part, add or subtract one, away from zero,
-- or add one towards positive infinity, if "up = 1"
	integer len, exponent
	n1 = EunAdjustRound(n1, adjustBy)
	len = length(n1[1])
	if len != 0 then
		exponent = n1[2]
		n1 = EunIntPart(n1)
		if exponent < 0 or exponent + 1 < len then
			-- add one, same sign
			if upOne = 0 then
				if n1[1][1] < 0 then
					upOne = -1
				else
					upOne = 1
				end if
			end if
			n1 = AddExp(n1[1], n1[2], {upOne}, 0, n1[3], n1[4])
		end if
	end if
	return n1
end function

public function EunModf(Eun fp)
-- similar to C's "modf()"
	return {EunIntPart(fp), EunFracPart(fp)}
end function

public function EunfDiv(Eun num, Eun den)
-- similar to C's "div()"
	-- returns quotient and remainder
	Eun div
	div = EunModf(EunDivide(num, den))
	div[2] = EunMultiply(div[2], den)
	return div
end function

public function EunfMod(Eun num, Eun den)
-- similar to C's "fmod()", just the "mod" or remainder
	return EunMultiply(EunFracPart(EunDivide(num, den)), den)
end function

--numio.e:

-- Compression functions to store an "Eun" in memory:

public function ToMemory(sequence n1, integer windows = FALSE, integer degrade = FALSE)
	integer offset, size, flag
	atom ma
	sequence n2
	if not Eun(n1) then
		n1 = ToEun(n1)
	end if
	n2 = EunConvert(n1, 256, 1 + Ceil(n1[3] * (log(n1[4]) / log(256))))
	n1[1] = length(n2[1])
	n1[2] = n2[2]
	n2 = n2[1]
	if length(n2) then
		if n2[1] < 0 then
			n1[3] = -n1[3] -- store sign information
			n2 = Negate(n2)
		end if
	end if
	flag = 1
ifdef BITS64 then
	if not degrade then
		flag = 0
		if windows then
			offset = 4 * 8 + 8
			size = offset + length(n2)
			ma = allocate_data(size)
			if ma = 0 then
				return 0 -- couldn't allocate data
			end if
			poke(ma, "eun" & 64 & "w   ") -- padded to 8-byte boundary
			poke8(ma + 8, n1[1..3])
			poke(ma + 4 * 8, atom_to_float64(n1[4]))
			poke(ma + offset, n2)
		else
			offset = 4 * 8 + 10
			size = offset + length(n2)
			ma = allocate_data(size)
			if ma = 0 then
				return 0 -- couldn't allocate data
			end if
			poke(ma, "eun" & 64 & "    ") -- padded to 8-byte boundary
			poke8(ma + 8, n1[1..3])
			poke(ma + 4 * 8, atom_to_float80(n1[4]))
			poke(ma + offset, n2)
		end if
	end if
end ifdef
	if flag then
		offset = 4 * 4 + 8
		size = offset + length(n2)
		ma = allocate_data(size)
		if ma = 0 then
			return 0 -- couldn't allocate data
		end if
		poke(ma, "eun" & 32)
		poke4(ma + 4, n1[1..3])
		poke(ma + 4 * 4, atom_to_float64(n1[4]))
		poke(ma + offset, n2)
	end if
	return {ma, size}
end function

public function FromMemoryToEun(atom ma)
	sequence n1, n2
	n1 = peek({ma, 4})
	if equal(n1, "eun" & 32) then
		n1 = peek4s({ma + 4, 3}) & float64_to_atom(peek({ma + 4 * 4, 8}))
		n2 = peek({ma + 4 * 4 + 8, n1[2]})
	else
ifdef BITS64 then
		n1 = peek({ma, 8})
		if equal(n1, "eun" & 64 & "    ") then
			n1 = peek8s({ma + 8, 3}) & float80_to_atom(peek({ma + 4 * 8, 10}))
			n2 = peek({ma + 4 * 8 + 10, n1[2]})
		elsif equal(n1, "eun" & 64 & "w   ") then
			n1 = peek8s({ma + 8, 3}) & float64_to_atom(peek({ma + 4 * 8, 8}))
			n2 = peek({ma + 4 * 8 + 8, n1[2]})
		else
			return 0 -- unsupported format
		end if
end ifdef
	end if
	if n1[3] < 0 then
		-- signed
		n1[3] = -n1[3]
		n2 = Negate(n2)
	end if
	n2 = NewEun(n2, n1[2], 1 + Ceil(n1[3] * (log(n1[4]) / log(256))), 256)
	n1 = EunConvert(n2, n1[4], n1[3])
	return n1
end function

public procedure FreeMemory(atom ma)
	free(ma)
end procedure

-- atom ma
-- 
-- adjustRound = 1
-- ma = ToMemory("-0.01234")
-- puts(1, ToString(FromMemoryToEun(ma)))
-- 
-- FreeMemory(ma)

public function ToString(object d, integer padWithZeros = FALSE)
-- converts an Eun or an atom to a string.
	if atom( d ) then
		-- dont change these:
ifdef BITS64 then
		if remainder( d, 1 ) or d >= 1e18 or d <= -1e18 then
			if padWithZeros then
				return sprintf( "%+0.17e", d ) -- 1e18, 17 is one less
			else
				return sprintf( "%+.17e", d ) -- 1e18, 17 is one less
			end if
		else
			return sprintf( "%+d", d ) -- can only do 18 decimal places for 80-bit, or 15 for 64-bit doubles
		end if
elsedef
		if remainder( d, 1 ) or d >= 1e15 or d <= -1e15 then
			if padWithZeros then
				return sprintf( "%+0.14e", d ) -- 1e15, 14 is one less
			else
				return sprintf( "%+.14e", d ) -- 1e15, 14 is one less
			end if
		else
			return sprintf( "%+d", d ) -- can only do 18 decimal places for 80-bit, or 15 for 64-bit doubles
		end if
end ifdef
	else
		sequence st
		integer f, len
		if sequence( d[1] ) then
			if d[4] != 10 then
				d = EunConvert( d, 10, Ceil((log(d[4]) / log(10)) * d[3]) )
			end if
			st = d[1]
			len = length( st )
			if len = 0 then
				if padWithZeros then
					return "0." & repeat('0', d[3] - 1) & "e" 
				else
					return "0"
				end if
			end if
			f = (st[1] < 0)
			if f then
				-- st = -st
				for i = 1 to len do
					st[i] = - (st[i])
					sleep(nanosleep)
				end for
			end if
			-- st += '0'
			for i = 1 to len do
				st[i] += '0'
				sleep(nanosleep)
			end for
			if padWithZeros then
				st = st & repeat('0', d[3] - len)
			end if
			if f then
				st = "-" & st
			elsif padWithZeros then
				st = "+" & st
				f = 1
			end if
		else
			if d[1] = 1 then -- (+infinity)
				return "inf"
			elsif d[1] = -1 then -- (-infinity)
				return "-inf"
			end if
		end if
		-- f = (st[1] = '-')
		f += 1 -- f is now 1 or 2
		if length( st ) > f then
			st = st[1..f] & "." & st[f + 1..length(st)]
		end if
		st = st & "e" & ToString( d[2] )
		return st
	end if
end function

-- converts to a floating point number
-- takes a string or a "Eun"
public function ToAtom(sequence s)
	if Eun(s) then
		s = ToString(s)
	end if
	s = value(s)
	if s[1] = GET_SUCCESS and atom(s[2]) then
		return s[2]
	end if
	return {} -- return empty sequence on error.
end function

public function StringToNumberExp(sequence st)
	integer isSigned, exp, f
	sequence ob
	if length(st) = 0 then
		return 0 -- undefined
	end if
	isSigned = ('-' = st[1])
	if isSigned or '+' = st[1] then
		st = st[2..length(st)]
	end if
	if equal(st, "inf") then
		-- returns values for +inf (1) and -inf (-1)
		if isSigned then
			return {-1, 0} -- represents negative infinity
		else
			return {1, 0} -- represents positive infinity
		end if
	end if
	f = find('e', st)
	if f = 0 then
		f = find('E', st)
	end if
	if f then
		ob = st[f+1..length(st)]
		ob = value( ob )
		if ob[1] != GET_SUCCESS then
			return 2 -- error in value() function
		end if
		exp = ob[2]
		st = st[1..f-1]
	else
		exp = 0
	end if
	while length(st) and st[1] = '0' do
		st = st[2..length(st)]
		sleep(nanosleep)
	end while
	f = find('.', st)
	if f then
		st = st[1..f - 1] & st[f + 1..length(st)]
		exp += (f - 2) -- 2 because f starts at 1. (1 if f starts at 0)
	else
		exp += (length(st) - 1)
	end if
	while length(st) and st[1] = '0' do
		exp -= 1
		st = st[2..length(st)]
		sleep(nanosleep)
	end while
	if length(st) = 0 then
		exp = 0
	end if
	-- st -= '0'
	for i = 1 to length(st) do
		st[i] -= '0'
		if st[i] > 9 or st[i] < 0 then
			return {0, 0}
		end if
		sleep(nanosleep)
	end for
	if isSigned then
		st = Negate(st)
	end if
	return {st, exp}
end function

public function ToEun(object s, AtomRadix radix = defaultRadix, TargetLength targetLength = defaultTargetLength)
-- Dropping support for atoms, use strings instead (strings are more accurate)
	if atom(s) then
		s = ToString(s)
	end if
	s = StringToNumberExp(s)
	s = s & {Ceil((log(radix) / logTen) * targetLength), 10, 0}
	if atom(s[1]) then
		return s
	end if
	if radix != 10 then
		s = EunConvert(s, radix, targetLength)
	end if
	return s
end function


-- Additions:

public function GetMoreAccuratePrec(Eun value1, PositiveScalar prec)
-- prec should be less than or equal to value1[3]
	return AdjustRound(value1[1], value1[2], prec + adjustRound, value1[4])
end function

public function EunGetPrec(Eun val)
	return val[3] - adjustRound
end function


-- Todo: Try to adjust the variables to give an accurate number.

public function EunTest(Eun val0, Eun ans)
	sequence val1, range
	-- val0 = EunMultiplicativeInvserse(val)
	val1 = ans
	val1[3] = val0[3]
	val1 = EunAdjustRound(val1)
	range = Equaln(val0[1], val1[1])
	return range
end function

-- Matrix support:
--
-- public type matrix(sequence s)
-- public function NewMatrix(integer rows, integer cols)
-- public function GetMatrixRows(sequence a)
-- public function GetMatrixCols(sequence a)
-- public function MatrixMultiply(matrix a, matrix b)
--
-- See also:
-- https://www.purplemath.com/modules/mtrxmult3.htm

public type matrix(sequence s)
	integer lenRows = length(s)
	if lenRows then
		integer lenCols
		lenCols = length(s[1])
		for i = 2 to lenRows do
			if length(s[i]) != lenCols then
				return 0
			end if
			sleep(nanosleep)
		end for
		return 1
	end if
	return 0
end type

public function NewMatrix(integer rows, integer cols)
	return repeat(repeat(NewEun(), cols), rows)
end function

public function GetMatrixRows(sequence a)
	return length(a)
end function

public function GetMatrixCols(sequence a)
	return length(a[1])
end function

public function MatrixMultiply(matrix a, matrix b)
-- ret[i] =
-- {
--  a[i][k1] * b[k1][j1] + a[i][k2] * b[k2][j1],
--  a[i][k1] * b[k1][j2] + a[i][k2] * b[k2][j2],
--  a[i][k1] * b[k1][j3] + a[i][k2] * b[k2][j3],
--  a[i][k1] * b[k1][j4] + a[i][k2] * b[k2][j4]
-- }
-- row0 = ret[i]
-- row1 = a[i]
	integer rows, cols, len
	sequence row0, row1, sum, ret
	-- Eun sum
	-- matrix ret
	len = GetMatrixRows(b)
	if GetMatrixCols(a) != len then
		puts(1, "Error(8):  In MyEuNumber, in MatrixMultiply(), column-row mix-match.\n  See file: ex.err\n")
		abort(1/0)
	end if
	rows = GetMatrixRows(a)
	cols = GetMatrixCols(b)
	ret = NewMatrix(rows, cols)
	for i = 1 to rows do -- rows of "a"
		row0 = ret[i]
		row1 = a[i]
		for j = 1 to cols do -- cols of "b"
			sum = NewEun()
			for k = 1 to len do -- k is cols of "a", rows of "b"
				sum = EunAdd(sum, EunMultiply(row1[k], b[k][j]))
				sleep(nanosleep)
			end for
			row0[j] = sum
			sleep(nanosleep)
		end for
		ret[i] = row0
		sleep(nanosleep)
	end for
	return ret
end function

public function MatrixTransformation(matrix a)
	integer rows, cols
	sequence ret, tmp
	rows = GetMatrixRows(a)
	cols = GetMatrixCols(a)
	ret = NewMatrix(cols, rows)
	for row = 1 to rows do
		tmp = a[row]
		for col = 1 to cols do
			ret[col][row] = tmp[col]
			sleep(nanosleep)
		end for
		sleep(nanosleep)
	end for
	return ret
end function

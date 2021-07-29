-- Copyright (c) 2016-2021 James Cook

-- Compiles into a dll

-- Can be used by either 32-bit or 64-bit "euc" to make it into a DLL:

-- Example for Watcom 32-bit:
-- C:\> euc -o libmyeun.dll -strict -dll -wat -keep libeun.e
-- Example for GCC:
-- C:\> euc -o libmyeun.dll -strict -dll -gcc -keep libeun.e

-- NOTE: Don't mix ids of different types.

include allocate.e
include dll.e
include classfile.e as pointers
include classfile.e as numArray
include classfile.e as eun
include classfile.e as complex
include classfile.e as matrix
include myeunumber.e as my

public function UsingHowManyBits()
ifdef BITS64 then
	return 64
elsedef
	return 32
end ifdef
end function

-- Pointers:

public function RegisterPointer(integer low, integer high)
-- Register your client application's pointer (in client/server model), so the DLL can use it.
-- Takes two 16-bit unsigned integers (low and high), to represent a complete 32-bit unsigned integer
-- returned as an id to be used as a pointer in other functions
ifdef BITS64 then
	return pointers:new_object_from_data(and_bits(low, #FFFFFFFF) + and_bits(high, #FFFFFFFF) * #100000000)
elsedef
	return pointers:new_object_from_data(and_bits(low, #FFFF) + and_bits(high, #FFFF) * #10000)
end ifdef
end function

public procedure UnRegisterPointer(integer id)
	pointers:delete_object(id)
end procedure

public function NewPointer()
-- In case you don't want to register one of your pointers, it will allocate one of its own for you (server-side).
ifdef BITS64 then
	atom pointer = allocate_data(8)
	if pointer = 0 then
		return 0
	end if
	poke8(pointer, 0)
elsedef
	atom pointer = allocate_data(4)
	if pointer = 0 then
		return 0
	end if
	poke4(pointer, 0)
end ifdef
	return pointers:new_object_from_data(pointer)
end function

public procedure CopyPointerToAddress(integer dstId, integer srcId)
ifdef BITS64 then
	poke8(pointers:get_data_from_object(dstId), pointers:get_data_from_object(srcId))
elsedef
	poke4(pointers:get_data_from_object(dstId), pointers:get_data_from_object(srcId))
end ifdef
end procedure

public procedure FreePointer(integer id)
-- Free one of the DLL's pointers and unregister it.
	atom ma = pointers:get_data_from_object(id)
	free(ma)
	-- It points to nothing now, so un-register pointer.
	UnRegisterPointer(id)
end procedure

-- NumArrays:

public function NewNumArrayFromCIntArray(integer id_of_ma, integer how_many)
ifdef BITS64 then
	sequence num = peek8s({pointers:get_data_from_object(id_of_ma), how_many})
elsedef
	sequence num = peek4s({pointers:get_data_from_object(id_of_ma), how_many})
end ifdef
	return numArray:new_object_from_data(num)
end function

public function NewCIntArrayFromNumArray(integer pointer_dstId, integer idOfNumArray)
	sequence num = numArray:get_data_from_object(idOfNumArray)
ifdef BITS64 then
	atom ma = allocate_data(length(num) * 8) -- 64bit integers are 8 bytes in size
	if ma = 0 then
		return 0
	end if
	poke8(ma, num)
	poke8(pointers:get_data_from_object(pointer_dstId), ma)
elsedef
	atom ma = allocate_data(length(num) * 4) -- 32bit integers are 4 bytes in size
	if ma = 0 then
		return 0
	end if
	poke4(ma, num)
	poke4(pointers:get_data_from_object(pointer_dstId), ma)
end ifdef
	return length(num)
end function

public procedure FreeNewCIntArrayFromNumArray(integer pointer_dstId)
ifdef BITS64 then
	atom ma = peek8u(pointers:get_data_from_object(pointer_dstId))
elsedef
	atom ma = peek4u(pointers:get_data_from_object(pointer_dstId))
end ifdef
	free(ma)
end procedure

public procedure DeleteNumArray(integer id)
	numArray:delete_object(id)
end procedure

public function CloneNumArray(integer id)
	return numArray:clone_object(id)
end function

public procedure CopyNumArray(integer dstId, integer srcId)
	numArray:replace_object(dstId, numArray:get_data_from_object(srcId))
end procedure

public function GetLengthOfNumArray(integer id)
	return length(numArray:get_data_from_object(id))
end function

-- Eun's:

function GetEun(integer id)
	return eun:get_data_from_object(id)
end function

public function IsEun(integer n1)
	return my:Eun(GetEun(n1))
end function

function NewFromEun(object n1)
	if my:Eun(n1) then
		return eun:new_object_from_data(n1)
	else
		return 0
	end if
end function

public function NewEun(integer arrayid, integer exp, integer targetLength, integer radix, Bool sign_of_exp)
	if sign_of_exp then
		exp = -(exp)
	end if
	return NewFromEun(my:NewEun(numArray:get_data_from_object(arrayid), exp, targetLength, radix))
end function

public function NewEunWithPointer(integer arrayid, integer signedExponentPointerId, integer targetLength, integer radix)
	ifdef BITS64 then
		return NewFromEun(my:NewEun(numArray:get_data_from_object(arrayid), peek8s(pointers:get_data_from_object(signedExponentPointerId)), targetLength, radix))
	elsedef
		return NewFromEun(my:NewEun(numArray:get_data_from_object(arrayid), peek4s(pointers:get_data_from_object(signedExponentPointerId)), targetLength, radix))
	end ifdef
end function

public procedure DeleteEun(integer id)
	eun:delete_object(id)
end procedure

public procedure StoreEun(integer id_dst, integer id_src) -- move operator
	eun:store_object(id_dst, id_src)
end procedure

public function CloneEun(integer id)
	return eun:clone_object(id)
end function

-- Access Data Members:
public function GetEunArray(integer id)
	sequence n1 = GetEun(id)
	integer arrayId = numArray:new_object_from_data(n1[1])
	return arrayId
end function

public procedure GetEunExponent(integer dstptrId, integer eunId)
	sequence n1 = GetEun(eunId)
ifdef BITS64 then
	poke8(pointers:get_data_from_object(dstptrId), n1[2])
elsedef
	poke4(pointers:get_data_from_object(dstptrId), n1[2])
end ifdef
end procedure

public function GetEunRadix(integer id)
	sequence n1 = GetEun(id)
	return n1[3]
end function

public function GetEunTargetLength(integer id)
	sequence n1 = GetEun(id)
	return n1[4]
end function

public procedure GetEunFlags(integer dstptrId, integer eunId) -- get rounding information
	sequence n1 = GetEun(eunId)
ifdef BITS64 then
	poke8(pointers:get_data_from_object(dstptrId), n1[5])
elsedef
	poke4(pointers:get_data_from_object(dstptrId), n1[5])
end ifdef
end procedure

public function EunToCString(integer id, integer isPadWithZeros) -- isPadWithZeros can be 0 or 1
	sequence st = my:ToString(GetEun(id), isPadWithZeros)
	atom ma = allocate_string(st)
	if ma = 0 then
		return 0
	end if
	return pointers:new_object_from_data(ma)
	-- be sure to call "FreePointer()" after you are done with it
end function

public function CStringToEun(integer id_ma, integer radix, integer targetLength)
	sequence st = peek_string(pointers:get_data_from_object(id_ma))
	return my:ToEun(st, radix, targetLength)
end function

function match_replace(object needle, sequence haystack, object replacement, 
			integer max=0)
	integer posn
	integer needle_len
	integer replacement_len
	integer scan_from
	integer cnt
	
	
	if max < 0 then
		return haystack
	end if
	
	cnt = length(haystack)
	if max != 0 then
		cnt = max
	end if
	
	if atom(needle) then
		needle = {needle}
	end if
	if atom(replacement) then
		replacement = {replacement}
	end if

	needle_len = length(needle) - 1
	replacement_len = length(replacement)

	scan_from = 1
	while posn with entry do
		haystack = replace(haystack, replacement, posn, posn + needle_len)

		cnt -= 1
		if cnt = 0 then
			exit
		end if
		scan_from = posn + replacement_len
	entry
		posn = match(needle, haystack, scan_from)
		sleep(nanosleep)
	end while

	return haystack
end function


public function EunPrintToCString(integer x)
	sequence st = pretty_sprint(GetEun(x), {0, 8}) -- 8 spaces for indent
	atom ma
	st = match_replace("\n", st, "\r\n")
	ma = allocate_string(st)
	if ma = 0 then
		return 0
	end if
	return pointers:new_object_from_data(ma)
	-- be sure to call "FreePointer()" after you are done with it
end function

-- Complex's:

function NewFromComplex(Complex c1)
	return complex:new_object_from_data(c1)
end function

public function NewComplex(integer realEunId, integer imaginaryEunId)
	return NewFromComplex(my:NewComplex(GetEun(realEunId), GetEun(imaginaryEunId)))
end function

public procedure DeleteComplex(integer id)
	complex:delete_object(id)
end procedure

function GetComplex(integer id)
	return complex:get_data_from_object(id)
end function

public procedure StoreComplex(integer id_dst, integer id_src)
	complex:store_object(id_dst, id_src)
end procedure

public function CloneComplex(integer id)
	return complex:clone_object(id)
end function

-- Access Data Members:

public function GetRealEun(integer complexId)
	sequence s = GetComplex(complexId)
	return NewFromEun(s[my:REAL])
end function

public function GetImaginaryEun(integer complexId)
	sequence s = GetComplex(complexId)
	return NewFromEun(s[my:IMAG])
end function

-- MyEuNumber functions:

-- list functions to export.

function new_doubleAtom(atom dbl)
	atom pointer
	sequence s
	pointer = allocate_data(8)
	if pointer = 0 then
		return 0
	end if
	s = atom_to_float64(dbl)
	poke(pointer, s)
	return pointers:new_object_from_data(pointer)
	-- use FreePointer() to free this data.
end function

function get_doubleAtom(integer pointer_to_double)
	return float64_to_atom(peek({pointers:get_data_from_object(pointer_to_double), 8}))
end function

public function GetNanoSleep()
-- returns a float64, or a C/C++ "double"
-- nanosleep is a fraction of a second (useful on Linux to make it run cooler); or -1 to disable sleep (this will make it run hot on Linux)
	atom dbl
	dbl = my:GetNanoSleep()
	return new_doubleAtom(dbl)
end function

public procedure SetNanoSleep(integer pointer_to_double) -- takes the registered pointer to a double, it should be a fraction of a second.
	atom d
	d = get_doubleAtom(pointer_to_double)
	my:SetNanoSleep(d)
end procedure

public function GetVersion()
	return my:GetVersion()
end function

public function GetDivideByZeroFlag()
	return my:GetDivideByZeroFlag()
end function

public procedure SetDivideByZeroFlag(integer i)
	my:SetDivideByZeroFlag(i)
end procedure

-- public Bool zeroDividedByZeroFlag = TRUE -- if true, zero divided by zero returns one (0/0 = 1)

public function GetZeroDividedByZeroFlag()
	return my:GetZeroDividedByZeroFlag()
end function

public procedure SetZeroDividedByZeroFlag(integer i)
	my:SetZeroDividedByZeroFlag(i)
end procedure

public procedure SetIsRoundToZero(integer i)
	my:SetIsRoundToZero(i)
end procedure

public function GetIsRoundToZero()
	return my:GetIsRoundToZero()
end function

-- More Accuracy variables:

public procedure SetMultiplicativeInverseMoreAccuracy(integer i)
	my:SetMultiplicativeInverseMoreAccuracy(i - 1)
end procedure

public function GetMultiplicativeInverseMoreAccuracy()
	return my:GetMultiplicativeInverseMoreAccuracy() + 1
end function

public procedure SetNthRootMoreAccuracy(integer i)
	my:SetNthRootMoreAccuracy(i - 1)
end procedure

public function GetNthRootMoreAccuracy()
	return my:GetNthRootMoreAccuracy() + 1
end function

public procedure SetExpMoreAccuracy(integer i)
	my:SetExpMoreAccuracy(i - 1)
end procedure

public function GetExpMoreAccuracy()
	return my:GetExpMoreAccuracy() + 1
end function

public procedure SetLogMoreAccuracy(integer i)
	my:SetLogMoreAccuracy(i - 1)
end procedure

public function GetLogMoreAccuracy()
	return my:GetLogMoreAccuracy() + 1
end function

public procedure SetSinMoreAccuracy(integer i)
	my:SetSinMoreAccuracy(i - 1)
end procedure

public function GetSinMoreAccuracy()
	return my:GetSinMoreAccuracy() + 1
end function

public procedure SetCosMoreAccuracy(integer i)
	my:SetCosMoreAccuracy(i - 1)
end procedure

public function GetCosMoreAccuracy()
	return my:GetCosMoreAccuracy() + 1
end function

public procedure SetArcSinMoreAccuracy(integer i)
	my:SetArcSinMoreAccuracy(i - 1)
end procedure

public function GetArcSinMoreAccuracy()
	return my:GetArcSinMoreAccuracy() + 1
end function

public procedure SetArcTanMoreAccuracy(integer i)
	my:SetArcTanMoreAccuracy(i - 1)
end procedure

public function GetArcTanMoreAccuracy()
	return my:GetArcTanMoreAccuracy() + 1
end function

-- End of More Accuracy Variables.

public procedure SetDefaultTargetLength(integer i)
	my:SetDefaultTargetLength(i)
end procedure

public function GetDefaultTargetLength()
	return my:GetDefaultTargetLength()
end function

public procedure SetDefaultRadix(integer i)
	my:SetDefaultRadix(i)
end procedure

public function GetDefaultRadix()
	return my:GetDefaultRadix()
end function

public procedure SetAdjustRound(integer i)
	my:SetAdjustRound(i)
end procedure

public function GetAdjustRound()
	return my:GetAdjustRound()
end function

public procedure SetCalcSpeed(integer ptrToDblId)
	atom speed
	speed = get_doubleAtom(ptrToDblId)
	my:SetCalcSpeed(speed)
end procedure

public function GetCalcSpeed()
	atom dbl
	dbl = my:GetCalcSpeed()
	return new_doubleAtom(dbl)
end function

public procedure SetRoundToNearestOption(integer boolean_value_num)
	my:SetRoundToNearestOption(boolean_value_num)
end procedure

public function GetRoundToNearestOption()
	return my:GetRoundToNearestOption()
end function

public procedure IntegerModeOn()
	my:IntegerModeOn()
end procedure

public procedure IntegerModeOff()
	my:IntegerModeOff()
end procedure

public function GetRoundInf() -- = 1 -- Round towards +infinity or -infinity, (positive or negative infinity)
	return my:ROUND_INF
end function

public function GetRoundZero() -- = 2 -- Round towards zero
	return my:ROUND_ZERO
end function

public function GetRoundTruncate() -- = 3 -- Don't round, truncate
	return my:ROUND_TRUNCATE
end function

public function GetRoundPosInf() -- = 4 -- Round towards positive +infinity
	return my:ROUND_POS_INF
end function

public function GetRoundNegInf() -- = 5 -- Round towards negative -infinity
	return my:ROUND_NEG_INF
end function

public function GetRoundEven() -- = 6 -- Round making number even on halfRadix
	return my:ROUND_EVEN
end function

public function GetRoundOdd() -- = 7 -- Round making number odd on halfRadix
	return my:ROUND_ODD
end function

public procedure SetRound(integer i)
	my:SetRound(i)
end procedure

public function GetRound()
	return my:GetRound()
end function

public function GetMultInvIter()
	return my:iter
end function
public procedure SetMultInvIter(integer i)
	my:iter = i
end procedure
public function GetMultInvLastIterCount()
	return my:lastIterCount
end function
public procedure SetMultInvLastIterCount(integer i)
	my:lastIterCount = i
end procedure

public function GetNthRootIter()
	return my:nthRootIter
end function
public procedure SetNthRootIter(integer i)
	my:nthRootIter = i
end procedure
public function GetLastNthRootIter()
	return my:lastNthRootIter
end function
public procedure SetLastNthRootIter(integer i)
	my:lastNthRootIter = i
end procedure

public function GetArcTanIter()
	return my:arcTanIter
end function
public procedure SetArcTanIter(integer i)
	my:arcTanIter = i
end procedure
public function GetLastArcTanCount()
	return my:arcTanCount
end function
public procedure SetLastArcTanCount(integer i)
	my:arcTanCount = i
end procedure

public function GetExp1Iter()
	return my:expExp1Iter
end function
public procedure SetExp1Iter(integer i)
	my:expExp1Iter = i
end procedure

public function GetExpIter()
	return my:expExpIter
end function
public procedure SetExpIter(integer i)
	my:expExpIter = i
end procedure
public function GetLastExpIterCount()
	return my:expExpCount
end function
public procedure SetLastExpIterCount(integer i)
	my:expExpCount = i
end procedure

public function GetExpFastIter()
	return my:GetExpFastIter()
end function
public procedure SetExpFastIter(integer i)
	my:SetExpFastIter(i)
end procedure

public function GetLogIter()
	return my:logIter
end function
public procedure SetLogIter(integer i)
	my:logIter = i
end procedure
public function GetLastLogIterCount()
	return my:logIterCount
end function
public procedure SetLastLogIterCount(integer i)
	my:logIterCount = i
end procedure

public function GetSinIter()
	return my:sinIter
end function
public procedure SetSinIter(integer i)
	my:sinIter = i
end procedure
public function GetLastSinIterCount()
	return my:sinIterCount
end function
public procedure SetLastSinIterCount(integer i)
	my:sinIterCount = i
end procedure

public function GetCosIter()
	return my:cosIter
end function
public procedure SetCosIter(integer i)
	my:cosIter = i
end procedure
public function GetLastCosIterCount()
	return my:cosIterCount
end function
public procedure SetLastCosIterCount(integer i)
	my:cosIterCount = i
end procedure

public function GetArcSinIter()
	return my:arcSinIter
end function
public procedure SetArcSinIter(integer i)
	my:arcSinIter = i
end procedure
public function GetLastArcSinIterCount()
	return my:arcSinIterCount
end function
public procedure SetLastArcSinIterCount(integer i)
	my:arcSinIterCount = i
end procedure

-- MyEuNumber Functions:

public function Min(integer a, integer b)
	if a < b then
		return a
	else
		return b
	end if
end function

public function Max(integer a, integer b)
	if a > b then
		return a
	else
		return b
	end if
end function

public function RoundTowardsZero(integer dblId)
	atom dbl
	dbl = get_doubleAtom(dblId)
	dbl = my:RoundTowardsZero(dbl)
	return new_doubleAtom(dbl)
end function

public function Round(integer dblId1, integer dblId2)
	atom result
	result = my:Round(get_doubleAtom(dblId1), get_doubleAtom(dblId2))
	return new_doubleAtom(result)
end function

public function RangeEqual(integer a, integer b, integer start, integer stop)
	return my:RangeEqual(numArray:get_data_from_object(a), numArray:get_data_from_object(b), start, stop)
end function

public function Equaln(integer a, integer b)
	sequence s
	s = Equaln(numArray:get_data_from_object(a), numArray:get_data_from_object(b))
	return s[1]
end function

public function IsIntegerOdd(integer i)
	return my:IsIntegerOdd(i)
end function

public function IsIntegerEven(integer i)
	return my:IsIntegerEven(i)
end function

public function Borrow(integer num, integer radix)
	return numArray:new_object_from_data(my:Borrow(numArray:get_data_from_object(num), radix))
end function

public function NegativeBorrow(integer num, integer radix)
	return numArray:new_object_from_data(my:NegativeBorrow(numArray:get_data_from_object(num), radix))
end function

public function Carry(integer num, integer radix)
	return numArray:new_object_from_data(my:Carry(numArray:get_data_from_object(num), radix))
end function

public function NegativeCarry(integer num, integer radix)
	return numArray:new_object_from_data(my:NegativeCarry(numArray:get_data_from_object(num), radix))
end function

public function Add(integer n1, integer n2)
	return numArray:new_object_from_data(my:Add(numArray:get_data_from_object(n1), numArray:get_data_from_object(n2)))
end function

public function Subtr(integer n1, integer n2)
	return numArray:new_object_from_data(my:Subtr(numArray:get_data_from_object(n1), numArray:get_data_from_object(n2)))
end function

public function ConvertRadix(integer num, integer fromRadix, integer toRadix)
	return numArray:new_object_from_data(my:ConvertRadix(numArray:get_data_from_object(num), fromRadix, toRadix))
end function

public function Multiply(integer n1, integer n2, integer sumOfLengths, integer flag)
	object ob1, ob2, val -- val for "value"
	ob1 = numArray:get_data_from_object(n1)
	ob2 = numArray:get_data_from_object(n2)
	if flag then
		val = my:Multiply(ob1, ob2)
	else
		val = my:Multiply(ob1, ob2, sumOfLengths - 1)
	end if
	return numArray:new_object_from_data(val)
end function

public function Square(integer n1)
	return numArray:new_object_from_data(my:Square(numArray:get_data_from_object(n1)))
end function

public function IsNegative(integer numId)
	return my:IsNegative(numArray:get_data_from_object(numId))
end function

public function Negate(integer num)
	return numArray:new_object_from_data(my:Negate(numArray:get_data_from_object(num)))
end function

public function AbsoluteValue(integer num)
	return numArray:new_object_from_data(my:AbsoluteValue(numArray:get_data_from_object(num)))
end function

public function Subtract(integer num, integer radix, integer isMixed)
	return numArray:new_object_from_data(my:Subtract(numArray:get_data_from_object(num), radix, isMixed))
end function

public function TrimLeadingZeros(integer num)
	return numArray:new_object_from_data(my:TrimLeadingZeros(numArray:get_data_from_object(num)))
end function

public function TrimTrailingZeros(integer num)
	return numArray:new_object_from_data(my:TrimTrailingZeros(numArray:get_data_from_object(num)))
end function

public function AdjustRound(integer num, integer exponent, integer targetLength, integer radix, integer optionsIsMixed, Bool isNegExp)
	-- default optionsIsMixed is 1, can be: (0, 1, 2)
	if isNegExp then
		exponent = - (exponent)
	end if
	return NewFromEun(my:AdjustRound(numArray:get_data_from_object(num), exponent, targetLength, radix, optionsIsMixed))
end function

--here, make Exp functions have flags for "exp1 and exp2"'s sign.


public function MultiplyExp(integer n1, integer exp1, integer n2, integer exp2, integer targetLength, integer radix, Bool isNegExp1, Bool isNegExp2)
	if isNegExp1 then
		exp1 = - (exp1)
	end if
	if isNegExp2 then
		exp2 = - (exp2)
	end if
	return NewFromEun(my:MultiplyExp(numArray:get_data_from_object(n1), exp1, numArray:get_data_from_object(n2), exp2, targetLength, radix))
end function

public function SquareExp(integer n1, integer exp1, integer targetLength, integer radix, Bool isNegExp1)
	if isNegExp1 then
		exp1 = - (exp1)
	end if
	return NewFromEun(my:SquareExp(numArray:get_data_from_object(n1), exp1, targetLength, radix))
end function

public function AddExp(integer n1, integer exp1, integer n2, integer exp2, integer targetLength, integer radix, Bool isNegExp1, Bool isNegExp2)
	if isNegExp1 then
		exp1 = - (exp1)
	end if
	if isNegExp2 then
		exp2 = - (exp2)
	end if
	return NewFromEun(my:AddExp(numArray:get_data_from_object(n1), exp1, numArray:get_data_from_object(n2), exp2, targetLength, radix))
end function

public function SubtractExp(integer n1, integer exp1, integer n2, integer exp2, integer targetLength, integer radix, Bool isNegExp1, Bool isNegExp2)
	if isNegExp1 then
		exp1 = - (exp1)
	end if
	if isNegExp2 then
		exp2 = - (exp2)
	end if
	return NewFromEun(my:SubtractExp(numArray:get_data_from_object(n1), exp1, numArray:get_data_from_object(n2), exp2, targetLength, radix))
end function

procedure dummy(integer i)
	-- procedure with no code in its body.
end procedure

my:divideCallBackId = routine_id("dummy")

public function MultiplicativeInverseExp(integer den1, integer exp1, integer targetLength, integer radix, integer guess, Bool isNegExp1)
	sequence s = {}
	-- guess is 0, when you want it to guess the result instead of you supplying the guess.
	if guess > 0 then
		s = numArray:get_data_from_object(guess)
	end if
	if isNegExp1 then
		exp1 = - (exp1)
	end if
	s = my:MultiplicativeInverseExp(numArray:get_data_from_object(den1), exp1, targetLength, radix, s)
	return NewFromEun(s)
end function
--here
public function DivideExp(integer num1, integer exp1, integer den2, integer exp2, integer targetLength, integer radix, Bool isNegExp1, Bool isNegExp2)
	sequence s
	if isNegExp1 then
		exp1 = - (exp1)
	end if
	if isNegExp2 then
		exp2 = - (exp2)
	end if
	s = my:DivideExp(numArray:get_data_from_object(num1), exp1, numArray:get_data_from_object(den2), exp2, targetLength, radix)
	return NewFromEun(s)
end function

public function ConvertExp(integer n1, integer exp1, integer targetLength, integer fromRadix, integer toRadix, Bool isNegExp1)
	sequence s
	if isNegExp1 then
		exp1 = - (exp1)
	end if
	s = my:ConvertExp(numArray:get_data_from_object(n1), exp1, targetLength, fromRadix, toRadix)
	return NewFromEun(s)
end function

public function IsProperLengthAndRadix(integer targetLength, integer radix)
	return my:IsProperLengthAndRadix(targetLength, radix)
end function


-- Begin Eun:

public function EunAdjustRound(integer n1, integer adjustBy) -- if adjustBy = 0 then use default adjustRound
	return NewFromEun(my:EunAdjustRound(GetEun(n1), adjustBy))
end function

public procedure RemoveLastDigits(integer n1, integer digits)
	eun:privateData[n1][1] = eun:privateData[n1][1][1..$-digits]
end procedure

public function EunMultiply(integer n1, integer n2)
	return NewFromEun(my:EunMultiply(GetEun(n1), GetEun(n2)))
end function

public function EunSquare(integer n1)
	return NewFromEun(my:EunSquare(GetEun(n1)))
end function

public function EunAdd(integer n1, integer n2)
	return NewFromEun(my:EunAdd(GetEun(n1), GetEun(n2)))
end function

public function EunNegate(integer n1)
	return NewFromEun(my:EunNegate(GetEun(n1)))
end function

public function EunAbsoluteValue(integer n1)
	return NewFromEun(my:EunAbsoluteValue(GetEun(n1)))
end function

public function EunSubtract(integer n1, integer n2)
	return NewFromEun(my:EunSubtract(GetEun(n1), GetEun(n2)))
end function

public function EunMultiplicativeInverse(integer n1)
	return NewFromEun(my:EunMultiplicativeInverse(GetEun(n1)))
end function

public function EunMultiplicativeInverseGuess(integer n1, integer array_guess_id)
	return NewFromEun(my:EunMultiplicativeInverse(GetEun(n1), numArray:get_data_from_object(array_guess_id)))
end function

public function EunDivide(integer n1, integer n2)
	return NewFromEun(my:EunDivide(GetEun(n1), GetEun(n2)))
end function

public function EunConvert(integer n1, integer toRadix, integer targetLength)
	return NewFromEun(my:EunConvert(GetEun(n1), toRadix, targetLength))
end function

public function CompareExp(sequence n1, integer exp1, sequence n2, integer exp2, Bool isNegExp1, Bool isNegExp2)
	if isNegExp1 then
		exp1 = - (exp1)
	end if
	if isNegExp2 then
		exp2 = - (exp2)
	end if
	return my:CompareExp(numArray:get_data_from_object(n1), exp1, numArray:get_data_from_object(n2), exp2) + 1
end function

public function GetEqualLength()
	return my:GetEqualLength()
end function

public function EunCompare(integer n1, integer n2)
	return my:EunCompare(GetEun(n1), GetEun(n2)) + 1
end function

public function EunReverse(integer n1) -- reverse endian
	return NewFromEun(my:EunReverse(GetEun(n1)))
end function

public function EunFracPart(integer n1)
	return NewFromEun(my:EunFracPart(GetEun(n1)))
end function

public function EunIntPart(integer n1)
	return NewFromEun(my:EunIntPart(GetEun(n1)))
end function

public function EunRoundSig(integer n1, integer sigDigits)
	return NewFromEun(my:EunRoundSig(GetEun(n1), sigDigits))
end function

public function EunRoundToInt(integer n1) -- Round to nearest integer
	return NewFromEun(my:EunRoundToInt(GetEun(n1)))
end function

public function EunCombInt(integer n1, integer adjustBy, integer up) -- values for "up" can be: 0, 1, 2
	return NewFromEun(my:EunCombInt(GetEun(n1), adjustBy, up - 1))
end function

public procedure EunModf(integer dstEunIdIntPart, integer dstEunIdFracPart, integer numId)
-- EunModf(Eun fp) -- similar to C's "modf()"
	sequence s = my:EunModf(GetEun(numId))
	eun:replace_object(dstEunIdIntPart, s[1])
	eun:replace_object(dstEunIdFracPart, s[2])
end procedure

public procedure EunfDiv(integer dstEunIdQuot, integer dstEunIdRem, integer numId, integer denId)
-- EunfDiv(Eun num, Eun den) -- similar to C's "div()"
	sequence s = my:EunfDiv(GetEun(numId), GetEun(denId))
	eun:replace_object(dstEunIdQuot, s[1])
	eun:replace_object(dstEunIdRem, s[2])
end procedure

public function EunfMod(integer numId, integer denId)
-- EunfMod(Eun num, Eun den) -- similar to C's "fmod()", just the "mod" or remainder
	return NewFromEun(my:EunfMod(GetEun(numId), GetEun(denId)))
end function

-- numio.e functions:

public function EunToMemory(integer n1)
	-- returns id to a pointer
	-- remember to use "FreePointer()" to deallocate it.
ifdef WINDOWS then
	atom ma = my:ToMemory(GetEun(n1), 1)
elsedef
	atom ma = my:ToMemory(GetEun(n1))
end ifdef
	return pointers:new_object_from_data(ma)
end function

public function FromMemoryToEun(integer pointer_id)
	sequence n1 = my:FromMemoryToEun(pointers:get_data_from_object(pointer_id))
	return NewFromEun(n1)
end function

-- NthRoot functions:

public function GetAdjustPrecision()
	return my:GetAdjustPrecision()
end function

public procedure SetAdjustPrecision(integer i)
	my:SetAdjustPrecision(i)
end procedure

public procedure SetRealMode(integer i)
	my:SetRealMode(i)
end procedure

public function GetRealMode()
	return my:GetRealMode()
end function

public function EunIntPower(integer toPower, integer id)
	sequence n1 = GetEun(id)
	return NewFromEun(my:IntPowerExp(toPower, n1[1], n1[2], n1[3], n1[4]))
end function

public function NthRootExp(PositiveScalar n, integer x1, integer exp1, integer guess, integer exp2, integer targetLength, integer radix, Bool isNegExp1, Bool isNegExp2)
	sequence n1, n2
	if isNegExp1 then
		exp1 = - (exp1)
	end if
	if isNegExp2 then
		exp2 = - (exp2)
	end if
	n1 = numArray:get_data_from_object(x1)
	n2 = numArray:get_data_from_object(guess)
	return NewFromEun(my:NthRootExp(n, n1, exp1, n2, exp2, targetLength, radix))
end function

public function EunNthRoot(integer dstEunId, integer dstExtraEunId, integer n, integer n1, integer guess1) -- guess can be zero (0)
	-- returns if result is imaginary, or multiplied by sqrt(-1)
	sequence s
	s = GetEun(n1)
	if guess1 = 0 then
		s = my:EunNthRoot(n, s)
	else
		s = my:EunNthRoot(n, s, GetEun(guess1))
	end if
	if length(s) = 3 then
		eun:replace_object(dstEunId, s[2])
		eun:replace_object(dstExtraEunId, s[3])
		return s[1]
	else
		eun:replace_object(dstEunId, s)
		if dstExtraEunId then
			eun:replace_object(dstExtraEunId, {}) -- {} is null in this case.
		end if
		return 0
	end if
end function

public function EunSquareRoot(integer dstEunId, integer dstExtraEunId, integer n1)
	return EunNthRoot(dstEunId, dstExtraEunId, 2, n1, 0)
end function

public procedure EunCubeRoot(integer dstEunId, integer n1)
	-- results are never imaginary for "EunCubeRoot()"
	-- return result in "dstEunId" just like the other NthRoot functions.
	-- treat function as procedure:
	if EunNthRoot(dstEunId, NULL, 3, n1, 0) then
	end if
end procedure

-- mymath.e functions:

-- ArcTan:

public function EunArcTan(integer n1)
	return NewFromEun(my:EunArcTan(GetEun(n1)))
end function

-- Exp: e^(n1)

public function EunExp(integer n1)
	-- ExpExp() doesn't like large numbers.
	-- so, factor
	return NewFromEun(my:EunExp(GetEun(n1)))
end function

public function EunExpFast(integer numerator, integer denominator)
	return NewFromEun(my:EunExpFast(GetEun(numerator), GetEun(denominator)))
end function

-- PI and E constants:

public function GetQuarterPI(integer targetLength, integer radix)
	return NewFromEun(my:GetQuarterPI(targetLength, radix))
end function
public function GetHalfPI(integer targetLength, integer radix)
	return NewFromEun(my:GetHalfPI(targetLength, radix))
end function
public function GetPI(integer targetLength, integer radix)
	return NewFromEun(my:GetPI(targetLength, radix))
end function
public function GetE(integer targetLength, integer radix)
	return NewFromEun(my:GetE(targetLength, radix))
end function

-- Logarithms:

public function EunLog(integer n1, integer guess1)
	sequence s = GetEun(n1)
	if guess1 = 0 then
		s = my:EunLog(s)
	else
		s = my:EunLog(s, GetEun(guess1))
	end if
	if length(s) != 2 then
		return NewFromEun(s)
	end if
	return 0 -- not doing complex mode, yet.
end function

-- Powers: a number (base), raised to the power of another number (raisedTo)

public function EunPower(integer base, integer raisedTo)
	return NewFromEun(my:EunPower(GetEun(base), GetEun(raisedTo)))
end function

-- Begin Trig Functions:

-- They all use Radians.

public function EunRadiansToDegrees(integer n1)
	return NewFromEun(my:EunRadiansToDegrees(GetEun(n1)))
end function
public function EunDegreesToRadians(integer n1)
	return NewFromEun(my:EunDegreesToRadians(GetEun(n1)))
end function


public function EunSin(integer n1)
	return NewFromEun(my:EunSin(GetEun(n1)))
end function
public function EunCos(integer n1)
	return NewFromEun(my:EunCos(GetEun(n1)))
end function
public function EunTan(integer n1)
	return NewFromEun(my:EunTan(GetEun(n1)))
end function

public function EunCsc(integer n1)
	return NewFromEun(my:EunCsc(GetEun(n1)))
end function
public function EunSec(integer n1)
	return NewFromEun(my:EunSec(GetEun(n1)))
end function
public function EunCot(integer n1)
	return NewFromEun(my:EunCot(GetEun(n1)))
end function

public function EunArcSin(integer n1)
	return NewFromEun(my:EunArcSin(GetEun(n1)))
end function
public function EunArcCos(integer n1)
	return NewFromEun(my:EunArcCos(GetEun(n1)))
end function

-- EunArcTan already defined, see above.

public function EunArcCsc(integer n1)
	return NewFromEun(my:EunArcCsc(GetEun(n1)))
end function
public function EunArcSec(integer n1)
	return NewFromEun(my:EunArcSec(GetEun(n1)))
end function
public function EunArcCot(integer n1)
	return NewFromEun(my:EunArcCot(GetEun(n1)))
end function

-- Hyperbolic functions:

public function EunSinh(integer n1)
	return NewFromEun(my:EunSinh(GetEun(n1)))
end function
public function EunCosh(integer n1)
	return NewFromEun(my:EunCosh(GetEun(n1)))
end function
public function EunTanh(integer n1)
	return NewFromEun(my:EunTanh(GetEun(n1)))
end function

public function EunCsch(integer n1)
	return NewFromEun(my:EunCsch(GetEun(n1)))
end function
public function EunSech(integer n1)
	return NewFromEun(my:EunSech(GetEun(n1)))
end function
public function EunCoth(integer n1)
	return NewFromEun(my:EunCoth(GetEun(n1)))
end function

public function EunArcSinh(integer n1)
	return NewFromEun(my:EunArcSinh(GetEun(n1)))
end function
public function EunArcCosh(integer n1)
	return NewFromEun(my:EunArcCosh(GetEun(n1)))
end function
public function EunArcTanh(integer n1)
	return NewFromEun(my:EunArcTanh(GetEun(n1)))
end function

public function EunArcCsch(integer n1)
	return NewFromEun(my:EunArcCsch(GetEun(n1)))
end function
public function EunArcSech(integer n1)
	return NewFromEun(my:EunArcSech(GetEun(n1)))
end function
public function EunArcCoth(integer n1)
	return NewFromEun(my:EunArcCoth(GetEun(n1)))
end function

-- Triangulation using two (2) points

public procedure EunTriangulation(integer eun_dst1, integer eun_dst2, integer eun_a, integer eun_b, integer eun_distance, integer whichOnes)
	object ret
	ret = my:EunTriangulation(GetEun(eun_a), GetEun(eun_b), GetEun(eun_distance), whichOnes)
	if and_bits(whichOnes, 1) then
		eun:replace_object(eun_dst1, ret[1])
	end if
	if and_bits(whichOnes, 2) then
		eun:replace_object(eun_dst2, ret[2])
	end if
end procedure

-- myeuroots.e:

-- "FindRoot" means: Find the roots (or zeros) of an equation.
-- Find the roots of the equation, (a callback function)

public function GetDelta() -- returns positive value of delta (which is negative)
	return - my:delta[2]
end function

public procedure SetDelta(integer i) -- positive number to negate
	-- "delta" should be a "small" negative number, such as (-10) to (-80)
	my:delta[2] = -i
end procedure

public procedure SetEurootsAdjustRound(PositiveInteger i)
	my:SetEurootsAdjustRound(i)
end procedure

public function GetEurootsAdjustRound()
	return my:GetEurootsAdjustRound()
end function

function Func1Exp(sequence n1, integer exp1, integer targetLength, integer radix, integer cfunc1)
	integer arrayId, ret
	sequence n2
	arrayId = numArray:new_object_from_data(n1)
	ret = c_func(cfunc1, {arrayId, exp1, targetLength, radix})
	DeleteNumArray(arrayId)
	n2 = GetEun(ret)
	DeleteEun(ret)
	return n2
end function
integer rid = routine_id("Func1Exp")

-- Define Euroots C Function before use, then use it as "callback_func1" in FindRootExp().

public function DefineEurootsCFunction(integer pointer_callback_func1)
	atom maFunc1
	integer cfunc1
	maFunc1 = pointers:get_data_from_object(pointer_callback_func1)
ifdef BITS64 then
	cfunc1 = define_c_func("", maFunc1, {C_LONGLONG, C_LONGLONG, C_LONGLONG, C_LONGLONG}, C_LONGLONG)
elsedef
	cfunc1 = define_c_func("", maFunc1, {C_INT, C_INT, C_INT, C_INT}, C_INT)
end ifdef
	return cfunc1
end function

public function FindRootExp(integer callback_func1, integer array_n1, integer exp1, 
		integer array_n2, integer exp2, integer len, integer radix)
	-- callback routine must accept four (4) arguments: func1(integer array_n1, integer exp1, integer targetLength, integer radix)
	-- and return an integer, the returned EunId.
	sequence s
	s = my:FindRootExp(rid, numArray:get_data_from_object(array_n1), exp1, numArray:get_data_from_object(array_n2), exp2, len, radix, 0, callback_func1)
	return NewFromEun(s)
end function

-- Complex functions:

public function ComplexCompare(integer c1, integer c2)
	-- Compares real parts, then imaginary parts of two (2) complex numbers
	return my:ComplexCompare(GetComplex(c1), GetComplex(c2)) + 1
end function

public function ComplexAdjustRound(integer c1, integer adjustBy)
	return NewFromComplex(my:ComplexAdjustRound(GetComplex(c1), adjustBy))
end function

public function NegateImaginary(integer complex_a)
	-- Negate the imaginary part of a Complex number
	return NewFromComplex(my:NegateImag(GetComplex(complex_a)))
end function

public function ComplexAdd(integer complex_a, integer complex_b)
	return NewFromComplex(my:ComplexAdd(GetComplex(complex_a), GetComplex(complex_b)))
end function

public function ComplexNegate(integer complex_a)
	return NewFromComplex(my:ComplexNegate(GetComplex(complex_a)))
end function

public function ComplexSubtract(integer complex_a, integer complex_b)
	return NewFromComplex(my:ComplexSubtract(GetComplex(complex_a), GetComplex(complex_b)))
end function

public function ComplexMultiply(integer complex_a, integer complex_b)
	-- n1 = (a+bi)
	-- n2 = (c+di)
	-- (a+bi)(c+di) <=> ac + adi + bci + bdii
	-- <=> (ac - bd) + (ad + bc)i
	return NewFromComplex(my:ComplexMultiply(GetComplex(complex_a), GetComplex(complex_b)))
end function

public function ComplexMultiplicativeInverse(integer complex_a)
	-- Eun a, b, c
	-- (a+bi)(a-bi) <=> a*a + b*b
	-- n2 = (a+bi)
	-- a = n2[1]
	-- b = n2[2]
	-- c = (a*a + b*b)
	-- 1 / n2 <=> (a-bi) / (a*a + b*b)
	-- <=> (a / (a*a + b*b)) - (b / (a*a + b*b))i
	-- <=> (a / c) - (b / c)i
	return NewFromComplex(my:ComplexMultiplicativeInverse(GetComplex(complex_a)))
end function

public function ComplexDivide(integer complex_a, integer complex_b)
	return NewFromComplex(my:ComplexDivide(GetComplex(complex_a), GetComplex(complex_b)))
end function

public procedure SetComplexSqrtAdjustRound(PositiveInteger i)
	my:SetComplexSqrtAdjustRound(i)
end procedure

public function GetComplexSqrtAdjustRound()
	return my:GetComplexSqrtAdjustRound()
end function

public procedure ComplexSqrt(integer complex_dst1, integer complex_dst2, integer complex_a)
	sequence ret
	ret = my:ComplexSqrt(GetComplex(complex_a))
	if length(ret) = 0 then
		abort(1/0) -- Error: something went wrong
	end if
	complex:replace_object(complex_dst1, ret[1])
	complex:replace_object(complex_dst2, ret[2])
end procedure

public procedure ComplexQuadraticEquation(integer complex_dst1, integer complex_dst2, integer complex_a, integer complex_b, integer complex_c)
	sequence ret
	ret = my:ComplexQuadraticEquation(GetComplex(complex_a), GetComplex(complex_b), GetComplex(complex_c))
	complex:replace_object(complex_dst1, ret[1])
	complex:replace_object(complex_dst2, ret[2])
end procedure

public function EunQuadraticEquation(integer eun_dst1, integer eun_dst2, integer complex_dst1, integer complex_dst2, integer eun_a, integer eun_b, integer eun_c)
	object ret
	ret = my:EunQuadraticEquation(GetEun(eun_a), GetEun(eun_b), GetEun(eun_c))
	if atom(ret) then
		abort(1/0) -- Error: something went wrong
	end if
	if my:Eun(ret[1]) then
		eun:replace_object(eun_dst1, ret[1])
		eun:replace_object(eun_dst2, ret[2])
		return 1 -- for eun's being stored
	else
		complex:replace_object(complex_dst1, ret[1])
		complex:replace_object(complex_dst2, ret[2])
		return 2 -- for complex's being stored
	end if
end function

-- Added functions:

-- Statistics

integer eun_sort_id = routine_id("EunCompare")
integer complex_sort_id = routine_id("ComplexCompare")

function custom_sort(integer custom_compare, sequence x, object data = {}, integer order = 1)
	integer gap, j, first, last
	object tempi, tempj, result
	sequence args = {0, 0}

	if order >= 0 then
		order = -1
	else
		order = 1
	end if

	if atom(data) then
		args &= data
	elsif length(data) then
		args = append(args, data[1])
	end if

	last = length(x)
	gap = floor(last / 10) + 1
	while 1 do
		first = gap + 1
		for i = first to last do
			tempi = x[i]
			args[1] = tempi
			j = i - gap
			while 1 do
				tempj = x[j]
				args[2] = tempj
				result = call_func(custom_compare, args)
				if sequence(result) then
					args[3] = result[2]
					result = result[1]
				end if
				if eu:compare(result, 0) != order then
					j += gap
					exit
				end if
				x[j+gap] = tempj
				if j <= gap then
					exit
				end if
				j -= gap
				sleep(nanosleep)
			end while
			x[j] = tempi
			sleep(nanosleep)
		end for
		if gap = 1 then
			return x
		else
			gap = floor(gap / 7) + 1
		end if
		sleep(nanosleep)
	end while
end function

function get4s_from_null_terminating_array(atom ma)
	atom findnull
	integer len
	findnull = ma
	while peek4s(findnull) != 0 do
		findnull += 4
		sleep(nanosleep)
	end while
	len = floor((findnull - ma) / 4)
	return peek4s({ma, len})
end function

public procedure EunSort(integer pointer_to_ids_null_terminating_array, integer order)
	atom ma
	sequence s
	ma = pointers:get_data_from_object(pointer_to_ids_null_terminating_array)
	s = get4s_from_null_terminating_array(ma)
	if order = 0 then
		order = -1
	end if
	s = custom_sort(eun_sort_id, s, {}, order) -- order is either: 1 or (-1).
	poke4(ma, s)
end procedure
public procedure ComplexSort(integer pointer_to_ids_null_terminating_array, integer order)
	atom ma
	sequence s
	ma = pointers:get_data_from_object(pointer_to_ids_null_terminating_array)
	s = get4s_from_null_terminating_array(ma)
	if order = 0 then
		order = -1
	end if
	s = custom_sort(complex_sort_id, s, {}, order) -- order is either: 1 or (-1).
	poke4(ma, s)
end procedure

public function EunSum(integer dstId, integer pointer_to_ids_null_terminating_array)
	atom ma
	sequence data, sum
	ma = pointers:get_data_from_object(pointer_to_ids_null_terminating_array)
	data = get4s_from_null_terminating_array(ma)
	sum = my:NewEun()
	for i = 1 to length(data) do
		sum = my:EunAdd(sum, GetEun(data[i]))
		sleep(nanosleep)
	end for
	eun:replace_object(dstId, sum)
	return length(data)
end function
public function ComplexSum(integer dstId, integer pointer_to_ids_null_terminating_array)
	atom ma
	sequence data, sum
	ma = pointers:get_data_from_object(pointer_to_ids_null_terminating_array)
	data = get4s_from_null_terminating_array(ma)
	sum = my:NewComplex()
	for i = 1 to length(data) do
		sum = my:ComplexAdd(sum, GetComplex(data[i]))
		sleep(nanosleep)
	end for
	complex:replace_object(dstId, sum)
	return length(data)
end function

-- mean, median, mode

-- mean -- average, sum / count

public function EunMean(integer pointer_to_ids_null_terminating_array)
	integer sumId, len
	sequence s
	sumId = eun:getNewId()
	len = EunSum(sumId, pointer_to_ids_null_terminating_array)
	s = GetEun(sumId)
	s = my:EunDivide(s, my:NewEun(my:Carry({len}, s[4]), 0, s[3], s[4]))
	eun:replace_object(sumId, s)
	return sumId -- actually retId.
end function
public function ComplexMean(integer pointer_to_ids_null_terminating_array)
	integer sumId, len
	sequence s
	sumId = complex:getNewId()
	len = ComplexSum(sumId, pointer_to_ids_null_terminating_array)
	s = GetComplex(sumId)
	s = my:ComplexDivide(s, {my:NewEun(my:Carry({len}, s[4]), 0, s[3], s[4]), my:NewEun({}, 0, s[3], s[4])})
	complex:replace_object(sumId, s)
	return sumId -- actually retId.
end function

-- median -- middle value, when sorted, averaged

public function EunMedian(integer pointer_to_ids_null_terminating_array, integer order)
	atom ma
	sequence s, num
	integer pos, resultId
	EunSort(pointer_to_ids_null_terminating_array, order)
	ma = pointers:get_data_from_object(pointer_to_ids_null_terminating_array)
	s = get4s_from_null_terminating_array(ma)
	pos = Ceil(length(s) / 2)
	if IsPositiveEven(length(s)) then
		-- Average the two in the middle:
		num = my:EunAdd(GetEun(s[pos]), GetEun(s[pos + 1]))
		num = my:EunDivide(num, my:NewEun({2}, 0, num[3], num[4]))
		resultId = NewFromEun(num)
	else
		resultId = CloneEun(s[pos])
	end if
	return resultId
end function
public function ComplexMedian(integer pointer_to_ids_null_terminating_array, integer order)
	atom ma
	sequence s, num
	integer pos, resultId
	ComplexSort(pointer_to_ids_null_terminating_array, order)
	ma = pointers:get_data_from_object(pointer_to_ids_null_terminating_array)
	s = get4s_from_null_terminating_array(ma)
	pos = Ceil(length(s) / 2)
	if IsPositiveEven(length(s)) then
		-- Average the two in the middle:
		num = my:ComplexAdd(GetComplex(s[pos]), GetComplex(s[pos + 1]))
		num = my:ComplexDivide(num, {my:NewEun({2}, 0, s[3], s[4]), my:NewEun({}, 0, s[3], s[4])})
		resultId = NewFromComplex(num)
	else
		resultId = CloneComplex(s[pos])
	end if
	return resultId
end function

-- mode -- numbers that show up 2 or more times, the max of them.

function sort(sequence x, integer order = 1)
	integer gap, j, first, last
	object tempi, tempj

	if order >= 0 then
		order = -1
	else
		order = 1
	end if


	last = length(x)
	gap = floor(last / 10) + 1
	while 1 do
		first = gap + 1
		for i = first to last do
			tempi = x[i]
			j = i - gap
			while 1 do
				tempj = x[j]
				if eu:compare(tempi, tempj) != order then
					j += gap
					exit
				end if
				x[j+gap] = tempj
				if j <= gap then
					exit
				end if
				j -= gap
				sleep(nanosleep)
			end while
			x[j] = tempi
			sleep(nanosleep)
		end for
		if gap = 1 then
			return x
		else
			gap = floor(gap / 7) + 1
		end if
		sleep(nanosleep)
	end while
end function


function raw_frequency(object data_set) --, object subseq_opt = ST_ALLNUM)
	
	sequence lCounts
	sequence lKeys
	integer lNew = 0
	integer lPos
	integer lMax = -1
	
	if atom(data_set) then
		return {{1,data_set}}
	end if
	
	-- data_set = massage(data_set, subseq_opt)
	
	if length(data_set) = 0 then
		return {{1,data_set}}
	end if
	lCounts = repeat({0,0}, length(data_set))
	lKeys   = repeat(0, length(data_set))
	for i = 1 to length(data_set) do
		lPos = find(data_set[i], lKeys)
		if lPos = 0 then
			lNew += 1
			lPos = lNew
			lCounts[lPos][2] = data_set[i]
			lKeys[lPos] = data_set[i]
			if lPos > lMax then
				lMax = lPos
			end if
		end if
		lCounts[lPos][1] += 1
		sleep(nanosleep)
	end for
	return sort(lCounts[1..lMax], -1)
	
end function

function mode(sequence data_set) --, object subseq_opt = ST_ALLNUM)
	
	sequence lCounts
	sequence lRes
	
	-- data_set = massage(data_set, subseq_opt)
	
	if not length( data_set ) then
		return {}
	end if

	lCounts = raw_frequency(data_set) --, subseq_opt)
	
	lRes = {lCounts[1][2]}
	for i = 2 to length(lCounts) do
		if lCounts[i][1] < lCounts[1][1] then
			exit
		end if
		lRes = append(lRes, lCounts[i][2])
		sleep(nanosleep)
	end for
	
	return lRes
	
end function


public function EunMode(integer pointer_to_ids_null_terminating_array)
	atom ma
	sequence s
	ma = pointers:get_data_from_object(pointer_to_ids_null_terminating_array)
	s = get4s_from_null_terminating_array(ma)
	for i = 1 to length(s) do
		s[i] = GetEun(s[i])
		sleep(nanosleep)
	end for
	s = mode(s)
	for i = 1 to length(s) do
		s[i] = NewFromEun(s[i])
		sleep(nanosleep)
	end for
	s = s & {0}
ifdef BITS64 then
	ma = allocate_data(length(s) * 8)
elsedef
	ma = allocate_data(length(s) * 4)
end ifdef
	if ma = 0 then
		return 0
	end if
	poke4(ma, s)
	return pointers:new_object_from_data(ma)
end function
public function ComplexMode(integer pointer_to_ids_null_terminating_array)
	atom ma
	sequence s
	ma = pointers:get_data_from_object(pointer_to_ids_null_terminating_array)
	s = get4s_from_null_terminating_array(ma)
	for i = 1 to length(s) do
		s[i] = GetComplex(s[i])
		sleep(nanosleep)
	end for
	s = mode(s)
	for i = 1 to length(s) do
		s[i] = NewFromComplex(s[i])
		sleep(nanosleep)
	end for
	s = s & {0}
ifdef BITS64 then
	ma = allocate_data(length(s) * 8)
elsedef
	ma = allocate_data(length(s) * 4)
end ifdef
	if ma = 0 then
		return 0
	end if
	poke4(ma, s)
	return pointers:new_object_from_data(ma)
end function

-- "Accurate" function:

public function GetMoreAccuratePrec(integer eun_n1, integer prec)
	return my:GetMoreAccuratePrec(GetEun(eun_n1), prec)
end function

-- dropped support for the other "accurate" functions
-- use the larger of the two, and then the smaller of the two's precision or targetLength

public function EunGetPrec(integer eun_n1)
	return my:EunGetPrec(GetEun(eun_n1))
end function

public function EunTest(integer eun_n1, integer eun_n2)
	sequence range
	range = my:EunTest(GetEun(eun_n1), GetEun(eun_n2))
	return range[1]
end function

-- Matrix support:

public function NewMatrix(integer rows, integer cols, integer pointer_to_eun_ids_null_terminating_array)
	integer pos
	atom ma
	sequence s, ret
	ma = pointers:get_data_from_object(pointer_to_eun_ids_null_terminating_array)
	s = get4s_from_null_terminating_array(ma)
	ret = my:NewMatrix(rows, cols)
	pos = 1
	for row = 1 to rows do
		for col = 1 to cols do
			ret[row][col] = GetEun(s[pos])
			pos += 1
			sleep(nanosleep)
		end for
		sleep(nanosleep)
	end for
	return matrix:new_object_from_data(ret)
end function

public procedure DeleteMatrix(integer i)
	matrix:delete_object(i)
end procedure

public procedure StoreMatrix(integer id_dst, integer id_src)
	matrix:store_object(id_dst, id_src)
end procedure

public function CloneMatrix(integer id)
	return matrix:clone_object(id)
end function

public function MatrixToArray(integer id)
	matrix a = matrix:get_data_from_object(id)
	integer rows, cols, len, size, offset
	atom ma
	rows = my:GetMatrixRows(a)
	cols = my:GetMatrixCols(a)
	len = rows * cols
	size = 4 * (len + 1)
	ma = allocate_data(size)
	if ma = 0 then
		return 0
	end if
	mem_set(ma, 0, size)
	offset = 0
	for row = 1 to rows do
		for col = 1 to cols do
			poke4(ma + offset, NewFromEun(a[row][col]))
			offset += 4
			sleep(nanosleep)
		end for
		sleep(nanosleep)
	end for
	return pointers:new_object_from_data(ma)
end function


public function GetMatrixRows(integer i)
	return my:GetMatrixRows(matrix:get_data_from_object(i))
end function

public function GetMatrixCols(integer i)
	return my:GetMatrixCols(matrix:get_data_from_object(i))
end function

public function MatrixMultiply(integer a, integer b)
	return matrix:new_object_from_data(my:MatrixMultiply(matrix:get_data_from_object(a), matrix:get_data_from_object(b)))
end function

public function MatrixTransformation(integer id)
	return matrix:new_object_from_data(my:MatrixTransformation(matrix:get_data_from_object(id)))
end function

-- end of file.

-- Copyright (c) 2016-2021 James Cook

-- See also videos and resources on the web about imaginary and complex numbers:
-- https://www.youtube.com/watch?v=T647CGsuOVU

-- without type_check

include std/console.e
include std/get.e

include my.e

-- with trace

puts(1, "EuNumber Calculator [v1.0.0a]\nMade to varify against Microsoft's Calculator.")

defaultTargetLength = 30
adjustRound = 0
calculationSpeed = 30

SetAdjustPrecision(2)

object tmp
sequence st, vars
integer n, ch, f
sequence n1, n2, n3, n4 -- real part
sequence c1, c2, c3, c4 -- imaginary part (of a complex number)

vars = {}
n1 = {}
c1 = {}
n = 0

procedure View()
	integer fn
	object ob, ob2
	if length(n1) = 0 then
		n1 = NewEun()
	end if
	if length(c1) = 0 then
		c1 = NewEun()
	end if
	ob = ToString(n1, TRUE)
	if length(c1[1]) then
		ob2 = ToString(c1, TRUE)
	end if
	
	fn = open("calcOut.txt", "w")
	if fn != -1 then
		puts(fn, "Value is:\n")
		puts(fn, "(1) Real part (REAL):\n")
		printf(fn, "%s\n", {ob})
		if length(c1[1]) then
			puts(fn, "(2) Imaginary part (IMAG) OR (IMAGINARY):\n")
			printf(fn, "%si\n", {ob2})
		end if
		close(fn)
	else
		puts(1, "Warning: Couldn\'t open \"calcOut.txt\" for text output\n")
	end if
	
	puts(1, "Value is:\n")
	puts(1, "(1) Real part (REAL):\n")
ifdef not WINDOWS then
	while length(ob) > 80 do
		printf(1, "%s\n", {ob[1..80]})
		ob = ob[81..$]
	end while
end ifdef
	if length(ob) then
		printf(1, "%s\n", {ob})
	end if
	if length(c1[1]) then
		puts(1, "(2) Imaginary part (IMAG) OR (IMAGINARY):\n")
ifdef not WINDOWS then
		while length(ob2) > 80 do
			printf(1, "%s\n", {ob2[1..80]})
			ob2 = ob2[81..$]
		end while
end ifdef
		if length(ob2) then
			printf(1, "%s\n", {ob2})
		end if
	end if
end procedure

function PromptNew(integer i)
	sequence str
	while 1 do
		str = prompt_string(sprintf("Enter number (%d), like: -1.234e-1234 or \'q\' to exit\n", {i}))
		if equal(str, "q") then
			return {}
		elsif equal(str, "exit") then
			puts(1, "Program shutdown normally.\n")
			abort(0)
		elsif length(str) then
			str = ToEun(str)
			if atom(str[1]) then
				puts(1, "Invalid number, try again.\n")
			else
				exit
			end if
		end if
	end while
	return str
end function

procedure GetNew()
	integer k
	k = prompt_number("Prompt: Real (1) or Complex (2), OR (0) for no change: ", {0, 2})
	if k != 0 then
		c1 = {}
	end if
	if k >= 1 then
		n1 = PromptNew(1)
		if not length(n1) then
			return
		end if
	end if
	if k >= 2 then
		c1 = PromptNew(2)
		if not length(c1) then
			return
		end if
	end if
	View()
	n = find({}, vars)
	if n then
		vars[n] = {n1, c1}
	else
		vars = append(vars, {n1, c1})
		n = length(vars)
	end if
	printf(1, "Stored as n%d.\n", {n})
	printf(1, "[Using %d as radix, %d as targetLength, and %g for calculationSpeed]\n", {defaultRadix, defaultTargetLength, calculationSpeed})
end procedure

function ClearPrompt(integer i)
	sequence s
	printf(1, "Are you sure you want to clear n%d? [yn] ", {i})
	s = prompt_string("")
	if length(s) then
		if find(s[1], "yY") then
			vars[i] = {}
			if i = n then
				n1 = {}
				c1 = {}
				n = 0
			end if
			printf(1, "n%d cleared\n", {i})
			return 1
		end if
	end if
	return 0
end function

procedure MemoryFunctions()
	while 1 do
		puts(1, "Memory Operations: [n]ew, [s]tore, [r]ecall, [c]lear, [v]eiw, [q]uit, [exit]\n")
		st = prompt_string(": ")
		if equal(st, "exit") then
			puts(1, "Program shutdown normally.\n")
			abort(0)
		end if
		if not length(st) or equal(st, "q") or not find(st[1], "nsrcv") then
			exit
		end if
		ch = st[1]
		if ch = 'n' then
			GetNew()
			continue -- go back to the beginning of the while loop
		elsif ch = 'v' then
			if n >= 1 then
				printf(1, "Current number is: n%d\n", {n})
				View()
			else
				puts(1, "No current number, use [r]ecall.\n")
			end if
			continue
		end if
		if length(st) = 1 then
			st = prompt_string("n")
		else
			if length(st) >= 3 and equal(st[2..3], " n") then
				st = st[4..$]
			else
				st = st[2..$]
			end if
		end if
		st = value(st)
		switch ch do
		case 's' then
			if st[1] = GET_SUCCESS then
				tmp = st[2]
				if integer(tmp) and tmp >= 1 then
					if tmp > length(vars) then
						vars = vars & repeat({}, tmp - length(vars))
					end if
					if length(vars[tmp]) then
						if not ClearPrompt(tmp) then
							continue
						end if
					end if
					vars[tmp] = {n1, c1}
					printf(1, "Stored n%d => n%d\n", {n, tmp})
					continue
				end if
			end if
			puts(1, "Couldn\'t store number\n")
		case 'r' then
			if st[1] = GET_SUCCESS then
				tmp = st[2]
				if integer(tmp) and tmp > 0 and tmp <= length(vars) and length(vars[tmp]) then
					n = tmp
					tmp = vars[n]
					if length(tmp) = 2 then
						n1 = tmp[1]
						c1 = tmp[2]
					else
						n1 = tmp
					end if
					printf(1, "Recalled n%d and made current\n", {n})
					continue
				end if
			end if
			puts(1, "Couldn\'t recall number\n")
			puts(1, "Valid numbers are:\n")
			for i = 1 to length(vars) do
				if length(vars[i]) then
					printf(1, "n%d ", {i})
				end if
			end for
			puts(1, "\n")
		case 'c' then
			if st[1] = GET_SUCCESS then
				tmp = st[2]
				if integer(tmp) and tmp > 0 and tmp <= length(vars) and length(vars[tmp]) then
					ClearPrompt(tmp)
					continue
				end if
			end if
			puts(1, "Couldn\'t clear memory\n")
		case else
		end switch
	end while
end procedure

function GetVars(integer num)
	sequence s
	integer i, f
	s = repeat(0, num)
	i = 1
	while i <= num do
		puts(1, "enter variable number, or zero (0) for memory options\n")
		f = prompt_number("n", {0, length(vars)})
		if f = 0 then
			MemoryFunctions()
			continue
		end if
		if length(vars[f]) then
			s[i] = vars[f]
			i += 1
		end if
	end while
	return s
end function

procedure StoreAnswers(integer num)
	
	if length(vars) < num then
		vars = vars & repeat(0, num)
	end if
	printf(1, "Answers (%d), to be stored to n1 through n%d\n", {num, num})
	for i = 1 to num do
		if length(st[i][1]) = 2 then
			printf(1, "n%d number is Complex (it has both a real and imaginary part)\n", {i})
		end if
		vars[i] = st[i] -- stores either a real or complex number (a number with a real and imaginary part)
		printf(1, "stored number %d answer to n%d\n", {i, i})
	end for
end procedure

procedure Operations()
	
	while 1 do
		puts(1, "Math Operations: [ q + - * / n i c = f p o t s r w e l S C AS AT Q ]\n")
		puts(1, " [+]Add [-]Subtract [*]Multiply [/]divide [n]egate [i]nverse\n")
		puts(1, " [c]convert [=]compare [f]fractpart [p]intpart [o]Round [t]RoundToInt\n")
		puts(1, " [s]sqrt [r]root [w]wholeExp [e]exp [l]log\n")
		puts(1, " [S]ine [C]os [AS]arcsin [AT]arctan [Q]quadraticEquation\n")
		puts(1, " [q]uit [exit]\n")
		puts(1, "Complex numbers: Complex [ + - * / n i s R I T N Q ]\n")
		puts(1, " [R]real [I]imaginary [T]swap real and imaginary [N]negate imaginary\n")
		-- NegateImag
		-- All these have similar Real number "equivalents"
		-- + ComplexAdd
		-- - ComplexSubtract
		-- * ComplexMult
		-- / ComplexDivide
		-- n ComplexNegate
		-- i ComplexMultInv
		-- s ComplexSqrt
		-- R Real
		-- I Imaginary
		-- T Swap Real and Imaginary
		-- N Negate only the imaginary part
		-- Q ComplexQuadraticEquation
		View()
		st = prompt_string(": ")
		if equal(st, "exit") then
			puts(1, "Program shutdown normally.\n")
			abort(0)
		end if
		if not length(st) or equal(st, "q") or not find(st[1], "+-*/nic=fpotsrwelSCAQRITN") then
			exit
		end if
		ch = st[1]
		switch ch do
		case 'R' then
			puts(1, "Complex Transformation: Get real part, Clear imaginary part:\n")
			c1 = {}
			puts(1, "SUCCESS!\n")
		case 'I' then
			puts(1, "Complex Transformation: Get imaginary part as real part, Clear imaginary part:\n")
			n1 = c1
			c1 = {}
			puts(1, "SUCCESS!\n")
		case 'T' then
			puts(1, "Complex Transformation: Swap real and imaginary parts:\n")
			tmp = n1
			n1 = c1
			c1 = tmp
			tmp = {}
			puts(1, "SUCCESS!\n")
		case 'N' then
			puts(1, "Negate only the imaginary part of a \"complex\" number:\n")
			c1[1] = Negate(c1[1])
			puts(1, "SUCCESS!\n")
		case '+' then
			puts(1, "Add:\n")
			st = GetVars(1)
			if length(st) then
				n2 = st[1][1]
				c2 = st[1][2]
				if n1[4] != n2[4] then
					n1 = EunConvert(n1, defaultRadix, defaultTargetLength)
					n2 = EunConvert(n2, defaultRadix, defaultTargetLength)
				end if
				if c1[4] != c2[4] then
					c1 = EunConvert(c1, defaultRadix, defaultTargetLength)
					c2 = EunConvert(c2, defaultRadix, defaultTargetLength)
				end if
				st = ComplexAdd({n1, c1}, {n2, c2})
				n1 = st[1]
				c1 = st[2]
				puts(1, "SUCCESS!\n")
			else
				puts(1, "aborted.\n")
			end if
		case '-' then
			puts(1, "Subtract:\n")
			st = GetVars(1)
			if length(st) then
				n2 = st[1][1]
				c2 = st[1][2]
				if n1[4] != n2[4] then
					n1 = EunConvert(n1, defaultRadix, defaultTargetLength)
					n2 = EunConvert(n2, defaultRadix, defaultTargetLength)
				end if
				if c1[4] != c2[4] then
					c1 = EunConvert(c1, defaultRadix, defaultTargetLength)
					c2 = EunConvert(c2, defaultRadix, defaultTargetLength)
				end if
				st = ComplexSubtract({n1, c1}, {n2, c2})
				n1 = st[1]
				c1 = st[2]
				puts(1, "SUCCESS!\n")
			else
				puts(1, "aborted.\n")
			end if
		case '*' then
			puts(1, "Multiply:\n")
			st = GetVars(1)
			if length(st) then
				n2 = st[1][1]
				c2 = st[1][2]
				if n1[4] != n2[4] then
					n1 = EunConvert(n1, defaultRadix, defaultTargetLength)
					n2 = EunConvert(n2, defaultRadix, defaultTargetLength)
				end if
				if c1[4] != c2[4] then
					c1 = EunConvert(c1, defaultRadix, defaultTargetLength)
					c2 = EunConvert(c2, defaultRadix, defaultTargetLength)
				end if
				st = ComplexMultiply({n1, c1}, {n2, c2})
				n1 = st[1]
				c1 = st[2]
				puts(1, "SUCCESS!\n")
			else
				puts(1, "aborted.\n")
			end if
		case '/' then
			puts(1, "divide:\n")
			st = GetVars(1)
			if length(st) then
				n2 = st[1][1]
				c2 = st[1][2]
				if n1[4] != n2[4] then
					n1 = EunConvert(n1, defaultRadix, defaultTargetLength)
					n2 = EunConvert(n2, defaultRadix, defaultTargetLength)
				end if
				if c1[4] != c2[4] then
					c1 = EunConvert(c1, defaultRadix, defaultTargetLength)
					c2 = EunConvert(c2, defaultRadix, defaultTargetLength)
				end if
				st = ComplexDivide({n1, c1}, {n2, c2})
				n1 = st[1]
				c1 = st[2]
				puts(1, "SUCCESS!\n")
			else
				puts(1, "aborted.\n")
			end if
		case 'n' then
			puts(1, "Negate:\n")
			n1[1] = Negate(n1[1])
			c1[1] = Negate(c1[1])
			puts(1, "SUCCESS!\n")
		case 'i' then
			puts(1, "MultiplicativeInverse:\n")
			st = ComplexMultiplicativeInverse({n1, c1})
			n1 = st[1]
			c1 = st[2]
			puts(1, "SUCCESS!\n")
		case 's' then
			puts(1, "Square root:\n") -- is both plus and minus (positive and negative)
			if length(c1[1]) then
				st = ComplexSqrt({n1, c1})
				n1 = st[1][1]
				c1 = st[1][2]
			else
				tmp = EunSqrt(n1)
				if tmp[1] = 1 then
					puts(1, "Warning: Imaginary Number\n")
					n1 = NewEun()
					c1 = tmp[2]
					n2 = NewEun()
					c2 = tmp[3]
				else
					n1 = tmp[2]
					c1 = NewEun()
					n2 = tmp[3]
					c2 = NewEun()
				end if
				st = {{n1, c1}, {n2, c2}}
			end if
			StoreAnswers(2) -- uses "st"
			puts(1, "SUCCESS!\n")
		case 'c' then
			puts(1, "convert:\n")
			defaultRadix = prompt_number("enter new defaultRadix: ", {2, MAX_RADIX})
			defaultTargetLength = prompt_number("enter new defaultTargetLength: ", {2, INT_MAX})
			printf(1, "Current Rounding method is %d\n", {ROUND})
			puts(1, " ROUND_TOWARDS_INFINITY = 1\n")
			puts(1, " ROUND_TOWARDS_ZERO = 2\n")
			puts(1, " ROUND_UP = 4\n")
			puts(1, " ROUND_DOWN = 5\n")
			puts(1, " ROUND_EVEN = 6\n")
			puts(1, " ROUND_ODD = 7\n")
			ch = prompt_number("enter new Rounding method, or 0 for no change: ", {0, 7})
			if ch > 0 then
				ROUND = ch
			end if
			printf(1, "calculationSpeed is: %d\n", {calculationSpeed})
			calculationSpeed = prompt_number("enter new calculationSpeed: ", {})
			n1 = EunConvert(n1, defaultRadix, defaultTargetLength)
			? n1
			c1 = EunConvert(c1, defaultRadix, defaultTargetLength)
			? c1
			puts(1, "SUCCESS!\n")
		case '=' then
			puts(1, "compare (real parts of numbers):\n")
			st = GetVars(2)
			if length(st) then
				if st[1][1][4] != st[2][1][4] then
					st[1][1] = EunConvert(st[1][1], defaultRadix, defaultTargetLength)
					st[2][1] = EunConvert(st[2][1], defaultRadix, defaultTargetLength)
				end if
				ch = EunCompare(st[1][1], st[2][1])
				if ch = 0 then
					puts(1, "First is EQUAL ( = ) to second.\n")
				elsif ch = -1 then
					puts(1, "First is LESS THAN ( < ) second.\n")
				elsif ch = 1 then
					puts(1, "First is GREATER THAN ( > ) second.\n")
				end if
				puts(1, "SUCCESS!\n")
			else
				puts(1, "aborted.\n")
			end if
		case 'f' then
			puts(1, "fraction part (real):\n")
			n1 = EunFracPart(n1)
			puts(1, "SUCCESS!\n")
		case 'p' then
			puts(1, "integer part (real):\n")
			n1 = EunIntPart(n1)
			puts(1, "SUCCESS!\n")
		case 'o' then
			puts(1, "Round (real):\n")
			ch = prompt_number("enter number of significant digits: ", {0, defaultTargetLength})
			n1 = EunRoundSig(n1, ch)
			puts(1, "SUCCESS!\n")
		case 't' then
			puts(1, "Round to integer (real):\n")
			n1 = EunRoundToInt(n1)
			puts(1, "SUCCESS!\n")
		case 'r' then
			puts(1, "root function (real part of numbers only):\n")
			ch = prompt_number("enter root (2 to intMax): ", {0, INT_MAX})
			if ch >= 2 and integer(ch) then
				puts(1, "enter guess:\n")
				st = GetVars(1)
				if length(st) then
					n2 = st[1][1]
					if n1[4] != n2[4] then
						n1 = EunConvert(n1, defaultRadix, defaultTargetLength)
						n2 = EunConvert(n2, defaultRadix, defaultTargetLength)
					end if
					n1 = EunNthRoot(ch, n1, n2)
					if length(n1) = 3 then
						tmp = n1
						puts(1, "Even roots have two answers, one is negative, the other is positive\n")
						if tmp[1] = 1 then
							puts(1, "Warning: Imaginary Number\n")
							n1 = NewEun()
							c1 = tmp[2]
							n2 = NewEun()
							c2 = tmp[3]
						else
							n1 = tmp[2]
							c1 = NewEun()
							n2 = tmp[3]
							c2 = NewEun()
						end if
						st = {{n1, c1}, {n2, c2}}
						StoreAnswers(2) -- uses "st"
					end if
				end if
				puts(1, "SUCCESS!\n")
			else
				puts(1, "aborted.\n")
			end if
		case 'w' then
			puts(1, "whole number exp(u, m) (real part of numbers only):\n")
			st = GetVars(1)
			if length(st) then
				st = {st[1][1], n1}
				n2 = st[1]
				for i = 1 to 2 do
					-- if EunFracPart is not equal to zero, then abort operation.
					if not equal(0, EunCompare(EunFracPart(st[i]), {{}, 0, defaultTargetLength, defaultRadix})) then
						puts(1, "aborted, not whole number integers, use [p]intpart on both operands.\n")
						break
					end if
				end for
				if n1[4] != n2[4] then
					n1 = EunConvert(n1, defaultRadix, defaultTargetLength)
					n2 = EunConvert(n2, defaultRadix, defaultTargetLength)
				end if
				n1 = EunExpWhole(n1, n2)
				puts(1, "SUCCESS!\n")
			else
				puts(1, "aborted.\n")
			end if
		case 'e' then
			puts(1, "exp(fract), (real part only) doesn\'t like large numbers, so factor:\n")
			View()
			ch = prompt_number("1 to continue, 0 for abort: ", {0, 1})
			if ch then
				n1 = EunExp(n1)
				puts(1, "SUCCESS!\n")
			else
				puts(1, "aborted.\n")
			end if
		case 'l' then
			puts(1, "log (real):\n")
			n1 = EunLog(n1)
			puts(1, "SUCCESS!\n")
		case 'S' then
			puts(1, "sine (real):\n")
			n1 = EunSin(n1)
			puts(1, "SUCCESS!\n")
		case 'C' then
			puts(1, "cos (real):\n")
			n1 = EunCos(n1)
			puts(1, "SUCCESS!\n")
		case 'A' then
			if st[2] = 'S' then
				puts(1, "arcsin (real):\n")
				n1 = EunArcSin(n1)
				puts(1, "SUCCESS!\n")
			elsif st[2] = 'T' then
				puts(1, "arctan (real):\n")
				n1 = EunArcTan(n1)
				puts(1, "SUCCESS!\n")
			else
				puts(1, "aborted.\n")
			end if
		case 'Q' then
			puts(1, "QuadraticEquation, enter three (3) numbers, Solve for x: ax^2 + bx + c = 0\n")
			tmp = {n1, c1}
			st = GetVars(3)
			if length(st) then
				n1 = st[1][1]
				n2 = st[2][1]
				n3 = st[3][1]
				if n1[4] != n2[4] or n1[4] != n3[4] then
					n1 = EunConvert(n1, defaultRadix, defaultTargetLength)
					n2 = EunConvert(n2, defaultRadix, defaultTargetLength)
					n3 = EunConvert(n3, defaultRadix, defaultTargetLength)
				end if
				c1 = st[1][2]
				c2 = st[2][2]
				c3 = st[3][2]
				st = {}
				if length(c1[1]) or length(c2[1]) or length(c3[1]) then
					if c1[4] != c2[4] or c1[4] != c3[4] then
						c1 = EunConvert(c1, defaultRadix, defaultTargetLength)
						c2 = EunConvert(c2, defaultRadix, defaultTargetLength)
						c3 = EunConvert(c3, defaultRadix, defaultTargetLength)
					end if
					puts(1, "Complex Quadratic Equation has at least two (2) answers:\n")
					puts(1, "imaginary part can be both plus and minus, in some cases.\n")
					st = ComplexQuadraticEquation({n1, c1}, {n2, c2}, {n3, c3})
					StoreAnswers(2)
				else
					puts(1, "Real Quadratic Equation has at least two (2) answers:\n")
					puts(1, "imaginary part can be both plus and minus, in some cases.\n")
					st = EunQuadraticEquation(n1, n2, n3)
					if Eun(st[1]) then
						st[1] = NewComplex(st[1], NewEun())
						st[2] = NewComplex(st[2], NewEun())
					else
						puts(1, "Warning: Imaginary Numbers\n")
					end if
					StoreAnswers(2)
				end if
				n1 = tmp[1]
				c1 = tmp[2]
				n3 = {}
				c3 = {}
				puts(1, "SUCCESS!\n")
			else
				puts(1, "aborted.\n")
			end if
		case else
			puts(1, "Status: Nothing done.\n")
		end switch
		if divideByZeroFlag then
			puts(1, "Attempted to divide by zero.\n")
			divideByZeroFlag = 0
		end if
		st = {}
		n2 = {}
		c2 = {}
		n = 0
		
	end while
end procedure

while 1 do
	
	printf(1, "Press any key then enter <OR> type: \'q\' or \'exit\' to exit:\n\n")
	
	MemoryFunctions()
	Operations()
	
end while



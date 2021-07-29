-- Copyright (c) 2016-2021 James Cook

-- done.

-- KEEP!

include std/console.e
include std/get.e
include std/io.e
include std/map.e
include std/pretty.e
include std/stack.e
--include std/text.e
include std/types.e

include my.e as my

-- with trace
-- trace(1)

constant OBJRid = 0
constant SEQRid = 1
constant INTEGERRid = 2


constant programName = "doFile.ex"

sequence cmd

cmd = command_line()

if length(cmd) < 4 then
	puts(1, "Use: eui " & programName & " outfile.txt infile1.txt [ infile2.txt ... ]\n")
	abort(0)
end if

sequence name, buffer = {}
integer line, col, newCol
integer eofFlag = 0
object file
integer fn

fn = open(cmd[3], "w")

procedure ErrorAbort(integer a)
	sequence msg = sprintf("Error number %d in reading file \"%s\" at line: %d, column: %d\n", {a, name, line, newCol})
	puts(fn, msg)
	puts(2, msg)
	close(fn)
	abort(a)
end procedure

constant whitespace = " \t\n\r"

function GetNextValue()
	sequence s
	integer f
	while 1 do
		col = 0
		while length(buffer) and find(buffer[1], whitespace) do
			buffer = buffer[2..$]
			newCol += 1
			col += 1
		end while
		if length(buffer) then
			if t_alnum(buffer[1]) then
				f = 0
				for i = 1 to length(buffer) do
					if find(buffer[i], whitespace) then
						f = i
						exit
					end if
				end for
				if f then
					buffer = "\"" & buffer[1..f-1] & "\"" & buffer[f..$]
				end if
			end if
		end if
		s = value(buffer, 1, GET_LONG_ANSWER)
		if s[1] = GET_SUCCESS then
			buffer = buffer[s[3] + 1..$]
			newCol += s[3]
			--col = s[4]
			--if col > 0 then
			--	col -= 1
			--end if
			exit
		end if
		
		-- skip blank lines:
		while length(file) and length(file[1]) = 0 do
			file = file[2..$]
			line += 1
		end while
		if length(file) = 0 then
			eofFlag = my:TRUE
			exit
		end if
		buffer = buffer & file[1] & "\n"
		file = file[2..$]
		line += 1
		newCol = 1
		
	end while
	
	return s
end function

function Myget(integer mytype, integer output = 0)
	sequence s
	integer flag
	object ob
	
	-- get next value in file:
	s = GetNextValue()
	if eofFlag = my:TRUE then
		return {}
	end if
	if s[1] != GET_SUCCESS then
		ErrorAbort(2)
	end if
	ob = s[2]
	if mytype = INTEGERRid then
		if sequence(ob) then
			s = ToAtom(ob)
		end if
		if not integer(ob) then
			ErrorAbort(2)
		end if
	end if
	
	if types:string(ob) then
		-- ob = lower(ob)
		if equal(ob, "equals") then
			s = GetNextValue()
			s = GetNextValue()
			if eofFlag = my:TRUE then
				return {}
			end if
			ob = Myget(OBJRid) -- return this ob
			return ob
		end if
		if length(ob) and t_alnum(ob[1]) then
			flag = 0
			for i = 1 to length(ob) do
				if find(ob[i], whitespace) then
					flag = 1
					exit
				end if
			end for
		else
			flag = 1
		end if
		if output then
			if flag then
				printf(fn, "%s\"%s\"\n", {repeat(' ', col), ob})
			else
				printf(fn, "%s%s\n", {repeat(' ', col), ob})
			end if
		end if
		if equal(ob, "begin comment") then
			ob = Myget(OBJRid)
			while not equal(ob, "end comment") do
				ob = Myget(OBJRid)
			end while
			ob = Myget(OBJRid)
			return ob
		end if
		if length(ob) >= 2 and equal(ob[1..2], "//") then
			-- Comment in quotation marks:
			printf(fn, "%s\"%s\"\n", {repeat(' ', col), ob})
			ob = Myget(OBJRid) -- return this ob
			return ob
		end if
	elsif output then
		puts(fn, repeat(' ', col))
		if atom(ob) then
			s = ToString(ob)
			puts(fn, s)
		else
			print(fn, ob)
		end if
		puts(fn, "\n")
	end if
	return ob
end function

map labels
labels = map:new()

map stored
stored = map:new()

stack nums
nums = stack:new()

function GetNum()
	object ob
	-- EuNumber: pop "ob" from stack "nums"
	if stack:is_empty(nums) then
		ErrorAbort(3)
	end if
	ob = stack:pop(nums)
	return ob
end function

function GetNums(integer size)
	sequence s
	s = repeat(0, size)
	for i = 1 to size do
		s[i] = GetNum()
	end for
	return s
end function

integer levelIf = 0
integer nestedIf = 0
sequence ifStatement = {0}

sequence s, ans
object ob, tmp
integer in

ans = {}
for i = 4 to length(cmd) do
	name = cmd[i]
	in = open(name, "r")
	if in = -1 then
		printf(1, "Error opening file \"%s\"\n", {name})
		abort(1)
	else
		file = read_lines(in)
		close(in)
		line = 0
		newCol = 0
		if atom(file) then
			ErrorAbort(10)
		end if
		
	while 1 do
		ob = Myget(OBJRid)
		if eofFlag then
			eofFlag = 0
			exit
		end if
		
		if ifStatement[$] = -1 then
			levelIf = nestedIf
			while not eofFlag do
				if types:string(ob) then
					if length(ob) >= 2 and equal(ob[1..2], "if") then
						nestedIf += 1
					elsif equal(ob, "endif") or equal(ob, "end if") then
						if nestedIf = levelIf then
							exit
						end if
						nestedIf -= 1
					end if
					if nestedIf = levelIf then
						if equal(ob, "else") then
							exit
						end if
					end if
				end if
				ob = Myget(OBJRid)
			end while
		end if
		-- trace(1)
		switch ob do
		case "set" then
			ob = Myget(SEQRid)
			switch ob do
			case "Round" then
				ob = Myget(SEQRid)
				switch ob do
				case "inf" then
					ROUND = ROUND_INF
				case "zero" then
					ROUND = ROUND_ZERO
				case "up" then
					ROUND = ROUND_POS_INF
				case "down" then
					ROUND = ROUND_NEG_INF
				case "even" then
					ROUND = ROUND_EVEN
				case "odd" then
					ROUND = ROUND_ODD
				case else
					ErrorAbort(4)
				end switch
			case "calcSpeed" then
				ob = Myget(INTEGERRid)
				calculationSpeed = ob
-- 			case "expMoreAccuracy" then
-- 				ob = Myget(INTEGERRid)
-- 				expMoreAccuracy = ob
-- 			case "lnMoreAccuracy" then
-- 				ob = Myget(INTEGERRid)
-- 				logMoreAccuracy = ob
			case "defaultRadix" then
				ob = Myget(INTEGERRid)
				defaultRadix = ob
			case "defaultTargetLength" then
				ob = Myget(INTEGERRid)
				defaultTargetLength = ob
			case else
				ErrorAbort(4)
			end switch
		case "RoundSig" then -- Round to defaultTargetLength significant digits
			s = GetNums(1)
			ans = EunRoundSig(s[1])
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans)
		case "show" then
			ob = ToString(ans)
			--tmp = ToAtom(ob)
			puts(1, ob & "\n")
			puts(fn, "\"equals\"\n")
			puts(fn, ob & "\n")
			pretty_print(fn, ans, {0,2})
			puts(fn, "\n")
			--? ans
			--pretty_print(2, ans, {0,2})
			--puts(2, "\n")
		case "label" then
			ob = Myget(SEQRid)
			if not (types:string(ob) and length(ob) and t_alnum(ob[1])) then
				ErrorAbort(5)
			end if
			map:put(labels, ob, line)
		case "goto" then
			ob = Myget(SEQRid)
			if not (types:string(ob) and length(ob) and t_alnum(ob[1])) then
				ErrorAbort(5)
			end if
			line = map:get(labels, ob)
		case "store" then
			ob = Myget(SEQRid)
			if not (types:string(ob) and length(ob) and t_alpha(ob[1])) then
				ErrorAbort(5)
			end if
			map:put(stored, ob, ans)
		case "pop" then
			-- EuNumber: pop "ob" from stack "nums"
			-- remove element from stack
			if stack:is_empty(nums) then
				ErrorAbort(6)
			end if
			ob = stack:pop(nums) -- changed semantics
		case "get" then
			if stack:is_empty(nums) then
				ErrorAbort(6)
			end if
			ob = stack:pop(nums) -- changed semantics
			if Eun(ob) then
				ans = ob
			end if
		case "compare" then
			s = GetNums(2)
			ob = EunCompare(s[2], s[1])
			ob = ToEun(ob)
			stack:push(nums, ob)
		case "ifneq", "if not eq", "if true" then -- values are not equal
			ob = GetNum()
			ob = EunCompare(ob, {{},0,defaultTargetLength,defaultRadix})
			if ob then
				ifStatement &= 1
			else
				ifStatement &= -1
			end if
			nestedIf += 1
		case "ifeq", "if eq", "if false" then -- values are equal
			ob = GetNum()
			ob = EunCompare(ob, {{},0,defaultTargetLength,defaultRadix})
			if not ob then
				ifStatement &= 1
			else
				ifStatement &= -1
			end if
			nestedIf += 1
		case "ifgt", "if gt", "if pos" then -- one value is greater than the other
			ob = GetNum()
			ob = EunCompare(ob, {{},0,defaultTargetLength,defaultRadix})
			if ob > 0 then
				ifStatement &= 1
			else
				ifStatement &= -1
			end if
			nestedIf += 1
		case "iflt", "if lt", "if neg" then -- one value is less than the other
			ob = GetNum()
			ob = EunCompare(ob, {{},0,defaultTargetLength,defaultRadix})
			if ob < 0 then
				ifStatement &= 1
			else
				ifStatement &= -1
			end if
			nestedIf += 1
		case "ifgteq", "if gt or eq" then -- one value is greater than or equal to the other
			ob = GetNum()
			ob = EunCompare(ob, {{},0,defaultTargetLength,defaultRadix})
			if ob >= 0 then
				ifStatement &= 1
			else
				ifStatement &= -1
			end if
			nestedIf += 1
		case "iflteq", "if lt or eq" then -- one value is less than or equal to the other
			ob = GetNum()
			ob = EunCompare(ob, {{},0,defaultTargetLength,defaultRadix})
			if ob <= 0 then
				ifStatement &= 1
			else
				ifStatement &= -1
			end if
			nestedIf += 1
		case "else" then
			ifStatement[$] *= -1
			
		case "endif", "end if" then
			nestedIf -= 1
			ifStatement = ifStatement[1..$-1]
			
-- BEGIN EUN functions:
		case "Add" then
			s = GetNums(2)
			ans = EunAdd(s[1],s[2])
			
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans)
			
			--? ans
		case "Negate" then
			s = GetNums(1)
			ans = EunNegate(s[1])
			
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans)
			
			--? ans
		case "Multiply" then
			s = GetNums(2)
			ans = EunMultiply(s[1],s[2])
			
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans)
			
			--? ans
		case "MulticativeInverse" then
			s = GetNums(1)
			ans = EunMultiplicativeInverse(s[1])
			
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans)
			
			--? ans
		case "divide" then
			s = GetNums(2)
			ans = EunDivide(s[2],s[1])
			
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans)
			
			--? ans
		case "convert" then
			s = GetNums(1)
			if 1 then
				integer toRadix, targetLength
				toRadix = Myget(INTEGERRid)
				targetLength = Myget(INTEGERRid)
				ans = EunConvert(s[1], toRadix, targetLength)
			end if
			
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans)
			
			--? ans
		case "reverse" then
			s = GetNums(1)
			ans = EunReverse(s[1])
			
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans)
			
			--? ans
		case "fpart", "frac part" then
			s = GetNums(1)
			ans = EunFracPart(s[1])
			
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans)
			
			--? ans
		case "ipart", "int part" then
			s = GetNums(1)
			ans = EunIntPart(s[1])
			
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans)
			
			--? ans
		case "RoundInt" then -- Rounds to nearest integer
			s = GetNums(1)
			ans = EunRoundToInt(s[1])
			
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans)
			
			--? ans
		case "root" then
			s = GetNums(2)
			if 1 then
				integer n
				n = Myget(INTEGERRid)
				ob = EunNthRoot(n, s[2], s[1])
				if atom(ob) then
					ErrorAbort(9)
				end if
				ans = ob
			end if
			
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans)
			
			--? ans
		case "sqrt" then -- square root, using a guess
			s = GetNums(2)
			ans = EunSquareRoot(s[2], s[1])
			
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans)
			
			--? ans
		case "getSign" then -- test for negative, use before using "sqrt2" below, to tell if number is imaginary or not
			s = GetNums(1)
			if length(s[1]) and s[1][1] < 0 then
				ans = {{-1}, 0, defaultTargetLength, defaultRadix}
			else
				ans = {{1}, 0, defaultTargetLength, defaultRadix}
			end if
			
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans)
			
			--? ans
		case "isZero" then -- toggles zero and one, can be used with "getSign" to test for zero.
			s = GetNums(1)
			if length(s[1]) = 0 then
				ans = {{1}, 0, defaultTargetLength, defaultRadix}
			else
				ans = {{}, 0, defaultTargetLength, defaultRadix}
			end if
			
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans)
			
			--? ans
		case "sqrt2" then -- sqrt(x)
			s = GetNums(1)
			ans = EunSqrt(s[1])
			
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans[2])
			
			--? ans
		case "exp" then -- ExpExp() doesn't like large numbers.
			-- preliminary work on log:
			s = GetNums(1)
			ans = EunExp(s[1])
			
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans)
			
			--? ans
		case "ln" then -- natural logarithm
			-- preliminary work on log:
			s = GetNums(1)
			ans = EunLog(s[1])
			if length(ans) = 2 then
				ans = ans[1]
			end if
			
			-- EuNumber: put "ob" into stack "nums"
			stack:push(nums, ans)
			
			--? ans
-- END EUN functions:
		case else
			if atom(ob) then
				ob = ToEun(ob)
			end if
			-- input Eun
			if not Eun(ob) then
				if types:string(ob) and length(ob) then
					if t_alpha(ob[1]) then
						-- variable, get from "stored" map
						ob = map:get(stored, ob)
						if atom(ob) then
							ErrorAbort(7)
						end if
					else
						ob = ToEun(ob)
					end if
					
				elsif number_array(ob) then
					-- (this option might become unsupported)
					ob = {ob, length(ob) - 1, defaultRadix, defaultTargetLength}
				else
					ErrorAbort(8)
				end if
			end if
			-- EuNumber: put "ob" into stack "nums"
			ans = ob
			stack:push(nums, ans)
			
		end switch
	end while
	
	end if

	-- free memory:
	file = {}
	map:clear(labels)
	
end for

puts(fn, "\"complete\"\n")
close(fn)
puts(1, "Program \'" & programName & "\' done.\n")
--puts(2, "Press any key to continue . . .")
--if waitKey() then
--end if
--puts(2, "\n")
abort(0) -- program terminates properly

-- end of file

-- Copyright (c) 2016-2021 James Cook

include std/console.e
include std/filesys.e
include std/io.e
include std/search.e
include std/sort.e
include std/text.e
include std/types.e

sequence ignore, identifiers, file
ignore = {"func", "for", "is", "cr", "r"}
identifiers = {}
file = {}

procedure replace_identifiers_within_file(sequence names)
	sequence Name, name, last
	if not length(names) then
		return
	end if
	names = sort(names)
	last = {}
	for i = 1 to length(names) do
		name = names[i]
		if length(name) > 2 and not equal(name, last) then
			-- replace first character with uppercase
			if not find(name, ignore) then
				Name = name
				Name[1] = upper(Name[1])
				file = match_replace(name, file, Name)
			end if
		end if
		last = names[1]
	end for
end procedure

function get_names(sequence search, sequence from)
	sequence ret, s
	integer pos, start
	s = match_all(search, from) + length(search)
	ret = {}
	for i = 1 to length(s) do
		pos = s[i]
		if t_lower(from[pos]) then
			start = pos
			pos += 1
			while char_test(from[pos], {{'0', '9'}, {'a', 'z'}, {'A', 'Z'}, {'_', '_'}}) do
				pos += 1
			end while
			ret = append(ret, from[start..pos - 1])
		end if
	end for
	return ret
end function

function get_identifiers(sequence path_name, sequence item)
	sequence s, ids, filename
	integer pos, ch
	
	if find('d', item[D_ATTRIBUTES]) then
		-- Ignore directories
		return 0
	end if

	filename = item[D_NAME]
	
	printf(1, "%s...", {filename})
	
	file = read_file(filename)
	
-- 	s = find_all('_', file)
-- 	
-- 	count = 0
-- 	for i = length(s) to 1 by -1 do
-- 		pos = s[i] + 1
-- 		ch = file[pos]
-- 		if ch >= 'a' and ch <= 'z' then
-- 			file[pos] -= 'a' - 'A' -- MAKE UPPERCASE
-- 			file = file[1..s[i] - 1] & file[pos..$]
-- 			count += 1
-- 		end if
-- 	end for
	
	identifiers = insertion_sort(identifiers,
		  get_names("public type ", file) & get_names("public procedure ", file) & get_names("public function ", file)
		& get_names("global type ", file) & get_names("global procedure ", file) & get_names("global function ", file)
		)
	
	s = sort(get_names("type ", file) & get_names("procedure ", file) & get_names("function ", file))
	
	for i = 1 to length(s) do
		puts(1, s[i] & " ")
	end for
	puts(1, "\n")
	
	replace_identifiers_within_file(merge(identifiers, s))
	
	if write_file(filename, file) = 1 then
		puts(1, "Success!\n")
	else
		puts(1, "Failed to write file.\n")
	end if
	return 0
end function

object exit_code

puts(1, "ConvertEuphoria in current directory\n\n")

puts(1, "Program will Capitalize all Euphoria: types, functions, and procedures\n")
puts(1, "With *.e and *.ex file extensions,\n")
printf(1, "In directory: %s\n", {current_dir()})
puts(1, "Replace all Euphoria identifiers with Uppercase?\n")
puts(1, "Ignoring variables less than three (3) characters, and: ")
for i = 1 to length(ignore) do
	puts(1, ignore[i] & " ")
end for
puts(1, "\n")

if 0 = prompt_number("\nEnter one (1) to continue, or zero (0) to exit: ", {0, 1}) then
	abort(1)
end if

exit_code = walk_dir("*.e", routine_id("get_identifiers"))
exit_code = walk_dir("*.e", routine_id("get_identifiers")) -- keep, it needs to scan it again
exit_code = walk_dir("*.ex", routine_id("get_identifiers"))

printf(1, "%d global or public identifiers found\n", {length(identifiers)})
for i = 1 to length(identifiers) do
	puts(1, identifiers[i] & " ")
end for
puts(1, "\nAll replaced.\n")
puts(1, "done.\n")

--end of file.

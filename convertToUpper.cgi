-- Copyright (c) 2016-2021 James Cook

include std/io.e
include std/search.e

constant cmdln = command_line()

sequence file, s
integer pos, ch, count

file = read_file(cmdln[3])

s = find_all('_', file)

count = 0
for i = length(s) to 1 by -1 do
	pos = s[i] + 1
	ch = file[pos]
	if ch >= 'a' and ch <= 'z' then
		file[pos] -= 'a' - 'A' -- MAKE UPPERCASE
		file = file[1..s[i] - 1] & file[pos..$]
		count += 1
	end if
end for


if write_file(cmdln[3], file) = 1 then
	puts(1, "Success!\n")
	printf(1, "%d underscore replacements\n", {count})
else
	puts(1, "Failed to write file.\n")
end if

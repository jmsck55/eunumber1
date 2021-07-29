-- Copyright (c) 2016-2021 James Cook
-- Eunumber, using tasks.

with define USE_TASK_YIELD

-- with trace

atom t, t0

t0 = time()

include my.e

useTaskYield = TRUE

defaultRadix = 1000 -- NOTE: If you change this, also change "%03d", below, as well.

type boolean(integer x)
	return Bool(x)
end type

boolean calc_running


procedure checkHowComplete()
	while 1 do
		printf(1, "expHowComplete: %d out of %d\n", expHowComplete)
		task_yield()
	end while
end procedure


procedure calc(integer len)
	object a
	integer fn
	
	task_yield()
	
	calculationSpeed = len
	adjustRound = floor(len / 10)
	
	a = GetE(len)
	pretty_print(1, a, {0, 2, 1, 78, "%03d"})
	puts(1, "\n\n")
	? length(a[1])
	a = ToString(a)
	fn = open("test.txt", "w")
	printf(fn, "%s\n", {a})
	close(fn)
	
	task_yield()
	
	calc_running = FALSE
end procedure


puts(1, "main task: start, press \"q\" to force stop\n")

atom t1, t2

t1 = task_create(routine_id("checkHowComplete"), {})
t2 = task_create(routine_id("calc"), {200})

task_schedule(t1, {1, 2})
task_schedule(t2, 1)

calc_running = TRUE


while calc_running do
	if get_key() = 'q' then
		abort(1/0)
	end if
	task_yield()
end while

puts(1, "main task: stop\n")

t = time() - t0
printf(1, "%f seconds\n", t)

puts(1, "done.\n")
abort(0)

-- program ends when main task is finished


About EuNumber: (Documentation version: 0.1a)

EuNumber is math software.  Its name is derived from (Eu)phoria and the word, "Number".

EuNumber can do sequence calculations.

EuNumber is written in Euphoria, indexes start at 1. (In C and Java, indexes start at 0.)

"myeunumber.e" is the main source file, containing all the main core functions.

EuNumber has been translated into other programming languages, some can be found at:
https://github.com/jmsck55


In your main file, you can use:


	-- Copyright (c) 2020 Your Company
	-- Description of your program, then

	include my.e as my

	my:eun n1 = {{1,2,3}, 0, 100, 10}
	my:eun n2 = {{1,2,3}, 0, 100, 10}
	my:eun n3 = my:eun_add(n1,n2)

	-- Rest of your program...


Use: "include my.e", As a quick way to include most of the files in the library.

"as my" allows you to use syntax like "my:eun" in your program (as a namespace).
It can also be just "eun" if there are no naming conflicts.


Special Variables and Constants:

	global integer default_max = 100
	global integer default_radix = 10

	public constant ROUND_INF = 1 -- round towards +infinity or -infinity
	public constant ROUND_ZERO = 2 -- round towards zero
	
	public constant ROUND_POS_INF = 4 -- round towards positive +infinity
	public constant ROUND_NEG_INF = 5 -- round towards negative -infinity

	public constant ROUND_DOWN = ROUND_NEG_INF
	public constant ROUND_UP = ROUND_POS_INF

	-- global for "do_file.ex":
	global integer ROUND = ROUND_INF

	global positive_integer more_accuracy = 15 -- increase this value if maxlength is greater than 100.

	ifdef BITS64 then

		public constant INT_MAX = power(2,62) - 1 -- value: 4611686018427387903
		public constant MAX_RADIX = power(2,floor(62/2)-4) -- value: 134217728
		public constant INT_MAX10 = power(10,18) -- value: 1000000000000000000
		public constant MAX_RADIX10 = power(10,9-1) -- value: 100000000

	elsedef

		public constant INT_MAX = power(2,30) - 1 -- value: 1073741823
		public constant MAX_RADIX = power(2,floor(30/2)-4) -- value: 2048
		public constant INT_MAX10 = power(10,9) -- value: 1000000000
		public constant MAX_RADIX10 = power(10,4-1) -- value: 1000

	end ifdef

Description:

"radix" is the base of a number in a number system (examples: 2,3,5,10,16,256,1000, any integer "equal to" or "greater than" "2")
"max" or "maxlength" is the maximum number of digits in the sequence array of digits
"arrays" and "objects" are dynamically allocated in this software library.
"digits" can be (-radix+1, to radix-1)
"negative numbers" contain only negative digits in the sequence array.
"positive numbers" contain only positive digits in the sequence array.
"value zero" contains no elements in the sequence array (example: zero = { {}, 0, max, radix } )

"an eun" contains elements: {sequence_array, exponent, maxlength, radix}
"sequence_array" can be of any length (non-negative) up to the memory on your computer. (ordered in big-endian)
"exponent" or "exp" can be any integer value, points to the exponent of the highest order digit (first digit in big-endian)
  similar to scientific notation
"maxlength": lengths greater than about 200 tend to take longer to calculate. (varies from computer to computer)
"radix": see above.

"default_max = 100" is used in certain functions when "max" is not known. (It does not have to be 100)
"default_radix = 10" is the default radix used when "radix" is not known. (It is better, for debugging, as a multiple of 10)

"ROUND" is used with the "adjust_round()" function.  When the last digit is half (example: 5) the radix (example: 10), then
adjust_round() will round it according to ROUND.

ROUND defaults to ROUND_INF, can be any of the following:

ROUND_INF: it will round it towards positive infinity or negative infinity (away from zero).
ROUND_ZERO: will round it towards zero.
ROUND_HALF: will keep the half radix (example: 5) and not round "up" or "down".
ROUND_POS_INF: will round "up" -- towards positive infinity.
ROUND_NEG_INF: will round "down" -- towards negative infinity.
ROUND_TRUNC: will not round at all, and will truncate the sequence array to "max"


*** "more_accuracy" is probably one of the most important variables of all. ***

It determines the accuracy of the "mult_inv" function, which all higher mathematics functions use.
With greater values of "maxlength", you will have to "increase" "more_accuracy" accordingly.
With smaller values of "maxlength", you can "decrease" "more_accuracy" somewhat.


Purpose:

The whole purpose of EuNumber is to be able to calculate numbers, with accuracy, to the last decimal point,
or next to the last decimal point.  

This can be accomplished using the variables and constants described in the above section.


In 64-bit, you can store larger integer values, greatly increasing the amount of data that can be stored in those numbers.
In 32-bit, you can safely set radix to 1000, but in 64-bit, you can safely set radix to 100000000.  That is 3 compared to 8.
You can then safely divide the old maxlength by log10(new radix) giving you an equivalent new maxlength to use with the new radix.
Other radixes can use: new_maxlength = old_maxlength / ( log(new_radix) / log(old_radix) )
Or, without parenthesis: new_maxlength = old_maxlength * log(old_radix) / log(new_radix)

This way you can achieve greater speed.

Use: eun_convert() to switch between radixes.


Functions, or API:

Source File: myeunumber.e

-- eun (type)
-- new_eun
-- eun_mult
-- eun_add
-- eun_negate
-- eun_subtract
-- eun_mult_inv
-- eun_divide
-- eun_convert

-- eun_compare
-- eun_reverse -- reverse endian
-- eun_frac_part -- returns the fraction part (no rounding)
-- eun_int_part -- returns the integer part (no rounding)
-- eun_round_sig -- round to number of significant digits
-- eun_round -- round to nearest integer

Source File: numio.e

-- public function base_radix(sequence s, integer radix)
-- public type string(sequence st)
-- public type data_t(sequence d)
-- 
-- public function to_string(data_t d)
-- public function to_sequence(data_t d)
-- public function to_atom(data_t d)
-- public function string_to_number_exp(sequence st)
-- public function to_eun(data_t d, integer radix = default_radix, integer max = default_max)

Source File: nth_root.e

-- public function eun_nth_root(integer n, eun n1, eun guess)
-- public function eun_square_root(eun n1, eun guess)
-- public function eun_sqrt(eun n1)

Source File: quadeq.e

-- public function eun_quadratic_equation(eun a, eun b, eun c)

Source File: mycomplex.e

-- public type complex(object x)
-- public function new_complex(eun real, eun imag)
-- public function negate_imag(complex a)
-- public function complex_add(complex a, complex b)
-- public function complex_negate(complex b)
-- public function complex_subtract(complex a, complex b)
-- public function complex_mult(complex n1, complex n2)
-- public function complex_mult_inv(complex n2)
-- public function complex_divide(complex n1, complex n2)
-- public function complex_sqrt(complex a)
-- public function complex_quadratic_equation(complex a, complex b, complex c)

Source File: mymath.e

Still a work in progress:

exponentiation, logarithms, and trig functions

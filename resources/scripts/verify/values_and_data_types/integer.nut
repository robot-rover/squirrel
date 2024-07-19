local a = 123 //decimal
local b = 0x0012 //hexadecimal
local c = 075 //octal
local d = 'w' //char code

::print(a + "\n")
::print(b + "\n")
::print(c + "\n")
::print(d + "\n")

// Also testing boolean and null here
local t = true
local f = false
local n = null

::print(t + "\n")
::print(f + "\n")
::print(n + "\n")

// Some basic arithmetic
::print((a + b) + "\n")
::print((a - b) + "\n")
::print((a * b) + "\n")
::print((a / b) + "\n")

// Test modulus w/ positive and negative numbers
::print((5 % 2) + "\n")
::print((5 % 3) + "\n")
::print((-5 % 2) + "\n")
::print((-5 % 3) + "\n")
::print((-5 % -2) + "\n")
::print((-5 % -3) + "\n")
::print((5 % -2) + "\n")
::print((5 % -3) + "\n")
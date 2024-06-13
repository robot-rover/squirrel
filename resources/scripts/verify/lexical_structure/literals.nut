function println(val) {
    ::print(val + "\n")
}

local int_b10 = 34
println(int_b10)
local int_hex = 0xFF00A120
println(int_hex)
local int_oct = 0753
println(int_oct)
local int_char = 'a'
println(int_char)

local float = 1.52
println(float)
local float_sci = 1.e-2
println(float_sci)
local float_sci2 = 1.2e2
println(float_sci2)
// TODO: Doesn't work in squirrel either, how to test failures?
// println(01.E+2)
// println(01.E+2)
// println(.1E+2)

local str = "I'm a string"
println(str)
local str_verb = @"I'm a verbatim string"
println(str_verb)
local str_verb_multi = @"I'm a verbatim string
that spans multiple lines"
println(str_verb_multi)
// TODO: Check escape sequences?

/*
multiline comment
*/

# single line comment
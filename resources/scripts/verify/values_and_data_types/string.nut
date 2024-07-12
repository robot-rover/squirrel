// Escape Sequences

local tab = "\t"
local a = "\a"
local b = "\b"
local newline = "\n"
local carriage = "\r"
local v = "\v"
local f = "\f"
local back = "\\"
local quote = "\""
local single = "\'"
local zero = "\0"
local hexcode = "\x66"
local unicodes = "\u0067"
local unicodel = "\U00000067"

::print(tab + "\n")
print(a + "\n")
print(b + "\n")
print(newline + "\n")
print(carriage + "\n")
print(v + "\n")
print(f + "\n")
print(back + "\n")
print(quote + "\n")
print(single + "\n")
print(zero + "\n")
print(hexcode + "\n")
print(unicodes + "\n")
print(unicodel + "\n")

local normal = "I'm a wonderful string\n"
// has a newline at the end of the string
local verb = @"I'm a verbatim string\n"
// the \n is literal, similar to "\\n" in a regular string.
local multiline = @"
    this is a multiline string
    it will ""embed"" all the new line
    characters
"
::print(normal + "\n")
::print(verb + "\n")
::print(multiline + "\n")
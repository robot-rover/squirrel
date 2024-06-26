local a = 0

function ref_a() {
    ::print(a + "\n")
    a = a + 1
}

ref_a()
ref_a()
a += 1
ref_a()
::print(a + "\n")

function dec_b() {
    local b = "I am b"
    function ref_b() {
        ::print(b + "\n")
    }

    return ref_b
}

local my_ref_b = dec_b()

my_ref_b()

local outer_ref_c

foreach (i in [0, 1]) {
    local c = "I am c"
    outer_ref_c = function() {
        ::print(c + "\n")
    }
}

outer_ref_c()

// Error: c is not defined
// ::print(c)

foreach (i in [0]) local d = "I am d"

// Error: d is not defined
// ::print(d)

{
    local e = "I am e"
}

// Error: e is not defined
// ::print(e)
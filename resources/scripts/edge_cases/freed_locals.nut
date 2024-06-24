local a = "I am a"

function ref_a() {
    ::print(a + "\n")
}

ref_a()

function dec_b() {
    local b = "I am b"
    function ref_b() {
        ::print(b + "\n")
    }

    return ref_b
}

local my_ref_b = dec_b()

my_ref_b()
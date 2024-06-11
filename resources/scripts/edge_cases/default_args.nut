a <- 0

function inc_a() {
    a = a + 1
    return a
}

function rerun_test(a_arg = inc_a()) {
    ::print(a_arg + "\n")
}

rerun_test()
rerun_test()
rerun_test()

tab <- { val = 0 }

function mut_test(b_arg = tab) {
    b_arg.val = b_arg.val + 1
    ::print(b_arg.val + "\n")
}

mut_test()
mut_test()
mut_test()

// TODO: Check if value types are mutable too
return 0
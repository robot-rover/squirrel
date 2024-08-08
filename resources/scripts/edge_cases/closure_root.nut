whoami <- "root"

a <- {
    whoami = "a"
}

b <- {
    whoami = "b"
}

local print = ::print

setroottable(b)

function show_root(p) {
    p("root: " + ::whoami + "\n")
}

show_root(print)

# TODO: Should have a test in verify for cloning behavior
local sr = show_root

show_root.setroot(a)

sr(print)
show_root(print)
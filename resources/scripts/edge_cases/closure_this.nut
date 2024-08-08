function show_this() {
    ::print("this: " + this.whoami + "\n")
}

whoami <- "root"

a <- {
    whoami = "a"
    show_this = show_this
}

local st = show_this

local st2 = a.show_this

::print("-- Should be root --\n")
show_this()
rawcall(show_this, getroottable())
st()
st2()

::print("-- Should be a --\n")
a.show_this()
rawcall(show_this, a)
rawcall(st, a)

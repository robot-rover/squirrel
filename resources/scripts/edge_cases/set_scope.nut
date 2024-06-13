function show_keys(t) {
    ::print(t + "[")
    foreach (idx, key in t.keys()) {
        ::print(key)
        if (idx < t.len() - 1) {
            ::print(", ")
        }
    }
    ::print("]\n")
}

a_del <- {
    a_del_field = "I am a_del"
    function _tostring() {
        return "<I am table a>"
    }
}

a <- {
    key1 = 1
    key2 = 2
    function what_is_this() {
        ::print("  this: " + this + "\n")
        ::print("  base: " + base + "\n")
        ::print("  del: " + this.getdelegate() + "\n")
        ::print("  a_del: " + a_del + "\n")
        ::print("  root: " + getroottable() + "\n")
    }
}
a.setdelegate(a_del)


print("a: ")
show_keys(a)

print("root: ")
show_keys(getroottable())
assert(getroottable() == this)

print("This Check:\n")
a.what_is_this()
print("End This Check\n")

dofile("resources/scripts/edge_cases/parent_scope.nut")

b <- {
    set_this = "unset"
    set = "unset2"
    function set_stuff() {
        this.set_this = "set this"
        this.ns_this <- "ns this"
        set = "set"
        ns <- "ns"
        ::print("Finding a in env")
        show_keys(this)
        show_keys(getroottable())
        ::print(a)
        // The fact that this works is insanity and I'm not implementing it
        ::print(this.a)
        ::print(this.getdelegate())
        ::print("\n")
    }
}

print("Set Stuff Check\n")
show_keys(b)
b.set_stuff()
show_keys(b)
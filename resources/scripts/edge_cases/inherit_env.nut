function parent() {
    local child = function() {
        a <- "test"
        return this
    }
    ::print(this + "\n")
    return child.bindenv({})
}

::print("First, the root table\n")
::print(getroottable() + "\n")
::print(parent()() + "\n")
::print("\n")

table <- {
    parent = parent.bindenv({})
}

::print("Now, a table with a parent function\n")
::print(table + "\n")
::print(table.parent()() + "\n")
::print("\n")
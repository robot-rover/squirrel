::print("With Tables:\n")
local customTable ={
    _typeof = function ()
    {
        ::print("Table _typeof called\n")
        return "customTable"
    }

    function _call(other) {
        ::print("Other: " + other + "\n")
        ::print("Table _call called (" + (other == getroottable()) + ")\n")
    }

    function _cloned(original) {
        ::print("Table _cloned called (" + original + ")\n")
    }

    function _tostring() {
        return "customTableStringed"
    }

    // NOTE: This doesn't work on tables, only classes
    function _nexti(prev_idx) {
        ::print("Table _nexti called (" + prev_idx + ")\n")
        if (prev_idx < 10) {
            return prev_idx + 1
        } else {
            throw null
        }
    }
}

local a={}.setdelegate(customTable);

::print("a: " + a + "\n")
a()
local ac = clone a
::print((typeof ac) + "\n")

// This iterates 0 times!
::print("Loop:\n")
foreach (i in a) {
    ::print(i + "\n")
}

customTable._call = function(other, a, b, c) {
    ::print("Table _call called with args (" + a + ", " + b + ", " + c + ")\n")
}

::print("Calling _call with args:\n")
a(1, 2, 3)

::print("\nWith Classes:\n")
class CustomClass {
    data = null
    _typeof = function ()
    {
        ::print("Class _typeof called\n")
        return "customClass"
    }

    function _call(other) {
        ::print("Class _call called (" + (other == getroottable()) + ")\n")
    }

    function _cloned(original) {
        ::print("Class _cloned called (" + original + ")\n")
    }

    function _tostring() {
        return "customClassStringed"
    }

    function _nexti(prev_idx) {
        ::print("Table _nexti called (" + prev_idx + ")\n")
        if (this.data == null) {
            this.data = 0
        } else {
            this.data += 1
        }
        if (this.data == 5) {
            this.data = null
            return null
        }
        return "data"
    }
}

local b = CustomClass();

::print("b: " + b + "\n")
b()
local bc = clone b
::print((typeof bc) + "\n")

::print("Loop:\n")
foreach (i in b) {
    ::print(i + "\n")
}

class CallArgs {
    function _call(other, a, b, c) {
        ::print("Class _call called with args (" + a + ", " + b + ", " + c + ")\n")
    }
}

local c = CallArgs()

::print("Calling _call with args:\n")
c(1, 2, 3)
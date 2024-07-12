::print("With Tables:\n")
local myslots={
    _set = function (idx, val)
    {
        ::print("Table _set called\n")
        if (this.rawin(idx)) {
            this.rawset(idx, val)
        } else {
            throw null
        }
    }

    function _get(idx)
    {
        ::print("Table _get called\n")
        if (this.rawin(idx)) {
            return this.rawget(idx)
        } else {
            throw null
        }
    }

    function _newslot(key, value) {
        ::print("Table _newslot called\n")
        this.rawset(key, value)
    }

    function _delslot(key) {
        ::print("Table _delslot called\n")
        this.rawdelete(key)
    }
}

local a={ desc="Hello" }.setdelegate(myslots);

::print(("ns" in a) + "\n")
a.ns <- "New Slot"
::print(("ns" in a) + "\n")
::print(a.ns + "\n")
a.ns = "New Value"
::print(a.ns + "\n")
delete a.ns
::print(("ns" in a) + "\n")


::print("\nWith Classes:\n")
class MySlots {
    data = null
    constructor() {
        this.data = {}
    }
    _set = function (idx, val)
    {
        ::print("Table _set called\n")
        if (this.data.rawin(idx)) {
            this.data.rawset(idx, val)
        } else {
            throw null
        }
    }

    function _get(idx)
    {
        ::print("Table _get called\n")
        if (this.data.rawin(idx)) {
            return this.data.rawget(idx)
        } else {
            throw null
        }
    }

    function _newslot(key, value) {
        ::print("Table _newslot called\n")
        this.data.rawset(key, value)
    }

    function _delslot(key) {
        ::print("Table _delslot called\n")
        this.data.rawdelete(key)
    }
}

local b = MySlots();

::print(("ns" in b.data) + "\n")
b.ns <- "New Slot"
::print(("ns" in b.data) + "\n")
::print(b.ns + "\n")
b.ns = "New Value"
::print(b.ns + "\n")
delete b.ns
::print(("ns" in b.data) + "\n")

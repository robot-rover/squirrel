::print("With Tables:\n")
local comparable={
    _cmp = function (other)
    {
        ::print("Table _cmp called\n")
        if(name<other.name)return -1;
        if(name>other.name)return 1;
        return 0;
    }
}

local a={ name="Alberto" }.setdelegate(comparable);
local b={ name="Wouter" }.setdelegate(comparable);

if(a>b)
    print("a>b\n")
else
    print("b<=a\n");

if(comparable==comparable)
    print("comparable==comparable\n")
else
    print("comparable!=comparable\n");

::print("\nWith Classes:\n")
class Comparable {
    constructor(n)
    {
        name = n;
    }
    function _cmp(other)
    {
        ::print("Class _cmp called\n")
        if(name<other.name) return -1;
        if(name>other.name) return 1;
        return 0;
    }
    name = null;
}

local a = Comparable("Alberto");
local b = Comparable("Wouter");

if(a>b)
    print("a>b\n")
else
    print("b<=a\n");
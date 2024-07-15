function gauntlet(one, two) {
    ::print((one + two) + "\n")
    ::print((two + one) + "\n")
    ::print((one - two) + "\n")
    ::print((two - one) + "\n")
    ::print((one * two) + "\n")
    ::print((two * one) + "\n")
    ::print((one / two) + "\n")
    ::print((two / one) + "\n")
    ::print(((one + two) % two) + "\n")
    ::print(((two + one) % one) + "\n")
}

::print("With Tables:\n")
function make_table_delegate(name) {
    return {
        _add = function (other)
        {
            ::print(name + " _add called\n")
            return {n=(this.n + other.n)}.setdelegate(this.getdelegate())
        }

        function _sub(other) {
            ::print(name + " _sub called\n")
            return {n=(this.n - other.n)}.setdelegate(this.getdelegate())
        }

        _mul = function (other)
        {
            ::print(name + " _mul called\n")
            return {n=(this.n * other.n)}.setdelegate(this.getdelegate())
        }

        function _div(other) {
            ::print(name + " _div called\n")
            return {n=(this.n / other.n)}.setdelegate(this.getdelegate())
        }

        function _modulo(other) {
            ::print(name + " _modulo called\n")
            return {n=(this.n % other.n)}.setdelegate(this.getdelegate())
        }

        function _tostring() {
            return name + "{n=" + this.n + "}"
        }
    }
}

local del1 = make_table_delegate("Table1")
local del2 = make_table_delegate("Table2")

local onet={n=1}.setdelegate(del1);
local twot={n=2}.setdelegate(del2);
gauntlet(onet, twot)

::print("\nWith Classes:\n")
local ArithC
ArithC = class {
    n = 0
    name = null
    constructor(n, name) {
        this.n = n
        this.name = name
    }
    _add = function (other)
    {
        ::print(this.name + " _add called\n")
        return ArithC(this.n + other.n, name)
    }

    function _sub(other) {
        ::print(this.name + " _sub called\n")
        return ArithC(this.n - other.n, name)
    }

    _mul = function (other)
    {
        ::print(this.name + " _mul called\n")
        return ArithC(this.n * other.n, name)
    }

    function _div(other) {
        ::print(this.name + " _div called\n")
        return ArithC(this.n / other.n, name)
    }

    function _modulo(other) {
        ::print(this.name + " _modulo called\n")
        return ArithC(this.n % other.n, name)
    }

    function _tostring() {
        return this.name + "(n=" + this.n + ")"
    }
}

local onec=ArithC(1, "Class1")
local twoc = ArithC(2, "Class2")

gauntlet(onec, twoc)

::print("\nWith a mix:\n")

gauntlet(onet, twoc)
gauntlet(onec, twot)

::print("\nNow with strings:\n")

local hello = "Hello World!"
::print(onet + hello)
::print("\n")
::print(hello + onet)
::print("\n")

::print(onec + hello)
::print("\n")
::print(hello + onec)
::print("\n")
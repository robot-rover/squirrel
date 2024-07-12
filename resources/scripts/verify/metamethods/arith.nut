::print("With Tables:\n")
local arithT = {
    _add = function (other)
    {
        ::print("Table _add called\n")
        return {n=(this.n + other.n)}.setdelegate(this.getdelegate())
    }

    function _sub(other) {
        ::print("Table _sub called\n")
        return {n=(this.n - other.n)}.setdelegate(this.getdelegate())
    }

    _mul = function (other)
    {
        ::print("Table _mul called\n")
        return {n=(this.n * other.n)}.setdelegate(this.getdelegate())
    }

    function _div(other) {
        ::print("Table _div called\n")
        return {n=(this.n / other.n)}.setdelegate(this.getdelegate())
    }

    function _modulo(other) {
        ::print("Table _modulo called\n")
        return {n=(this.n % other.n)}.setdelegate(this.getdelegate())
    }

    function _unm() {
        ::print("Table _unm called\n")
        return {n=-this.n}.setdelegate(this.getdelegate())
    }

    function _tostring() {
        return "{n=" + this.n + "}"
    }
}

local one={n=1}.setdelegate(arithT);
local two={n=2}.setdelegate(arithT);

::print((one + two) + "\n")
::print((one - two) + "\n")
::print((one * two) + "\n")
::print((one / two) + "\n")
::print(((one + two) % two) + "\n")

::print("\nWith Classes:\n")
local ArithC
ArithC = class {
    n = 0
    constructor(n) {
        this.n = n
    }
    _add = function (other)
    {
        ::print("Class _add called\n")
        return ArithC(this.n + other.n)
    }

    function _sub(other) {
        ::print("Class _sub called\n")
        return ArithC(this.n - other.n)
    }

    _mul = function (other)
    {
        ::print("Class _mul called\n")
        return ArithC(this.n * other.n)
    }

    function _div(other) {
        ::print("Class _div called\n")
        return ArithC(this.n / other.n)
    }

    function _modulo(other) {
        ::print("Class _modulo called\n")
        return ArithC(this.n % other.n)
    }

    function _unm() {
        ::print("Class _unm called\n")
        return ArithC(n=-this.n)
    }

    function _tostring() {
        return "ArithC(n=" + this.n + ")"
    }
}

local one=ArithC(1)
local two = ArithC(2)

::print((one + two) + "\n")
::print((one - two) + "\n")
::print((one * two) + "\n")
::print((one / two) + "\n")
::print(((one + two) % two) + "\n")

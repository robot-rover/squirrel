local t={}
local test=
{
    a=10
    b=function(a) { return a+1; }
}

::print(t.tostring().slice(0,11) + "\n")
::print(test.tostring().slice(0,11) + "\n")
::print(test.a + "\n")
::print(test.b(test.a) + "\n")
local t={}
local test=
{
    a=10
    b=function(a) { return a+1; }
}

::print(t + "\n")
::print(test + "\n")
::print(test.a + "\n")
::print(test.b(test.a) + "\n")
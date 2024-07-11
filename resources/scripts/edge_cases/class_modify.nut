class Foo {
    a = 0
    b = 0
}

Foo.c <- 0

Foo.constructor <- function(){
    ::print("Constructing Foo\n")
}

local fooinst = Foo()

// This is an error
// Foo.d <- 0
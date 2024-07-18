class MyClass {
    function constructor(n) {
        this.n = n
    }

    n = 0
    test = null
}
MyClass.test2 <- "test2"
MyClass[10] <- 10

local one = MyClass(1)

::print(one.n + "\n")
::print(one.test + "\n")
// Classes don't support newslots
// one.test <- "Hello"
::print(one.test + "\n")
::print(one.test2 + "\n")
::print(one[10] + "\n")

::print(MyClass.n + "\n")
::print(MyClass.test2 + "\n")
::print(MyClass[10] + "\n")
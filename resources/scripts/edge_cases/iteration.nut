table <- {
    a = 0
    c = 2
    b = 1
}

arr <- [0, 1, 2]

function generator() {
    for (local i = 0; i < 3; i++) {
        yield i
    }
}

class Class {
    a = 0
    b = 1
    c = 2
}

::print("table\n")
foreach (k, v in table) {
    ::print(k + " " + v + "\n")
}
::print("\n")

::print("arr\n")
foreach (k, v in arr) {
    ::print(k + " " + v + "\n")
}
::print("\n")

::print("generator\n")
foreach (k, v in generator()) {
    ::print(k + " " + v + "\n")
}
::print("\n")

::print("class\n")
foreach (k, v in Class) {
    ::print(k + " " + v + "\n")
}
::print("\n")
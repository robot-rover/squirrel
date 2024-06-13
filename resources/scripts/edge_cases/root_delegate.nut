new_root <- {
    a = "Hello from new_root"
}

new_root_del <- {
    b = "Hello from new_root_del"
}

function array_tostring(arr) {
    local str = "["
    foreach (idx, val in arr) {
        str = str + val
        if (idx < arr.len() - 1) {
            str = str + ", "
        }
    }
    return str + "]"
}

new_root.setdelegate(new_root_del)

::print(new_root.a + "\n")
::print(new_root.b + "\n")

function is_changed(print) {
    print(::a + "\n")
    print(::b + "\n")
}

is_changed.setroot(new_root)

is_changed(print)
::print("Done\n")

// function test2(print) {
//     print(a + "\n")
// }

// test2(print)
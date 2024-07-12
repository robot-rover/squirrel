local table = {
    function delete_a() {
        // This will never delete a from the root table, only this!
        delete a;
    }

    function delete_root_a() {
        delete ::a;
    }

    a = "Hello"
}

a <- "Root A"

::print(("a" in this) + ", " + ("a" in table) + "\n")
table.delete_a()
::print(("a" in this) + ", " + ("a" in table) + "\n")
table.delete_root_a()
::print(("a" in this) + ", " + ("a" in table) + "\n")
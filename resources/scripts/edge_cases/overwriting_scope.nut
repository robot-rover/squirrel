a <- "a defined at root"

function fn_newslot() {
    a <- "a defined in fn_newslot"
}

function fn_set() {
    a = "a defined in fn_set"
}

::print(a + "\n")
fn_newslot()
::print(a + "\n")
fn_set()
::print(a + "\n")

::print("\n");

tab <- {
    b = "b defined in tab"

    function fn_newslot() {
        b <- "b defined in tab.fn_newslot"
    }

    function fn_set() {
        b = "b defined in tab.fn_set"
    }
}

::print(tab.b + "\n")
tab.fn_newslot()
::print(tab.b + "\n")
tab.fn_set()
::print(tab.b + "\n")

::print("\n");

c <- "c defined at root"
tab2 <- {
    function fn_newslot() {
        c <- "c defined in tab2.fn_newslot"
    }

    function fn_set() {
        c = "c defined in tab2.fn_set"
    }

    function fn_this() {
        this.c <- "c defined in tab2.fn_this"
    }

    function insane() {
        ::print("insane: " + this.c + "\n")
    }
}

::print(c + "\n")
tab2.insane()

tab2.fn_newslot()
::print(c + "\n")
tab2.insane()

tab2.fn_set()
::print(c + "\n")
tab2.insane()

tab2.fn_this()
::print(c + "\n")
tab2.insane()

::print("\n");

del <- {
    d = "d defined in del"
}

inst <- {
    function fn_newslot() {
        d <- "d defined in inst.fn_newslot"
    }

    function fn_set() {
        d = "d defined in inst.fn_set"
    }

    function fn_this() {
        this.d <- "d defined in inst.fn_this"
    }
}
inst.setdelegate(del)

::print(" del.d: " + del.d + "\n")
::print("inst.d: " + inst.d + "\n")

inst.fn_set()
::print(" del.d: " + del.d + "\n")
::print("inst.d: " + inst.d + "\n")

inst.fn_newslot()
::print(" del.d: " + del.d + "\n")
::print("inst.d: " + inst.d + "\n")

inst.fn_this()
::print(" del.d: " + del.d + "\n")
::print("inst.d: " + inst.d + "\n")
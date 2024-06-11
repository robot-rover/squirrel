table_a <- {
    ident = "Table A"
}

table_b <- {
    ident = "Table B"
}

ident <- "File Table"

function get_ident() {
    if (this == null) {
        ::print("No ident\n")
    } else {
        ::print(this.ident + "\n")
    }
}

get_ident()

table_a.get_ident <- get_ident
table_a.get_ident()

table_b.get_ident <- get_ident
table_b.get_ident()

rawcall(get_ident, null)

arr <- [get_ident]
arr[0]()
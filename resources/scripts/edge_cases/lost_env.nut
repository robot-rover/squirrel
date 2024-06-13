env <- {
    field = "I am ENV"
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

function going_to_lose() {
    ::print(this + "\n")
    this.field <- "I am new"
    return this
}

rawcall(going_to_lose, env)
::print(array_tostring(env.keys()) + "\n")
rawcall(going_to_lose, null)
if ("a" in this) {
    ::print(a.tostring() + "\n")
}

print("root: ")
print(getroottable())
assert(this == getroottable())
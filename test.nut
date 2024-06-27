local arr = [0, 1, 2]
foreach(v in arr) {
    print(v)
    if (v < 3) {
        arr.push(v + 3)
    }
}
print("\n")

local obj = {[3]="3", [4]="4", [5]="5"}
foreach (k, v in obj) {
    print(k + " " + v + "\n")
    if (k >= 3) {
        obj[k - 3] <- (k - 3).tostring()
    }
}

print("\n")
foreach (k, v in obj) {
    print(k + " " + v + "\n")
}
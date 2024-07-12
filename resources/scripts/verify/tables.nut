local a = {}

// This would fail
// a.newslot = 1234

a.newslot <- 1234

a[1] <- "I'm the value of the new slot";

local num = delete a.newslot;

assert(num == 1234)
assert(!("newslot" in a))

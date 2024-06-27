## Differences from Squirrel

### Assigning to `this` will not access the root table
In squirrel, accessing a bare identifier will resolve in the following order
1. matching local variables
    a. from this scope
    b. from outer scopes
2. matching slots in the environment
    a. from this environment
    b. from any delegates
3. matching slots on the root table on the root table

in the squirrel reference implementation, accessing a slot on the `this` object
will access a matching slot on the root table if there is no slots matching
**2.** from above. Likewise when assigning to `this` with normal (not newslot)
assignment. *Chipmunk* does not match this behavior. If you access `this` in
chipmunk, only slots in **2..** from above will be resolved.

### Mutation during iteration
In squirrel, mutating a value that is being iterated causes wierd behavior.

Mutating an array by appending to it while iterating produces the intended
behaior, but inserting at or before the current element causes elements to
be returned multible times.

Adding slots to a table that is being iterated can cause duplicated elements,
missed elements, or any combination thereof, depending on the ordering of key
hashes.

In *Chipmunk*, it is an error to mutate a container that is being iterated.
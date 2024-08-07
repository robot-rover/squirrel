env_mem <- "env_mem"

obj <- {
    obj_mem = "obj_mem"
}

local local_mem = "local_mem"

delete env_mem
delete obj.obj_mem
// This is an error
// delete local_mem

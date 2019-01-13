require "src/preemptive"

local args = { ... }
local file = args[1]
local chunk, err = loadfile(file)
print(err)
preempt(chunk)
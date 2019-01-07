require "preemptive"

local args = { ... }
local file = args[1]
local chunk = loadfile(file)


preempt(chunk)
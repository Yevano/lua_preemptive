require "src/chunk"
require "src/instruction"

function preempt(f)
    local file = io.open("test.b", "wb")
    file:write(string.dump(f))
    file:close()

    local chunk = Chunk.new(ChunkInputStream.new(f))
    --[[chunk.insert_instruction(1, instruction(30, 0, 1))

    for i = 1, #chunk.instructions do
        print(inst_to_string(chunk.instructions[i]))
    end]]

    local modified, err = chunk.compile()

    if modified then
        modified()
    else
        print(err)
    end
end
ChunkInputStream = {
    new = function(chunk)
        local pos = 0
        local chunk_str = string.dump(chunk)

        local self = {
            read_byte = function()
                local ret = chunk_str:sub(pos, pos)
                pos = pos + 1
                return ret
            end
        }

        return self
    end
}

function parse_chunk(chunk)
    local chunk_in = ChunkInputStream.new(chunk)
end
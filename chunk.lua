local bit = require "bitop-lua/src/bitop/funcs"

ChunkInputStream = {
    new = function(chunk)
        local pos = 1
        local chunk_str = string.dump(chunk)

        local self
        self = {
            read_byte = function()
                local ret = chunk_str:sub(pos, pos):byte()
                pos = pos + 1
                return ret
            end,

            read_uint = function()
                return 
                      bit.lshift(self.read_byte(), 24)
                    + bit.lshift(self.read_byte(), 16)
                    + bit.lshift(self.read_byte(), 8)
                    + self.read_byte()
            end
        }

        return self
    end
}

function parse_chunk(chunk)
    local parsed_chunk = { }
    parsed_chunk.chunk_in = ChunkInputStream.new(chunk)
    parsed_chunk.header = chunk_in.read_uint()
    parsed_chunk.version = chunk_in.read_byte()
    parsed_chunk.format_version = chunk_in.read_byte()
    parsed_chunk.endianness = chunk_in.read_byte()
    parsed_chunk.int_size = chunk_in.read_byte()
    parsed_chunk.size_t_size = chunk_in.read_byte()
    parsed_chunk.instruction_size = chunk_in.read_byte()
    parsed_chunk.lua_number_size = chunk_in.read_byte()
    parsed_chunk.integral_flag = chunk_in.read_byte()

    -- TODO: Check if header is supported.

    
end
local bit = require "bitop-lua/src/bitop/funcs"

ChunkInputStream = {
    new = function(chunk)
        local pos = 1
        local chunk_str = string.dump(chunk)

        local signature
        local version
        local format_version
        local endianness
        local int_size
        local size_t_size
        local instruction_size
        local lua_number_size
        local integral_flag

        local self
        self = {
            read_byte = function()
                local ret = chunk_str:sub(pos, pos):byte()
                pos = pos + 1
                return ret
            end,

            read_uint = function(n)
                local ret = 0
                
                for i = 1, n do
                    ret = ret + (bit.lshift(self.read_byte(), (i - 1) * 8))
                end

                return ret
            end,

            read_integer = function()
                return self.read_uint(int_size)
            end,

            read_size_t = function()
                return self.read_uint(size_t_size)
            end,

            read_string = function()
                local size = self.read_size_t()

                if size == 0 then return nil end

                local ret = { }
                
                for i = 1, size - 1 do
                    ret[i] = string.char(self.read_byte())
                end

                pos = pos + 1
                return table.concat(ret)
            end
        }

        signature = self.read_uint(4)
        version = self.read_byte()
        format_version = self.read_byte()
        endianness = self.read_byte()
        int_size = self.read_byte()
        size_t_size = self.read_byte()
        instruction_size = self.read_byte()
        lua_number_size = self.read_byte()
        integral_flag = self.read_byte()

        print("signature: " .. signature)
        print("version: " .. version)
        print("format_version: " .. format_version)
        print("endianness: " .. endianness)
        print("int_size: " .. int_size)
        print("size_t_size: " .. size_t_size)
        print("instruction_size: " .. instruction_size)
        print("lua_number_size: " .. lua_number_size)
        print("integral_flag: " .. integral_flag)

        return self
    end
}

local FormatTypes = { ABC = 0, ABx = 1, AsBx = 2 }

local inst_formats = {
    MOVE         = FormatTypes.ABC,
    LOADK        = FormatTypes.ABx,
    LOADBOOL     = FormatTypes.ABC,
    LOADNIL      = FormatTypes.ABC,
    GETUPVAL     = FormatTypes.ABC,
    GETGLOBAL    = FormatTypes.ABx,
    GETTABLE     = FormatTypes.ABC,
    SETGLOBAL    = FormatTypes.ABx,
    SETUPVAL     = FormatTypes.ABC,
    SETTABLE     = FormatTypes.ABC,
    NEWTABLE     = FormatTypes.ABC,
    SELF         = FormatTypes.ABC,
    ADD          = FormatTypes.ABC,
    SUB          = FormatTypes.ABC,
    MUL          = FormatTypes.ABC,
    DIV          = FormatTypes.ABC,
    MOD          = FormatTypes.ABC,
    POW          = FormatTypes.ABC,
    UNM          = FormatTypes.ABC,
    NOT          = FormatTypes.ABC,
    LEN          = FormatTypes.ABC,
    CONCAT       = FormatTypes.ABC,
    JMP          = FormatTypes.sBx,
    EQ           = FormatTypes.ABC,
    LT           = FormatTypes.ABC,
    LE           = FormatTypes.ABC,
    TEST         = FormatTypes.ABC,
    TESTSET      = FormatTypes.ABC,
    CALL         = FormatTypes.ABC,
    TAILCALL     = FormatTypes.ABC,
    RETURN       = FormatTypes.ABC,
    FORLOOP      = FormatTypes.AsBx,
    FORPREP      = FormatTypes.AsBx,
    TFORLOOP     = FormatTypes.ABC,
    SETLIST      = FormatTypes.ABC,
    CLOSE        = FormatTypes.ABC,
    CLOSURE      = FormatTypes.ABx,
    VARARG       = FormatTypes.ABC
}

local ConstantTypes = { NIL = 0, BOOLEAN = 1, NUMBER = 2, STRING = 3 }

function parse_instruction(data)
    local opcode = bit.band(0x3F, data)
    local format_type = inst_formats[opcode + 1]

    if format_type == FormatTypes.ABC then
        return {
            A = bit.band(0x3FC0, data),
            B = bit.band(0xFF800000, data),
            C = bit.band(0x7FC000, data)
        }
    elseif format_type == FormatTypes.ABx then
        return {
            A = bit.band(0x3FC0, data),
            Bx = bit.band(0xFFFFC000, data)
        }
    elseif format_type == FormatTypes.AsBx then
        return {
            A = bit.band(0x3FC0, data),
            sBx = bit.band(0xFFFFC000, data) - 131071
        }
    end
end

function parse_number(data_high, data_low)
    local sign = bit.band(0x80000000, data_high) == 0 and 1 or -1
    local exp = bit.band(0x7FF00000, data_high) - 1023
    local man = 1 / bit.lshift(bit.band(0xFFFFF, data_high), 32) + data_low

    if sign == 1 and exp == 0 and man == 0 then
        return 0.0
    end

    if exp == 0x7FF then
        if man == 0 then
            return math.huge * sign
        else
            return 0/0
        end
    end

    return sign * (1 + man) * 2 ^ exp
end

function parse_chunk(chunk)
    local parsed_chunk = { }
    local chunk_in = ChunkInputStream.new(chunk)

    local source_name = chunk_in.read_string()
    local line_defined = chunk_in.read_integer()
    local last_line_defined = chunk_in.read_integer()
    local num_upvalues = chunk_in.read_byte()
    local num_parameters = chunk_in.read_byte()
    local is_vararg_flag = chunk_in.read_byte()
    local max_stack_size = chunk_in.read_byte()

    local inst_list_size = chunk_in.read_integer()
    local instructions = { }

    print("source_name: " .. tostring(source_name))
    print("line_defined: " .. tostring(line_defined))
    print("last_line_defined: " .. tostring(last_line_defined))
    print("num_upvalues: " .. tostring(num_upvalues))
    print("num_parameters: " .. tostring(num_parameters))
    print("is_vararg_flag: " .. tostring(is_vararg_flag))
    print("max_stack_size: " .. tostring(max_stack_size))

    print(inst_list_size)
    for i = 1, inst_list_size do
        local inst = chunk_in.read_uint(4)
        instructions[#instructions + 1] = parse_instruction(inst)
    end

    local const_list_size = chunk_in.read_integer()
    local constants = { }

    for i = 1, const_list_size do
        local const_type = chunk_in.read_byte()
        local const

        if const_type == ConstantTypes.BOOLEAN then
            const = chunk_in.read_byte()
        elseif const_type == ConstantTypes.NUMBER then
            local data_high = chunk_in.read_uint(4)
            local data_low = chunk_in.read_uint(4)
            const = parse_number(data_high, data_low)
        elseif const_type == ConstantTypes.STRING then
            const = chunk_in.read_string()
        end

        print(const)

        constants[#constants + 1] = const
    end
end
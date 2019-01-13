local bit = require "bitop-lua/src/bitop/funcs"
require "src/instruction"

ChunkInputStream = { }

function ChunkInputStream.new(chunk)
    local self = { }

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

    function self.get_pos()
        return pos
    end

    function self.seek_abs(n)
        pos = n
    end

    function self.seek_rel(n)
        pos = pos + n
    end

    function self.read_byte()
        local ret = chunk_str:sub(pos, pos):byte()
        pos = pos + 1
        return ret
    end

    function self.read_bytes(n)
        local ret = 0

        for i = 1, n do
            ret = ret + (bit.lshift(self.read_byte(), (n - i) * 8)) 
        end

        return ret
    end

    function self.read_uint(n)
        local ret = 0
        
        for i = 1, n do
            ret = ret + (bit.lshift(self.read_byte(), (i - 1) * 8))
        end

        return ret
    end

    function self.read_integer()
        return self.read_uint(int_size)
    end

    function self.read_size_t()
        return self.read_uint(size_t_size)
    end

    function self.read_string()
        local size = self.read_size_t()

        if size == 0 then return nil end

        local ret = { }
        
        for i = 1, size - 1 do
            ret[i] = string.char(self.read_byte())
        end

        pos = pos + 1
        return table.concat(ret)
    end

    signature = self.read_uint(4)
    version = self.read_byte()
    format_version = self.read_byte()
    endianness = self.read_byte()
    int_size = self.read_byte()
    size_t_size = self.read_byte()
    instruction_size = self.read_byte()
    lua_number_size = self.read_byte()
    integral_flag = self.read_byte()

    self.header = {
        signature = signature,
        version = version,
        format_version = format_version,
        endianness = endianness,
        int_size = int_size,
        size_t_size = size_t_size,
        instruction_size = instruction_size,
        lua_number_size = lua_number_size,
        integral_flag = integral_flag
    }

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

ChunkOutputStream = { }

function ChunkOutputStream.new(header)
    local self = { }
    self.buffer = { }
    local pos = 1

    function self.seek_abs(n)
        pos = n
    end

    function self.seek_rel(n)
        pos = pos + n
    end

    function self.write_char(char)
        self.buffer[pos] = char
        pos = pos + 1
    end

    function self.write_byte(byte)
        self.write_char(string.char(byte))
    end

    function self.write_uint(n, uint)
        for i = 1, n do
            self.write_byte(bit.band(0xFF, bit.rshift(uint, (i - 1) * 8)))
        end
    end

    function self.write_integer(int)
        self.write_uint(header.int_size, int)
    end

    function self.write_size_t(int)
        self.write_uint(header.size_t_size, int)
    end

    function self.write_string(str)
        if not str then
            self.write_size_t(0)
            return
        end

        self.write_size_t(#str + 1)

        for i = 1, #str do
            self.write_byte(string.byte(str:sub(i, i)))
        end

        self.write_byte(0)
    end

    function self.write_buffer(buf)
        for i = 1, #buf do
            self.write_char(buf[i])
        end
    end

    function self.to_string()
        return table.concat(self.buffer)
    end

    self.write_uint(4, header.signature)
    self.write_byte(header.version)
    self.write_byte(header.format_version)
    self.write_byte(header.endianness)
    self.write_byte(header.int_size)
    self.write_byte(header.size_t_size)
    self.write_byte(header.instruction_size)
    self.write_byte(header.lua_number_size)
    self.write_byte(header.integral_flag)

    return self
end

Chunk = { }

function Chunk.new(chunk_in)
    local self = { }

    self.chunk_in = chunk_in

    self.source_name = chunk_in.read_string()
    self.line_defined = chunk_in.read_integer()
    self.last_line_defined = chunk_in.read_integer()
    self.num_upvalues = chunk_in.read_byte()
    self.num_parameters = chunk_in.read_byte()
    self.is_vararg_flag = chunk_in.read_byte()
    self.max_stack_size = chunk_in.read_byte()

    print("source_name: " .. tostring(self.source_name))
    print("line_defined: " .. tostring(self.line_defined))
    print("last_line_defined: " .. tostring(self.last_line_defined))
    print("num_upvalues: " .. tostring(self.num_upvalues))
    print("num_parameters: " .. tostring(self.num_parameters))
    print("is_vararg_flag: " .. tostring(self.is_vararg_flag))
    print("max_stack_size: " .. tostring(self.max_stack_size))

    local inst_list_size = chunk_in.read_integer()
    self.instructions = { }

    for i = 1, inst_list_size do
        local inst = chunk_in.read_uint(4)
        local parsed_inst = parse_instruction(inst)
        self.instructions[i] = parsed_inst
        print(inst_to_string(parsed_inst))
    end

    local const_list_size = chunk_in.read_integer()
    self.constants = { }

    for i = 1, const_list_size do
        local const_type = chunk_in.read_byte()
        local const

        if const_type == ConstantTypes.BOOLEAN then
            const = chunk_in.read_byte()
        elseif const_type == ConstantTypes.NUMBER then
            local data_low = chunk_in.read_uint(4)
            local data_high = chunk_in.read_uint(4)
            print(bits_to_string(data_high), bits_to_string(data_low))
            const = parse_number(data_high, data_low)
        elseif const_type == ConstantTypes.STRING then
            const = chunk_in.read_string()
        end

        print(const_type, const)

        self.constants[i] = const
    end

    local proto_list_size = chunk_in.read_integer()
    self.prototypes = { }

    for i = 1, proto_list_size do
        self.prototypes[i] = Chunk.new(chunk_in)
    end

    local source_line_pos_list_size = chunk_in.read_integer()
    self.source_line_positions = { }

    for i = 1, source_line_pos_list_size do
        self.source_line_positions[i] = chunk_in.read_integer()
        print(self.source_line_positions[i])
    end

    local local_list_size = chunk_in.read_integer()
    self.local_list = { }

    for i = 1, local_list_size do
        local var = { }
        var.name = chunk_in.read_string()
        var.start_pc = chunk_in.read_integer()
        var.end_pc = chunk_in.read_integer()
        self.local_list[i] = var
        print(string.format("name: %s, start_pc: %s, end_pc: %s", var.name, var.start_pc, var.end_pc))
    end

    local upvalue_list_size = chunk_in.read_integer()
    self.upvalue_list = { }

    for i = 1, upvalue_list_size do
        upvalue_list[i] = chunk_in.read_string()
        print(string.format("upvalue name: %s", upvalue_list[i]))
    end

    function self.compile()
        local chunk_out = ChunkOutputStream.new(chunk_in.header)

        chunk_out.write_string(self.source_name)
        chunk_out.write_integer(self.line_defined)
        chunk_out.write_integer(self.last_line_defined)
        chunk_out.write_byte(self.num_upvalues)
        chunk_out.write_byte(self.num_parameters)
        chunk_out.write_byte(self.is_vararg_flag)
        chunk_out.write_byte(self.max_stack_size)

        chunk_out.write_integer(#self.instructions)

        for i = 1, #self.instructions do
            local inst = self.instructions[i]

            if inst.format == FormatTypes.ABC then
                local inst_bits = inst.opcode
                inst_bits = bit.bor(inst_bits, bit.lshift(inst.A, 6))
                inst_bits = bit.bor(inst_bits, bit.lshift(inst.C, 14))
                inst_bits = bit.bor(inst_bits, bit.lshift(inst.B, 23))
                chunk_out.write_uint(4, inst_bits)
            elseif inst.format == FormatTypes.ABx then
                local inst_bits = inst.opcode
                inst_bits = bit.bor(inst_bits, bit.lshift(inst.A, 6))
                inst_bits = bit.bor(inst_bits, bit.lshift(inst.Bx, 14))
                chunk_out.write_uint(4, inst_bits)
            elseif inst.format == FormatTypes.AsBx then
                local inst_bits = inst.opcode
                inst_bits = bit.bor(inst_bits, bit.lshift(inst.A, 6))
                inst_bits = bit.bor(inst_bits, bit.lshift(inst.sBx + 131071, 14))
                chunk_out.write_uint(4, inst_bits)
            end
        end

        chunk_out.write_integer(#self.constants)

        for i = 1, #self.constants do
            local const = self.constants[i]

            if type(const) == "nil" then
                chunk_out.write_byte(0)
            elseif type(const) == "boolean" then
                chunk_out.write_byte(1)
                chunk_out.write_byte(const and 1 or 0)
            elseif type(const) == "number" then
                chunk_out.write_byte(3)
                local data_high, data_low = pack_number(const)
                chunk_out.write_uint(4, data_low)
                chunk_out.write_uint(4, data_high)
            elseif type(const) == "string" then
                chunk_out.write_byte(4)
                chunk_out.write_string(const)
            end
        end

        chunk_out.write_integer(#self.prototypes)

        for i = 1, #self.prototypes do
            local proto = self.prototypes[i]
            chunk_out.write_buffer(proto.buffer)
        end

        chunk_out.write_integer(#self.source_line_positions)

        for i = 1, #self.source_line_positions do
            local slp = self.source_line_positions[i]
            chunk_out.write_integer(slp)
        end

        chunk_out.write_integer(#self.local_list)

        for i = 1, #self.local_list do
            local l = self.local_list
            chunk_out.write_string(l.name)
            chunk_out.write_integer(l.start_pc or 0)
            chunk_out.write_integer(l.end_pc or 0)
        end

        chunk_out.write_integer(#self.upvalue_list)

        for i = 1, #self.upvalue_list do
            local upvalue = self.upvalue_list[i]
            chunk_out.write_string(upvalue)
        end

        local s = chunk_out.to_string()
        local f = io.open("out.b", "wb")
        f:write(s)
        f:close()
        return loadstring(chunk_out.to_string())
    end

    function self.append_instruction(inst)
        self.instructions[#self.instructions + 1] = inst
    end

    function self.insert_instruction(idx, inst)
        table.insert(self.instructions, idx, inst)
    end

    return self
end

ConstantTypes = { NIL = 0, BOOLEAN = 1, NUMBER = 3, STRING = 4 }

function parse_instruction(data)
    local opcode = bit.band(0x3F, data)
    local format_type = inst_formats[opcode + 1]

    if format_type == FormatTypes.ABC then
        return {
            opcode = opcode,
            format = format_type,
            A = bit.rshift(bit.band(0x3FC0, data), 6),
            B = bit.rshift(bit.band(0xFF800000, data), 23),
            C = bit.rshift(bit.band(0x7FC000, data), 14)
        }
    elseif format_type == FormatTypes.ABx then
        return {
            opcode = opcode,
            format = format_type,
            A = bit.rshift(bit.band(0x3FC0, data), 6),
            Bx = bit.rshift(bit.band(0xFFFFC000, data), 14)
        }
    elseif format_type == FormatTypes.AsBx then
        return {
            opcode = opcode,
            format = format_type,
            A = bit.rshift(bit.band(0x3FC0, data), 6),
            sBx = bit.rshift(bit.band(0xFFFFC000, data), 14) - 131071
        }
    end
end

function inst_to_string(inst)
    if inst.format == FormatTypes.ABC then
        return table.concat { "opcode: ", inst.opcode, ", A: ", inst.A, ", B: ", inst.B, ", C: ", inst.C }
    elseif inst.format == FormatTypes.ABx then
        return table.concat { "opcode: ", inst.opcode, ", A: ", inst.A, ", Bx: ", inst.Bx }
    elseif inst.format == FormatTypes.AsBx then
        return table.concat { "opcode: ", inst.opcode, ", A: ", inst.A, ", sBx: ", inst.sBx }
    end
end

function toBits(num,bits)
    -- returns a table of bits, most significant first.
    bits = bits or math.max(1, select(2, math.frexp(num)))
    local t = {} -- will contain the bits        
    for b = bits, 1, -1 do
        t[b] = math.fmod(num, 2)
        num = math.floor((num - t[b]) / 2)
    end
    return t
end

function map(t, f)
    local ret = { }

    for i = 1, #t do
        ret[i] = f(t[i])
    end

    return ret
end

function bits_to_string(data)
    local t = toBits(data)
    return table.concat(map(t, function(e)
        return tostring(e)
    end))
end

function mantissa_to_fraction(man)
    local r = 0

    for i = 52, 1, -1 do
        local bit = man % 2
        r = r + bit * 2^(-i)
        man = math.floor(man / 2)
    end

    return r
end

function parse_number(data_high, data_low)
    local sign_bit = bit.rshift(bit.band(0x80000000, data_high), 31)
    local exp_bits = bit.rshift(bit.band(0x7FF00000, data_high), 20)
    local man_bits = bit.band(0xFFFFF, data_high) * 2^32 + data_low

    print("sign_bit: " .. bits_to_string(sign_bit))
    print("exponent_bits: " .. bits_to_string(exp_bits))
    print("mantissa_bits: " .. bits_to_string(man_bits))

    local sign = sign == 0 and -1 or 1
    local exponent = exp_bits - 1023
    local fraction = mantissa_to_fraction(man_bits)

    print("sign: " .. sign)
    print("exponent: " .. exponent)
    print("fraction: " .. fraction)

    if sign == 1 and exp == 0 and man == 0 then
        return 0.0
    end

    if exp_bits == 0x7FF then
        if man_bits == 0 then
            return math.huge * sign
        else
            return 0/0
        end
    end

    return sign * (1 + fraction) * 2 ^ exponent
end

function fraction_to_mantissa(frac)
    local man_bits = 0

    for i = 1, 52 do
        man_bits = man_bits * 2

        if frac >= 2^(-i) then
            frac = frac - 2^(-i)
            man_bits = man_bits + 1
        end
    end

    return man_bits
end

function pack_number(n)
    local frac, exp = math.frexp(n)
    print(string.format("fraction: %s", frac))
    print(string.format("exponent: %s", exp))
    local sign_bit = n < 0 and 1 or 0
    local exp_bits = exp + 1022
    local man_bits = fraction_to_mantissa(math.abs(frac * 2 - 1))
    local data_high = bit.lshift(sign_bit, 31) + bit.lshift(exp_bits, 20) + man_bits * 2^(-32)
    local data_low = bit.band(0xFFFFFFFF, man_bits)
    print(string.format("sign_bit: %s", sign_bit))
    print(string.format("exp_bits: %s", bits_to_string(exp_bits)))
    print(string.format("man_bits: %s", bits_to_string(man_bits)))
    print(bits_to_string(data_high), bits_to_string(data_low))
    return data_high, data_low
end
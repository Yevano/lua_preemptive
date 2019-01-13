FormatTypes = { ABC = 0, ABx = 1, AsBx = 2 }

inst_formats = {
    --[[MOVE         = ]] FormatTypes.ABC,
    --[[LOADK        = ]] FormatTypes.ABx,
    --[[LOADBOOL     = ]] FormatTypes.ABC,
    --[[LOADNIL      = ]] FormatTypes.ABC,
    --[[GETUPVAL     = ]] FormatTypes.ABC,
    --[[GETGLOBAL    = ]] FormatTypes.ABx,
    --[[GETTABLE     = ]] FormatTypes.ABC,
    --[[SETGLOBAL    = ]] FormatTypes.ABx,
    --[[SETUPVAL     = ]] FormatTypes.ABC,
    --[[SETTABLE     = ]] FormatTypes.ABC,
    --[[NEWTABLE     = ]] FormatTypes.ABC,
    --[[SELF         = ]] FormatTypes.ABC,
    --[[ADD          = ]] FormatTypes.ABC,
    --[[SUB          = ]] FormatTypes.ABC,
    --[[MUL          = ]] FormatTypes.ABC,
    --[[DIV          = ]] FormatTypes.ABC,
    --[[MOD          = ]] FormatTypes.ABC,
    --[[POW          = ]] FormatTypes.ABC,
    --[[UNM          = ]] FormatTypes.ABC,
    --[[NOT          = ]] FormatTypes.ABC,
    --[[LEN          = ]] FormatTypes.ABC,
    --[[CONCAT       = ]] FormatTypes.ABC,
    --[[JMP          = ]] FormatTypes.AsBx,
    --[[EQ           = ]] FormatTypes.ABC,
    --[[LT           = ]] FormatTypes.ABC,
    --[[LE           = ]] FormatTypes.ABC,
    --[[TEST         = ]] FormatTypes.ABC,
    --[[TESTSET      = ]] FormatTypes.ABC,
    --[[CALL         = ]] FormatTypes.ABC,
    --[[TAILCALL     = ]] FormatTypes.ABC,
    --[[RETURN       = ]] FormatTypes.ABC,
    --[[FORLOOP      = ]] FormatTypes.AsBx,
    --[[FORPREP      = ]] FormatTypes.AsBx,
    --[[TFORLOOP     = ]] FormatTypes.ABC,
    --[[SETLIST      = ]] FormatTypes.ABC,
    --[[CLOSE        = ]] FormatTypes.ABC,
    --[[CLOSURE      = ]] FormatTypes.ABx,
    --[[VARARG       = ]] FormatTypes.ABC
}

ArgTypes = { ABC = 0, ABx = 1, AsBx = 2, AB = 3, AC = 4, A = 5, sBx = 6 }

inst_args = {
    ArgTypes.AB,
    ArgTypes.ABx,
    ArgTypes.ABC,
    ArgTypes.AB,
    ArgTypes.AB,
    ArgTypes.ABx,
    ArgTypes.ABC,
    ArgTypes.ABx,
    ArgTypes.AB,
    ArgTypes.ABC,
    ArgTypes.ABC,
    ArgTypes.ABC,
    ArgTypes.ABC,
    ArgTypes.ABC,
    ArgTypes.ABC,
    ArgTypes.ABC,
    ArgTypes.ABC,
    ArgTypes.ABC,
    ArgTypes.AB,
    ArgTypes.AB,
    ArgTypes.AB,
    ArgTypes.ABC,
    ArgTypes.sBx,
    ArgTypes.ABC,
    ArgTypes.ABC,
    ArgTypes.ABC,
    ArgTypes.AC,
    ArgTypes.ABC,
    ArgTypes.ABC,
    ArgTypes.ABC,
    ArgTypes.AB,
    ArgTypes.AsBx,
    ArgTypes.AsBx,
    ArgTypes.AC,
    ArgTypes.ABC,
    ArgTypes.A,
    ArgTypes.ABx,
    ArgTypes.AB
}

function instruction(opcode, ...)
    local args = { ... }
    local args_form = inst_args[opcode + 1]
    local format = inst_formats[opcode + 1]

    if args_form == ArgTypes.ABC then
        return {
            format = format,
            opcode = opcode,
            A = args[1], B = args[2], C = args[3]
        }
    elseif args_form == ArgTypes.ABx then
        return {
            format = format,
            opcode = opcode,
            A = args[1], Bx = args[2]
        }
    elseif args_form == ArgTypes.AsBx then
        return {
            format = format,
            opcode = opcode,
            As = args[1], sBx = args[2]
        }
    elseif args_form == ArgTypes.AB then
        return {
            format = format,
            opcode = opcode,
            A = args[1], B = args[2], C = 0
        }
    elseif args_form == ArgTypes.AC then
        return {
            format = format,
            opcode = opcode,
            A = args[1], B = 0, C = args[2]
        }
    elseif args_form == ArgTypes.A then
        return {
            format = format,
            opcode = opcode,
            A = args[1], B = 0, C = 0
        }
    elseif args_form == ArgTypes.sBx then
        return {
            format = format,
            opcode = opcode,
            A = 0, sBx = args[1]
        }
    end
end
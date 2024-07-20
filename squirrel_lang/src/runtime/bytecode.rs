
#[repr(u8)]
enum Tag {
    // Comparison Instructions
    // data is register index
    // acc = acc (op) reg[data]
    ISLT, // less
    ISLE, // less or equal
    ISGT, // greater
    ISGE, // greater or equal
    ISEQ, // equal
    ISNE, // not equal

    // Move Instructions
    LDR, // acc = reg[data]
    STR, // reg[data] = acc
    SWPR, // acc <=> reg[data]
    LDL, // acc = locals[data]
    STL, // locals[data] = acc
    SWPL, // acc <=> locals[data]

    LDC, // acc = const[data]
    LDP, // acc = match data { 0 => false, 1 => true, 2 => null }
    LDI, // acc = data (as an sign extended integer)

    // Arithmetic Instructions
    NEG, // acc = -acc
    ADD, // acc = reg[data] + acc
    SUB, // acc = reg[data] - acc
    MUL, // acc = reg[data] * acc
    DIV, // acc = reg[data] / acc
    MOD, // acc = reg[data] % acc
    POW, // acc = reg[data] ^ acc

    // Logical Instructions
    LNOT, // acc = !acc
    // LAND, // acc = !acc ? acc : reg[data]
    // LOR, // acc = acc ? acc : reg[data]

    // Bitwise Instructions
    BNOT, // acc = ~acc
    BAND, // acc = acc & reg[data]
    BOR, // acc = acc | reg[data]
    BXOR, // acc = acc ^ reg[data]
    BLSH, // acc = acc >> reg[data]
    BASH, // acc = acc >>> reg[data]

    // Control Flow
    JMP, // goes to blocks[data]
    JT, // goes to blocks[data] if acc
    JF, // goes to blocks[data] if !acc
    RET, // returns from function (returns acc)
    RETN, // returns from function (returns null)

    // Function Calls
    // CAL<X> = calls function with X arguments
    // function is in acc, arguments are in reg[data..data+X]
    CAL0,
    CAL1,
    CAL2,
    CAL3,
    CAL4,
    CAL5,
    CAL6,
    // CALN = calls function with N arguments
    // N is in acc, function is in reg[data], arguments are in reg[data+1..data+N+1]
    CALN,

    // Field Instructions
    GETF, // acc = reg[data][acc]
    GETFS, // acc = acc in reg[data] ? reg[data][acc] : null
    SETF, // reg[data][reg[data+1]] = acc
    SETFS, // reg[data][reg[data+1]] = acc (newslot)

    GET, // acc = env[acc]
    GETS, // acc = acc in env[acc] ? env[acc] : null
    SET, // env[reg[data]] = acc
    SETS, // env[reg[data]] = acc (newslot)
}

#[repr(C)]
struct Inst {
    tag: Tag,
    data: u8,
}


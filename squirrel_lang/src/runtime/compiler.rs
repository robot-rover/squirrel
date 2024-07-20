struct Function {
    code: Vec<Inst>,
    constants: Vec<Literal>,
    varargs: bool,
    num_params: u32,
    locals: Vec<(String, u32)>,
}
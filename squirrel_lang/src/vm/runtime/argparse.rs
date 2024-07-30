use std::{
    any::type_name,
    ops::{Range, RangeBounds},
};

pub fn validate_num_args(
    expected: Range<usize>,
    actual: usize,
    call_info: &CallInfo,
) -> Result<(), ExecError> {
    if expected.start <= actual && actual <= expected.end {
        Ok(())
    } else {
        Err(ExecError::wrong_arg_count(
            call_info.clone(),
            expected,
            actual,
            None,
        ))
    }
}

pub fn convert_arg<T: TypeName>(
    value: Value,
    arg_index: usize,
    call_info: &CallInfo,
) -> Result<T, ExecError> {
    match T::typed_from(value) {
        Ok(conv) => Ok(conv),
        Err(unconv) => Err(ExecError::wrong_arg_type(
            call_info.clone(),
            arg_index,
            T::type_name().to_string(),
            unconv.type_str().to_string(),
        )),
    }
}

pub fn parse_option<A>(args: Vec<Value>, call_info: &CallInfo) -> Result<Option<A>, ExecError>
where
    A: TypeName,
{
    validate_num_args(0..1, args.len(), call_info)?;
    let mut arg_iter = args.into_iter();
    let arg = match arg_iter.next() {
        Some(val) => Some(convert_arg(val, 0, call_info)?),
        None => None,
    };
    Ok(arg)
}

// Uncomment this if we ever need > 3 arguments
// pub fn take_n<const N: usize>(
//     args: Vec<Value>,
//     n: usize,
//     call_info: &CallInfo,
// ) -> Result<[Value; N], ExecError> {
//     validate_num_args(n..n, args.len(), call_info)?;
//     Ok(args.try_into().unwrap())
// }

pub fn parse0(args: Vec<Value>, call_info: &CallInfo) -> Result<(), ExecError> {
    validate_num_args(0..0, args.len(), call_info)
}

pub fn parse1<A>(args: Vec<Value>, call_info: &CallInfo) -> Result<A, ExecError>
where
    A: TypeName,
{
    validate_num_args(1..1, args.len(), call_info)?;
    let mut arg_iter = args.into_iter();
    let arg0 = arg_iter.next().unwrap();
    let arg0 = convert_arg(arg0, 0, call_info)?;
    Ok(arg0)
}

pub fn parse2<A, B>(args: Vec<Value>, call_info: &CallInfo) -> Result<(A, B), ExecError>
where
    A: TypeName,
    B: TypeName,
{
    validate_num_args(2..2, args.len(), call_info)?;
    let mut arg_iter = args.into_iter();
    let arg0 = arg_iter.next().unwrap();
    let arg0 = convert_arg(arg0, 0, call_info)?;
    let arg1 = arg_iter.next().unwrap();
    let arg1 = convert_arg(arg1, 1, call_info)?;
    Ok((arg0, arg1))
}

pub fn parse3<A, B, C>(args: Vec<Value>, call_info: &CallInfo) -> Result<(A, B, C), ExecError>
where
    A: TypeName,
    B: TypeName,
    C: TypeName,
{
    validate_num_args(3..3, args.len(), call_info)?;
    let mut arg_iter = args.into_iter();
    let arg0 = arg_iter.next().unwrap();
    let arg0 = convert_arg(arg0, 0, call_info)?;
    let arg1 = arg_iter.next().unwrap();
    let arg1 = convert_arg(arg1, 1, call_info)?;
    let arg2 = arg_iter.next().unwrap();
    let arg2 = convert_arg(arg2, 2, call_info)?;
    Ok((arg0, arg1, arg2))
}

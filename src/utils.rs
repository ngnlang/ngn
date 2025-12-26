use crate::value::Number;

pub fn _is_number_value(v: &Number) -> bool {
    matches!(v, Number::I64(_) | Number::I32(_) | Number::I16(_) | Number::I8(_) | Number::U64(_) | Number::U32(_) | Number::U16(_) | Number::U8(_) | Number::F64(_) | Number::F32(_))
}

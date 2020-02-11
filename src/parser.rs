//use nom::IResult;

use nom::error::ErrorKind;

pub type IResult<I, O, E=(I, ErrorKind)> = Result<(I, O), Err<E>>;
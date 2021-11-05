use std::io;
use std::result;

pub type Result<T> = result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error("this function has not been implemented")]
    NotImplemented,

    #[error("something went wrong with I/O")]
    Io(#[from] io::Error),
}

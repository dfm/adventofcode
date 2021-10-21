use reqwest;
use std::error::Error as StdError;
use std::fmt;
use std::io;
use std::result;

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
pub struct Error(Box<ErrorKind>);

impl Error {
    pub fn new(kind: ErrorKind) -> Error {
        Error(Box::new(kind))
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    Io(io::Error),
    Reqwest(reqwest::Error),
    NotImplementedError,
    Message(String),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::new(ErrorKind::Io(err))
    }
}

impl From<reqwest::Error> for Error {
    fn from(err: reqwest::Error) -> Error {
        Error::new(ErrorKind::Reqwest(err))
    }
}

impl From<&str> for Error {
    fn from(err: &str) -> Error {
        Error::new(ErrorKind::Message(err.to_string()))
    }
}

impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match *self.0 {
            ErrorKind::Io(ref err) => Some(err),
            ErrorKind::Reqwest(ref err) => Some(err),
            ErrorKind::NotImplementedError => None,
            ErrorKind::Message(_) => None,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.0 {
            ErrorKind::Io(ref err) => err.fmt(f),
            ErrorKind::Reqwest(ref err) => err.fmt(f),
            ErrorKind::NotImplementedError => f.write_str("not implemented"),
            ErrorKind::Message(ref msg) => f.write_str(msg),
            _ => unreachable!(),
        }
    }
}

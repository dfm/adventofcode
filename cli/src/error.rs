use aoc::error::Error as AocError;
use reqwest;
use std::error::Error as StdError;
use std::fmt;
use std::io;
use std::result;
use toml_edit::TomlError;

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
pub struct Error(Box<ErrorKind>);

impl Error {
    pub fn new(kind: ErrorKind) -> Error {
        Error(Box::new(kind))
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub enum ErrorKind {
    Aoc(AocError),
    Io(io::Error),
    Reqwest(reqwest::Error),
    Toml(TomlError),
    Message(String),
}

impl From<AocError> for Error {
    fn from(err: AocError) -> Error {
        Error::new(ErrorKind::Aoc(err))
    }
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

impl From<TomlError> for Error {
    fn from(err: TomlError) -> Error {
        Error::new(ErrorKind::Toml(err))
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
            ErrorKind::Aoc(ref err) => Some(err),
            ErrorKind::Io(ref err) => Some(err),
            ErrorKind::Reqwest(ref err) => Some(err),
            ErrorKind::Toml(ref err) => Some(err),
            ErrorKind::Message(_) => None,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.0 {
            ErrorKind::Aoc(ref err) => err.fmt(f),
            ErrorKind::Io(ref err) => err.fmt(f),
            ErrorKind::Reqwest(ref err) => err.fmt(f),
            ErrorKind::Toml(ref err) => err.fmt(f),
            ErrorKind::Message(ref msg) => f.write_str(msg),
        }
    }
}

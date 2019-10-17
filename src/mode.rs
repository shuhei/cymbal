use std::fmt;

pub enum Mode {
    Eval,
    Compile,
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Mode::Eval => write!(f, "eval mode"),
            Mode::Compile => write!(f, "compile mode"),
        }
    }
}

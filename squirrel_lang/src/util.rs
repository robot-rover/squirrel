use std::io;

pub trait IntoOwned<T> {
    fn into_owned(self) -> T;
}

impl<T> IntoOwned<T> for T {
    fn into_owned(self) -> T {
        self
    }
}

impl<T> IntoOwned<T> for &T
where
    T: Clone,
{
    fn into_owned(self) -> T {
        self.clone()
    }
}

pub enum WriteOption<'a> {
    Dyn(&'a mut dyn io::Write),
    Stdout(io::Stdout),
    Stderr(io::Stderr),
}

impl<'a> io::Write for &mut WriteOption<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self {
            WriteOption::Dyn(write) => write.write(buf),
            WriteOption::Stdout(stdout) => stdout.write(buf),
            WriteOption::Stderr(stderr) => stderr.write(buf),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self {
            WriteOption::Dyn(write) => write.flush(),
            WriteOption::Stdout(stdout) => stdout.flush(),
            WriteOption::Stderr(stderr) => stderr.flush(),
        }
    }
}

impl<'a> From<Option<&'a mut dyn io::Write>> for WriteOption<'a> {
    fn from(value: Option<&'a mut dyn io::Write>) -> Self {
        value
            .map(|write| WriteOption::Dyn(write))
            .unwrap_or_else(|| WriteOption::Stdout(io::stdout()))
    }
}

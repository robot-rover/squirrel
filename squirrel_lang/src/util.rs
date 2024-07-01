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
pub trait NoRepeatPush<T> {
    fn push_no_repeat(&mut self, item: T);
}

impl<T: Eq> NoRepeatPush<T> for Vec<T> {
    fn push_no_repeat(&mut self, item: T) {
        if !self.contains(&item) {
            self.push(item);
        }
    }
}

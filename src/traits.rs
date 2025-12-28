use ant_type_checker::ty::Ty;

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

pub trait NeedGc {
    fn need_gc(&self) -> bool;
}

impl NeedGc for Ty {
    fn need_gc(&self) -> bool {
        match self {
            Ty::BigInt => true,
            Ty::Function { .. } => false,
            Ty::Struct { .. } => true,
            Ty::Generic(_) => true,
            Ty::IntTy(_) => false,
            Ty::Bool => false,
            Ty::Unit => false,
            Ty::Str => false,
            Ty::Unknown => false,
        }
    }
}

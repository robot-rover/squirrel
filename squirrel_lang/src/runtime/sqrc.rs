use std::{alloc::{self, handle_alloc_error, Layout}, cell::RefCell, fmt, mem, ptr::{self, addr_of, addr_of_mut, from_raw_parts, from_raw_parts_mut}, rc::{Rc, Weak}};
use std::hash::Hash;

use super::value::{Value, Object, Closure};

macro_rules! call_with_rc_ptr {
    ($rcptr:ident, $func:path) => {{
        let discrim = *($rcptr.ptr as *const RcDiscrim);
        match discrim {
            RcDiscrim::String => {
                let len = (*($rcptr.ptr as *const StringLen)).len;
                $func(from_raw_parts::<StringStrg>($rcptr.ptr as *const (), len))
            },
            RcDiscrim::Object => $func($rcptr.ptr as *const ObjectStrg),
            RcDiscrim::Array => $func($rcptr.ptr as *const ArrayStrg),
            RcDiscrim::Closure => $func($rcptr.ptr as *const ClosureStrg),
        }}
    };
}

// TODO: Should these be nonnull?
pub struct SqRc {
    ptr: *const (),
}

pub struct SqWk {
    ptr: *const (),
}

impl PartialEq for SqRc {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}

impl Eq for SqRc {}
impl Hash for SqRc {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        todo!()
    }
}

impl Clone for SqRc {
    fn clone(&self) -> Self {
        unsafe { call_with_rc_ptr!(self, Rc::increment_strong_count) };
        SqRc { ptr: self.ptr }
    }
}
impl Clone for SqWk {
    fn clone(&self) -> Self {
        unsafe { call_with_rc_ptr!(self, increment_weak_count) };
        Self { ptr: self.ptr }
    }
}

impl Drop for SqRc {
    fn drop(&mut self) {
        unsafe { call_with_rc_ptr!(self, Rc::decrement_strong_count) };
    }
}

unsafe fn decrement_weak_count<T: ?Sized>(ptr: *const T) {
    let _ = Weak::from_raw(ptr);
}
impl Drop for SqWk {
    fn drop(&mut self) {
        unsafe { call_with_rc_ptr!(self, decrement_weak_count) };
    }
}

impl fmt::Debug for SqRc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // RcEnum::from(*self).fmt(f)
        todo!()
    }
}
impl fmt::Debug for SqWk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

unsafe fn into_ref<'a, T: ?Sized + 'a>(ptr: *const T) -> SqRef<'a> where
SqRef<'a>: From<&'a T> {
    (&*ptr).into()
}

unsafe fn increment_weak_count<T: ?Sized>(ptr: *const T) {
    let rc = Rc::from_raw(ptr);
    let weak = Rc::downgrade(&rc);
    mem::forget(rc);
    mem::forget(weak);
}
impl SqRc {
    pub fn downgrade(&self) -> SqWk {
        unsafe { call_with_rc_ptr!(self, increment_weak_count) };
        SqWk { ptr: self.ptr }
    }

    pub fn by_ref<'a>(&'a self) -> SqRef<'a> {
        unsafe { call_with_rc_ptr!(self, into_ref) }
    }
}

unsafe fn upgrade_weak<T: ?Sized>(ptr: *const T) -> Option<SqRc>
where SqRcEnum: From<Rc<T>> {
    Weak::from_raw(ptr).upgrade().map(SqRcEnum::from).map(SqRcEnum::stash)
}

impl SqWk {
    pub fn upgrade(&self) -> Option<SqRc> {
        unsafe { call_with_rc_ptr!(self, upgrade_weak) }
    }
}

macro_rules! impl_ref {
    (struct $name:ident, $variant:ident, $data:ty, $inner_data:ty) => {
        #[derive(Debug)]
        #[repr(C)]
        pub struct $name {
            discrim: RcDiscrim,
            data: $data,
        }

        impl $name {
            pub fn new(data: $inner_data) -> Rc<Self> {
                let data = RefCell::new(data);
                Rc::new(Self { discrim: RcDiscrim::$variant, data })
            }
        }

        impl_ref!($name, $variant, $data);
    };
    ($name:ident, $variant:ident, $data:ty) => {

        impl $name {
            pub fn get_data(&self) -> &$data {
                &self.data
            }
        }

        impl<'a> From<&'a Rc<$name>> for SqRef<'a> {
            fn from(storage: &'a Rc<$name>) -> Self {
                SqRef::$variant(storage)
            }
        }

        impl From<Rc<$name>> for SqRcEnum {
            fn from(rc: Rc<$name>) -> Self {
                SqRcEnum::$variant(rc)
            }
        }

        impl From<Weak<$name>> for SqWkEnum {
            fn from(weak: Weak<$name>) -> Self {
                SqWkEnum::$variant(weak)
            }
        }
    };
}

impl_ref!(struct ObjectStrg, Object, RefCell<Object>, Object);
impl_ref!(struct ArrayStrg, Array, RefCell<Vec<Value>>, Vec<Value>);
impl_ref!(struct ClosureStrg, Closure, RefCell<Closure>, Closure);
impl_ref!(StringStrg, String, str);

#[repr(C)]
#[derive(Debug)]
pub struct StringStrg {
    discrim: RcDiscrim,
    len: usize,
    data: str,
}

impl StringStrg {
    pub fn new(string: &str) -> Rc<Self> {
        let discrim_offset = 0;
        let layout = Layout::from_size_align(0, 1).unwrap();
        let (layout, len_offset) = layout.extend(Layout::new::<usize>()).unwrap();
        let (layout, data_offset) = layout.extend(Layout::array::<u8>(string.len()).unwrap()).unwrap();
        let layout = layout.pad_to_align();
        let raw = unsafe { alloc::alloc(layout) };
        if raw.is_null() {
            handle_alloc_error(layout);
        }
        let boxed = unsafe {
            // This is a hack to create a fat pointer from a pointer and a length
            // The pointer was allocated with the correct layout, so it is safe to cast it to StringStrg
            let typed = from_raw_parts_mut::<StringStrg>(raw as *mut (), string.len());
            // Now that we have a typed pointer, initialize the fields
            ptr::write(addr_of_mut!((*typed).discrim), RcDiscrim::String);
            ptr::write(addr_of_mut!((*typed).len), string.len());
            let (data_start, len) = (addr_of_mut!((*typed).data)).to_raw_parts();
            debug_assert_eq!(len, string.len());
            ptr::copy_nonoverlapping(string.as_ptr(), data_start as *mut u8, string.len());
            // The type is initialized, we can safely create a box
            Box::from_raw(typed)
        };
        // This function has to copy string twice, once into the Box and then a second time into the Rc,
        // it would be better if Rc had a way to allocate an uninit Rc from a layout.
        Rc::from(boxed)
    }
}

#[repr(C)]
#[derive(Debug)]
struct StringLen {
    discrim: RcDiscrim,
    len: usize,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
enum RcDiscrim {
    String,
    Object,
    Array,
    Closure,
}

macro_rules! impl_sq_enum {
    ($wide:ident, $ptr:ident) => {
        #[derive(Debug, Clone)]
        #[repr(u8)]
        pub enum $wide {
            String($ptr<StringStrg>) = RcDiscrim::String as u8,
            Object($ptr<ObjectStrg>) = RcDiscrim::Object as u8,
            Array($ptr<ArrayStrg>) = RcDiscrim::Array as u8,
            Closure($ptr<ClosureStrg>) = RcDiscrim::Closure as u8,
        }
    };
    (impl $wide:ident, $ptr:ident, $narrow:ident) => {
        impl_sq_enum!($wide, $ptr);
        impl $wide {
            pub fn stash(self) -> $narrow {
                let ptr = match self {
                    $wide::String(rc) => $ptr::into_raw(rc) as *const (),
                    $wide::Object(rc) => $ptr::into_raw(rc) as *const (),
                    $wide::Array(rc) => $ptr::into_raw(rc) as *const (),
                    $wide::Closure(rc) => $ptr::into_raw(rc) as *const (),
                };
                $narrow { ptr }
            }
        }

        impl $narrow {
            unsafe fn into_rc_enum<'a, T: ?Sized + 'a>(ptr: *const T) -> $narrow where
            $wide: From<$ptr<T>> {
                $wide::from($ptr::from_raw(ptr)).stash()
            }

            pub fn unstash(self) -> Self {
                unsafe { call_with_rc_ptr!(self, Self::into_rc_enum)}
            }
        }

    };
}

// TODO: Replace SqRcEnum with impl_sq_enum and also create SqWkEnum
#[derive(Debug, Clone)]
#[repr(u8)]
pub enum SqRef<'a> {
    String(&'a Rc<StringStrg>) = RcDiscrim::String as u8,
    Object(&'a Rc<ObjectStrg>) = RcDiscrim::Object as u8,
    Array(&'a Rc<ArrayStrg>) = RcDiscrim::Array as u8,
    Closure(&'a Rc<ClosureStrg>) = RcDiscrim::Closure as u8,
    // Class(/* TODO */),
    // Generator(/* TODO */),
    // UserData(/* TODO */),
    // Thread(/* TODO */),
}

impl_sq_enum!(impl SqRcEnum, Rc, SqRc);
impl_sq_enum!(impl SqWkEnum, Weak, SqWk);

impl SqRcEnum {
    pub fn string(val: &str) -> Self {
        SqRcEnum::String(StringStrg::new(val))
    }

    pub fn object(val: Object) -> Self {
        SqRcEnum::Object(ObjectStrg::new(val))
    }

    pub fn array(val: Vec<Value>) -> Self {
        SqRcEnum::Array(ArrayStrg::new(val))
    }

    pub fn closure(val: Closure) -> Self {
        SqRcEnum::Closure(ClosureStrg::new(val))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

   #[test]
   fn basic_test() {
        let string = "Hello, World!";
        let ss = StringStrg::new(string);
        let rc = SqRcEnum::String(ss).stash();
        let weak = rc.downgrade();
        let rc2 = weak.upgrade();
        match rc2.as_ref().map(SqRc::by_ref) {
           Some(SqRef::String(str)) => assert_eq!(str.get_data(), string),
           _ => panic!("Expected String, got {:?}", rc2),
        }
        drop(rc);
        drop(rc2);
        assert_eq!(weak.upgrade(), None);
   }
}
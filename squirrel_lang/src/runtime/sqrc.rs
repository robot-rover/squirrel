use std::hash::Hash;
use std::{
    alloc::{self, handle_alloc_error, Layout},
    cell::RefCell,
    fmt,
    mem::ManuallyDrop,
    ptr::{self, addr_of_mut, from_raw_parts, from_raw_parts_mut},
    rc::{Rc, Weak},
};

use super::value::{Closure, Object, Value};

macro_rules! call_with_rc_ptr {
    ($rcptr:ident, $func:path) => {{
        let discrim = *($rcptr.ptr as *const RcDiscrim);
        match discrim {
            RcDiscrim::String => {
                let len = (*($rcptr.ptr as *const StringLen)).len;
                $func(from_raw_parts::<StringStrg>($rcptr.ptr as *const (), len))
            }
            RcDiscrim::Object => $func($rcptr.ptr as *const ObjectStrg),
            RcDiscrim::Array => $func($rcptr.ptr as *const ArrayStrg),
            RcDiscrim::Closure => $func($rcptr.ptr as *const ClosureStrg),
        }
    }};
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
        self.borrow().as_ref().fmt(f)
    }
}
impl fmt::Debug for SqWk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

unsafe fn into_ref<'a, T: ?Sized + 'a>(ptr: *const T) -> SqRefAnc<'a>
where
    SqRefData: From<ManuallyDrop<Rc<T>>>,
{
    let data = ManuallyDrop::new(Rc::from_raw(ptr)).into();
    SqRefAnc(data, std::marker::PhantomData)
}

unsafe fn increment_weak_count<T: ?Sized>(ptr: *const T) {
    let rc = ManuallyDrop::new(Rc::from_raw(ptr));
    let _weak = ManuallyDrop::new(Rc::downgrade(&rc));
}
impl SqRc {
    pub fn downgrade(&self) -> SqWk {
        unsafe { call_with_rc_ptr!(self, increment_weak_count) };
        SqWk { ptr: self.ptr }
    }

    pub fn borrow<'a>(&'a self) -> SqRefAnc<'a> {
        unsafe { call_with_rc_ptr!(self, into_ref) }
    }
}

unsafe fn upgrade_weak<T: ?Sized>(ptr: *const T) -> Option<SqRc>
where
    SqRcEnum: From<Rc<T>>,
{
    let weak = ManuallyDrop::new(Weak::from_raw(ptr));
    weak.upgrade().map(SqRcEnum::from).map(SqRcEnum::stash)
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
                Rc::new(Self {
                    discrim: RcDiscrim::$variant,
                    data,
                })
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

        impl From<ManuallyDrop<Rc<$name>>> for SqRefData {
            fn from(rc: ManuallyDrop<Rc<$name>>) -> Self {
                SqRefData::$variant(rc)
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
        let (layout, discrim_offset) = layout.extend(Layout::new::<RcDiscrim>()).unwrap();
        let (layout, len_offset) = layout.extend(Layout::new::<usize>()).unwrap();
        let (layout, data_offset) = layout
            .extend(Layout::array::<u8>(string.len()).unwrap())
            .unwrap();
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

macro_rules! generic_tree {
    ($root:ident, $($t:tt),*) => {
        $root<generic_tree!($( $t ),*)>
    };
    ($lt:lifetime, $($t:tt),*) => {
        &$lt generic_tree!($( $t ),*)
    };
    ($root:ident) => { $root };
}

macro_rules! impl_sq_enum {
    ($wide:ident $(<$gen:lifetime>)?, $( $wrap:tt);+) => {
        #[derive(Debug, Clone)]
        #[repr(u8)]
        pub enum $wide $(<$gen>)? {
            String( generic_tree!($( $wrap),+ , StringStrg )) = RcDiscrim::String  as u8,
            Object( generic_tree!($( $wrap),+ , ObjectStrg )) = RcDiscrim::Object  as u8,
            Array(  generic_tree!($( $wrap),+ , ArrayStrg  )) = RcDiscrim::Array   as u8,
            Closure(generic_tree!($( $wrap),+ , ClosureStrg)) = RcDiscrim::Closure as u8,
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

impl_sq_enum!(impl SqRcEnum, Rc, SqRc);
impl_sq_enum!(impl SqWkEnum, Weak, SqWk);
impl_sq_enum!(SqRefData, ManuallyDrop; Rc);
impl_sq_enum!(SqRef<'a>, 'a; Rc);

pub struct SqRefAnc<'a>(SqRefData, std::marker::PhantomData<&'a ()>);

impl<'a> SqRefAnc<'a> {
    pub fn as_ref(&'a self) -> SqRef<'a> {
        match &self.0 {
            SqRefData::Array(rc) => SqRef::Array(&&*rc),
            SqRefData::Closure(rc) => SqRef::Closure(&&*rc),
            SqRefData::Object(rc) => SqRef::Object(&&*rc),
            SqRefData::String(rc) => SqRef::String(&&*rc),
        }
    }
}

impl<'a> SqRef<'a> {
    pub fn get_field(&self, key: &Value) -> Option<Value> {
        todo!()
    }

    pub fn get_field_str(&self, key: &str) -> Option<Value> {
        todo!()
    }
}

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
        let rce = SqRcEnum::String(ss);
        let rc = rce.stash();
        let weak = rc.downgrade();
        let rc2 = weak.upgrade();
        let sqr = rc2.as_ref().map(SqRc::borrow);
        match sqr.as_ref().map(|sqr| sqr.as_ref()) {
            Some(SqRef::String(str)) => assert_eq!(str.get_data(), string),
            _ => panic!("Expected String, got {:?}", rc2),
        }
        drop(rc);
        drop(rc2);
        assert_eq!(weak.upgrade(), None);
        drop(weak);
    }
}

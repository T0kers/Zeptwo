#[cfg(target_pointer_width = "64")]
pub type VMString = String;
#[cfg(target_pointer_width = "64")]
pub type VMInt = i64;
#[cfg(target_pointer_width = "64")]
pub type VMFlt = f64;
#[cfg(target_pointer_width = "64")]
pub type VMBool = bool;

#[cfg(target_pointer_width = "32")]
pub type VMString = String;
#[cfg(target_pointer_width = "32")]
pub type VMInt = i32;
#[cfg(target_pointer_width = "32")]
pub type VMFlt = f32;
#[cfg(target_pointer_width = "32")]
pub type VMBool = bool;

pub const VMSTRING_SIZE: usize = std::mem::size_of::<VMString>();
pub const VMINT_SIZE: usize = std::mem::size_of::<VMInt>();
pub const VMFLT_SIZE: usize = std::mem::size_of::<VMFlt>();
pub const VMBOOL_SIZE: usize = std::mem::size_of::<VMBool>();
pub const VMNUL_SIZE: usize = 0;

pub const VMWORD_SIZE: usize = if VMINT_SIZE == VMFLT_SIZE {
    VMINT_SIZE
} else {
    panic!()
};

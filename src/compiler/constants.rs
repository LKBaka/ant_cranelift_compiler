use cranelift_codegen::isa::CallConv;

#[cfg(windows)]
pub const CALL_CONV: CallConv = CallConv::WindowsFastcall;

#[cfg(target_os = "linux")]
pub const CALL_CONV: CallConv = CallConv::SystemV;

#[cfg(target_os = "macos")]
pub const CALL_CONV: CallConv = CallConv::AppleAarch64;

#[cfg(not(any(windows, target_os = "linux", target_os = "macos")))]
pub const CALL_CONV: CallConv = CallConv::SystemV;

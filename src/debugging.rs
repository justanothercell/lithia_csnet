#[macro_export]
macro_rules! dbg_eval {
    ($expr: expr) => {
        println!("{} => {:?}", stringify!($expr), $expr)
    };
}
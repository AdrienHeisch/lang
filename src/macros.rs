#[macro_export]
macro_rules! invalid_type_error {
    ($id:expr) => {
        eprintln!("Invalid type id : {:?}", $id);
        eprintln!("Valid type ids would be : {:?}", vec!(std::any::TypeId::of::<i32>(), std::any::TypeId::of::<f32>()));
        panic!();
    };
}

#[macro_export]
macro_rules! get_dyn_size {
    ($bx:expr) => {
        match $bx.id()
        {
            //☺
            t if t == std::any::TypeId::of::<f32>() => std::mem::size_of::<f32>(),
            t if t == std::any::TypeId::of::<String>() => $bx.downcast_ref::<String>().unwrap().as_bytes().len(),
            t => {
                invalid_type_error!(t);
            }
        }
    };
}

#[macro_export]
macro_rules! call_on_dyn_box {
    ($bx:expr, $f:ident) => {
        match $bx.id()
        {
            //☺
            t if t == std::any::TypeId::of::<f32>() => *$f::<f32>(*$bx.downcast_ref::<f32>().unwrap()),
            t => {
                invalid_type_error!(t);
            }
        }
    };
    ($bx:expr, $f:ident, $T2:ty) => {
        match $bx.id()
        {
            //☺
            t if t == std::any::TypeId::of::<f32>() => &$f::<f32, $T2>(*$bx.downcast_ref::<f32>().unwrap()),
            t => {
                invalid_type_error!(t);
            }
        }
    };
}

#[macro_export]
macro_rules! clone_dynamic_box {
    ($bx:ident) => {
        match $bx.id()
        {
            //☺
            t if t == std::any::TypeId::of::<f32>() => Dynamic::new(*$bx.downcast_ref::<f32>().unwrap()),
            t => {
                invalid_type_error!(t);
            }
        }
    };
}

#[macro_export]
macro_rules! dyn_box_to_string {
    ($bx:expr) => {
        match $bx.id()
        {
            //☺
            t if t == std::any::TypeId::of::<f32>() => $bx.downcast_ref::<f32>().unwrap().to_string(),
            t if t == std::any::TypeId::of::<String>() => $bx.downcast_ref::<String>().unwrap().clone(),
            t => {
                crate::invalid_type_error!(t);
            }
        }
    };
}
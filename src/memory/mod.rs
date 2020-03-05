use crate::langval::{ LangVal, LangType };

//IN BYTES
const MEMORY_SIZE:usize = 32;

//TODO retry other types of memory with fixed benchmarking
//TODO stack, faster heap allocator ?
//RESEARCH look at C stack heap implementations (stack growing from the end)
pub struct Memory
{
    ram: [u8; MEMORY_SIZE],
    allocation_map: [bool; MEMORY_SIZE]
}

#[derive(Debug, Copy, Clone, Default, PartialEq)]
pub struct Pointer
{
    t: LangType,
    ptr: RawPointer
}

#[derive(Debug, Copy, Clone, Default, PartialEq)]
struct RawPointer
{
    pos:usize,
    len:usize
}

// PUBLIC
impl Memory
{

    pub fn new () -> Self
    {
        Self
        {
            ram: [u8::default(); MEMORY_SIZE],
            allocation_map: [false; MEMORY_SIZE]
        }
    }

    #[allow(dead_code)]
    pub fn clear (&mut self)
    {
        for byte in self.ram.iter_mut() {
            *byte = u8::default();
        }
        for is_allocated in self.allocation_map.iter_mut() {
            *is_allocated = false;
        }
    }

    pub fn make_pointer_for_type (&mut self, t:LangType) -> Pointer
    {
        match t
        {
            LangType::Number => Pointer {
                t: LangType::Number,
                ptr: self.alloc(std::mem::size_of::<f32>())
            },
            LangType::Bool => Pointer {
                t: LangType::Bool,
                ptr: self.alloc(std::mem::size_of::<bool>())
            },
            LangType::Str => Pointer {
                t: LangType::Str,
                ptr: self.alloc(1)
            },
            LangType::Obj   => unimplemented!(),
            LangType::Void => panic!() //DESIGN make pointer for Void ?
        }
    }

    pub fn get_var (&self, ptr:&Pointer) -> LangVal
    {
        match ptr.t
        {
            LangType::Number => {
                LangVal::Number(f32::from_ne_bytes([self.ram[ptr.ptr.pos], self.ram[ptr.ptr.pos + 1], self.ram[ptr.ptr.pos + 2], self.ram[ptr.ptr.pos + 3]]))
            },
            LangType::Bool  => LangVal::Bool(self.access(&ptr.ptr)[0] == 1),
            LangType::Str   => LangVal::Str(String::from_utf8(self.access(&ptr.ptr).to_vec()).ok().unwrap()),
            LangType::Obj   => unimplemented!(),
            LangType::Void  => LangVal::Void,
        }
    }

    pub fn set_var (&mut self, mut ptr:Pointer, value:&LangVal) -> Pointer //TODO remove return
    {
        use LangVal::*;
        match value
        {
            Number(f) => {
                self.access_mut(&ptr.ptr).copy_from_slice(&(*f).to_ne_bytes());
            },
            Bool(b) => {
                self.access_mut(&ptr.ptr)[0] = if *b { 1u8 } else { 0u8 };
            },
            Str(s) =>  {
                let bytes = s.as_bytes();
                let len = bytes.len();
                if len != ptr.ptr.len {
                    self.realloc(&mut ptr, len);
                }
                self.access_mut(&ptr.ptr).copy_from_slice(bytes);
            },
            Obj(_) => unimplemented!(),
            Void => panic!() //DESIGN set var to Void ?
        }
        ptr
    }

    #[allow(dead_code)]
    pub fn print_ram (&self)
    {
        println!("RAM: {:?}", crate::utils::slice_to_string(&self.ram));
        println!("MAP: \"{:?}\"", self.allocation_map.iter().map::<u8, _>(|b| (*b).into()).collect::<Vec<_>>());
    }

}

// PRIVATE
impl Memory
{

    fn alloc (&mut self, len:usize) -> RawPointer
    {
        if len > MEMORY_SIZE {
            eprintln!("Tried to allocate more bytes than the memory can contain : {} / {}", len, MEMORY_SIZE);
            panic!();
        }

        let mut pos = 0;
        let ptr = loop
        {
            while self.allocation_map[pos] {
                if pos + len > MEMORY_SIZE - 1 { //TODO RETURN ERROR INSTEAD
                    eprintln!("----------");
                    eprintln!("OUT OF MEMORY !");
                    eprintln!("Tried to allocate {} bytes at index {}", len, pos);
                    eprintln!("----------");
                    self.print_ram();
                    eprintln!("----------");
                    panic!();
                }
                pos += 1;
            }
            let mut is_valid = true;
            let mut next_pos_found = false;
            let init_pos = pos;
            'inner: for i in init_pos..(init_pos + len)
            {
                let is_allocated = self.allocation_map[i];
                if is_allocated {
                    is_valid = false;
                } else if !is_valid
                {
                    pos = i;
                    next_pos_found = true;
                    break 'inner;
                }
            }
            if is_valid {
                break RawPointer { pos, len };
            }
            if !next_pos_found
            {
                pos += len;
            }
        };

        for is_allocated in self.allocation_map.iter_mut().skip(pos).take(len) {//pos..(pos + len) {
            *is_allocated = true;
        }
        ptr
    }

    //FIXME memory is never freed !
    fn free (&mut self, ptr:&RawPointer) //RESEARCH shift everything to the right so there is always free memory on the left ?
    {
        for byte in self.ram.iter_mut().skip(ptr.pos).take(ptr.len) {
            *byte = u8::default(); //TODO this is probably useless
        }
        for state in self.allocation_map.iter_mut().skip(ptr.pos).take(ptr.len) {
            *state = false;
        }
    }

    fn realloc (&mut self, var:&mut Pointer, new_len:usize)
    {
        self.free(&var.ptr);
        var.ptr = self.alloc(new_len);
    }

    fn access (&self, ptr:&RawPointer) -> &[u8]
    {
        &self.ram[ptr.pos..(ptr.pos + ptr.len)]
    }

    fn access_mut (&mut self, ptr:&RawPointer) -> &mut [u8]
    {
        &mut self.ram[ptr.pos..(ptr.pos + ptr.len)]
    }

}
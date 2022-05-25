//IN BYTES
const MEMORY_SIZE: usize = 32;

//TODO retry other types of memory with fixed benchmarking
//TODO stack, faster heap allocator ?
//RESEARCH look at C stack heap implementations (stack growing from the end)
pub struct RawMemory {
    ram: [u8; MEMORY_SIZE],
    allocation_map: [bool; MEMORY_SIZE],
}

#[derive(Debug, Copy, Clone, Default, PartialEq)]
pub struct RawPointer {
    pos: usize,
    len: usize,
}

// PUBLIC
impl RawMemory {
    pub fn new() -> Self {
        Self {
            ram: [u8::default(); MEMORY_SIZE],
            allocation_map: [false; MEMORY_SIZE],
        }
    }

    pub fn clear(&mut self) {
        for byte in self.ram.iter_mut() {
            *byte = u8::default();
        }
        for is_allocated in self.allocation_map.iter_mut() {
            *is_allocated = false;
        }
    }

    pub fn alloc(&mut self, len: usize) -> RawPointer {
        if len > MEMORY_SIZE {
            panic!(
                "Tried to allocate more bytes than the memory can contain : {} / {}",
                len, MEMORY_SIZE
            );
        }

        let mut pos = 0;
        let ptr = loop {
            while self.allocation_map[pos] {
                //TODO RETURN ERROR INSTEAD
                if pos + len > MEMORY_SIZE - 1 {
                    eprintln!("----------");
                    eprintln!("OUT OF MEMORY !");
                    eprintln!("Tried to allocate {} bytes at index {}", len, pos);
                    eprintln!("----------");
                    self.print_ram();
                    eprintln!("----------");
                    panic!("OUT OF MEMORY !");
                }
                pos += 1;
            }
            let mut is_valid = true;
            let mut next_pos_found = false;
            let init_pos = pos;
            'inner: for i in init_pos..(init_pos + len) {
                let is_allocated = self.allocation_map[i];
                if is_allocated {
                    is_valid = false;
                } else if !is_valid {
                    pos = i;
                    next_pos_found = true;
                    break 'inner;
                }
            }
            if is_valid {
                break RawPointer { pos, len };
            }
            if !next_pos_found {
                pos += len;
            }
        };

        for is_allocated in self.allocation_map.iter_mut().skip(pos).take(len) {
            //pos..(pos + len) {
            *is_allocated = true;
        }
        ptr
    }

    pub fn free(&mut self, ptr: RawPointer)
    //RESEARCH shift everything to the right so there is always free memory on the left ?
    {
        for state in self.allocation_map.iter_mut().skip(ptr.pos).take(ptr.len) {
            *state = false;
        }
    }

    pub fn realloc(&mut self, ptr: RawPointer, new_len: usize) -> RawPointer {
        self.free(ptr);
        self.alloc(new_len)
    }

    pub fn access(&self, ptr: RawPointer) -> &[u8] {
        &self.ram[ptr.pos..(ptr.pos + ptr.len)]
    }

    pub fn access_mut(&mut self, ptr: RawPointer) -> &mut [u8] {
        &mut self.ram[ptr.pos..(ptr.pos + ptr.len)]
    }

    pub fn print_ram(&self) {
        if cfg!(lang_benchmark) {
            return;
        }
        println!("RAM: {:?}", crate::utils::slice_to_string(&self.ram));
        println!(
            "MAP: \"{:?}\"",
            self.allocation_map
                .iter()
                .map::<u8, _>(|b| (*b).into())
                .collect::<Vec<_>>()
        );
    }
}

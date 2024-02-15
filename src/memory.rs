//IN BYTES
const MEMORY_SIZE: usize = 64;

#[derive(Debug)]
pub struct MemoryError {
    pub msg: String,
}

//TODO retry other types of memory with fixed benchmarking
//TODO stack, faster heap allocator ?
//RESEARCH look at C stack heap implementations (stack growing from the end)
pub struct RawMemory {
    ram: [u8; MEMORY_SIZE],
    allocation_map: [bool; MEMORY_SIZE],
}

#[derive(Debug, Copy, Clone, Default, PartialEq)]
pub struct Address {
    //TODO refactor to integer
    pub pos: usize,
    pub len: usize,
}

#[allow(dead_code)]
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

    pub fn alloc(&mut self, len: usize) -> Result<Address, MemoryError> {
        if len > MEMORY_SIZE {
            return Err(MemoryError {
                msg: format!(
                    "Tried to allocate more bytes than the memory can contain : {} / {}",
                    len, MEMORY_SIZE
                ),
            });
        }

        let mut pos = 0;
        let ptr = loop {
            while self.allocation_map[pos] {
                //TODO RETURN ERROR INSTEAD
                if pos + len > MEMORY_SIZE - 1 {
                    return Err(MemoryError {
                        msg: format!("----------\nOUT OF MEMORY !\nTried to allocate {} bytes at index {}\n----------\n{}\n----------\nOUT OF MEMORY !", len, pos, self.print_ram())
                    });
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
                break Address { pos, len };
            }
            if !next_pos_found {
                pos += len;
            }
        };

        for is_allocated in self.allocation_map.iter_mut().skip(pos).take(len) {
            //pos..(pos + len) {
            *is_allocated = true;
        }
        Ok(ptr)
    }

    pub fn free(&mut self, ptr: Address)
    //RESEARCH shift everything to the right so there is always free memory on the left ?
    {
        for state in self.allocation_map.iter_mut().skip(ptr.pos).take(ptr.len) {
            *state = false;
        }
    }

    pub fn free_from(&mut self, addr: usize) {
        for state in self.allocation_map.iter_mut().skip(addr) {
            *state = false;
        }
    }

    pub fn realloc(&mut self, ptr: Address, new_len: usize) -> Result<Address, MemoryError> {
        self.free(ptr);
        self.alloc(new_len)
    }

    pub fn access(&self, ptr: Address) -> &[u8] {
        &self.ram[ptr.pos..(ptr.pos + ptr.len)]
    }

    pub fn access_mut(&mut self, ptr: Address) -> &mut [u8] {
        &mut self.ram[ptr.pos..(ptr.pos + ptr.len)]
    }

    pub fn print_ram(&self) -> String {
        use std::fmt::Write;

        let mut buf = String::new();
        if cfg!(not(lang_benchmark)) {
            writeln!(buf, "RAM: {:?}", crate::utils::slice_to_string(&self.ram)).unwrap();
            writeln!(
                buf,
                "MAP: \"{:?}\"",
                self.allocation_map
                    .iter()
                    .map::<u8, _>(|b| (*b).into())
                    .collect::<Vec<_>>()
            )
            .unwrap();
        }
        buf
    }
}

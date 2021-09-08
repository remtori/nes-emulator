#[derive(Debug)]
pub struct Bus {
    // Fake RAM for testing CPU
    pub ram: [u8; 64 * 1048],
}

impl Default for Bus {
    fn default() -> Self {
        Self { ram: [0; 64 * 1048] }
    }
}

impl Bus {
    pub fn read(&self, addr: u16, _read_only: bool) -> u8 {
        if (0x0000..=0xFFFF).contains(&addr) {
            // System RAM Address Range, mirrored every 2048
            // self.ram[(addr & 0x07FF) as usize]

            self.ram[addr as usize]
        } else {
            0
        }
    }

    pub fn write(&mut self, addr: u16, data: u8) {
        if (0x0000..=0xFFFF).contains(&addr) {
            // System RAM Address Range, mirrored every 2048
            self.ram[(addr & 0x07FF) as usize] = data;
        }
    }
}

mod bus;
mod cpu6502;

pub use bus::Bus;
pub use cpu6502::{Cpu6502, Flags};

pub struct NesEmulator {
    cpu: Cpu6502,
    pub bus: Bus,
}

impl NesEmulator {
    pub fn new() -> Self {
        let mut nes = Self {
            cpu: Cpu6502::default(),
            bus: Bus::default(),
        };

        nes.cpu.reset(&mut nes.bus);

        nes
    }

    pub fn irq(&mut self) {
        self.cpu.irq(&mut self.bus);
    }

    pub fn nmi(&mut self) {
        self.cpu.nmi(&mut self.bus);
    }

    pub fn reset(&mut self) {
        self.cpu.reset(&mut self.bus);
    }

    pub fn clock(&mut self) {
        loop {
            self.cpu.clock(&mut self.bus);
            if self.cpu.is_complete() {
                break;
            }
        }
    }

    pub fn cpu_flag(&self, flag: Flags) -> bool {
        self.cpu.get(flag) == 1
    }

    pub fn cpu(&self) -> &Cpu6502 {
        &self.cpu
    }
}

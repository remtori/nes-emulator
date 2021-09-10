use std::{collections::BTreeMap, fmt::Write, ops::RangeInclusive};

use bitflags::bitflags;

use super::Bus;

mod addressing;
mod instruction;
mod lookup_table;

pub use addressing::{AddressingData, AddressingMode};

bitflags! {
    /// 6502 processor flags
    pub struct Flags: u8 {
        /// Carry flag - Set if the last instruction resulted in an over or underflow.
        /// Used for arithmetic on numbers larger than one byte,
        /// where the next instruction is carry-flag aware
        const C = 1 << 0;

        /// Zero flag - Set if the last instruction resulted in a value of 0
        const Z = 1 << 1;

        /// Interrupt Disable - Set to disable responding to maskable interrupts
        const I = 1 << 2;

        /// Decimal Mode - Set to enable BCD mode.
        /// This doesn't affect the 2A03 so flipping this value doesn't do anything
        const D = 1 << 3;

        /// Break Command - Set to indicate a `BRK` instruction was executed
        const B = 1 << 4;

        /// Bit 5 is unused
        const U = 1 << 5;

        /// Overflow flag - Set when an invalid two's complement number is the result of an operation.
        /// An example is adding 2 positive numbers
        /// which results in the sign bit being set, making the result a negative
        const V = 1 << 6;

        /// Negative flag - Set if the number is negative, determined by checking the sign bit (7th bit)
        const N = 1 << 7;
    }
}

/// NES's CPU little-endian and uses the same instruction set as the 6502 with some minor differences
///
/// This struct hold all the register and is responsible for executing instruction
#[derive(Debug)]
pub struct Cpu6502 {
    /// Accumulator, related to all arithmetic related instructions
    a: u8,

    /// X general purpose register, can be used to set or get the SP
    x: u8,

    /// Y general purpose register
    y: u8,

    /// Processor status flags register
    status: Flags,

    /// Points to the current size of the stack,
    /// and is the least significant byte of an address starting at offset 0x100 in memory.
    ///
    /// This means that the stack is located in memory from 0x100 to 0x1FF.
    ///
    /// The SP grows downwards in memory space,
    /// meaning that pushing onto the stack will decrement the stack pointer, and popping will increment it.
    sp: u8,

    /// Program Counter, get increase after an instruction is fetched
    pc: u16,

    /// Count how many cycles the current instruction has remaining
    cycles: u8,

    /// A global accumulation of the number of clocks
    clock_count: usize,

    /// Current opcode
    opcode: u8,
}

impl Default for Cpu6502 {
    fn default() -> Self {
        Self {
            a: 0,
            x: 0,
            y: 0,
            status: Flags::U,
            sp: 0xFD,
            pc: 0xFFFC,
            cycles: 0,
            clock_count: 0,
            opcode: 0,
        }
    }
}

impl Cpu6502 {
    /// Forces the 6502 into a known state. This is hard-wired inside the CPU. The
    /// registers are set to 0x00, the status register is cleared except for unused
    /// bit which remains at 1. An absolute address is read from location 0xFFFC
    /// which contains a second address that the program counter is set to. This
    /// allows the programmer to jump to a known and programmable location in the
    /// memory to start executing from. Typically the programmer would set the value
    /// at location 0xFFFC at compile time.
    pub fn reset(&mut self, bus: &mut Bus) {
        // Get address to set program counter to
        let address = 0xFFFC;
        self.pc = bus.read(address + 0, false) as u16;
        self.pc |= (bus.read(address + 1, false) as u16) << 8;

        // Reset internal registers
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sp = 0xFD;
        self.status = Flags::U;

        // Reset takes time
        self.cycles = 8;
    }

    /// Interrupt requests are a complex operation and only happen if the
    /// "disable interrupt" flag is 0. IRQs can happen at any time, but
    /// you dont want them to be destructive to the operation of the running
    /// program. Therefore the current instruction is allowed to finish
    /// (which I facilitate by doing the whole thing when cycles == 0) and
    /// then the current program counter is stored on the stack. Then the
    /// current status register is stored on the stack. When the routine
    /// that services the interrupt has finished, the status register
    /// and program counter can be restored to how they where before it
    /// occurred. This is impemented by the "RTI" instruction. Once the IRQ
    /// has happened, in a similar way to a reset, a programmable address
    /// is read form hard coded location 0xFFFE, which is subsequently
    /// set to the program counter.
    pub fn irq(&mut self, bus: &mut Bus) {
        // If interrupts are allowed
        if self.get(Flags::I) == 0 {
            // Push the program counter to the stack
            self.push_u16(bus, self.pc);

            // Then Push the status register to the stack
            self.set(Flags::B, false);
            self.set(Flags::U, true);
            self.set(Flags::I, true);
            self.push(bus, self.status.bits);

            // Read new program counter location from fixed address
            let address = 0xFFFE;
            self.pc = bus.read(address + 0, false) as u16;
            self.pc |= (bus.read(address + 1, false) as u16) << 8;

            // IRQs take time
            self.cycles = 7;
        }
    }

    /// A Non-Maskable Interrupt cannot be ignored. It behaves in exactly the
    /// same way as a regular IRQ, but reads the new program counter address
    /// form location 0xFFFA.
    pub fn nmi(&mut self, bus: &mut Bus) {
        // Push the program counter to the stack
        self.push_u16(bus, self.pc);

        // Then Push the status register to the stack
        self.set(Flags::B, false);
        self.set(Flags::U, true);
        self.set(Flags::I, true);
        self.push(bus, self.status.bits);

        // Read new program counter location from fixed address
        let address = 0xFFFA;
        self.pc = bus.read(address + 0, false) as u16;
        self.pc |= (bus.read(address + 1, false) as u16) << 8;

        self.cycles = 8;
    }

    /// Perform one clock cycles worth of emulation
    pub fn clock(&mut self, bus: &mut Bus) {
        // Each instruction requires a variable number of clock cycles to execute.
        // In my emulation, I only care about the final result and so I perform
        // the entire computation in one hit. In hardware, each clock cycle would
        // perform "microcode" style transformations of the CPUs state.
        //
        // To remain compliant with connected devices, it's important that the
        // emulation also takes "time" in order to execute instructions, so I
        // implement that delay by simply counting down the cycles required by
        // the instruction. When it reaches 0, the instruction is complete, and
        // the next one is ready to be executed.
        if self.cycles == 0 {
            // Read next instruction byte. This 8-bit value is used to index
            // the translation table to get the relevant information about
            // how to implement the instruction
            self.opcode = bus.read(self.pc, false);

            // Always set the unused status flag bit to 1
            self.set(Flags::U, true);

            // Increment program counter, we read the opcode byte
            self.pc += 1;

            let instruction = &lookup_table::INSTRUCTION_LOOKUP_TABLE[self.opcode as usize];

            // Perform fetch of intermediate data using the
            // required addressing mode
            let addressing = instruction.addressing_mode.load(self, bus);

            // Perform operation
            let require_additional_cycle = (instruction.operate)(self, bus, &addressing);

            // The address mode and opcode may have altered the number
            // of cycles this instruction requires before its completed
            self.cycles = instruction.cycles;
            if addressing.require_more_cycle && require_additional_cycle {
                self.cycles += 1;
            }

            // Always set the unused status flag bit to 1
            self.set(Flags::U, true);
        }

        // Increment global clock count
        self.clock_count += 1;

        // Decrement the number of cycles remaining for this instruction
        self.cycles -= 1;
    }

    pub fn is_complete(&self) -> bool {
        self.cycles == 0
    }

    pub fn get(&self, flag: Flags) -> u8 {
        if self.status.contains(flag) {
            1
        } else {
            0
        }
    }

    pub fn set(&mut self, flag: Flags, value: bool) {
        self.status.set(flag, value);
    }

    #[inline]
    fn pop(&mut self, bus: &mut Bus) -> u8 {
        self.sp += 1;
        bus.read(0x0100 + self.sp as u16, false)
    }

    #[inline]
    fn push(&mut self, bus: &mut Bus, value: u8) {
        bus.write(0x0100 + self.sp as u16, value);
        self.sp -= 1;
    }

    #[inline]
    fn pop_u16(&mut self, bus: &mut Bus) -> u16 {
        let mut out = self.pop(bus) as u16;
        out |= (self.pop(bus) as u16) << 8;
        out
    }

    #[inline]
    fn push_u16(&mut self, bus: &mut Bus, value: u16) {
        self.push(bus, (value >> 8) as u8);
        self.push(bus, (value & 0xFF) as u8);
    }

    pub fn disassemble(range: RangeInclusive<u16>, bus: &mut Bus) -> BTreeMap<u16, String> {
        let mut out = BTreeMap::new();

        let mut addr = *range.start() as u32;
        let end = *range.end() as u32;

        let read = |addr: &mut u32| -> u8 {
            let ret = bus.read(*addr as u16, true);
            *addr += 1;
            ret
        };

        let read_u16 = |addr: &mut u32| -> u16 {
            let lo = bus.read(*addr as u16, true);
            *addr += 1;
            let hi = bus.read(*addr as u16, true);
            *addr += 1;

            (lo as u16) | ((hi as u16) << 8)
        };

        while addr <= end {
            let line_addr = addr;

            let opcode = read(&mut addr);

            let instruction = &lookup_table::INSTRUCTION_LOOKUP_TABLE[opcode as usize];

            let str_ins = &mut format!("${:04X}: {} ", line_addr, instruction.name);

            // Get operands from desired locations, and form the
            // instruction based upon its addressing mode. These
            // routines mimic the actual fetch routine of the
            // 6502 in order to get accurate data as part of the
            // instruction
            match instruction.addressing_mode {
                AddressingMode::Implied => write!(str_ins, " {{IMP}}"),
                AddressingMode::Immediate => write!(str_ins, "#${:02X} {{IMM}}", read(&mut addr)),
                AddressingMode::ZeroPage => write!(str_ins, "${:02X} {{ZP0}}", read(&mut addr)),
                AddressingMode::ZeroPageX => write!(str_ins, "${:02X}, X {{ZPX}}", read(&mut addr)),
                AddressingMode::ZeroPageY => write!(str_ins, "${:02X}, Y {{ZPY}}", read(&mut addr)),
                AddressingMode::Relative => {
                    let value = read(&mut addr);
                    write!(str_ins, "${:02X}[${:04X}] {{REL}}", value, addr as u16 + value as u16)
                }
                AddressingMode::Absolute => write!(str_ins, "${:04X} {{ABS}}", read_u16(&mut addr)),
                AddressingMode::AbsoluteX => write!(str_ins, "${:04X}, X {{ABX}}", read_u16(&mut addr)),
                AddressingMode::AbsoluteY => write!(str_ins, "${:04X}, Y {{ABY}}", read_u16(&mut addr)),
                AddressingMode::Indirect => write!(str_ins, "(${:04X}) {{IND}}", read_u16(&mut addr)),
                AddressingMode::IndirectX => write!(str_ins, "(${:02X}, X) {{IZX}}", read(&mut addr)),
                AddressingMode::IndirectY => write!(str_ins, "(${:02X}), Y {{IZY}}", read(&mut addr)),
            }
            .unwrap();

            out.insert(line_addr as u16, std::mem::take(str_ins));
        }

        out
    }

    pub fn pc(&self) -> u16 {
        self.pc
    }

    pub fn a(&self) -> u8 {
        self.a
    }

    pub fn x(&self) -> u8 {
        self.x
    }

    pub fn y(&self) -> u8 {
        self.y
    }

    pub fn sp(&self) -> u8 {
        self.sp
    }
}

use super::{Bus, Cpu6502};

#[derive(Debug)]
pub enum AddressingMode {
    IMP,
    IMM,
    ZP0,
    ZPX,
    ZPY,
    REL,
    ABS,
    ABX,
    ABY,
    IND,
    IZX,
    IZY,
}

/// Store addressing data
#[derive(Debug)]
pub struct AddressingData {
    /// Addressing Mode of this instruction
    pub addressing_mode: AddressingMode,

    /// This addressing mode may require 1 more cycle
    pub require_more_cycle: bool,

    /// The address we fetch value from
    pub address: u16,

    /// Pre-fetched value
    fetched: Option<u8>,
}

impl AddressingData {
    pub fn fetch(&self, bus: &mut Bus) -> u8 {
        self.fetched.unwrap_or_else(|| bus.read(self.address, false))
    }
}

// The 6502 can address between 0x0000 - 0xFFFF. The high byte is often referred
// to as the "page", and the low byte is the offset into that page. This implies
// there are 256 pages, each containing 256 bytes.
//
// Several addressing modes have the potential to require an additional clock
// cycle if they cross a page boundary. This is combined with several instructions
// that enable this additional clock cycle. So each addressing function returns
// a flag saying it has potential, as does each instruction. If both instruction
// and address function return 1, then an additional clock cycle is required.

/// Address Mode: Implied
/// There is no additional data required for this instruction. The instruction
/// does something very simple like like sets a status bit. However, we will
/// target the accumulator, for instructions like PHA
pub(super) fn imp(cpu: &mut Cpu6502, _bus: &Bus) -> AddressingData {
    AddressingData {
        addressing_mode: AddressingMode::IMP,
        require_more_cycle: false,
        fetched: Some(cpu.a),
        address: 0x0,
    }
}

/// Address Mode: Immediate
/// The instruction expects the next byte to be used as a value, so we'll prep
/// the read address to point to the next byte
pub(super) fn imm(cpu: &mut Cpu6502, _bus: &Bus) -> AddressingData {
    let address = cpu.pc;
    cpu.pc += 1;

    AddressingData {
        addressing_mode: AddressingMode::IMM,
        require_more_cycle: false,
        fetched: None,
        address,
    }
}

/// Address Mode: Zero Page
/// To save program bytes, zero page addressing allows you to absolutely address
/// a location in first 0xFF bytes of address range. Clearly this only requires
/// one byte instead of the usual two.
pub(super) fn zp0(cpu: &mut Cpu6502, bus: &Bus) -> AddressingData {
    let address = bus.read(cpu.pc, false) as u16 & 0x00FF;
    cpu.pc += 1;

    AddressingData {
        addressing_mode: AddressingMode::ZP0,
        require_more_cycle: false,
        fetched: None,
        address,
    }
}

/// Address Mode: Zero Page with X Offset
/// Fundamentally the same as Zero Page addressing, but the contents of the X Register
/// is added to the supplied single byte address. This is useful for iterating through
/// ranges within the first page.
pub(super) fn zpx(cpu: &mut Cpu6502, bus: &Bus) -> AddressingData {
    let address = bus.read(cpu.pc + cpu.x as u16, false) as u16 & 0x00FF;
    cpu.pc += 1;

    AddressingData {
        addressing_mode: AddressingMode::ZPX,
        require_more_cycle: false,
        fetched: None,
        address,
    }
}

/// Address Mode: Zero Page with Y Offset
/// Same as above but uses Y Register for offset
pub(super) fn zpy(cpu: &mut Cpu6502, bus: &Bus) -> AddressingData {
    let address = bus.read(cpu.pc + cpu.y as u16, false) as u16 & 0x00FF;
    cpu.pc += 1;

    AddressingData {
        addressing_mode: AddressingMode::ZPY,
        require_more_cycle: false,
        fetched: None,
        address,
    }
}

/// Address Mode: Relative
/// This address mode is exclusive to branch instructions. The address
/// must reside within -128 to +127 of the branch instruction, i.e.
/// you cant directly branch to any address in the addressable range.
pub(super) fn rel(cpu: &mut Cpu6502, bus: &Bus) -> AddressingData {
    let mut address = bus.read(cpu.pc, false) as u16;
    cpu.pc += 1;

    if address & 0x80 > 0 {
        address |= 0xFF00;
    }

    AddressingData {
        addressing_mode: AddressingMode::REL,
        require_more_cycle: false,
        fetched: None,
        address,
    }
}

/// Address Mode: Absolute
/// A full 16-bit address is loaded and used
pub(super) fn abs(cpu: &mut Cpu6502, bus: &Bus) -> AddressingData {
    let lo = bus.read(cpu.pc, false) as u16;
    cpu.pc += 1;

    let hi = bus.read(cpu.pc, false) as u16;
    cpu.pc += 1;

    let address = (hi << 8) | lo;

    AddressingData {
        addressing_mode: AddressingMode::ABS,
        require_more_cycle: false,
        fetched: None,
        address,
    }
}

/// Address Mode: Absolute with X Offset
/// Fundamentally the same as absolute addressing, but the contents of the X Register
/// is added to the supplied two byte address. If the resulting address changes
/// the page, an additional clock cycle is required
pub(super) fn abx(cpu: &mut Cpu6502, bus: &Bus) -> AddressingData {
    let lo = bus.read(cpu.pc, false) as u16;
    cpu.pc += 1;

    let hi = bus.read(cpu.pc, false) as u16;
    cpu.pc += 1;

    let address = (hi << 8) | lo;
    let address = address + cpu.x as u16;

    AddressingData {
        addressing_mode: AddressingMode::ABX,
        require_more_cycle: (address & 0xFF00) != (hi << 8),
        fetched: None,
        address,
    }
}

/// Address Mode: Absolute with Y Offset
/// Fundamentally the same as absolute addressing, but the contents of the Y Register
/// is added to the supplied two byte address. If the resulting address changes
/// the page, an additional clock cycle is required
pub(super) fn aby(cpu: &mut Cpu6502, bus: &Bus) -> AddressingData {
    let lo = bus.read(cpu.pc, false) as u16;
    cpu.pc += 1;

    let hi = bus.read(cpu.pc, false) as u16;
    cpu.pc += 1;

    let address = (hi << 8) | lo;
    let address = address + cpu.y as u16;

    AddressingData {
        addressing_mode: AddressingMode::ABY,
        require_more_cycle: (address & 0xFF00) != (hi << 8),
        fetched: None,
        address,
    }
}

// Note: The next 3 address modes use indirection (aka Pointers!)

/// Address Mode: Indirect
/// The supplied 16-bit address is read to get the actual 16-bit address. This is
/// instruction is unusual in that it has a bug in the hardware! To emulate its
/// function accurately, we also need to emulate this bug. If the low byte of the
/// supplied address is 0xFF, then to read the high byte of the actual address
/// we need to cross a page boundary. This doesnt actually work on the chip as
/// designed, instead it wraps back around in the same page, yielding an
/// invalid actual address
pub(super) fn ind(cpu: &mut Cpu6502, bus: &Bus) -> AddressingData {
    let ptr_lo = bus.read(cpu.pc, false) as u16;
    cpu.pc += 1;
    let ptr_hi = bus.read(cpu.pc, false) as u16;
    cpu.pc += 1;

    let ptr = (ptr_hi << 8) | ptr_lo;

    let address = if ptr_lo == 0x00FF
    // Simulate page boundary hardware bug
    {
        ((bus.read(ptr & 0xFF00, false) as u16) << 8) | bus.read(ptr + 0, false) as u16
    } else
    // Behave normally
    {
        ((bus.read(ptr + 1, false) as u16) << 8) | bus.read(ptr + 0, false) as u16
    };

    AddressingData {
        addressing_mode: AddressingMode::IND,
        require_more_cycle: false,
        fetched: None,
        address,
    }
}

/// Address Mode: Indirect X
/// The supplied 8-bit address is offset by X Register to index
/// a location in page 0x00. The actual 16-bit address is read
/// from this location
pub(super) fn izx(cpu: &mut Cpu6502, bus: &Bus) -> AddressingData {
    let ptr = bus.read(cpu.pc, false) as u16;
    cpu.pc += 1;

    let lo = bus.read((ptr + cpu.x as u16) & 0x00FF, false) as u16;
    let hi = bus.read((ptr + cpu.x as u16 + 1) & 0x00FF, false) as u16;

    let address = (hi << 8) | lo;

    AddressingData {
        addressing_mode: AddressingMode::IZX,
        require_more_cycle: false,
        fetched: None,
        address,
    }
}

/// Address Mode: Indirect Y
/// The supplied 8-bit address indexes a location in page 0x00. From
/// here the actual 16-bit address is read, and the contents of
/// Y Register is added to it to offset it. If the offset causes a
/// change in page then an additional clock cycle is required.
pub(super) fn izy(cpu: &mut Cpu6502, bus: &Bus) -> AddressingData {
    let ptr = bus.read(cpu.pc, false) as u16;
    cpu.pc += 1;

    let lo = bus.read(ptr & 0x00FF, false) as u16;
    let hi = bus.read((ptr + 1) & 0x00FF, false) as u16;

    let address = (hi << 8) | lo;
    let address = address + cpu.y as u16;

    AddressingData {
        addressing_mode: AddressingMode::IZY,
        require_more_cycle: (address & 0xFF00) != (hi << 8),
        fetched: None,
        address,
    }
}

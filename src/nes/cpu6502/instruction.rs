use super::{AddressingData, AddressingMode, Bus, Cpu6502, Flags};

pub struct Instruction {
    pub(super) name: &'static str,
    pub(super) operate: fn(&mut Cpu6502, &mut Bus, &AddressingData) -> bool,
    pub(super) addressing_mode: AddressingMode,
    pub(super) cycles: u8,
}

macro_rules! set_flags {
    ($cpu:ident, $var:expr, NZ) => {
        $cpu.set(Flags::Z, ($var & 0xFF) == 0);
        $cpu.set(Flags::N, ($var & 0x80) > 0);
    };
    ($cpu:ident, $var:expr, NZC) => {
        $cpu.set(Flags::Z, ($var & 0xFF) == 0);
        $cpu.set(Flags::N, ($var & 0x80) > 0);
        $cpu.set(Flags::C, ($var & 0xFF00) > 0);
    };
}

/// Capture illegal opcodes
pub(super) fn xxx(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    println!("Illegal opcode {:#X} [{}] executed", cpu.opcode, cpu.opcode);
    false
}

/// Add with Carry In
///
/// Function: A = A + M + C
///
/// Flags out: N, Z, C, V
///
/// Explanation:
/// The purpose of this function is to add a value to the accumulator and a carry bit. If
/// the result is > 255 there is an overflow setting the carry bit. Ths allows you to
/// chain together ADC instructions to add numbers larger than 8-bits. This in itself is
/// simple, however the 6502 supports the concepts of Negativity/Positivity and Signed Overflow.
///
/// 10000100 = 128 + 4 = 132 in normal circumstances, we know this as unsigned and it allows
/// us to represent numbers between 0 and 255 (given 8 bits). The 6502 can also interpret
/// this word as something else if we assume those 8 bits represent the range -128 to +127,
/// i.e. it has become signed.
///
/// Since 132 > 127, it effectively wraps around, through -128, to -124. This wraparound is
/// called overflow, and this is a useful to know as it indicates that the calculation has
/// gone outside the permissable range, and therefore no longer makes numeric sense.
///
/// Note the implementation of ADD is the same in binary, this is just about how the numbers
/// are represented, so the word 10000100 can be both -124 and 132 depending upon the
/// context the programming is using it in. We can prove this!
///
///  10000100 =  132  or  -124
/// +00010001 = + 17      + 17
///  ========    ===       ===     See, both are valid additions, but our interpretation of
///  10010101 =  149  or  -107     the context changes the value, not the hardware!
///
/// In principle under the -128 to 127 range:
/// 10000000 = -128, 11111111 = -1, 00000000 = 0, 00000000 = +1, 01111111 = +127
/// therefore negative numbers have the most significant set, positive numbers do not
///
/// To assist us, the 6502 can set the overflow flag, if the result of the addition has
/// wrapped around. V <- ~(A^M) & A^(A+M+C) :D lol, let's work out why!
///
/// Let's suppose we have A = 30, M = 10 and C = 0
///          A = 30 = 00011110
///          M = 10 = 00001010+
///     RESULT = 40 = 00101000
///
/// Here we have not gone out of range. The resulting significant bit has not changed.
/// So let's make a truth table to understand when overflow has occurred. Here I take
/// the MSB of each component, where R is RESULT.
///
/// A  M  R | V | A^R | A^M |~(A^M) |
/// 0  0  0 | 0 |  0  |  0  |   1   |
/// 0  0  1 | 1 |  1  |  0  |   1   |
/// 0  1  0 | 0 |  0  |  1  |   0   |
/// 0  1  1 | 0 |  1  |  1  |   0   |  so V = ~(A^M) & (A^R)
/// 1  0  0 | 0 |  1  |  1  |   0   |
/// 1  0  1 | 0 |  0  |  1  |   0   |
/// 1  1  0 | 1 |  1  |  0  |   1   |
/// 1  1  1 | 0 |  0  |  0  |   1   |
///
/// We can see how the above equation calculates V, based on A, M and R. V was chosen
/// based on the following hypothesis:
///       Positive Number + Positive Number = Negative Result -> Overflow
///       Negative Number + Negative Number = Positive Result -> Overflow
///       Positive Number + Negative Number = Either Result -> Cannot Overflow
///       Positive Number + Positive Number = Positive Result -> No Overflow
///       Negative Number + Negative Number = Negative Result -> No Overflow
pub(super) fn adc(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    let fetched = data.fetch(bus);

    // Add is performed in 16-bit domain for emulation to capture any
    // carry bit, which will exist in bit 8 of the 16-bit word
    let sum = cpu.a as u16 + fetched as u16 + cpu.get(Flags::C) as u16;

    // The signed Overflow flag is set based on all that up there!
    cpu.set(Flags::V, (!(cpu.a as u16 ^ fetched as u16) & (cpu.a as u16 ^ sum)) & 0x0080 > 0);
    set_flags!(cpu, sum, NZC);

    // Load the result into the accumulator
    cpu.a = sum as u8;

    true
}

/// Subtraction with Borrow In
///
/// Function: A = A - M - (1 - C)
///
/// Flags out: N, Z, C, V
///
/// Explanation:
/// Given the explanation for ADC above, we can reorganize our data
/// to use the same computation for addition, for subtraction by multiplying
/// the data by -1, i.e. make it negative
///
/// A = A - M - (1 - C)  ->  A = A + -1 * (M - (1 - C))  ->  A = A + (-M + 1 + C)
///
/// To make a signed positive number negative, we can invert the bits and add 1
/// (OK, I lied, a little bit of 1 and 2s complement :P)
///
///  5 = 00000101
/// -5 = 11111010 + 00000001 = 11111011 (or 251 in our 0 to 255 range)
///
/// The range is actually unimportant, because if I take the value 15, and add 251
/// to it, given we wrap around at 256, the result is 10, so it has effectively
/// subtracted 5, which was the original intention. (15 + 251) % 256 = 10
///
/// Note that the equation above used (1-C), but this got converted to + 1 + C.
/// This means we already have the +1, so all we need to do is invert the bits
/// of M, the data(!) therefore we can simply add, exactly the same way we did
/// before.
pub(super) fn sbc(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    // We can invert the bottom 8 bits with bitwise xor
    let value = data.fetch(bus) as u16 ^ 0xFF;

    // Notice this is exactly the same as addition from here!
    let sum = cpu.a as u16 + value as u16 + cpu.get(Flags::C) as u16;
    cpu.set(Flags::V, (sum ^ cpu.a as u16) & (sum ^ value) & 0x0080 > 0);
    set_flags!(cpu, sum, NZC);

    cpu.a = sum as u8;

    true
}

/// Bitwise Logic AND
///
/// Function: A = A & M
///
/// Flags out: N, Z
pub(super) fn and(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    cpu.a = cpu.a & data.fetch(bus);
    set_flags!(cpu, cpu.a, NZ);
    true
}

/// Bitwise Logic XOR
///
/// Function: A = A ^ M
///
/// Flags Out:   N, Z
pub(super) fn eor(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    cpu.a = cpu.a ^ data.fetch(bus);
    set_flags!(cpu, cpu.a, NZ);
    true
}

/// Instruction: Bitwise Logic OR
/// Function:    A = A | M
/// Flags Out:   N, Z
pub(super) fn ora(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    cpu.a = data.fetch(bus);
    set_flags!(cpu, cpu.a, NZ);
    true
}

/// Arithmetic Shift Left
///
/// Function: A = C <- (A << 1) <- 0
///
/// Flags out: N, Z, C
pub(super) fn asl(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    let value = (data.fetch(bus) as u16) << 1;
    set_flags!(cpu, value, NZC);

    if matches!(data.addressing_mode, AddressingMode::Implied) {
        cpu.a = (value & 0xFF) as u8;
    } else {
        bus.write(data.address, (value & 0xFF) as u8);
    }

    false
}

/// Logical Shift Right
///
/// Function: Y = M / 2
///
/// Flags out: N, Z, C
pub(super) fn lsr(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    let fetched = data.fetch(bus);
    cpu.set(Flags::C, fetched & 1 > 0);

    let fetched = fetched >> 1;
    set_flags!(cpu, fetched, NZ);

    if matches!(data.addressing_mode, AddressingMode::Implied) {
        cpu.a = fetched;
    } else {
        bus.write(data.address, fetched);
    }

    false
}

/// Rotate Left
///
/// Function: M = (M << 1) | C
///
/// Flags out: N, Z, C
pub(super) fn rol(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    let value = ((data.fetch(bus) as u16) << 1) | cpu.get(Flags::C) as u16;
    set_flags!(cpu, value, NZC);

    if matches!(data.addressing_mode, AddressingMode::Implied) {
        cpu.a = value as u8;
    } else {
        bus.write(data.address, value as u8);
    }

    false
}

/// Rotate Right
///
/// Function: M = (M >> 1) | (C << 7)
///
/// Flags out: N, Z, C
pub(super) fn ror(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    let fetched = data.fetch(bus);
    let value = (fetched >> 1) | (cpu.get(Flags::C) << 7);

    // Set to contents of old bit 0
    cpu.set(Flags::C, fetched & 1 > 0);
    set_flags!(cpu, value, NZ);

    if matches!(data.addressing_mode, AddressingMode::Implied) {
        cpu.a = value;
    } else {
        bus.write(data.address, value);
    }

    false
}

macro_rules! branch_impl {
    (
        $(
            $(#[$meta:meta])*
            fn $func:ident($flag:ident == $value:expr)
        )+
    ) => {
        $(
            $(#[$meta])*
            pub(super) fn $func(cpu: &mut Cpu6502, _bus: &mut Bus, data: &AddressingData) -> bool {
                if cpu.get(Flags::$flag) == $value {
                    cpu.cycles += 1;

                    let address = cpu.pc.wrapping_add(data.address);
                    if (address & 0xFF00) != (cpu.pc & 0xFF00) {
                        cpu.cycles += 1;
                    }

                    cpu.pc = address;
                }

                false
            }
        )+
    };
}

branch_impl! {
    /// Branch if Carry Clear
    ///
    /// Function: if(C == 0) pc = address
    fn bcc(C == 0)

    /// Branch if Carry Set
    ///
    /// Function: if(C == 1) pc = address
    fn bcs(C == 1)

    /// Branch if Equal
    ///
    /// Function: if(Z == 1) pc = address
    fn beq(Z == 1)

    /// Branch if Not Equal
    ///
    /// Function: if(Z == 0) pc = address
    fn bne(Z == 0)

    /// Branch if Negative
    ///
    /// Function: if(N == 1) pc = address
    fn bmi(N == 1)

    /// Branch if Positive
    ///
    /// Function: if(N == 0) pc = address
    fn bpl(N == 0)

    /// Branch if Overflow set
    ///
    /// Function: if(V == 1) pc = address
    fn bvs(V == 1)

    /// Branch if Overflow clear
    ///
    /// Function: if(V == 0) pc = address
    fn bvc(V == 0)
}

/// Bit Test
///
/// Flags out: N, Z, V
pub(super) fn bit(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    let value = cpu.a & data.fetch(bus);
    set_flags!(cpu, value, NZ);
    cpu.set(Flags::V, value & 0x40 > 0);

    false
}

/// Clear Carry Flag
///
/// Function: C = 0
pub(super) fn clc(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.set(Flags::C, false);
    false
}

/// Clear Decimal Flag
///
/// Function:    D = 0
pub(super) fn cld(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.set(Flags::D, false);
    false
}

/// Disable Interrupts / Clear Interrupt Flag
///
/// Function: I = 0
pub(super) fn cli(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.set(Flags::I, false);
    false
}

/// Clear Overflow Flag
///
/// Function: V = 0
pub(super) fn clv(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.set(Flags::V, false);
    false
}

/// Set Carry Flag
///
/// Function:    C = 1
pub(super) fn sec(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.set(Flags::C, true);
    false
}

/// Set Decimal Flag
///
/// Function:    D = 1
pub(super) fn sed(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.set(Flags::D, true);
    false
}

/// Set Interrupt Flag / Enable Interrupts
///
/// Function:    I = 1
pub(super) fn sei(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.set(Flags::I, true);
    false
}

/// Compare Accumulator
///
/// Function: C := A >= M      Z := (A - M) == 0
///
/// Flags out: N, C, Z
pub(super) fn cmp(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    let fetched = data.fetch(bus);
    let value = cpu.a as u16 - fetched as u16;

    cpu.set(Flags::C, cpu.a > fetched);
    set_flags!(cpu, value, NZ);
    true
}

/// Compare X Register
///
/// Function: C := X >= M      Z := (X - M) == 0
///
/// Flags out: N, C, Z
pub(super) fn cpx(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    let fetched = data.fetch(bus);
    let value = cpu.x as u16 - fetched as u16;

    cpu.set(Flags::C, cpu.x > fetched);
    set_flags!(cpu, value, NZ);
    true
}

/// Compare Y Register
///
/// Function: C := Y >= M      Z := (Y - M) == 0
///
/// Flags out: N, C, Z
pub(super) fn cpy(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    let fetched = data.fetch(bus);
    let value = cpu.y as u16 - fetched as u16;

    cpu.set(Flags::C, cpu.y > fetched);
    set_flags!(cpu, value, NZ);
    true
}

/// Decrement Value at Memory Location
///
/// Function: M = M - 1
///
/// Flags out: N, Z
pub(super) fn dec(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    let value = data.fetch(bus) - 1;
    bus.write(data.address, value);
    set_flags!(cpu, value, NZ);
    false
}

/// Decrement X Register
///
/// Function: X = X - 1
///
/// Flags out: N, Z
pub(super) fn dex(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.x -= 1;
    set_flags!(cpu, cpu.x, NZ);
    false
}

/// Decrement Y Register
///
/// Function: Y = Y - 1
///
/// Flags out: N, Z
pub(super) fn dey(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.y -= 1;
    set_flags!(cpu, cpu.y, NZ);
    false
}

/// Increment Value at Memory Location
///
/// Function: M = M + 1
///
/// Flags out: N, Z
pub(super) fn inc(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    let value = data.fetch(bus) + 1;
    bus.write(data.address, value);
    set_flags!(cpu, value, NZ);
    false
}

/// Increment X Register
///
/// Function: X = X + 1
///
/// Flags out: N, Z
pub(super) fn inx(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.x += 1;
    set_flags!(cpu, cpu.x, NZ);
    false
}

/// Increment Y Register
///
/// Function: Y = Y + 1
///
/// Flags out: N, Z
pub(super) fn iny(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.y += 1;
    set_flags!(cpu, cpu.y, NZ);
    false
}

/// Jump To Location
///
/// Function: pc = address
pub(super) fn jmp(cpu: &mut Cpu6502, _bus: &mut Bus, data: &AddressingData) -> bool {
    cpu.pc = data.address;
    false
}

/// Jump To Sub-Routine
///
/// Function: Push current pc to stack, pc = address
pub(super) fn jsr(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    cpu.pc -= 1;
    cpu.push_u16(bus, cpu.pc);
    cpu.pc = data.address;

    false
}

/// Load The Accumulator
///
/// Function: A = M
///
/// Flags out: N, Z
pub(super) fn lda(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    cpu.a = data.fetch(bus);
    set_flags!(cpu, cpu.a, NZ);
    true
}

/// Load The X Register
///
/// Function: X = M
///
/// Flags out: N, Z
pub(super) fn ldx(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    cpu.x = data.fetch(bus);
    set_flags!(cpu, cpu.x, NZ);
    true
}

/// Load The Y Register
///
/// Function: Y = M
///
/// Flags out: N, Z
pub(super) fn ldy(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    cpu.y = data.fetch(bus);
    set_flags!(cpu, cpu.y, NZ);
    true
}

/// No Operation
pub(super) fn nop(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    match cpu.opcode {
        0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => true,
        _ => false,
    }
}

/// Push Accumulator to Stack
///
/// Function: A -> stack
pub(super) fn pha(cpu: &mut Cpu6502, bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.push(bus, cpu.a);
    false
}

/// Pop Accumulator off Stack
///
/// Function: A <- stack
/// Flags out: N, Z
pub(super) fn pla(cpu: &mut Cpu6502, bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.a = cpu.pop(bus);
    set_flags!(cpu, cpu.a, NZ);
    false
}

/// Push Status Register to Stack
///
/// Function: status -> stack
///
/// Note: Break flag is set to 1 before push
pub(super) fn php(cpu: &mut Cpu6502, bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.push(bus, (cpu.status | Flags::B | Flags::U).bits);
    cpu.set(Flags::B | Flags::U, false);
    false
}

/// Pop Status Register off Stack
///
/// Function: Status <- stack
pub(super) fn plp(cpu: &mut Cpu6502, bus: &mut Bus, _data: &AddressingData) -> bool {
    // unwrap is ok because we declare all 8 bit
    cpu.status = Flags::from_bits(cpu.pop(bus)).unwrap();

    cpu.set(Flags::U, true);

    false
}

/// Return from Interrupt
///
/// Function: Pulls the processor flags from the stack followed by the program counter
pub(super) fn rti(cpu: &mut Cpu6502, bus: &mut Bus, _data: &AddressingData) -> bool {
    // unwrap is ok because we declare all 8 bit
    cpu.status = Flags::from_bits(cpu.pop(bus)).unwrap();
    cpu.set(Flags::B | Flags::U, false);

    cpu.pc = cpu.pop_u16(bus);

    false
}

/// Return from Subroutine
///
/// Function: Pulls the program counter (minus one) from the stack
pub(super) fn rts(cpu: &mut Cpu6502, bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.pc = cpu.pop_u16(bus);
    cpu.pc += 1;

    false
}

/// Store Accumulator at Address
///
/// Function: M = A
pub(super) fn sta(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    bus.write(data.address, cpu.a);
    false
}

/// Store X Register at Address
///
/// Function: M = X
pub(super) fn stx(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    bus.write(data.address, cpu.x);
    false
}

/// Store Y Register at Address
///
/// Function: M = Y
pub(super) fn sty(cpu: &mut Cpu6502, bus: &mut Bus, data: &AddressingData) -> bool {
    bus.write(data.address, cpu.y);
    false
}

/// Transfer Accumulator to X Register
///
/// Function: X = A
///
/// Flags out: N, Z
pub(super) fn tax(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.x = cpu.a;
    set_flags!(cpu, cpu.x, NZ);
    false
}

/// Transfer Accumulator to Y Register
///
/// Function: Y = A
///
/// Flags out: N, Z
pub(super) fn tay(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.y = cpu.a;
    set_flags!(cpu, cpu.y, NZ);
    false
}

/// Transfer X Register to Accumulator
///
/// Function: A = X
///
/// Flags out: N, Z
pub(super) fn txa(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.a = cpu.x;
    set_flags!(cpu, cpu.a, NZ);
    false
}

/// Transfer Y Register to Accumulator
///
/// Function: A = Y
///
/// Flags out: N, Z
pub(super) fn tya(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.a = cpu.y;
    set_flags!(cpu, cpu.a, NZ);
    false
}

/// Transfer Stack Pointer to X Register
///
/// Function: X = stack pointer
///
/// Flags out: N, Z
pub(super) fn tsx(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.x = cpu.sp;
    set_flags!(cpu, cpu.x, NZ);
    false
}

/// Transfer X Register to Stack Pointer
///
/// Function: stack pointer = X
pub(super) fn txs(cpu: &mut Cpu6502, _bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.sp = cpu.x;
    false
}

/// Break
///
/// Function: Program Sourced Interrupt
pub(super) fn brk(cpu: &mut Cpu6502, bus: &mut Bus, _data: &AddressingData) -> bool {
    cpu.pc += 1;

    cpu.set(Flags::I, true);
    cpu.push_u16(bus, cpu.pc);

    cpu.set(Flags::B, true);
    cpu.push(bus, cpu.status.bits);
    cpu.set(Flags::B, false);

    cpu.pc = bus.read(0xFFFE, false) as u16;
    cpu.pc |= (bus.read(0xFFFF, false) as u16) << 8;

    false
}

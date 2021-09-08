use std::collections::BTreeMap;

use eframe::{
    egui::{self, Color32, FontDefinitions, FontFamily, Frame, Key, Rect, TextStyle, TextureId, Vec2},
    epi,
};

use crate::nes::{Cpu6502, Flags, NesEmulator};

const NES_WIDTH: usize = 256;
const NES_HEIGHT: usize = 240;

pub struct App {
    visual_ram_offset: u16,
    nes: NesEmulator,
    map_asm: Option<BTreeMap<u16, String>>,
    pixels_tex_id: Option<TextureId>,
    pixels: Vec<Color32>,
}

impl Default for App {
    fn default() -> Self {
        let mut app = Self {
            visual_ram_offset: 0x8000,
            nes: NesEmulator::new(),
            map_asm: None,
            pixels_tex_id: None,
            pixels: vec![Color32::LIGHT_BLUE; NES_WIDTH * NES_HEIGHT],
        };

        // Paint something temporarily so we know the render is working
        for y in 0..NES_HEIGHT {
            app.pixels[y + y * NES_WIDTH] = Color32::RED;
        }

        // Write test program
        use std::io::Write;
        app.nes.bus.ram[0x8000..]
            .as_mut()
            .write(&[
                0xA2, 0x0A, 0x8E, 0x00, 0x00, 0xA2, 0x03, 0x8E, 0x01, 0x00, 0xAC, 0x00, 0x00, 0xA9, 0x00, 0x18, 0x6D, 0x01, 0x00, 0x88,
                0xD0, 0xFA, 0x8D, 0x02, 0x00, 0xEA, 0xEA, 0xEA,
            ])
            .unwrap();

        // Set Reset Vector
        app.nes.bus.ram[0xFFFC] = 0x00;
        app.nes.bus.ram[0xFFFD] = 0x80;

        app.map_asm = Some(Cpu6502::disassemble(0x4000..=0x8FFF, &mut app.nes.bus));
        app.nes.reset();

        app
    }
}

impl epi::App for App {
    fn name(&self) -> &str {
        "NES Emulator"
    }

    fn setup(&mut self, ctx: &egui::CtxRef, _frame: &mut epi::Frame<'_>, _storage: Option<&dyn epi::Storage>) {
        let mut fonts = FontDefinitions::default();
        fonts.family_and_size.insert(TextStyle::Body, (FontFamily::Monospace, 16.0));
        ctx.set_fonts(fonts);
    }

    /// Called each time the UI needs repainting, which may be many times per second.
    fn update(&mut self, ctx: &egui::CtxRef, frame: &mut epi::Frame<'_>) {
        if ctx.input().key_pressed(Key::Space) {
            self.nes.clock();
        }

        if ctx.input().key_pressed(Key::R) {
            self.nes.reset();
        }

        if ctx.input().key_pressed(Key::I) {
            self.nes.irq();
        }

        if ctx.input().key_pressed(Key::N) {
            self.nes.nmi();
        }

        if ctx.input().key_pressed(Key::V) {
            self.visual_ram_offset = if self.visual_ram_offset == 0x0000 { 0x8000 } else { 0x0000 };
        }

        egui::SidePanel::right("Info")
            .frame(Frame::none().margin((0.0, 0.0)))
            .width_range(500.0..=2048.0)
            .resizable(true)
            .show(ctx, |ui| {
                #[cfg_attr(rustfmt, rustfmt::skip)]
                ui.horizontal(|ui| {
                    ui.colored_label(Color32::WHITE, "STATUS:");
                    ui.colored_label(if self.nes.cpu_flag(Flags::N) { Color32::GREEN } else { Color32::RED }, "N");
                    ui.colored_label(if self.nes.cpu_flag(Flags::V) { Color32::GREEN } else { Color32::RED }, "V");
                    ui.colored_label(if self.nes.cpu_flag(Flags::U) { Color32::GREEN } else { Color32::RED }, "-");
                    ui.colored_label(if self.nes.cpu_flag(Flags::B) { Color32::GREEN } else { Color32::RED }, "B");
                    ui.colored_label(if self.nes.cpu_flag(Flags::D) { Color32::GREEN } else { Color32::RED }, "D");
                    ui.colored_label(if self.nes.cpu_flag(Flags::I) { Color32::GREEN } else { Color32::RED }, "I");
                    ui.colored_label(if self.nes.cpu_flag(Flags::Z) { Color32::GREEN } else { Color32::RED }, "Z");
                    ui.colored_label(if self.nes.cpu_flag(Flags::C) { Color32::GREEN } else { Color32::RED }, "C");
                });

                let cpu = self.nes.cpu();
                ui.colored_label(Color32::WHITE, &format!("PC: ${:04X}", cpu.pc()));
                ui.colored_label(Color32::WHITE, &format!("A : ${:02X} [{}]", cpu.a(), cpu.a()));
                ui.colored_label(Color32::WHITE, &format!("X : ${:02X} [{}]", cpu.x(), cpu.x()));
                ui.colored_label(Color32::WHITE, &format!("Y : ${:02X} [{}]", cpu.y(), cpu.y()));
                ui.colored_label(Color32::WHITE, &format!("SP: ${:04X}", cpu.sp()));

                if let Some(map_asm) = self.map_asm.as_ref() {
                    ui.separator();

                    map_asm
                        .range(cpu.pc().checked_sub(8).unwrap_or(0)..)
                        .take(10)
                        .for_each(|(addr, str)| {
                            ui.colored_label(if *addr == cpu.pc() { Color32::GREEN } else { Color32::WHITE }, str);
                        });
                }

                ui.separator();

                use std::fmt::Write;
                let mut addr = self.visual_ram_offset;
                for _ in 0..16 {
                    let mut str = format!("${:04X}:", addr);
                    for _ in 0..16 {
                        write!(&mut str, " {:02X}", self.nes.bus.read(addr, true)).unwrap();
                        addr += 1;
                    }
                    ui.label(str);
                }

                ui.separator();

                ui.colored_label(
                    Color32::WHITE,
                    "SPACE = Step Instruction  R = RESET  I = IRQ  N = NMI\nV = Change Visual Ram Range",
                )
            });

        egui::CentralPanel::default()
            .frame(Frame::none().margin((0.0, 0.0)))
            .show(ctx, |ui| {
                // Allocate a new texture every time we repaint
                let tex_allocator = frame.tex_allocator();
                if let Some(tex_id) = self.pixels_tex_id {
                    tex_allocator.free(tex_id);
                }

                let tex_id = tex_allocator.alloc_srgba_premultiplied((NES_WIDTH, NES_HEIGHT), &self.pixels);

                ui.image(tex_id, optimal_emulator_size(&ctx.available_rect()));

                self.pixels_tex_id = Some(tex_id);
            });
    }
}

/// Get the optimal size for the emulator that respect the NES ratio
fn optimal_emulator_size(rect: &Rect) -> Vec2 {
    let width = rect.width();
    let height = rect.height();
    let ratio = NES_WIDTH as f32 / NES_HEIGHT as f32;

    if width > height * ratio {
        (height * ratio, height).into()
    } else {
        (width, width / ratio).into()
    }
}

use eframe::{
    egui::{self, Color32, Frame, Rect, TextureId, Vec2},
    epi,
};

const NES_WIDTH: usize = 256;
const NES_HEIGHT: usize = 240;

pub struct App {
    pixels_tex_id: Option<TextureId>,
    pixels: Vec<Color32>,
}

impl Default for App {
    fn default() -> Self {
        let mut app = Self {
            pixels_tex_id: None,
            pixels: vec![Color32::LIGHT_BLUE; NES_WIDTH * NES_HEIGHT],
        };

        // Paint something temporarily so we know the render is working
        for y in 0..NES_HEIGHT {
            app.pixels[y + y * NES_WIDTH] = Color32::RED;
        }

        app
    }
}

impl epi::App for App {
    fn name(&self) -> &str {
        "NES Emulator"
    }

    /// Called each time the UI needs repainting, which may be many times per second.
    fn update(&mut self, ctx: &egui::CtxRef, frame: &mut epi::Frame<'_>) {
        egui::SidePanel::right("Info")
            .frame(Frame::none().margin((0.0, 0.0)))
            .resizable(true)
            .show(ctx, |ui| {
                ui.heading("Register: ");
            });

        egui::CentralPanel::default()
            .frame(Frame::none().margin((0.0, 0.0)))
            .show(ctx, |ui| {
                // Allocate a new texture every time we repaint
                let tex_allocator = frame.tex_allocator();
                if let Some(tex_id) = self.pixels_tex_id {
                    tex_allocator.free(tex_id);
                }

                let tex_id =
                    tex_allocator.alloc_srgba_premultiplied((NES_WIDTH, NES_HEIGHT), &self.pixels);

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

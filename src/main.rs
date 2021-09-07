#![cfg_attr(not(debug_assertions), deny(warnings))]
#![warn(clippy::all, rust_2018_idioms)]

// When compiling natively
#[cfg(not(target_arch = "wasm32"))]
fn main() {
    let app = nes::App::default();
    let native_options = eframe::NativeOptions {
        always_on_top: false,
        decorated: true,
        drag_and_drop_support: false,
        icon_data: None,
        initial_window_size: Some((1280.0, 720.0).into()),
        resizable: true,
        transparent: false,
    };
    eframe::run_native(Box::new(app), native_options);
}

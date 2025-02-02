
use egui::{
    text::CCursorRange, Key, KeyboardShortcut, Modifiers, ScrollArea, TextBuffer, TextEdit, Ui,
};
use egui_code_editor::{CodeEditor, ColorTheme, Syntax};


pub struct ObfuscatorApp {
    code: String,
    obfuscated_code: String,
    control_flow: bool,
    dead_code: bool,
    numeric: bool,
    remove_comments: bool,
    renaming: bool,
    string: bool,
}




impl Default for ObfuscatorApp {
    fn default() -> Self {
        Self {
            code: r#"
function hello_str() {
    let str = "hello world";
    return str;
}

function fn2() {
    function hello() {
        console.log("hello again");
        console.log("hello again");
        console.log("hello again");
    }

    let loop_v = 5;

    while (loop_v) {
        console.log("loop it:", loop_v % 5);
        loop_v -= 1;
    }

    console.log("firstline");
    console.log("secondline");
    let num = 5;
    console.log("thirdline", num);
    hello();
}
console.log(hello_str());
fn2();
                "#.to_owned(),
            obfuscated_code: r#"
function hello_str() {
    let str = "hello world";
    return str;
}

function fn2() {
    function hello() {
        console.log("hello again");
        console.log("hello again");
        console.log("hello again");
    }

    let loop_v = 5;

    while (loop_v) {
        console.log("loop it:", loop_v % 5);
        loop_v -= 1;
    }

    console.log("firstline");
    console.log("secondline");
    let num = 5;
    console.log("thirdline", num);
    hello();
}
console.log(hello_str());
fn2();
                "#.to_owned(),

            control_flow: false,
            dead_code: false,
            numeric: false,
            remove_comments: false,
            renaming: false,
            string: false,

        }
    }
}

impl eframe::App for ObfuscatorApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.panels(ctx);
    } 
}


impl ObfuscatorApp {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        // This is also where you can customize the look and feel of egui using
        // `cc.egui_ctx.set_visuals` and `cc.egui_ctx.set_fonts`.

        Default::default()
    }

    fn panels(&mut self, ctx: &egui::Context) {
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.checkbox(&mut self.control_flow, "Control Flow Obfuscation");
                ui.checkbox(&mut self.dead_code, "Dead Code Injection");
                ui.checkbox(&mut self.numeric, "Numeric Obfuscation");
                ui.checkbox(&mut self.remove_comments, "Comment Remover");
                ui.checkbox(&mut self.renaming, "Renamer");
                ui.checkbox(&mut self.string, "String Encoding");
                if ui.button("Obfuscate!").clicked() { 
                    self.obfuscate_code();
                }
            });

        });

        egui::CentralPanel::default().show(ctx, |ui| {
            self.ui(ui, ctx);

        });

        egui::TopBottomPanel::bottom("Bottom Panel").show(ctx, |ui| { 
            ui.columns(1, |cols| { 
                cols[0].vertical_centered(|ui| { 
                    ui.add(egui::github_link_file!(
                        "https://github.com/Calastrophe/hide-my-js",
                        "Source code."
                    ));
                });
            });
        });
    }

    fn ui(&mut self, ui: &mut egui::Ui, ctx: &egui::Context) {
        ui.columns(2, |columns| { 
            let width = ctx.input(|i: &egui::InputState| i.screen_rect()).width();
            columns[1].set_max_width(width/5.0); //set width to 1/5th of windows size
            // i have no idea why this doesnt work
            ScrollArea::vertical()
                .id_salt("source")
                .show(&mut columns[0], |ui| self.editor_ui(ui));

            ScrollArea::vertical()
                .id_salt("obfs")
                .show(&mut columns[1], |ui| self.obfuscated_code_view(ui));
        })
    }

    fn editor_ui(&mut self, ui: &mut egui::Ui) {
        CodeEditor::default()
            .id_source("code editor")
            .with_rows(12)
            .with_fontsize(15.0)
            .with_theme(ColorTheme::GRUVBOX)
            .with_syntax(Syntax::rust())
            .with_numlines(false)
            .stick_to_bottom(true)
            .show(ui, &mut self.code);

    }

    fn obfuscated_code_view(&mut self, ui: &mut egui::Ui) {
        CodeEditor::default()
            .id_source("obfs code")
            .with_rows(12)
            .with_fontsize(15.0)
            .with_theme(ColorTheme::GRUVBOX)
            .with_syntax(Syntax::rust())
            .with_numlines(false)
            .stick_to_bottom(true)
            .show(ui, &mut self.obfuscated_code);

    }
    // possibly move this function into the obfuscator codebase out of the frontend so that oxc is not required as as dep 
    fn obfuscate_code(&mut self) { 
        self.obfuscated_code = hide_my_js::obfuscate_code(
            self.code.clone(),
            self.control_flow,
            self.dead_code,
            self.numeric,
            self.remove_comments,
            self.renaming,
            self.string,
        )
    }
}

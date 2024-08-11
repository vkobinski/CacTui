use std::{
    collections::HashMap,
    default,
    fmt::Display,
    io::{stdout, Result},
    os::linux::raw::stat,
};

use ratatui::{
    crossterm::{
        event::{self, KeyCode, KeyEvent, KeyEventKind},
        terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
        ExecutableCommand,
    },
    layout::{Alignment, Constraint, Rect},
    prelude::CrosstermBackend,
    style::{self, Modifier, Style, Stylize},
    widgets::{self, Block, Cell as RatCell, Padding, Row},
    Frame, Terminal,
};

mod calc;

enum CellValue {
    Empty,
    Number(f64),
    Text(String),
    Date(chrono::NaiveDate),
    Formula(String),
    Error(String),
}

struct CellFormat {}

struct Cell {
    val: CellValue,
    format: CellFormat,
}

#[derive(Default)]
enum VimState {
    #[default]
    Normal,
    Insert,
    Visual,
    Command(String),
    Exit,
}

impl Display for VimState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            VimState::Normal => "NORMAL",
            VimState::Insert => "INSERT",
            VimState::Visual => "VISUAL",
            VimState::Command(com) => com,
            VimState::Exit => "EXIT",
        })
    }
}

impl VimState {
    fn style(&self) -> Style {
        match self {
            VimState::Normal => Style::new().black().on_green(),
            VimState::Insert => Style::new().black().on_red(),
            VimState::Visual => Style::new().black().on_magenta(),
            VimState::Command(_) => Style::new().black().on_cyan(),
            VimState::Exit => Style::new().black().on_white(),
        }
    }
}

struct State {
    selection: (usize, usize),
    vim: VimState,
    last: Option<char>,
    hor_cells: usize,
    ver_cells: usize,
}

impl State {
    fn new() -> Self {
        let hor_cells = usize::from(FRAME_WIDTH / CELL_X);
        let ver_cells = usize::from(FRAME_HEIGHT / CELL_Y);

        Self {
            selection: (0, 0),
            vim: VimState::Normal,
            last: None,
            hor_cells,
            ver_cells,
        }
    }
}

type Sheet = HashMap<(usize, usize), Cell>;

const CELL_X: u16 = 30;
const CELL_Y: u16 = 20;
const FRAME_WIDTH: u16 = 500;
const FRAME_HEIGHT: u16 = 800;

fn render_text(cell: &Cell) -> String {
    match &cell.val {
        CellValue::Empty => String::from(""),
        CellValue::Number(num) => num.to_string(),
        CellValue::Text(text) => text.to_string(),
        CellValue::Date(date) => date.to_string(),
        CellValue::Formula(_) => todo!(),
        CellValue::Error(err) => String::from("#"),
    }
}

fn render_cell(cell: &Cell) -> RatCell {
    RatCell::new(render_text(cell))
}

fn draw_cells(frame: &mut Frame, cells: &Sheet, state: &State) {
    let mut rows: Vec<Row> = Vec::new();

    let sel = state.selection;

    let hor_cells = state.hor_cells;
    let ver_cells = state.ver_cells;

    for y in 0..ver_cells {
        let mut row_cells: Vec<RatCell> = Vec::new();
        for x in 0..hor_cells {
            let mut is_sel = false;
            let style = match (x, y) {
                (x, y) if x == sel.0 && y == sel.1 => {
                    is_sel = true;
                    Style::new().white().on_cyan()
                }
                _ => Style::new().black().on_white(),
            };
            draw_selection(frame, "");

            let cell = cells.get(&(x, y)).map_or_else(
                || RatCell::new("").style(style),
                |val| {
                    if is_sel {
                        draw_selection(frame, &render_text(val));
                    }
                    render_cell(val).style(style)
                },
            );

            row_cells.push(cell);
        }

        rows.push(Row::new(row_cells));
    }

    // TODO: This could be used to make the columns size editable
    let mut contraints: Vec<Constraint> = vec![];
    for _ in 0..hor_cells {
        contraints.push(Constraint::Length(ver_cells.try_into().unwrap()));
    }

    let table = widgets::Table::new(rows, contraints).block(Block::new().padding(Padding::left(1)));

    let wid = frame.size().width;
    let hei = frame.size().height;

    frame.render_widget(table, Rect::new(0, 1, wid, hei));
    draw_vim_state(frame, state);
}

fn draw_vim_state(frame: &mut Frame, state: &State) {
    let widget = ratatui::widgets::Paragraph::new(state.vim.to_string())
        .style(state.vim.style())
        .block(Block::new().padding(Padding::left(1)));

    let hei = frame.size().height;
    let wid = frame.size().width;

    frame.render_widget(widget, Rect::new(0, hei - 1, wid, 1));

    if let Some(last) = state.last {
        let widget = ratatui::widgets::Paragraph::new(last.to_string())
            .alignment(Alignment::Right)
            .block(Block::new().padding(Padding::right(5)));

        let wid = frame.size().width;

        frame.render_widget(widget, Rect::new(0, hei - 1, wid, 1));
    }
}

fn draw_selection(frame: &mut Frame, text: &str) {
    {
        let widget = ratatui::widgets::Paragraph::new(text)
            .style(Style::new().black().on_magenta())
            .block(Block::new().padding(Padding::left(1)));

        let wid = frame.size().width;

        frame.render_widget(widget, Rect::new(0, 0, wid, 1));
    }
}

fn handle_selection_key_press(state: &mut State, key: &KeyEvent) {
    if key.kind == KeyEventKind::Press {
        if let Some(last) = state.last {
            match last {
                'g' if key.code == KeyCode::Char('g') => state.selection.1 = 0,
                _ => state.last = None,
            }
        }

        match key.code {
            KeyCode::Right | KeyCode::Char('l') => state.selection.0 += 1,
            KeyCode::Left | KeyCode::Char('h') if (state.selection.0 > 0) => state.selection.0 -= 1,
            KeyCode::Up | KeyCode::Char('k') if (state.selection.1 > 0) => state.selection.1 -= 1,
            KeyCode::Down | KeyCode::Char('j') => state.selection.1 += 1,
            KeyCode::Char('o') if state.selection.1 < state.ver_cells - 1 => {
                state.selection.1 += 1;
                state.vim = VimState::Insert;
            }
            KeyCode::Char('O') if state.selection.1 > 0 => {
                state.selection.1 -= 1;
                state.vim = VimState::Insert;
            }
            KeyCode::Char('G') => {
                state.selection.1 = state.ver_cells - 1;
            }
            KeyCode::Char('g') => {
                state.last = Some('g');
            }
            KeyCode::Char(':') => {
                state.vim = VimState::Command(':'.to_string());
            }
            _ => {}
        }
    }
}

fn execute_command(command: String, state: &mut State) {
    match command.as_str() {
        ":q" => state.vim = VimState::Exit,
        _ => state.vim = VimState::Command("Command not recognized".to_string()),
    }
}

fn main() -> Result<()> {
    stdout().execute(EnterAlternateScreen)?;
    enable_raw_mode()?;

    let mut example_sheet = Sheet::default();
    example_sheet.insert(
        (1, 0),
        Cell {
            val: CellValue::Number(10.0),
            format: CellFormat {},
        },
    );

    example_sheet.insert(
        (0, 1),
        Cell {
            val: CellValue::Number(20.0),
            format: CellFormat {},
        },
    );

    let mut terminal = Terminal::new(CrosstermBackend::new(stdout()))?;
    terminal.clear()?;

    let mut state = State::new();

    loop {
        terminal.draw(|frame| {
            draw_cells(frame, &example_sheet, &state);
        })?;

        if event::poll(std::time::Duration::from_millis(16))? {
            if let event::Event::Key(key) = event::read()? {
                if key.kind == KeyEventKind::Press {
                    match state.vim {
                        VimState::Normal if key.kind == KeyEventKind::Press => {
                            if key.code == KeyCode::Char('q') {
                                break;
                            }
                            if let KeyCode::Char('i') = key.code {
                                state.vim = match state.vim {
                                    VimState::Normal => VimState::Insert,
                                    _ => state.vim,
                                };
                            }

                            handle_selection_key_press(&mut state, &key);
                        }
                        VimState::Insert => match key.code {
                            KeyCode::Esc => {
                                state.vim = VimState::Normal;
                            }
                            KeyCode::Backspace => {
                                let cell = example_sheet.get(&state.selection).map_or_else(
                                    || CellValue::Empty,
                                    |cell_val| match &cell_val.val {
                                        CellValue::Empty => CellValue::Empty,
                                        CellValue::Number(num) => {
                                            let old_num = num.to_string();
                                            CellValue::Number(
                                                old_num[0..old_num.len() - 1].parse().unwrap(),
                                            )
                                        }
                                        CellValue::Text(old) => {
                                            CellValue::Text(old[0..old.len() - 1].to_string())
                                        }
                                        CellValue::Date(_) => todo!(),
                                        CellValue::Formula(_) => todo!(),
                                        CellValue::Error(_) => todo!(),
                                    },
                                );

                                example_sheet.insert(
                                    state.selection,
                                    Cell {
                                        val: cell,
                                        format: CellFormat {},
                                    },
                                );
                            }
                            KeyCode::Char(c) => {
                                let cell = example_sheet.get(&state.selection).map_or_else(
                                    || CellValue::Text(c.to_string()),
                                    |cell_val| match &cell_val.val {
                                        CellValue::Empty => CellValue::Text(c.to_string()),
                                        CellValue::Number(num) => {
                                            let old_num = num.to_string();
                                            let last_digit = c.to_string().parse::<f64>().unwrap();
                                            CellValue::Number(
                                                format!("{}{}", old_num, last_digit)
                                                    .parse()
                                                    .unwrap(),
                                            )
                                        }
                                        CellValue::Text(old) => {
                                            CellValue::Text(format!("{}{}", old, c))
                                        }
                                        CellValue::Date(_) => todo!(),
                                        CellValue::Formula(_) => todo!(),
                                        CellValue::Error(_) => todo!(),
                                    },
                                );

                                example_sheet.insert(
                                    state.selection,
                                    Cell {
                                        val: cell,
                                        format: CellFormat {},
                                    },
                                );
                            }
                            _ => {}
                        },
                        VimState::Visual => todo!(),
                        VimState::Command(ref till) => match key.code {
                            KeyCode::Enter => execute_command(till.clone(), &mut state),
                            KeyCode::Esc => state.vim = VimState::Normal,
                            KeyCode::Char(c) => {
                                state.vim = VimState::Command(format!("{}{}", till, c));
                            }
                            _ => {}
                        },
                        VimState::Exit => {
                            break;
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    stdout().execute(LeaveAlternateScreen)?;
    disable_raw_mode()?;
    Ok(())
}

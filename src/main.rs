use std::{
    collections::HashMap,
    fs::File,
    io::{stdout, BufReader, Result},
    str::FromStr,
};

use ratatui::{
    crossterm::{
        event::{self, KeyCode, KeyEventKind},
        style::StyledContent,
        terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
        ExecutableCommand,
    },
    layout::{Constraint, Rect},
    prelude::CrosstermBackend,
    style::{Color, Style, Styled, Stylize},
    widgets::{self, Cell as RatCell, Paragraph, Row},
    Frame, Terminal,
};
use xml::{name::OwnedName, reader::XmlEvent, EventReader};

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

type Sheet = HashMap<(usize, usize), Cell>;

const CELL_X: u16 = 30;
const CELL_Y: u16 = 20;
const FRAME_WIDTH: u16 = 500;
const FRAME_HEIGHT: u16 = 500;

fn draw_cells(frame: &mut Frame, cells: &Sheet, state: &State) {
    let mut rows: Vec<Row> = Vec::new();

    let hor_cells = usize::from(FRAME_WIDTH / CELL_X);
    let ver_cells = usize::from(FRAME_HEIGHT / CELL_Y);

    let sel = state.selection;

    for y in 0..ver_cells {
        let mut row_cells: Vec<RatCell> = Vec::new();
        for x in 0..hor_cells {
            let style = match (x, y) {
                (x, y) if x == sel.0 && y == sel.1 => Style::new().white().on_cyan(),
                _ => Style::new().black().on_white(),
            };

            let cell = cells.get(&(x, y)).map_or_else(
                || RatCell::new("").style(style),
                |val| {
                    RatCell::new(match &val.val {
                        CellValue::Empty => String::from(""),
                        CellValue::Number(num) => num.to_string(),
                        CellValue::Text(text) => text.to_string(),
                        CellValue::Date(date) => date.to_string(),
                        CellValue::Formula(_) => todo!(),
                        CellValue::Error(err) => String::from("#"),
                    })
                    .style(style)
                },
            );

            row_cells.push(cell);
        }

        rows.push(Row::new(row_cells));
    }

    let mut contraints: Vec<Constraint> = vec![];
    for i in 0..hor_cells {
        contraints.push(Constraint::Length(ver_cells.try_into().unwrap()));
    }

    let table = widgets::Table::new(rows, contraints);

    frame.render_widget(table, Rect::new(0, 0, FRAME_WIDTH, FRAME_HEIGHT));
}

struct State {
    selection: (usize, usize),
    editing: bool,
}

fn main() -> Result<()> {
    let sheet1 = File::open("sheet1.xml")?;
    let sheet1 = BufReader::new(sheet1);

    let parser = EventReader::new(sheet1);
    let mut depth = 0;

    for e in parser {
        match e {
            Ok(XmlEvent::StartElement {
                name, attributes, ..
            }) => {
                if name.local_name == "row" {}
                println!(
                    "{:spaces$}+{name:#?} {attributes:#?}",
                    "",
                    spaces = depth * 2,
                );
                depth += 1;
            }
            Ok(XmlEvent::EndElement { name }) => {
                depth -= 1;
                println!("{:spaces$}-{name}", "", spaces = depth * 2);
            }
            Ok(XmlEvent::Characters(char)) => {
                println!("{:spaces$}{char}", "", spaces = depth * 2)
            }
            Ok(XmlEvent::CData(data)) => {
                println!("{:spaces$}{data}", "", spaces = depth * 2)
            }
            Err(e) => {
                eprintln!("Error: {e}");
                break;
            }
            _ => {}
        }
    }

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

    let mut state = State {
        selection: (0, 0),
        editing: false,
    };

    loop {
        terminal.draw(|frame| {
            draw_cells(frame, &example_sheet, &state);
        })?;

        if event::poll(std::time::Duration::from_millis(16))? {
            if let event::Event::Key(key) = event::read()? {
                if !state.editing {
                    if key.kind == KeyEventKind::Press && key.code == KeyCode::Char('q') {
                        break;
                    }

                    if key.kind == KeyEventKind::Press && key.code == KeyCode::Right {
                        state.selection.0 += 1;
                    }
                    if key.kind == KeyEventKind::Press
                        && key.code == KeyCode::Left
                        && state.selection.0 > 0
                    {
                        state.selection.0 -= 1;
                    }
                    if key.kind == KeyEventKind::Press
                        && key.code == KeyCode::Up
                        && state.selection.1 > 0
                    {
                        state.selection.1 -= 1;
                    }
                    if key.kind == KeyEventKind::Press && key.code == KeyCode::Down {
                        state.selection.1 += 1;
                    }
                } else if key.kind == KeyEventKind::Press {
                    if let KeyCode::Backspace = key.code {
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

                    if let KeyCode::Char(c) = key.code {
                        let cell = example_sheet.get(&state.selection).map_or_else(
                            || CellValue::Text(c.to_string()),
                            |cell_val| match &cell_val.val {
                                CellValue::Empty => CellValue::Text(c.to_string()),
                                CellValue::Number(num) => {
                                    let old_num = num.to_string();
                                    let last_digit = c.to_string().parse::<f64>().unwrap();
                                    CellValue::Number(
                                        format!("{}{}", old_num, last_digit).parse().unwrap(),
                                    )
                                }
                                CellValue::Text(old) => CellValue::Text(format!("{}{}", old, c)),
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
                    };
                }

                if key.kind == KeyEventKind::Press && key.code == KeyCode::Enter {
                    state.editing = !state.editing;
                }
            }
        }
    }

    stdout().execute(LeaveAlternateScreen)?;
    disable_raw_mode()?;
    Ok(())
}

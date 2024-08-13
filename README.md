# CalcTui 🦀

CalcTui is a terminal-based Excel clone written in Rust. It leverages vim motions for efficient navigation and editing, providing a powerful and familiar interface for users who are comfortable with vim.

## Features


- **Calc Language**: A domain-specific language to run Excel-like expressions.
- **Vim-like Navigation**: Navigate through cells using vim motions.
- **Multiple Modes**: Switch between Normal, Insert, Visual, Command, and Exit modes.
- **Terminal-Based**: Run entirely in the terminal, making it lightweight and fast.

<img src="./demo.gif?raw=true" width="800"/>

## Installation

To install CalcTui, you need to have Rust and Cargo installed on your system. You can install Rust using `rustup`:

```sh
$ curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

Once Rust is installed, clone the repository and build the project:

```sh
$ git clone https://github.com/vkobinski/CalcTui.git
$ cd CalcTui
$ cargo build --release
```

## Usage

To run CalcTui, use the following command:

```sh
$ cargo run --release
```

### Modes

## The Calc Language

CalcTui includes a domain-specific language called Calc Language for running Excel-like expressions. This language allows you to perform calculations directly within cells using familiar syntax.

### Examples

- **Basic Arithmetic**: `=A1 + B2`
- **Functions**: `=SUM(A1:A10)`
- **Conditional Expressions**: `=IF(A1 > 10, "High", "Low")`
- **Range**: `=A1:A10`, get values from A1 to A10

The Calc Language supports a variety of functions and operations, making it a powerful tool for data manipulation and analysis within the terminal.

- **Normal Mode**: Default mode for navigation.
- **Insert Mode**: Mode for editing cell values.
- **Visual Mode**: Mode for selecting multiple cells.
- **Command Mode**: Mode for executing commands.
- **Exit Mode**: Mode to exit the application.

### Key Bindings

- **h, j, k, l**: Move left, down, up, and right respectively.
- **i**: Enter Insert mode.
- **v**: Enter Visual mode.
- **:**: Enter Command mode.

## Contributing

Contributions are welcome! Please fork the repository and submit a pull request for any features, bug fixes, or improvements.

## License

This project is licensed under the MIT License. See the LICENSE file for details.

## Acknowledgements

- Inspired by vim and its powerful navigation capabilities.
- Built with Rust for performance and safety.

Enjoy using CalcTui!
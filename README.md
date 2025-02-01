# Welcome hid-my-js

We protect your intellectual property because let's face it, no one is smarter than you.

## Prerequisites to Build

### Install Rust

We are using Cargo as the package manager, which is installed by default with Rust.

You can reference the official docs [here](https://www.rust-lang.org/tools/install) or follow the commands below.

For Linux machines:

```shell
curl https://sh.rustup.rs -sSf | sh
rustup default nightly
```

For Windows machines:

```powershell
Invoke-WebRequest -Uri https://sh.rustup.rs -OutFile rustup-init.exe; ./rustup-init.exe
rustup default nightly
```

#### add to path

```shell
export PATH="$HOME/.cargo/bin:$PATH"
```


### Verify Rust Install

Ensure you are on the latest version: `rustc 1.86.0-nightly`.

```shell
rustc --version
```

## Pull Dependencies

```shell
cargo fetch
```

## Run Code

Navigate to your tests/ directory and run with cargo

```shell
cargo test --package hide-my-js --test renaming -- variable_renaming --exact --show-output
```

### or use vscode GUI and press play in the file. but install vscode extension

If you are using VS Code, a handy extension will be your friend. This is called `rust-lang.rust-analyzer`.

To install all recommended extensions from the `.vscode` folder, you can navigate to the extensions: marketplace and search for '@recommended'



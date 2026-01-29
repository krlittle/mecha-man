# Mecha Man

A NES game built from scratch in 6502 Assembly.

## Prerequisites

### macOS

Install the **cc65** toolchain (assembler + linker) via [Homebrew](https://brew.sh/):

```bash
brew install cc65
```

Verify the installation:

```bash
ca65 --version
ld65 --version
```

You will also need an NES emulator to run the ROM:

- [Mesen](https://www.mesen.ca/) — Recommended. Excellent debugging tools including a PPU viewer, memory inspector, and disassembler.
- [FCEUX](https://fceux.com/) — Another solid option with built-in debugging.

### Windows

Install the **cc65** toolchain using one of these methods:

**Option A: Pre-built binaries (simplest)**

1. Download the latest Windows snapshot from [cc65.github.io](https://cc65.github.io/) under "Windows Snapshot."
2. Extract the zip to a folder (e.g., `C:\cc65`).
3. Add the `bin` folder to your system PATH:
   - Search "Environment Variables" in the Start menu.
   - Under "System variables," find `Path`, click Edit, and add `C:\cc65\bin`.

**Option B: Via MSYS2**

```bash
pacman -S mingw-w64-x86_64-cc65
```

Verify the installation:

```cmd
ca65 --version
ld65 --version
```

For an NES emulator on Windows:

- [Mesen](https://www.mesen.ca/) — Recommended.
- [FCEUX](https://fceux.com/)

If you are using Windows without `make` available, see the [Building without Make](#building-without-make) section below.

## Project Structure

```
mecha-man/
├── hello.asm      # Assembly source code
├── nes.cfg        # Linker configuration (memory layout for the ROM)
├── Makefile       # Build automation
├── .gitignore
└── README.md
```

## Building the ROM

### Using Make (macOS / Linux / Windows with Make)

Build the ROM:

```bash
make
```

Clean build artifacts:

```bash
make clean
```

### Building without Make

If `make` is not available (common on Windows), run the two build steps manually:

```cmd
ca65 -o hello.o hello.asm
ld65 -o hello.nes -C nes.cfg hello.o
```

Step 1 (`ca65`) assembles the source code into an object file. Step 2 (`ld65`) links the object file into a final `.nes` ROM using the memory layout defined in `nes.cfg`.

## Running the ROM

Open `hello.nes` in any NES emulator:

**macOS:**

```bash
open hello.nes
```

This opens the file with whatever application is associated with `.nes` files. You can also drag and drop `hello.nes` into your emulator window, or open it from the emulator's File menu.

**Windows:**

Double-click `hello.nes` if you have an emulator associated with the file type, or open it from the emulator's File > Open menu.

**Expected output:** White text reading "HELLO WORLD!" centered on a black background.

## Learning Resources

- [Classic Game Programming on the NES](https://www.amazon.com/Classic-Game-Programming-NES-Modern/dp/B0DDJYKBVP) by Tony Cruise — The book this project follows alongside.
- [NESdev Wiki](https://www.nesdev.org/wiki/) — Comprehensive NES hardware and programming reference.
- [6502 Instruction Set Reference](https://www.masswerk.at/6502/6502_instruction_set.html) — Complete reference for every 6502 CPU instruction.
- [ca65 Users Guide](https://cc65.github.io/doc/ca65.html) — Documentation for the assembler.

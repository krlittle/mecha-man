# ============================================================================
# NES ROM Build System
# ============================================================================
#
# Build pipeline:
#   1. ca65 (assembler): hello.asm → hello.o (object file)
#   2. ld65 (linker):    hello.o + nes.cfg → hello.nes (final ROM)
#
# Usage:
#   make          - Build the ROM
#   make clean    - Remove build artifacts

# The assembler and linker
ASM = ca65
LINK = ld65

# Source files
SRC = hello.asm
CFG = nes.cfg

# Output files
OBJ = hello.o
ROM = hello.nes

# Build the ROM
$(ROM): $(OBJ)
	$(LINK) -o $(ROM) -C $(CFG) $(OBJ)

# Assemble the source
$(OBJ): $(SRC)
	$(ASM) -o $(OBJ) $(SRC)

# Clean up build artifacts
clean:
	rm -f $(OBJ) $(ROM)

.PHONY: clean

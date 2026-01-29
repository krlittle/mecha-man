; ============================================================================
; Hello World - NES ROM
; ============================================================================
;
; This program displays "HELLO WORLD!" on the NES screen.
;
; Key Concepts:
;   - The NES PPU (Picture Processing Unit) draws the screen.
;   - We can't draw pixels directly. Instead, we place TILE INDICES into a
;     32x30 grid called the "nametable." Each cell references an 8x8 pixel
;     tile from the CHR-ROM (pattern table).
;   - To display text, we need a font stored as tiles in CHR-ROM, and we
;     write the correct tile indices into the nametable.
;   - All PPU communication happens through memory-mapped registers at
;     $2000-$2007 on the CPU bus.
; ============================================================================

; ---------------------------------------------------------------------------
; PPU Register Definitions
; ---------------------------------------------------------------------------
; These are the memory-mapped addresses the CPU uses to talk to the PPU.
; Giving them names makes our code much more readable.

PPUCTRL   = $2000   ; PPU Control Register 1
                     ;   Bit 7: Generate NMI on VBlank (1 = enable)
                     ;   Bit 4: Background pattern table (0 = $0000, 1 = $1000)
                     ;   Bit 3: Sprite pattern table (0 = $0000, 1 = $1000)
                     ;   Bits 0-1: Base nametable address

PPUMASK   = $2001   ; PPU Control Register 2
                     ;   Bit 3: Show background (1 = enable)
                     ;   Bit 4: Show sprites (1 = enable)
                     ;   Bit 1: Show background in leftmost 8 pixels

PPUSTATUS = $2002   ; PPU Status Register (read-only)
                     ;   Bit 7: VBlank has started (1 = yes)
                     ;   Reading this register resets the address latch used
                     ;   by PPUADDR.

PPUADDR   = $2006   ; PPU Address Register (write twice: high byte, then low)
                     ;   Sets the target address in PPU memory for the next
                     ;   read/write through PPUDATA.

PPUDATA   = $2007   ; PPU Data Register
                     ;   Reads/writes one byte to PPU memory at the address
                     ;   set by PPUADDR. The address auto-increments after
                     ;   each access (by 1 or 32, controlled by PPUCTRL bit 2).

; ---------------------------------------------------------------------------
; iNES Header
; ---------------------------------------------------------------------------
; Every NES ROM begins with a 16-byte header that tells the emulator (or
; hardware) about the cartridge configuration.

.segment "HEADER"

    .byte "NES",$1A   ; Magic number: "NES" followed by MS-DOS end-of-file
                       ; This identifies the file as an iNES ROM.

    .byte $01          ; Number of 16KB PRG-ROM banks (1 = 16KB of code space)
    .byte $01          ; Number of 8KB CHR-ROM banks (1 = 8KB of graphics)

    .byte $00          ; Flags 6: Mapper 0 (NROM), horizontal mirroring
                       ;   Bit 0: Mirroring (0=horizontal, 1=vertical)
                       ;   Bits 4-7: Lower nibble of mapper number

    .byte $00          ; Flags 7: Mapper 0 (upper nibble), NES 1.0 format
    .byte $00          ; PRG-RAM size (0 = 8KB, not used here)
    .byte $00          ; Flags 9: NTSC
    .byte $00          ; Flags 10: unused
    .byte $00,$00,$00,$00,$00  ; Padding to reach 16 bytes

; ---------------------------------------------------------------------------
; Zero Page Variables
; ---------------------------------------------------------------------------
; The 6502's zero page ($0000-$00FF) is special: instructions that access
; it are 1 byte shorter and 1 cycle faster. Use it for your most
; frequently accessed variables.

.segment "ZEROPAGE"

; (No variables needed for this simple program)

; ---------------------------------------------------------------------------
; Main Code
; ---------------------------------------------------------------------------

.segment "CODE"

; ===== RESET: Entry point when the NES is powered on or reset =====
;
; When the NES powers on, the CPU reads the address stored at $FFFC-$FFFD
; (the "reset vector") and jumps there. That's our Reset label.
;
; The first thing we MUST do is a careful initialization sequence.
; The PPU takes about 30,000 CPU cycles to warm up, and accessing it
; before it's ready causes glitches. The standard approach:
;   1. Disable interrupts and decimal mode
;   2. Set up the stack pointer
;   3. Disable PPU rendering and NMIs
;   4. Wait for two VBlank periods (the PPU's warm-up time)

Reset:
    sei             ; SEt Interrupt disable flag.
                    ; Prevents IRQ interrupts from firing during init.
                    ; We don't want anything interrupting our setup.

    cld             ; CLear Decimal mode flag.
                    ; The NES's 6502 doesn't have decimal mode, but it's
                    ; good practice to explicitly disable it.

    ldx #$FF        ; LoaD X register with $FF (255).
    txs             ; Transfer X to Stack pointer.
                    ; The stack lives at $0100-$01FF and grows downward.
                    ; Setting S=$FF means the stack starts at $01FF.

    ; --- Disable PPU features during initialization ---
    lda #$00        ; LoaD Accumulator with 0.
    sta PPUCTRL     ; Disable NMI (bit 7 = 0). We'll enable it later.
    sta PPUMASK     ; Disable rendering (all bits 0). Screen is off.

    ; --- Wait for first VBlank ---
    ; VBlank is the period when the PPU is not drawing (between frames).
    ; Bit 7 of PPUSTATUS is set when VBlank begins.
WaitVBlank1:
    bit PPUSTATUS   ; BIT test - copies bit 7 of PPUSTATUS into the
                    ;   Negative flag (N). This is a neat trick:
                    ;   we don't need to load the value, just test bit 7.
    bpl WaitVBlank1 ; Branch if Plus (N=0). Keep looping until N=1,
                    ;   meaning VBlank has started.

    ; --- Clear all RAM while we wait for the second VBlank ---
    ; The NES RAM ($0000-$07FF) contains random garbage at power-on.
    ; We zero it all out for a clean state.
    lda #$00        ; Value to fill: zero
    ldx #$00        ; Loop counter, starts at 0
ClearRAM:
    sta $0000, x    ; Store zero at address $0000 + X
    sta $0100, x    ; Store zero at $0100 + X (stack page)
    sta $0200, x    ; Store zero at $0200 + X
    sta $0300, x    ; Store zero at $0300 + X
    sta $0400, x    ; Store zero at $0400 + X
    sta $0500, x    ; Store zero at $0500 + X
    sta $0600, x    ; Store zero at $0600 + X
    sta $0700, x    ; Store zero at $0700 + X
    inx             ; INcrement X. When X wraps from $FF to $00...
    bne ClearRAM    ; ...the Zero flag is set, so BNE (Branch if Not Equal
                    ;   to zero) will stop looping. This clears all 2KB.

    ; --- Wait for second VBlank ---
    ; After two VBlanks, the PPU is guaranteed to be warmed up and ready.
WaitVBlank2:
    bit PPUSTATUS
    bpl WaitVBlank2

    ; ==================================================================
    ; PPU is now ready! Let's set up our graphics.
    ; ==================================================================

    ; --- Load the color palette ---
    ; The NES has 32 bytes of palette RAM (at PPU address $3F00-$3F1F).
    ;   $3F00-$3F0F: Background palettes (4 palettes x 4 colors)
    ;   $3F10-$3F1F: Sprite palettes (4 palettes x 4 colors)
    ;
    ; To write to PPU memory, we:
    ;   1. Read PPUSTATUS (resets the address latch)
    ;   2. Write the HIGH byte of the target address to PPUADDR
    ;   3. Write the LOW byte to PPUADDR
    ;   4. Write data bytes to PPUDATA (address auto-increments)

    bit PPUSTATUS   ; Reset the address latch (important!)
    lda #$3F
    sta PPUADDR     ; High byte of $3F00
    lda #$00
    sta PPUADDR     ; Low byte of $3F00. PPU address is now $3F00.

    ; Write our palette data. We'll use a loop.
    ldx #$00
LoadPalette:
    lda PaletteData, x  ; Load byte from our palette table (defined below)
    sta PPUDATA          ; Write it to PPU palette RAM
    inx
    cpx #$20             ; ComPare X to 32 ($20). Have we written all 32 bytes?
    bne LoadPalette      ; If not, keep going.

    ; --- Write "HELLO WORLD!" to the nametable ---
    ; The nametable is a 32x30 grid of tile indices starting at PPU $2000.
    ; Each row is 32 bytes. To calculate the address for row R, column C:
    ;   address = $2000 + (R * 32) + C
    ;
    ; Let's place our text roughly in the center of the screen:
    ;   Row 14, Column 10
    ;   Address = $2000 + (14 * 32) + 10 = $2000 + 448 + 10 = $21CA

    bit PPUSTATUS   ; Reset address latch
    lda #$21
    sta PPUADDR     ; High byte of $21CA
    lda #$CA
    sta PPUADDR     ; Low byte. PPU address is now $21CA.

    ; Write the tile indices for each letter.
    ; In our CHR-ROM (defined at the bottom), we've arranged the tiles so
    ; that ASCII-like indices map to letters. Tile $00 is blank,
    ; and letters start where we define them.
    ; Our font starts at tile $00, with:
    ;   'H' = $08, 'E' = $05, 'L' = $0C, 'O' = $0F
    ;   'W' = $17, 'R' = $12, 'D' = $04, '!' = $21
    ;   ' ' = $00 (blank tile)

    ldx #$00
LoadMessage:
    lda MessageData, x  ; Load next character tile index
    cmp #$FF             ; Is it our end-of-string marker?
    beq DoneMessage      ; If yes, we're done.
    sta PPUDATA          ; Write tile index to nametable
    inx
    jmp LoadMessage      ; JuMP back to load next character
DoneMessage:

    ; --- Enable rendering ---
    ; Now we turn on the PPU and let it start drawing.

    ; First, reset the scroll position to (0,0).
    ; If we don't do this, the screen may appear shifted.
    bit PPUSTATUS   ; Reset latch
    lda #$00
    sta $2005       ; PPUSCROLL X = 0
    sta $2005       ; PPUSCROLL Y = 0

    ; Enable NMI and set background pattern table to $0000.
    lda #%10000000  ; Bit 7 = 1: Enable NMI on VBlank
                    ; Bit 4 = 0: Background uses pattern table 0 ($0000)
    sta PPUCTRL

    ; Turn on background rendering.
    lda #%00001010  ; Bit 3 = 1: Show background
                    ; Bit 1 = 1: Show background in leftmost 8 pixels
    sta PPUMASK

    ; --- Main loop: do nothing forever ---
    ; Our Hello World is static. The NMI handler runs every frame
    ; (60 times per second on NTSC) and could be used for animation,
    ; input, etc. For now, we just loop.
Forever:
    jmp Forever     ; Infinite loop. The NMI interrupt will still fire.


; ===== NMI: Called every VBlank (once per frame, ~60 Hz) =====
; The PPU triggers a Non-Maskable Interrupt at the start of each VBlank
; period. This is the heartbeat of your game — where you'd update sprites,
; scroll, read controllers, etc.

NMI:
    rti             ; ReTurn from Interrupt. Nothing to do yet.


; ===== IRQ: Hardware interrupt handler =====
; Triggered by mapper hardware or the APU. We don't use it.

IRQ:
    rti


; ---------------------------------------------------------------------------
; Read-Only Data
; ---------------------------------------------------------------------------

.segment "RODATA"

; --- Color Palette ---
; 32 bytes: 4 background palettes, then 4 sprite palettes.
; Each palette is 4 bytes. Color $0 in each BG palette is shared as the
; universal background color.
;
; NES colors are indices into a fixed 64-color palette. Some common values:
;   $0F = black,  $30 = white,  $16 = red,    $11 = blue
;   $1A = green,  $00 = gray,   $20 = white2,  $2D = dark gray

PaletteData:
    ; Background palette 0 (used for our text)
    .byte $0F       ; Universal background color: black
    .byte $30       ; Color 1: white (this is our text color)
    .byte $16       ; Color 2: red   (unused in this demo)
    .byte $1A       ; Color 3: green (unused in this demo)

    ; Background palettes 1-3 (unused, but we must fill all 32 bytes)
    .byte $0F,$30,$16,$1A
    .byte $0F,$30,$16,$1A
    .byte $0F,$30,$16,$1A

    ; Sprite palettes 0-3 (unused in this demo)
    .byte $0F,$30,$16,$1A
    .byte $0F,$30,$16,$1A
    .byte $0F,$30,$16,$1A
    .byte $0F,$30,$16,$1A

; --- Message Data ---
; These are tile indices, NOT ASCII codes. They correspond to the tiles
; we define in CHR-ROM below. $FF marks end of string.
;
; Our tile layout: tile $00 = blank, then tiles $01-$1A = A-Z,
; tile $21 = '!'. So:
;   H=08, E=05, L=0C, L=0C, O=0F, (space)=00,
;   W=17, O=0F, R=12, L=0C, D=04, !=21

MessageData:
    .byte $08,$05,$0C,$0C,$0F  ; H E L L O
    .byte $00                   ; (space - blank tile)
    .byte $17,$0F,$12,$0C,$04  ; W O R L D
    .byte $21                   ; !
    .byte $FF                   ; End marker

; ---------------------------------------------------------------------------
; Interrupt Vectors
; ---------------------------------------------------------------------------
; The 6502 has three hard-wired vectors at the top of memory:
;   $FFFA-$FFFB = NMI vector   (called on VBlank)
;   $FFFC-$FFFD = RESET vector (called on power-on/reset)
;   $FFFE-$FFFF = IRQ vector   (called on hardware interrupt)
;
; These are 16-bit addresses (little-endian) pointing to our handlers.

.segment "VECTORS"

    .word NMI       ; $FFFA: NMI handler address
    .word Reset     ; $FFFC: Reset handler address
    .word IRQ       ; $FFFE: IRQ handler address

; ---------------------------------------------------------------------------
; CHR-ROM: Graphics Tile Data
; ---------------------------------------------------------------------------
; This is the actual pixel data for our tiles. The NES PPU uses 8x8 pixel
; tiles, with each tile encoded as 16 bytes (2 "bit planes").
;
; Each tile is 8 rows. For each row, there are TWO bytes:
;   - Plane 0 byte (low bit of each pixel's color)
;   - Plane 1 byte (high bit of each pixel's color)
;   Plane 0 comes first (8 bytes), then Plane 1 (8 bytes).
;
; The two planes combine to give a 2-bit color index (0-3) per pixel:
;   Plane1 Plane0 -> Color
;     0      0    ->   0  (background/transparent)
;     0      1    ->   1
;     1      0    ->   2
;     1      1    ->   3
;
; For our font, we'll use color 0 (background) and color 1 (white).
; That means Plane 1 is always $00, and Plane 0 holds the pixel pattern.
;
; IMPORTANT: The tile INDEX in the nametable corresponds to the tile's
; position in CHR-ROM. Tile 0 is bytes 0-15, tile 1 is bytes 16-31, etc.
;
; Our layout:
;   Tile $00 = blank (space)
;   Tiles $01-$1A = letters A through Z
;   Tiles $1B-$20 = unused
;   Tile $21 = '!'

.segment "CHARS"

; --- Tile $00: Blank (space) ---
.byte $00,$00,$00,$00,$00,$00,$00,$00  ; Plane 0: all pixels off
.byte $00,$00,$00,$00,$00,$00,$00,$00  ; Plane 1: all zeros

; --- Tile $01: A ---
.byte %00111100  ; Row 0:   ****
.byte %01100110  ; Row 1:  **  **
.byte %01100110  ; Row 2:  **  **
.byte %01111110  ; Row 3:  ******
.byte %01100110  ; Row 4:  **  **
.byte %01100110  ; Row 5:  **  **
.byte %01100110  ; Row 6:  **  **
.byte %00000000  ; Row 7:  (empty)
.byte $00,$00,$00,$00,$00,$00,$00,$00  ; Plane 1

; --- Tile $02: B ---
.byte %01111100
.byte %01100110
.byte %01100110
.byte %01111100
.byte %01100110
.byte %01100110
.byte %01111100
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $03: C ---
.byte %00111100
.byte %01100110
.byte %01100000
.byte %01100000
.byte %01100000
.byte %01100110
.byte %00111100
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $04: D ---
.byte %01111000
.byte %01101100
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01101100
.byte %01111000
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $05: E ---
.byte %01111110
.byte %01100000
.byte %01100000
.byte %01111100
.byte %01100000
.byte %01100000
.byte %01111110
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $06: F ---
.byte %01111110
.byte %01100000
.byte %01100000
.byte %01111100
.byte %01100000
.byte %01100000
.byte %01100000
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $07: G ---
.byte %00111100
.byte %01100110
.byte %01100000
.byte %01101110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $08: H ---
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01111110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $09: I ---
.byte %00111100
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00111100
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $0A: J ---
.byte %00011110
.byte %00000110
.byte %00000110
.byte %00000110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $0B: K ---
.byte %01100110
.byte %01101100
.byte %01111000
.byte %01110000
.byte %01111000
.byte %01101100
.byte %01100110
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $0C: L ---
.byte %01100000
.byte %01100000
.byte %01100000
.byte %01100000
.byte %01100000
.byte %01100000
.byte %01111110
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $0D: M ---
.byte %01100011
.byte %01110111
.byte %01111111
.byte %01101011
.byte %01100011
.byte %01100011
.byte %01100011
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $0E: N ---
.byte %01100110
.byte %01110110
.byte %01111110
.byte %01111110
.byte %01101110
.byte %01100110
.byte %01100110
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $0F: O ---
.byte %00111100
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $10: P ---
.byte %01111100
.byte %01100110
.byte %01100110
.byte %01111100
.byte %01100000
.byte %01100000
.byte %01100000
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $11: Q ---
.byte %00111100
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01101010
.byte %01101100
.byte %00110110
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $12: R ---
.byte %01111100
.byte %01100110
.byte %01100110
.byte %01111100
.byte %01111000
.byte %01101100
.byte %01100110
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $13: S ---
.byte %00111100
.byte %01100110
.byte %01110000
.byte %00111100
.byte %00001110
.byte %01100110
.byte %00111100
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $14: T ---
.byte %01111110
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $15: U ---
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $16: V ---
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00011000
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $17: W ---
.byte %01100011
.byte %01100011
.byte %01100011
.byte %01101011
.byte %01111111
.byte %01110111
.byte %01100011
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $18: X ---
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00011000
.byte %00111100
.byte %01100110
.byte %01100110
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $19: Y ---
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $1A: Z ---
.byte %01111110
.byte %00000110
.byte %00001100
.byte %00011000
.byte %00110000
.byte %01100000
.byte %01111110
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tiles $1B-$20: Unused (padding to reach tile $21) ---
; Each tile is 16 bytes. We need 6 tiles of padding (6 x 16 = 96 bytes).
.res 96, $00

; --- Tile $21: ! (exclamation mark) ---
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00011000
.byte %00000000
.byte %00011000
.byte %00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

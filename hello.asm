; ============================================================================
; Mecha Man - NES ROM
; ============================================================================
;
; Displays "HELLO WORLD!" on the background and a Mega Man character that
; can run left and right using the D-pad. Features:
;   - Multi-tile metasprites (2x3 = 16x24 pixel character)
;   - Standing and running animation
;   - Horizontal flip for left/right facing
;
; CHR-ROM layout:
;   Pattern Table 0 ($0000-$0FFF): Background tiles (font A-Z, etc.)
;   Pattern Table 1 ($1000-$1FFF): Sprite tiles (Mega Man poses)
;
; ============================================================================

; ---------------------------------------------------------------------------
; PPU Register Definitions
; ---------------------------------------------------------------------------

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
                     ;   Reading this register resets the address latch.

OAMADDR   = $2003   ; OAM Address Register
OAMDATA   = $2004   ; OAM Data Register
PPUSCROLL = $2005   ; PPU Scroll Register (write twice: X, then Y)
PPUADDR   = $2006   ; PPU Address Register (write twice: high, then low)
PPUDATA   = $2007   ; PPU Data Register
OAMDMA    = $4014   ; OAM DMA Register

JOYPAD1   = $4016   ; Controller 1 Port
JOYPAD2   = $4017   ; Controller 2 Port

; ---------------------------------------------------------------------------
; Constants
; ---------------------------------------------------------------------------

; PPUCTRL value: NMI enabled, BG uses pattern table 0, sprites use table 1
PPUCTRL_VAL = %10001000

; Animation timing (frames between animation ticks)
ANIM_SPEED_RUN  = 6     ; ~10 fps run cycle at 60 Hz
MOVE_SPEED      = 2     ; Pixels per frame

; Sprite attribute bits
FLIP_H          = %01000000  ; Horizontal flip

; Screen bounds (accounting for 16x24 metasprite)
BOUND_LEFT      = $02
BOUND_RIGHT     = $EE        ; 238 = 256 - 16 - 2

; ---------------------------------------------------------------------------
; iNES Header
; ---------------------------------------------------------------------------

.segment "HEADER"

    .byte "NES",$1A   ; Magic number
    .byte $01          ; 1 x 16KB PRG-ROM bank
    .byte $01          ; 1 x 8KB CHR-ROM bank
    .byte $00          ; Flags 6: Mapper 0 (NROM), horizontal mirroring
    .byte $00          ; Flags 7: Mapper 0, NES 1.0
    .byte $00,$00,$00  ; Flags 8-10
    .byte $00,$00,$00,$00,$00  ; Padding

; ---------------------------------------------------------------------------
; Zero Page Variables
; ---------------------------------------------------------------------------
; The 6502 zero page ($0000-$00FF) provides fast access (1 byte shorter,
; 1 cycle faster). We use it for all frequently-accessed game state.

.segment "ZEROPAGE"

sprite_x:   .res 1   ; Metasprite anchor X position
sprite_y:   .res 1   ; Metasprite anchor Y position
controller: .res 1   ; Current controller button state
                      ;   Bit 7: A     Bit 3: Up
                      ;   Bit 6: B     Bit 2: Down
                      ;   Bit 5: Sel   Bit 1: Left
                      ;   Bit 4: Start Bit 0: Right

; Animation state
facing_dir: .res 1   ; 0 = facing right, 1 = facing left
anim_state: .res 1   ; 0 = idle/standing, 1 = running
anim_frame: .res 1   ; Current frame index within animation
anim_timer: .res 1   ; Frame counter until next animation tick

; Metasprite rendering
meta_ptr:   .res 2   ; 16-bit pointer to current metasprite data (lo, hi)

; ---------------------------------------------------------------------------
; Main Code
; ---------------------------------------------------------------------------

.segment "CODE"

; ===== RESET: Entry point on power-on/reset =====

Reset:
    sei
    cld
    ldx #$FF
    txs

    ; Disable PPU during init
    lda #$00
    sta PPUCTRL
    sta PPUMASK

    ; Wait for first VBlank
WaitVBlank1:
    bit PPUSTATUS
    bpl WaitVBlank1

    ; Clear all RAM
    lda #$00
    ldx #$00
ClearRAM:
    sta $0000, x
    sta $0100, x
    sta $0200, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x
    inx
    bne ClearRAM

    ; Wait for second VBlank
WaitVBlank2:
    bit PPUSTATUS
    bpl WaitVBlank2

    ; ==================================================================
    ; PPU is ready. Set up graphics.
    ; ==================================================================

    ; --- Load color palette ---
    bit PPUSTATUS
    lda #$3F
    sta PPUADDR
    lda #$00
    sta PPUADDR

    ldx #$00
LoadPalette:
    lda PaletteData, x
    sta PPUDATA
    inx
    cpx #$20
    bne LoadPalette

    ; --- Write "HELLO WORLD!" to nametable ---
    ; Row 14, Column 10 = PPU address $21CA
    bit PPUSTATUS
    lda #$21
    sta PPUADDR
    lda #$CA
    sta PPUADDR

    ldx #$00
LoadMessage:
    lda MessageData, x
    cmp #$FF
    beq DoneMessage
    sta PPUDATA
    inx
    jmp LoadMessage
DoneMessage:

    ; --- Initialize player state ---
    lda #200             ; Start near bottom of screen
    sta sprite_y
    lda #120             ; Roughly centered horizontally
    sta sprite_x
    lda #$00
    sta facing_dir       ; Face right
    sta anim_state       ; Idle
    sta anim_frame       ; Frame 0
    sta anim_timer       ; Timer at 0

    ; --- Draw initial metasprite to OAM buffer ---
    lda #<MetaStand
    sta meta_ptr
    lda #>MetaStand
    sta meta_ptr+1
    jsr DrawMetasprite

    ; --- Enable rendering ---
    bit PPUSTATUS
    lda #$00
    sta PPUSCROLL
    sta PPUSCROLL

    lda #PPUCTRL_VAL
    sta PPUCTRL

    lda #%00011110       ; Show sprites + background, leftmost 8px
    sta PPUMASK

    ; Main loop: NMI does all the work
Forever:
    jmp Forever


; ===== NMI: Called every VBlank (~60 Hz) =====
; This is the game loop heartbeat.

NMI:
    pha
    txa
    pha
    tya
    pha

    ; --- Step 1: OAM DMA ---
    lda #$00
    sta OAMADDR
    lda #$02
    sta OAMDMA

    ; --- Step 2: Read controller ---
    jsr ReadControllerSub

    ; --- Step 3: Update facing direction ---
    ; Check right first, then left. Last pressed wins if both held.
    lda controller
    and #%00000001       ; Right
    beq @CheckLeftFace
    lda #$00
    sta facing_dir
    jmp @FaceDone
@CheckLeftFace:
    lda controller
    and #%00000010       ; Left
    beq @FaceDone
    lda #$01
    sta facing_dir
@FaceDone:

    ; --- Step 4: Move player (left/right only) ---
    jsr MovePlayer

    ; --- Step 5: Update animation and draw metasprite ---
    jsr UpdateAnimation

    ; --- Step 6: Reset scroll ---
    bit PPUSTATUS
    lda #$00
    sta PPUSCROLL
    sta PPUSCROLL
    lda #PPUCTRL_VAL
    sta PPUCTRL

    pla
    tay
    pla
    tax
    pla
    rti


; ===== IRQ =====
IRQ:
    rti


; ---------------------------------------------------------------------------
; Subroutines
; ---------------------------------------------------------------------------

; --- ReadControllerSub ---
; Reads controller 1 and packs button state into 'controller'.
; Bit layout: A B Sel Start Up Down Left Right (bits 7..0)

ReadControllerSub:
    lda #$01
    sta JOYPAD1
    lda #$00
    sta JOYPAD1

    ldx #$08
@Loop:
    lda JOYPAD1
    lsr a
    rol controller
    dex
    bne @Loop
    rts


; --- MovePlayer ---
; Moves the metasprite anchor position based on left/right input.
; Movement speed: MOVE_SPEED pixels per frame.

MovePlayer:
    ; Check LEFT (bit 1)
    lda controller
    and #%00000010
    beq @CheckRight
    lda sprite_x
    cmp #BOUND_LEFT
    bcc @CheckRight
    dec sprite_x
    dec sprite_x
@CheckRight:
    lda controller
    and #%00000001
    beq @Done
    lda sprite_x
    cmp #BOUND_RIGHT
    bcs @Done
    inc sprite_x
    inc sprite_x
@Done:
    rts


; --- UpdateAnimation ---
; Determines current animation state (idle vs running), advances the
; animation timer, resolves the current frame, and calls DrawMetasprite.

UpdateAnimation:
    ; Determine desired state from input
    lda controller
    and #%00000011       ; Left or Right pressed?
    bne @WantRun

    ; --- Want idle ---
    lda anim_state
    beq @AlreadyIdle     ; Already idle? Skip transition
    ; Transition to idle
    lda #$00
    sta anim_state
    sta anim_frame
    sta anim_timer
@AlreadyIdle:
    jmp @ResolveFrame

@WantRun:
    ; --- Want running ---
    lda anim_state
    bne @AlreadyRunning  ; Already running? Skip transition
    ; Transition to running
    lda #$01
    sta anim_state
    lda #$00
    sta anim_frame
    sta anim_timer
@AlreadyRunning:

    ; --- Advance animation timer ---
    inc anim_timer
    lda anim_timer
    cmp #ANIM_SPEED_RUN
    bcc @ResolveFrame    ; Timer hasn't reached threshold

    ; Timer expired: advance to next frame
    lda #$00
    sta anim_timer
    inc anim_frame

    ; Wrap frame index (3 run frames: 0, 1, 2)
    lda anim_frame
    cmp #$03
    bcc @ResolveFrame
    lda #$00
    sta anim_frame

@ResolveFrame:
    ; Look up the metasprite pointer for current state + frame
    lda anim_state
    bne @RunFrame

    ; Idle: always use standing frame
    lda #<MetaStand
    sta meta_ptr
    lda #>MetaStand
    sta meta_ptr+1
    jmp DrawMetasprite

@RunFrame:
    ; Running: index into run frame table
    ldx anim_frame
    lda RunFramesL, x
    sta meta_ptr
    lda RunFramesH, x
    sta meta_ptr+1
    jmp DrawMetasprite   ; Tail call (DrawMetasprite ends with rts)


; --- DrawMetasprite ---
; Reads the metasprite definition pointed to by meta_ptr and writes
; hardware sprites to the OAM buffer at $0200.
;
; Handles horizontal flipping: if facing_dir is 1 (left), each sub-sprite
; gets the H-flip attribute bit set and its X offset is mirrored.
;
; Uses Y as the read index into metasprite data, X as the write index
; into the OAM buffer.

DrawMetasprite:
    ldx #$00             ; OAM write index
    ldy #$00             ; Metasprite data read index

@Loop:
    lda (meta_ptr), y    ; Byte 0: Y offset (or $80 = end marker)
    cmp #$80
    beq @HideRest

    ; --- Y position = sprite_y + y_offset ---
    clc
    adc sprite_y
    sta $0200, x         ; OAM byte 0: Y position
    iny

    ; --- Tile index (straight copy) ---
    lda (meta_ptr), y
    sta $0201, x         ; OAM byte 1: tile index
    iny

    ; --- Attributes ---
    lda (meta_ptr), y    ; Base attribute byte
    pha                  ; Save it
    lda facing_dir
    beq @NoFlipAttr
    pla
    ora #FLIP_H          ; Set horizontal flip bit
    pha
@NoFlipAttr:
    pla
    sta $0202, x         ; OAM byte 2: attributes
    iny

    ; --- X position ---
    ; If facing right: X = sprite_x + x_offset
    ; If facing left:  X = sprite_x + (8 - x_offset)
    ;   where 8 = (metasprite_width - tile_width) = (16 - 8)
    lda facing_dir
    bne @FlipX

    ; Right-facing: simple add
    lda (meta_ptr), y    ; x_offset
    clc
    adc sprite_x
    jmp @StoreX

@FlipX:
    ; Left-facing: mirror the offset
    lda #$08
    sec
    sbc (meta_ptr), y    ; A = 8 - x_offset
    clc
    adc sprite_x

@StoreX:
    sta $0203, x         ; OAM byte 3: X position
    iny

    ; Advance OAM index by 4 bytes (next hardware sprite slot)
    inx
    inx
    inx
    inx

    jmp @Loop

@HideRest:
    ; Hide all remaining hardware sprites by setting Y = $FE (off-screen)
    lda #$FE
@HideLoop:
    cpx #$00             ; X wrapped to 0 = all 64 sprites written
    beq @HideDone
    sta $0200, x         ; Set Y off-screen
    inx
    inx
    inx
    inx
    bne @HideLoop        ; Loop until X wraps
@HideDone:
    rts


; ---------------------------------------------------------------------------
; Read-Only Data
; ---------------------------------------------------------------------------

.segment "RODATA"

; --- Color Palette ---
; 32 bytes: 4 background palettes, then 4 sprite palettes.

PaletteData:
    ; Background palette 0 (text)
    .byte $0F       ; Universal background color: black
    .byte $30       ; Color 1: white (text color)
    .byte $16       ; Color 2: red (unused)
    .byte $1A       ; Color 3: green (unused)

    ; Background palettes 1-3 (unused)
    .byte $0F,$30,$16,$1A
    .byte $0F,$30,$16,$1A
    .byte $0F,$30,$16,$1A

    ; Sprite palette 0 (Mega Man)
    ; Mapped to CHR color indices:
    ;   Index 0: transparent
    ;   Index 1: dark outline (near-black in CHR → NES $0F black)
    ;   Index 2: body blue
    ;   Index 3: skin/highlight
    .byte $0F       ; Color 0: transparent (shows background)
    .byte $0F       ; Color 1: black (outlines)
    .byte $11       ; Color 2: medium blue (body)
    .byte $36       ; Color 3: light peach (skin/highlights)

    ; Sprite palettes 1-3 (unused)
    .byte $0F,$0F,$11,$36
    .byte $0F,$0F,$11,$36
    .byte $0F,$0F,$11,$36

; --- Message Data ---
; Tile indices for "HELLO WORLD!" using background pattern table 0.

MessageData:
    .byte $08,$05,$0C,$0C,$0F  ; H E L L O
    .byte $00                   ; (space)
    .byte $17,$0F,$12,$0C,$04  ; W O R L D
    .byte $21                   ; !
    .byte $FF                   ; End marker

; --- Metasprite Definitions ---
; Each entry: Y-offset, Tile, Attribute, X-offset
; Terminated by $80.
;
; Tile indices reference Pattern Table 1 (sprites).
; Layout in CHR (128px wide = 16 tiles per row):
;   Stand: tiles $00,$01 / $10,$11 / $20,$21
;   Run1:  tiles $02,$03 / $12,$13 / $22,$23
;   Run2:  tiles $04,$05 / $14,$15 / $24,$25
;   Run3:  tiles $06,$07 / $16,$17 / $26,$27

MetaStand:
    .byte $00, $00, $00, $00   ; top-left
    .byte $00, $01, $00, $08   ; top-right
    .byte $08, $10, $00, $00   ; mid-left
    .byte $08, $11, $00, $08   ; mid-right
    .byte $10, $20, $00, $00   ; bot-left
    .byte $10, $21, $00, $08   ; bot-right
    .byte $80                   ; end

MetaRun1:
    .byte $00, $02, $00, $00   ; top-left
    .byte $00, $03, $00, $08   ; top-right
    .byte $08, $12, $00, $00   ; mid-left
    .byte $08, $13, $00, $08   ; mid-right
    .byte $10, $22, $00, $00   ; bot-left
    .byte $10, $23, $00, $08   ; bot-right
    .byte $80                   ; end

MetaRun2:
    .byte $00, $04, $00, $00   ; top-left
    .byte $00, $05, $00, $08   ; top-right
    .byte $08, $14, $00, $00   ; mid-left
    .byte $08, $15, $00, $08   ; mid-right
    .byte $10, $24, $00, $00   ; bot-left
    .byte $10, $25, $00, $08   ; bot-right
    .byte $80                   ; end

MetaRun3:
    .byte $00, $06, $00, $00   ; top-left
    .byte $00, $07, $00, $08   ; top-right
    .byte $08, $16, $00, $00   ; mid-left
    .byte $08, $17, $00, $08   ; mid-right
    .byte $10, $26, $00, $00   ; bot-left
    .byte $10, $27, $00, $08   ; bot-right
    .byte $80                   ; end

; --- Animation Frame Pointer Tables ---
; Used to look up the metasprite definition for each run frame.

RunFramesL:
    .byte <MetaRun1, <MetaRun2, <MetaRun3
RunFramesH:
    .byte >MetaRun1, >MetaRun2, >MetaRun3


; ---------------------------------------------------------------------------
; Interrupt Vectors
; ---------------------------------------------------------------------------

.segment "VECTORS"

    .word NMI
    .word Reset
    .word IRQ


; ---------------------------------------------------------------------------
; CHR-ROM: Graphics Tile Data
; ---------------------------------------------------------------------------
; 8KB total: Pattern Table 0 (background) + Pattern Table 1 (sprites).

.segment "CHARS"

; === Pattern Table 0 ($0000-$0FFF): Background tiles ===
; Font tiles for the "HELLO WORLD!" message.
; Tile $00 = blank, $01-$1A = A-Z, $21 = '!'

; --- Tile $00: Blank ---
.byte $00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $01: A ---
.byte %00111100,%01100110,%01100110,%01111110,%01100110,%01100110,%01100110,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $02: B ---
.byte %01111100,%01100110,%01100110,%01111100,%01100110,%01100110,%01111100,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $03: C ---
.byte %00111100,%01100110,%01100000,%01100000,%01100000,%01100110,%00111100,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $04: D ---
.byte %01111000,%01101100,%01100110,%01100110,%01100110,%01101100,%01111000,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $05: E ---
.byte %01111110,%01100000,%01100000,%01111100,%01100000,%01100000,%01111110,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $06: F ---
.byte %01111110,%01100000,%01100000,%01111100,%01100000,%01100000,%01100000,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $07: G ---
.byte %00111100,%01100110,%01100000,%01101110,%01100110,%01100110,%00111100,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $08: H ---
.byte %01100110,%01100110,%01100110,%01111110,%01100110,%01100110,%01100110,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $09: I ---
.byte %00111100,%00011000,%00011000,%00011000,%00011000,%00011000,%00111100,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $0A: J ---
.byte %00011110,%00000110,%00000110,%00000110,%01100110,%01100110,%00111100,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $0B: K ---
.byte %01100110,%01101100,%01111000,%01110000,%01111000,%01101100,%01100110,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $0C: L ---
.byte %01100000,%01100000,%01100000,%01100000,%01100000,%01100000,%01111110,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $0D: M ---
.byte %01100011,%01110111,%01111111,%01101011,%01100011,%01100011,%01100011,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $0E: N ---
.byte %01100110,%01110110,%01111110,%01111110,%01101110,%01100110,%01100110,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $0F: O ---
.byte %00111100,%01100110,%01100110,%01100110,%01100110,%01100110,%00111100,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $10: P ---
.byte %01111100,%01100110,%01100110,%01111100,%01100000,%01100000,%01100000,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $11: Q ---
.byte %00111100,%01100110,%01100110,%01100110,%01101010,%01101100,%00110110,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $12: R ---
.byte %01111100,%01100110,%01100110,%01111100,%01111000,%01101100,%01100110,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $13: S ---
.byte %00111100,%01100110,%01110000,%00111100,%00001110,%01100110,%00111100,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $14: T ---
.byte %01111110,%00011000,%00011000,%00011000,%00011000,%00011000,%00011000,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $15: U ---
.byte %01100110,%01100110,%01100110,%01100110,%01100110,%01100110,%00111100,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $16: V ---
.byte %01100110,%01100110,%01100110,%01100110,%01100110,%00111100,%00011000,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $17: W ---
.byte %01100011,%01100011,%01100011,%01101011,%01111111,%01110111,%01100011,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $18: X ---
.byte %01100110,%01100110,%00111100,%00011000,%00111100,%01100110,%01100110,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $19: Y ---
.byte %01100110,%01100110,%01100110,%00111100,%00011000,%00011000,%00011000,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $1A: Z ---
.byte %01111110,%00000110,%00001100,%00011000,%00110000,%01100000,%01111110,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tiles $1B-$20: Unused padding ---
.res 96, $00

; --- Tile $21: ! ---
.byte %00011000,%00011000,%00011000,%00011000,%00011000,%00000000,%00011000,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Pad rest of Pattern Table 0 to exactly 4096 bytes ---
; Tiles $00-$21 = 34 tiles = 544 bytes. Need 4096 - 544 = 3552 bytes.
.res 3552, $00

; === Pattern Table 1 ($1000-$1FFF): Sprite tiles ===
; Mega Man poses extracted by tools/extract_poses.py.
; See tile index layout in the metasprite definitions above.
.incbin "assets/chr/megaman_sprites.chr"

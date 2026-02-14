; ============================================================================
; Mecha Man - NES ROM
; ============================================================================
;
; Displays "HELLO WORLD!" on the background and a Mario character that
; can run left and right using the D-pad. Features:
;   - Multi-tile metasprites (2x2 = 16x16 pixel character)
;   - Standing and running animation
;   - Horizontal flip for left/right facing
;
; CHR-ROM layout:
;   Pattern Table 0 ($0000-$0FFF): Background tiles (font A-Z, etc.)
;   Pattern Table 1 ($1000-$1FFF): Sprite tiles (Mario poses)
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

; World bounds (accounting for 16x16 metasprite)
; Level is 2 screens wide (512 pixels)
LEVEL_WIDTH_LO  = $00        ; Low byte of 512
LEVEL_WIDTH_HI  = $02        ; High byte of 512
WORLD_LEFT_LO   = $02        ; Left world bound
WORLD_LEFT_HI   = $00
WORLD_RIGHT_LO  = $EE        ; Right world bound (512 - 16 - 2 = 494 = $01EE)
WORLD_RIGHT_HI  = $01

; Camera constants
CAMERA_LEFT_EDGE  = $50      ; Player screen X for left edge of dead zone (80)
CAMERA_RIGHT_EDGE = $A0      ; Player screen X for right edge of dead zone (160)

; Physics constants
GRAVITY         = 1          ; Added to vel_y each frame
JUMP_VELOCITY   = $F4        ; -12 in two's complement (upward)
GROUND_Y        = 184        ; Y position of the floor (row 13 metatile = tile row 26 = 208px, minus 24px sprite height)
TERMINAL_VEL    = 5          ; Maximum falling speed

; ---------------------------------------------------------------------------
; iNES Header
; ---------------------------------------------------------------------------

.segment "HEADER"

    .byte "NES",$1A   ; Magic number
    .byte $01          ; 1 x 16KB PRG-ROM bank
    .byte $01          ; 1 x 8KB CHR-ROM bank
    .byte $01          ; Flags 6: Mapper 0 (NROM), vertical mirroring
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

; Level rendering temporaries
level_ptr:  .res 2   ; 16-bit pointer into level data
temp:       .res 1   ; General purpose temp
temp2:      .res 1   ; Second temp
col_count:  .res 1   ; Column counter for level init

; Physics state
vel_y:      .res 1   ; Vertical velocity (signed: negative=up, positive=down)
on_ground:  .res 1   ; 1 = on ground, 0 = in air

; Scrolling and Camera (16-bit world coordinates)
camera_x_lo:     .res 1   ; Camera world X low byte (0-255)
camera_x_hi:     .res 1   ; Camera world X high byte (0-1 for 2 screens)
player_world_x_lo: .res 1 ; Player world X low byte
player_world_x_hi: .res 1 ; Player world X high byte
scroll_x_hi:     .res 1   ; Nametable select bit (0 or 1)
prev_scroll_col: .res 1   ; Last drawn column (for seam detection)
col_to_draw:     .res 1   ; Next column to draw
seam_ready:      .res 1   ; Flag: seam buffer ready to write

; ---------------------------------------------------------------------------
; BSS (Uninitialized RAM at $0300+)
; ---------------------------------------------------------------------------
; Note: $0200-$02FF is reserved for OAM sprite buffer

.segment "BSS"

seam_buffer:    .res 30  ; One column of tiles for seam update

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

    ; --- Draw level to both nametables ---
    jsr DrawLevel

    ; --- Initialize player state ---
    lda #GROUND_Y        ; Start on the ground
    sta sprite_y
    lda #120             ; Screen X position (centered)
    sta sprite_x

    ; World position starts at (120, 200)
    lda #120
    sta player_world_x_lo
    lda #$00
    sta player_world_x_hi

    lda #$00
    sta facing_dir       ; Face right
    sta anim_state       ; Idle
    sta anim_frame       ; Frame 0
    sta anim_timer       ; Timer at 0
    sta vel_y            ; No vertical velocity

    ; Camera starts at world X = 0
    sta camera_x_lo
    sta camera_x_hi
    sta scroll_x_hi      ; Start on nametable 0
    sta seam_ready       ; No seam to write yet
    sta prev_scroll_col  ; Leftmost column is 0 at camera_x=0

    lda #$01
    sta on_ground        ; Start on ground

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
;
; CRITICAL: Vblank is limited! Do PPU writes first:
;   1. OAM DMA
;   2. Seam column write (if ready)
;   3. Set scroll and PPUCTRL
; Then do game logic (can run outside vblank).

NMI:
    pha
    txa
    pha
    tya
    pha

    ; ==========================================
    ; VBLANK-CRITICAL SECTION - PPU writes only
    ; ==========================================

    ; --- Step 1: OAM DMA (must be first) ---
    lda #$00
    sta OAMADDR
    lda #$02
    sta OAMDMA

    ; --- Step 2: Write seam column if ready ---
    lda seam_ready
    beq @SkipSeam
    jsr WriteSeam
    lda #$00
    sta seam_ready
@SkipSeam:

    ; --- Step 3: Apply scroll to PPU (must be last PPU write) ---
    bit PPUSTATUS
    lda camera_x_lo
    sta PPUSCROLL          ; X scroll (low 8 bits)
    lda #$00
    sta PPUSCROLL          ; Y scroll (fixed at 0)

    ; Set PPUCTRL with nametable select bit
    lda #PPUCTRL_VAL
    ora scroll_x_hi        ; Add nametable bit (0 or 1)
    sta PPUCTRL

    ; ==========================================
    ; GAME LOGIC SECTION - Can run outside vblank
    ; ==========================================

    ; --- Step 4: Read controller ---
    jsr ReadControllerSub

    ; --- Step 5: Update facing direction ---
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

    ; --- Step 6: Move player (left/right only) ---
    jsr MovePlayer

    ; --- Step 7: Handle jumping and gravity ---
    jsr HandleJump
    jsr ApplyGravity

    ; --- Step 8: Update camera/scroll ---
    jsr UpdateCamera

    ; --- Step 9: Detect and prepare seam ---
    jsr PrepareSeam

    ; --- Step 10: Update animation and draw metasprite ---
    jsr UpdateAnimation

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
; Moves the player's world position based on left/right input.
; Movement speed: MOVE_SPEED pixels per frame.
; After moving, sprite_x is calculated from world position - camera position.

MovePlayer:
    ; Check LEFT (bit 1)
    lda controller
    and #%00000010
    beq @CheckRight

    ; Check left world bound (16-bit compare)
    lda player_world_x_hi
    cmp #WORLD_LEFT_HI
    bcc @CheckRight          ; High byte less = at bound
    bne @DoLeft              ; High byte greater = can move
    lda player_world_x_lo
    cmp #WORLD_LEFT_LO
    bcc @CheckRight          ; At left bound
    beq @CheckRight

@DoLeft:
    ; Subtract MOVE_SPEED from world X (16-bit)
    lda player_world_x_lo
    sec
    sbc #MOVE_SPEED
    sta player_world_x_lo
    lda player_world_x_hi
    sbc #$00
    sta player_world_x_hi

@CheckRight:
    lda controller
    and #%00000001
    beq @Done

    ; Check right world bound (16-bit compare)
    lda player_world_x_hi
    cmp #WORLD_RIGHT_HI
    bcc @DoRight             ; High byte less = can move
    bne @Done                ; High byte greater = at bound
    lda player_world_x_lo
    cmp #WORLD_RIGHT_LO
    bcs @Done                ; At right bound

@DoRight:
    ; Add MOVE_SPEED to world X (16-bit)
    lda player_world_x_lo
    clc
    adc #MOVE_SPEED
    sta player_world_x_lo
    lda player_world_x_hi
    adc #$00
    sta player_world_x_hi

@Done:
    rts


; --- HandleJump ---
; Initiates a jump when A is pressed and player is on ground.

HandleJump:
    ; Check if on ground
    lda on_ground
    beq @Done               ; Not on ground, can't jump

    ; Check if A button pressed (bit 7)
    lda controller
    bpl @Done               ; A not pressed (bit 7 = 0)

    ; Start jump: set upward velocity, leave ground
    lda #JUMP_VELOCITY
    sta vel_y
    lda #$00
    sta on_ground

@Done:
    rts


; --- ApplyGravity ---
; Applies gravity to velocity, updates Y position, handles ground collision.

ApplyGravity:
    ; If on ground and not jumping, skip gravity
    lda on_ground
    bne @Done

    ; Apply gravity: vel_y += GRAVITY
    lda vel_y
    clc
    adc #GRAVITY
    sta vel_y

    ; Clamp to terminal velocity (only when falling, i.e., vel_y > 0)
    ; Check if vel_y is positive and > TERMINAL_VEL
    bmi @ApplyVelocity      ; Negative = still rising, don't clamp
    cmp #TERMINAL_VEL
    bcc @ApplyVelocity      ; Less than terminal, ok
    beq @ApplyVelocity      ; Equal to terminal, ok
    lda #TERMINAL_VEL       ; Clamp to terminal velocity
    sta vel_y

@ApplyVelocity:
    ; Add vel_y to sprite_y (signed addition)
    lda sprite_y
    clc
    adc vel_y
    sta sprite_y

    ; Check for ground collision
    ; If sprite_y >= GROUND_Y, land
    lda sprite_y
    cmp #GROUND_Y
    bcc @Done               ; sprite_y < GROUND_Y, still in air

    ; Land on ground
    lda #GROUND_Y
    sta sprite_y
    lda #$00
    sta vel_y
    lda #$01
    sta on_ground

@Done:
    rts


; --- UpdateCamera ---
; Updates camera position to follow player with dead zone.
; Also calculates sprite_x (screen position) from world position.

UpdateCamera:
    ; First, calculate player's screen X = world_x - camera_x
    ; This is 16-bit subtraction, but screen X is 8-bit
    lda player_world_x_lo
    sec
    sbc camera_x_lo
    sta sprite_x           ; Low byte is screen X (assuming camera tracks well)

    ; Check if player is outside dead zone and adjust camera

    ; Check if player screen X < CAMERA_LEFT_EDGE (need to scroll left)
    lda sprite_x
    cmp #CAMERA_LEFT_EDGE
    bcs @CheckRightEdge

    ; Player is left of dead zone - move camera left
    ; camera_x -= (CAMERA_LEFT_EDGE - sprite_x)
    ; Simplified: camera_x = player_world_x - CAMERA_LEFT_EDGE
    lda player_world_x_lo
    sec
    sbc #CAMERA_LEFT_EDGE
    sta camera_x_lo
    lda player_world_x_hi
    sbc #$00
    ; Clamp camera to 0 if it went negative
    bpl @StoreCameraHi
    lda #$00
    sta camera_x_lo
    sta camera_x_hi
    jmp @UpdateScreenX

@StoreCameraHi:
    sta camera_x_hi
    jmp @UpdateScreenX

@CheckRightEdge:
    ; Check if player screen X > CAMERA_RIGHT_EDGE (need to scroll right)
    lda sprite_x
    cmp #CAMERA_RIGHT_EDGE
    bcc @UpdateScreenX

    ; Player is right of dead zone - move camera right
    ; camera_x = player_world_x - CAMERA_RIGHT_EDGE
    lda player_world_x_lo
    sec
    sbc #CAMERA_RIGHT_EDGE
    sta camera_x_lo
    lda player_world_x_hi
    sbc #$00
    sta camera_x_hi

    ; Clamp camera to max (LEVEL_WIDTH - 256 = 256 = $0100)
    lda camera_x_hi
    cmp #$01
    bcc @UpdateScreenX    ; High byte < 1, ok
    bne @ClampCameraMax   ; High byte > 1, clamp
    lda camera_x_lo
    cmp #$00
    bcc @UpdateScreenX    ; At exactly $0100 or less, ok
    beq @UpdateScreenX
@ClampCameraMax:
    lda #$00
    sta camera_x_lo
    lda #$01
    sta camera_x_hi

@UpdateScreenX:
    ; Recalculate screen X with clamped camera
    lda player_world_x_lo
    sec
    sbc camera_x_lo
    sta sprite_x

    ; Set scroll_x_hi from camera_x high byte (nametable select)
    lda camera_x_hi
    and #$01
    sta scroll_x_hi

    rts


; --- PrepareSeam ---
; Detects when a new column has scrolled into view and prepares the seam buffer.
; Called every frame after UpdateCamera.
;
; Tracks the leftmost visible column. When it changes (scroll right),
; draws the new rightmost column.
; Note: Left scrolling works because level is pre-drawn; no dynamic update needed.

PrepareSeam:
    ; Calculate current leftmost tile column
    ; left_col = (camera_x_hi << 5) | (camera_x_lo >> 3)
    lda camera_x_lo
    lsr a
    lsr a
    lsr a                ; A = camera_x_lo >> 3 (0-31)
    ldx camera_x_hi
    beq @NoHighAdd
    ora #$20             ; If camera_x_hi = 1, set bit 5 (add 32)
@NoHighAdd:
    sta temp             ; temp = current leftmost column (0-63)

    ; Check if leftmost column changed from previous frame
    cmp prev_scroll_col
    beq @NothingToDraw

    ; Column changed! Update tracking
    sta prev_scroll_col

    ; Calculate seam column (rightmost + 1 = leftmost + 32)
    clc
    adc #32
    and #$3F             ; Mask to 64 columns
    sta col_to_draw

    ; Calculate which metatile column this tile column is in
    ; metatile_col = tile_col >> 1 (each metatile is 2 tiles wide)
    lsr a
    tax                  ; X = metatile column index (0-31)

    ; Calculate level data pointer: Level1Data + (metatile_col * 15)
    ; We need to multiply X by 15
    lda #$00
    sta level_ptr+1
    txa
    ; Multiply by 15 = multiply by 16, subtract original
    asl a               ; *2
    asl a               ; *4
    asl a               ; *8
    asl a               ; *16
    sta level_ptr
    bcc @NoCarry1
    inc level_ptr+1
@NoCarry1:
    txa                 ; Get original back
    sta temp
    lda level_ptr
    sec
    sbc temp            ; *16 - 1 = *15
    sta level_ptr
    lda level_ptr+1
    sbc #$00
    sta level_ptr+1

    ; Add base address of level data
    lda level_ptr
    clc
    adc #<Level1Data
    sta level_ptr
    lda level_ptr+1
    adc #>Level1Data
    sta level_ptr+1

    ; Determine if we're drawing left or right tile column of metatile
    ; If col_to_draw is even, draw left tiles; if odd, draw right tiles
    lda col_to_draw
    and #$01
    sta temp             ; 0 = left column, 1 = right column

    ; Fill seam_buffer with 30 tiles (15 metatiles * 2 tiles each)
    ; Use Y for (level_ptr),y indexing, X for seam_buffer index
    ldy #$00             ; Index into level column (0-14)
    ldx #$00             ; Index into seam_buffer (0-29)

@FillLoop:
    ; Get metatile index at this row
    lda (level_ptr), y   ; A = metatile index
    sty temp2            ; Save Y (level index)
    asl a
    asl a                ; A = metatile index * 4 (offset into MetatileDefs)
    tay                  ; Y = offset into MetatileDefs

    ; Check if left or right column
    lda temp
    bne @RightTiles

    ; Left tiles: TL at +0, BL at +2
    lda MetatileDefs, y
    sta seam_buffer, x
    inx
    lda MetatileDefs+2, y
    sta seam_buffer, x
    inx
    jmp @NextMetatile

@RightTiles:
    ; Right tiles: TR at +1, BR at +3
    lda MetatileDefs+1, y
    sta seam_buffer, x
    inx
    lda MetatileDefs+3, y
    sta seam_buffer, x
    inx

@NextMetatile:
    ldy temp2            ; Restore Y (level index)
    iny
    cpy #15              ; 15 metatiles per column
    bne @FillLoop

    ; Mark seam as ready for writing
    lda #$01
    sta seam_ready

@NothingToDraw:
    rts


; --- WriteSeam ---
; Writes the seam_buffer to the appropriate nametable column.
; Must be called during vblank.
;
; The column to write is col_to_draw (0-63).
; Columns 0-31 go to nametable 0, columns 32-63 go to nametable 1.

WriteSeam:
    ; Calculate PPU address for this column
    ; NT0 base = $2000, NT1 base = $2400
    ; Column within NT = col_to_draw & $1F

    lda col_to_draw
    and #$20             ; Check bit 5: 0 = NT0, 1 = NT1
    beq @UseNT0Seam
    lda #$24             ; Nametable 1
    jmp @SetSeamNT
@UseNT0Seam:
    lda #$20             ; Nametable 0
@SetSeamNT:
    sta temp2            ; High byte of PPU address

    lda col_to_draw
    and #$1F             ; Column within nametable (0-31)
    sta temp             ; Low byte of PPU address

    ; Set PPU address
    bit PPUSTATUS
    lda temp2
    sta PPUADDR
    lda temp
    sta PPUADDR

    ; Set PPUCTRL for vertical increment (+32 per write)
    lda #%10001100       ; NMI on, BG PT0, Sprites PT1, vert increment
    sta PPUCTRL

    ; Write 30 tiles from seam_buffer
    ldx #$00
@WriteLoop:
    lda seam_buffer, x
    sta PPUDATA
    inx
    cpx #30
    bne @WriteLoop

    ; Restore PPUCTRL to horizontal increment
    lda #PPUCTRL_VAL
    sta PPUCTRL
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

    ; Wrap frame index (3 run frames: 0-2)
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


; --- DrawLevel ---
; Draws the entire level to both nametables at startup.
; Uses metatile data to expand 16x16 metatiles into 8x8 NES tiles.
; Must be called during VBlank/rendering disabled.
;
; Nametable 0: $2000-$23BF (32x30 tiles = 960 bytes)
; Nametable 1: $2400-$27BF (with vertical mirroring, right of NT0)
;
; Level is 32 metatile columns x 15 metatile rows.
; Each metatile column = 2 tile columns, each metatile row = 2 tile rows.
; So: 64 tile columns x 30 tile rows total across both nametables.

DrawLevel:
    ; Point level_ptr to start of level data
    lda #<Level1Data
    sta level_ptr
    lda #>Level1Data
    sta level_ptr+1

    ; Process 32 metatile columns (16 per nametable)
    lda #$00
    sta col_count

@ColumnLoop:
    ; Calculate which nametable and column within it
    ; Metatile columns 0-15 -> Nametable 0, columns 0-31 (2 tiles each)
    ; Metatile columns 16-31 -> Nametable 1, columns 0-31

    ; Set PPU address for this column
    ; Base: $2000 (NT0) or $2400 (NT1)
    ; Column offset: (col_count & $0F) * 2

    lda col_count
    and #$10             ; Check if >= 16 (second nametable)
    beq @UseNT0
    lda #$24             ; Nametable 1 high byte
    jmp @SetNTHigh
@UseNT0:
    lda #$20             ; Nametable 0 high byte
@SetNTHigh:
    sta temp2            ; Store NT high byte

    ; Column offset within nametable: (col_count & $0F) * 2
    lda col_count
    and #$0F
    asl a                ; * 2 (each metatile is 2 tile columns)
    sta temp             ; Store column offset

    ; Draw left tile column of this metatile column (15 metatiles = 30 tiles)
    jsr @DrawMetatileColumnLeft

    ; Draw right tile column (offset by 1)
    inc temp
    jsr @DrawMetatileColumnRight

    ; Advance level pointer to next metatile column (15 bytes)
    lda level_ptr
    clc
    adc #15
    sta level_ptr
    lda level_ptr+1
    adc #$00
    sta level_ptr+1

    ; Next column
    inc col_count
    lda col_count
    cmp #32              ; 32 metatile columns total
    bne @ColumnLoop

    rts

; Draw left tile column of metatiles (top-left and bottom-left tiles)
@DrawMetatileColumnLeft:
    ; Set PPU address: high byte from temp2, low byte from temp
    bit PPUSTATUS
    lda temp2
    sta PPUADDR
    lda temp
    sta PPUADDR

    ; Set PPUCTRL for vertical increment (+32 per write)
    lda #%10001100       ; NMI on, BG PT0, Sprites PT1, vert increment
    sta PPUCTRL

    ; Write 30 tiles (15 metatiles * 2 rows each)
    ldy #$00             ; Index into level column data
@LeftTileLoop:
    ; Get metatile index
    lda (level_ptr), y
    asl a                ; * 4 (each metatile def is 4 bytes)
    asl a
    tax

    ; Write top-left tile
    lda MetatileDefs, x
    sta PPUDATA

    ; Write bottom-left tile (offset +2 in metatile def)
    lda MetatileDefs+2, x
    sta PPUDATA

    iny
    cpy #15              ; 15 metatiles per column
    bne @LeftTileLoop

    ; Restore PPUCTRL to horizontal increment
    lda #PPUCTRL_VAL
    sta PPUCTRL
    rts

; Draw right tile column of metatiles (top-right and bottom-right tiles)
@DrawMetatileColumnRight:
    ; Set PPU address: high byte from temp2, low byte from temp
    bit PPUSTATUS
    lda temp2
    sta PPUADDR
    lda temp
    sta PPUADDR

    ; Set PPUCTRL for vertical increment
    lda #%10001100
    sta PPUCTRL

    ; Write 30 tiles
    ldy #$00
@RightTileLoop:
    ; Get metatile index
    lda (level_ptr), y
    asl a
    asl a
    tax

    ; Write top-right tile (offset +1)
    lda MetatileDefs+1, x
    sta PPUDATA

    ; Write bottom-right tile (offset +3)
    lda MetatileDefs+3, x
    sta PPUDATA

    iny
    cpy #15
    bne @RightTileLoop

    ; Restore PPUCTRL
    lda #PPUCTRL_VAL
    sta PPUCTRL
    rts


; ---------------------------------------------------------------------------
; Read-Only Data
; ---------------------------------------------------------------------------

.segment "RODATA"

; --- Color Palette ---
; 32 bytes: 4 background palettes, then 4 sprite palettes.

PaletteData:
    ; Background palette 0 (level tiles: sky, ground, platforms)
    .byte $21       ; Universal background color: light blue (sky)
    .byte $19       ; Color 1: green (grass surface)
    .byte $07       ; Color 2: brown (ground fill)
    .byte $30       ; Color 3: white (platform highlight)

    ; Background palettes 1-3 (unused, copy of palette 0)
    .byte $21,$19,$07,$30
    .byte $21,$19,$07,$30
    .byte $21,$19,$07,$30

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
;   Frame 0 (stand/run1): tiles $00,$01 / $10,$11
;   Frame 1 (run2):       tiles $02,$03 / $12,$13
;   Frame 2 (run3):       tiles $04,$05 / $14,$15

MetaStand:
    .byte $00, $00, $00, $00   ; top-left
    .byte $00, $01, $00, $08   ; top-right
    .byte $08, $10, $00, $00   ; bot-left
    .byte $08, $11, $00, $08   ; bot-right
    .byte $80                   ; end

MetaRun1:
    .byte $00, $00, $00, $00   ; top-left
    .byte $00, $01, $00, $08   ; top-right
    .byte $08, $10, $00, $00   ; bot-left
    .byte $08, $11, $00, $08   ; bot-right
    .byte $80                   ; end

MetaRun2:
    .byte $00, $02, $00, $00   ; top-left
    .byte $00, $03, $00, $08   ; top-right
    .byte $08, $12, $00, $00   ; bot-left
    .byte $08, $13, $00, $08   ; bot-right
    .byte $80                   ; end

MetaRun3:
    .byte $00, $04, $00, $00   ; top-left
    .byte $00, $05, $00, $08   ; top-right
    .byte $08, $14, $00, $00   ; bot-left
    .byte $08, $15, $00, $08   ; bot-right
    .byte $80                   ; end

; --- Animation Frame Pointer Tables ---
; Used to look up the metasprite definition for each run frame.

RunFramesL:
    .byte <MetaRun1, <MetaRun2, <MetaRun3
RunFramesH:
    .byte >MetaRun1, >MetaRun2, >MetaRun3


; ---------------------------------------------------------------------------
; Level/Metatile Data
; ---------------------------------------------------------------------------

; Metatile definitions: each metatile is 2x2 NES tiles (16x16 pixels)
; Format: TL, TR, BL, BR (tile indices for top-left, top-right, bottom-left, bottom-right)
; Tile $00 = blank/sky, $1B = ground surface, $1C = ground fill, $1D = platform

METATILE_SKY    = 0
METATILE_GROUND = 1
METATILE_PLAT   = 2

MetatileDefs:
    ; Metatile 0: Sky (all blank)
    .byte $00, $00, $00, $00

    ; Metatile 1: Ground (surface on top, fill on bottom)
    .byte $1B, $1B, $1C, $1C

    ; Metatile 2: Platform (floating platform)
    .byte $1D, $1D, $00, $00

; Level data: 32 metatile columns x 15 metatile rows (covers 2 screens)
; Stored column-major for easy seam updates
; Each column is 15 bytes (15 metatiles from top to bottom)
; Row 0 = top of screen, Row 14 = bottom of screen
; Ground is at row 13-14 (last 2 rows = bottom 32 pixels)

; Shorthand for level data
SKY = METATILE_SKY
GND = METATILE_GROUND
PLT = METATILE_PLAT

Level1Data:
    ; Column 0-7: Ground only (start area)
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 0
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 1
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 2
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 3
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 4
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 5
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 6
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 7

    ; Column 8-11: Gap (pit)
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY  ; Col 8
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY  ; Col 9
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY  ; Col 10
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY  ; Col 11

    ; Column 12-15: Ground again with platform
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 12
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,PLT,SKY,SKY,SKY,GND,GND  ; Col 13
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,PLT,SKY,SKY,SKY,GND,GND  ; Col 14
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 15

    ; Column 16-23: Screen 2 - more ground with platforms
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 16
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 17
    .byte SKY,SKY,SKY,SKY,SKY,SKY,PLT,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 18
    .byte SKY,SKY,SKY,SKY,SKY,SKY,PLT,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 19
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 20
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 21
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 22
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 23

    ; Column 24-31: End area
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 24
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 25
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 26
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 27
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 28
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 29
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 30
    .byte SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,SKY,GND,GND  ; Col 31


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

; --- Tiles $1B-$1E: Platform tiles for level graphics ---

; --- Tile $1B: Ground surface top (grass-like pattern) ---
.byte %11111111,%11111111,%01010101,%10101010,%00000000,%00000000,%00000000,%00000000
.byte %00000000,%00000000,%10101010,%01010101,%11111111,%11111111,%11111111,%11111111

; --- Tile $1C: Ground fill (brick pattern) ---
.byte %11111111,%10000001,%10000001,%11111111,%11111111,%00010000,%00010000,%11111111
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tile $1D: Platform top (for floating platforms) ---
.byte %11111111,%11111111,%11000011,%11000011,%11000011,%11000011,%11111111,%11111111
.byte %00000000,%00000000,%00111100,%00111100,%00111100,%00111100,%00000000,%00000000

; --- Tile $1E: Sky decoration (cloud piece, optional) ---
.byte %00000000,%00000000,%00111100,%01111110,%11111111,%11111111,%01111110,%00111100
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Tiles $1F-$20: Unused padding ---
.res 32, $00

; --- Tile $21: ! ---
.byte %00011000,%00011000,%00011000,%00011000,%00011000,%00000000,%00011000,%00000000
.byte $00,$00,$00,$00,$00,$00,$00,$00

; --- Pad rest of Pattern Table 0 to exactly 4096 bytes ---
; Tiles $00-$21 = 34 tiles = 544 bytes. Need 4096 - 544 = 3552 bytes.
.res 3552, $00

; === Pattern Table 1 ($1000-$1FFF): Sprite tiles ===
; Mario poses extracted by tools/extract_gif_frames.py.
; See tile index layout in the metasprite definitions above.
.incbin "assets/chr/mario_sprites.chr"

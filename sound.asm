; ============================================================================
; Mecha Man - Sound Module
; ============================================================================
;
; Simple fire-and-forget sound effects using the NES APU.
;
; Usage from hello.asm:
;   1. Add `.include "sound.asm"` near the end of the CODE segment
;      (before the RODATA / VECTORS segments).
;   2. Call `jsr SoundInit` once during Reset, after RAM clear.
;   3. Call `jsr SFX_Jump` when the player jumps.
;   4. Call `jsr SFX_Land` when the player lands on the ground.
;   5. (Optional) Call `jsr SFX_Step` when a run-frame changes.
;
; Each SFX writes one set of APU registers and relies on the APU's
; length counter to stop the tone automatically. No per-frame update
; routine is required.
;
; APU register map (quick reference):
;   $4000  Pulse1 duty/loop/env/volume   DDLC VVVV
;   $4001  Pulse1 sweep                   EPPP NSSS
;   $4002  Pulse1 timer low               TTTT TTTT
;   $4003  Pulse1 length/timer high       LLLL LTTT
;   $4004-$4007  Pulse2 (same layout)
;   $400C  Noise  loop/env/volume         --LC VVVV
;   $400E  Noise  period/mode             M--- PPPP
;   $400F  Noise  length                  LLLL L---
;   $4015  Channel enable                 ---D NT21
;   $4017  Frame counter                  MI-- ----
; ============================================================================

; --- APU register constants ---
APU_PULSE1_CTRL   = $4000
APU_PULSE1_SWEEP  = $4001
APU_PULSE1_TLO    = $4002
APU_PULSE1_THI    = $4003
APU_PULSE2_CTRL   = $4004
APU_PULSE2_SWEEP  = $4005
APU_PULSE2_TLO    = $4006
APU_PULSE2_THI    = $4007
APU_NOISE_CTRL    = $400C
APU_NOISE_PERIOD  = $400E
APU_NOISE_LEN     = $400F
APU_STATUS        = $4015
APU_FRAME         = $4017


; --- SoundInit ---
; Powers up the APU and enables pulse1, pulse2, and noise channels.
; Call once during Reset after the RAM clear.

SoundInit:
    ; Silence all channels briefly
    lda #$00
    sta APU_STATUS

    ; Enable Pulse1 (bit 0), Pulse2 (bit 1), Noise (bit 3)
    lda #%00001011
    sta APU_STATUS

    ; Frame counter: 4-step mode, IRQ inhibited
    lda #%01000000
    sta APU_FRAME

    ; Zero the sweep units so pitch doesn't drift on first use
    lda #$08               ; Negative flag cleared, no shift = sweep disabled
    sta APU_PULSE1_SWEEP
    sta APU_PULSE2_SWEEP
    rts


; --- SFX_Jump ---
; A short rising "boop" on Pulse1. Medium duty, medium volume,
; with an upward sweep so the pitch rises during the ~0.1s sound.

SFX_Jump:
    ; Duty 50% (bit 7-6 = 10), length counter enabled (bit 5 = 0),
    ; constant volume (bit 4 = 1), volume = 8 (bits 3-0)
    lda #%10111000
    sta APU_PULSE1_CTRL

    ; Sweep: enabled, period=2, negate=0 (pitch rises), shift=3
    ; Rising pitch because lower timer value = higher frequency,
    ; and with negate=0 the sweep ADDS to the timer... wait, NES sweep
    ; with negate=0 actually shifts timer toward higher values =
    ; LOWER pitch. For a rising pitch we want negate=1.
    lda #%10000111         ; enable, period=0, negate=1, shift=7  -> rapid rise
    sta APU_PULSE1_SWEEP

    ; Starting timer = $0C0 (a mid-low pitch that will sweep up)
    lda #$C0
    sta APU_PULSE1_TLO
    ; Length counter index 1 = short, timer high = 0
    lda #%00001000
    sta APU_PULSE1_THI
    rts


; --- SFX_Land ---
; A brief low thud on the noise channel.

SFX_Land:
    ; Noise: length counter enabled, constant volume, vol = 6
    lda #%00010110
    sta APU_NOISE_CTRL

    ; Mode=0 (long noise), period index = $08 (low-ish rumble)
    lda #$08
    sta APU_NOISE_PERIOD

    ; Length counter index 1 = very short burst
    lda #%00001000
    sta APU_NOISE_LEN
    rts


; --- SFX_Step ---
; Tiny click on Pulse2 for footsteps. Safe to call rapidly; each call
; just retriggers the length counter.

SFX_Step:
    ; Duty 25%, length enabled, constant volume, vol = 3 (quiet)
    lda #%01110011
    sta APU_PULSE2_CTRL

    ; Disable sweep
    lda #$08
    sta APU_PULSE2_SWEEP

    ; High pitch tick (small timer value)
    lda #$40
    sta APU_PULSE2_TLO
    lda #%00001000         ; length index 1, timer hi = 0
    sta APU_PULSE2_THI
    rts

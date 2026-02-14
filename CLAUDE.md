# Mecha Man — NES Game Project

## Overview
NES game built in 6502 assembly. Side-scrolling platformer with a 2-screen level,
animated Mario character, metatile-based background, and dead zone camera.

## Toolchain
- **Assembler**: ca65 (cc65 suite v2.18)
- **Linker**: ld65 with nes.cfg
- **Build**: `make clean && make` → hello.nes (24KB NROM mapper 0 ROM)
- **Asset pipeline**: Python 3 + Pillow

## Architecture
- **hello.asm**: All game code — reset handler, NMI game loop, subroutines, data
- **nes.cfg**: Linker memory layout (16KB PRG + 8KB CHR, mapper 0, vertical mirroring)
- **CHR-ROM layout**:
  - Pattern Table 0 ($0000-$0FFF): Inline BG tiles (font A-Z, ground, platforms)
  - Pattern Table 1 ($1000-$1FFF): `.incbin "assets/chr/mario_sprites.chr"`
  - PPUCTRL = %10001000 (BG uses table 0, sprites use table 1)

## Current Features (as of session 07)
- 16x16 pixel metasprites (2x2 tiles, 4 hardware sprites per frame)
- Standing idle + 3-frame running animation
- Horizontal flip for left-facing (hardware bit + mirrored X offsets)
- Gravity and jumping (velocity-based physics)
- True horizontal scrolling (2-screen level, 512 pixels wide)
- Dead zone camera (player can move freely in center before camera follows)
- Metatile system (16x16 blocks reduce level data to 480 bytes)
- Dynamic seam updates (new columns drawn as they scroll into view)
- Column-major level data format (optimized for horizontal scrolling)
- Level with ground, pits, and floating platforms

## Asset Pipeline
```
python3 tools/extract_gif_frames.py assets/raw/mario_running.gif assets/raw/mario_poses.png
python3 tools/png2chr.py assets/raw/mario_poses.png assets/chr/mario_sprites.chr
make
```
- extract_gif_frames.py: Extracts frames from animated GIFs, outputs frame sheet
- png2chr.py: Converts PNG to NES CHR binary (sorts colors by brightness per tile)
- extract_poses.py: Picks specific sprites by index from source art
- extract_run_cycle.py: Specialized tool for run animation extraction

## Key Files
| File | Purpose |
|------|---------|
| hello.asm | Main source (all code + inline font CHR) |
| nes.cfg | Linker memory map (vertical mirroring for horiz. scroll) |
| Makefile | Build system |
| assets/chr/mario_sprites.chr | 4KB sprite CHR (Pattern Table 1) |
| assets/raw/mario_running.gif | Source animation |
| assets/raw/mario_poses.png | Extracted poses (128x128, 4 colors) |
| roms/ | Versioned ROM snapshots (v1-v7) |

## Session History
Detailed architecture notes and lessons learned live in `sessions/`:
- session-01: Hello World, PPU basics, iNES header, nametable
- session-02: Hardware sprites, OAM DMA, controller input, movement
- session-03: Asset pipeline (extract_sprites.py, png2chr.py), CHR format
- session-04: Metasprites, animation state machine, split pattern tables, H-flip
- session-05: Gravity and jumping, signed arithmetic, velocity physics
- session-06: Parallax scrolling, 6-frame animation, Mario sprite switch
- session-07: True horizontal scrolling, metatiles, dead zone camera, seam updates

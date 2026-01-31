# Mecha Man — NES Game Project

## Overview
NES game built in 6502 assembly. Currently displays "HELLO WORLD!" on the
background with an animated Mega Man character that runs left/right.

## Toolchain
- **Assembler**: ca65 (cc65 suite v2.18)
- **Linker**: ld65 with nes.cfg
- **Build**: `make clean && make` → hello.nes (24KB NROM mapper 0 ROM)
- **Asset pipeline**: Python 3 + Pillow

## Architecture
- **hello.asm**: All game code — reset handler, NMI game loop, subroutines, data
- **nes.cfg**: Linker memory layout (16KB PRG + 8KB CHR, mapper 0)
- **CHR-ROM layout**:
  - Pattern Table 0 ($0000-$0FFF): Inline font tiles (A-Z, !)
  - Pattern Table 1 ($1000-$1FFF): `.incbin "assets/chr/megaman_sprites.chr"`
  - PPUCTRL = %10001000 (BG uses table 0, sprites use table 1)

## Current Features (as of session 04)
- 16x24 pixel metasprites (2x3 tiles, 6 hardware sprites per frame)
- Standing idle + 3-frame running animation
- Horizontal flip for left-facing (hardware bit + mirrored X offsets)
- Animation state machine (facing_dir, anim_state, anim_frame, anim_timer)
- Subroutine-based NMI: ReadControllerSub, MovePlayer, UpdateAnimation, DrawMetasprite
- Left/right movement only (side-scroller style), 2px/frame

## Asset Pipeline
```
python3 tools/extract_poses.py assets/raw/8bitmegaman.png assets/raw/megaman_poses.png
python3 tools/png2chr.py assets/raw/megaman_poses.png assets/chr/megaman_sprites.chr
make
```
- extract_poses.py: Picks specific sprites by index from source art, packs into 128x128 PNG
- png2chr.py: Converts PNG to NES CHR binary (sorts colors by brightness per tile)
- extract_sprites.py: Bulk extractor (session 03, not used in current build)

## Key Files
| File | Purpose |
|------|---------|
| hello.asm | Main source (all code + inline font CHR) |
| nes.cfg | Linker memory map |
| Makefile | Build system |
| assets/chr/megaman_sprites.chr | 4KB sprite CHR (Pattern Table 1) |
| assets/raw/8bitmegaman.png | Source spritesheet (322x405) |
| assets/raw/megaman_poses.png | Extracted poses (128x128, 4 colors) |
| roms/ | Versioned ROM snapshots (v2=session02, v3=session04) |

## Session History
Detailed architecture notes and lessons learned live in `sessions/`:
- session-01: Hello World, PPU basics, iNES header, nametable
- session-02: Hardware sprites, OAM DMA, controller input, movement
- session-03: Asset pipeline (extract_sprites.py, png2chr.py), CHR format
- session-04: Metasprites, animation state machine, split pattern tables, H-flip

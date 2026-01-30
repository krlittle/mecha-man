#!/usr/bin/env python3
"""
png2chr.py - Convert a PNG spritesheet to NES CHR-ROM binary format.

Usage:
    python3 png2chr.py input.png output.chr

The input PNG should be:
    - 128 pixels wide (16 tiles across)
    - 128 pixels tall for 4KB (one pattern table, 256 tiles)
    - 256 pixels tall for 8KB (two pattern tables, 512 tiles)
    - Indexed color with at most 4 colors per 8x8 tile

Color mapping:
    The script sorts unique colors per tile by brightness and maps them
    to NES color indices 0-3, where index 0 is the darkest (typically
    the background/transparent color).

    To get predictable results, use an indexed-color PNG with exactly
    4 colors. The darkest color becomes index 0 (transparent for sprites).

Requirements:
    pip install Pillow
"""

import sys
from pathlib import Path

try:
    from PIL import Image
except ImportError:
    print("Error: Pillow is required. Install it with: pip install Pillow", file=sys.stderr)
    sys.exit(1)


def brightness(color):
    """Perceived brightness of an RGB or RGBA color."""
    r, g, b = color[0], color[1], color[2]
    return r * 0.299 + g * 0.587 + b * 0.114


def tile_to_chr(pixels, x, y):
    """Convert an 8x8 tile at (x, y) pixel offset to 16 bytes of NES CHR data."""
    # Collect unique colors in this tile
    colors = set()
    for row in range(8):
        for col in range(8):
            px = pixels[x + col, y + row]
            # Normalize RGBA to RGB for comparison
            rgb = px[:3] if len(px) >= 3 else (px[0], px[0], px[0])
            colors.add(rgb)

    if len(colors) > 4:
        tile_col = x // 8
        tile_row = y // 8
        print(
            f"Warning: Tile ({tile_col}, {tile_row}) has {len(colors)} colors "
            f"(max 4). Extra colors will be merged.",
            file=sys.stderr,
        )

    # Sort by brightness: darkest = index 0 (background/transparent)
    palette = sorted(colors, key=brightness)
    # If more than 4, keep only the 4 most distinct
    if len(palette) > 4:
        palette = palette[:4]
    color_to_index = {c: i for i, c in enumerate(palette)}

    plane0 = []  # Low bit plane
    plane1 = []  # High bit plane

    for row in range(8):
        low_byte = 0
        high_byte = 0
        for col in range(8):
            px = pixels[x + col, y + row]
            rgb = px[:3] if len(px) >= 3 else (px[0], px[0], px[0])
            # Find closest palette entry if color was merged
            if rgb in color_to_index:
                idx = color_to_index[rgb]
            else:
                idx = min(range(len(palette)), key=lambda i: sum(
                    (a - b) ** 2 for a, b in zip(rgb, palette[i])
                ))
            bit = 7 - col
            low_byte |= ((idx >> 0) & 1) << bit
            high_byte |= ((idx >> 1) & 1) << bit
        plane0.append(low_byte)
        plane1.append(high_byte)

    return bytes(plane0 + plane1)


def png_to_chr(input_path, output_path):
    img = Image.open(input_path).convert("RGBA")
    width, height = img.size

    if width != 128:
        print(f"Error: Image must be 128 pixels wide (got {width}).", file=sys.stderr)
        sys.exit(1)

    if height not in (128, 256):
        print(
            f"Error: Image must be 128 (4KB) or 256 (8KB) pixels tall (got {height}).",
            file=sys.stderr,
        )
        sys.exit(1)

    pixels = img.load()
    chr_data = bytearray()

    tiles_x = width // 8    # 16
    tiles_y = height // 8   # 16 or 32

    for ty in range(tiles_y):
        for tx in range(tiles_x):
            chr_data.extend(tile_to_chr(pixels, tx * 8, ty * 8))

    Path(output_path).write_bytes(chr_data)

    tile_count = tiles_x * tiles_y
    size_kb = len(chr_data) // 1024
    print(f"Converted {tile_count} tiles ({size_kb}KB) -> {output_path}")


def main():
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} input.png output.chr", file=sys.stderr)
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = sys.argv[2]

    if not Path(input_path).exists():
        print(f"Error: File not found: {input_path}", file=sys.stderr)
        sys.exit(1)

    png_to_chr(input_path, output_path)


if __name__ == "__main__":
    main()

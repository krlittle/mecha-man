#!/usr/bin/env python3
"""
extract_run_cycle.py - Extract run cycle using column-gap detection

Finds sprites by detecting vertical gaps of background color, which handles
the variable-width packing in megaman_tiles.png.
"""

import sys
import argparse
from collections import Counter
from pathlib import Path

try:
    from PIL import Image, ImageDraw
except ImportError:
    print("Error: Pillow required. Install with: pip install Pillow", file=sys.stderr)
    sys.exit(1)


def detect_background_color(img):
    pixels = img.load()
    w, h = img.size
    edge_colors = []
    for x in range(w):
        edge_colors.append(pixels[x, 0])
        edge_colors.append(pixels[x, h - 1])
    for y in range(h):
        edge_colors.append(pixels[0, y])
        edge_colors.append(pixels[w - 1, y])
    return Counter(edge_colors).most_common(1)[0][0]


def colors_match(c1, c2, threshold=30):
    return all(abs(a - b) <= threshold for a, b in zip(c1[:3], c2[:3]))


def find_column_gaps(img, bg_color, y_start, y_end):
    """Find x positions where entire column is background (gaps between sprites)."""
    pixels = img.load()
    w = img.size[0]
    gaps = []

    for x in range(w):
        is_gap = True
        for y in range(y_start, y_end):
            if not colors_match(pixels[x, y], bg_color):
                is_gap = False
                break
        if is_gap:
            gaps.append(x)

    return gaps


def find_sprites_in_row(img, bg_color, y_start, y_end):
    """Find sprite bounding boxes in a horizontal band."""
    pixels = img.load()
    w = img.size[0]

    # Find columns that have any non-background pixels
    col_has_content = []
    for x in range(w):
        has_content = False
        for y in range(y_start, y_end):
            if not colors_match(pixels[x, y], bg_color):
                has_content = True
                break
        col_has_content.append(has_content)

    # Find contiguous runs of content columns
    sprites = []
    in_sprite = False
    sprite_start = 0

    for x in range(w + 1):
        has_content = col_has_content[x] if x < w else False

        if has_content and not in_sprite:
            in_sprite = True
            sprite_start = x
        elif not has_content and in_sprite:
            in_sprite = False
            # Find actual y bounds within this x range
            min_y, max_y = y_end, y_start
            for sx in range(sprite_start, x):
                for sy in range(y_start, y_end):
                    if not colors_match(pixels[sx, sy], bg_color):
                        min_y = min(min_y, sy)
                        max_y = max(max_y, sy)

            if max_y >= min_y:
                sprites.append((sprite_start, min_y, x - sprite_start, max_y - min_y + 1))

    return sprites


def collect_colors(img, regions, bg_color):
    pixels = img.load()
    color_counts = Counter()
    for x, y, w, h in regions:
        for py in range(y, min(y + h, img.size[1])):
            for px in range(x, min(x + w, img.size[0])):
                c = pixels[px, py]
                if not colors_match(c, bg_color):
                    color_counts[c[:3]] += 1
    return color_counts


def build_palette(color_counts):
    if not color_counts:
        return [(0, 0, 0), (24, 24, 24), (128, 128, 128), (255, 255, 255)]

    dark, light = [], []
    for color, count in color_counts.items():
        bri = color[0] * 0.299 + color[1] * 0.587 + color[2] * 0.114
        (dark if bri < 50 else light).append((color, count))

    outline = (24, 24, 24)
    if dark:
        dark.sort(key=lambda x: -x[1])
        outline = dark[0][0]
        if outline[0] * 0.299 + outline[1] * 0.587 + outline[2] * 0.114 < 20:
            outline = (24, 24, 24)

    light.sort(key=lambda x: -x[1])
    fills = [c for c, _ in light[:2]]
    while len(fills) < 2:
        fills.append((128, 128, 128))
    fills.sort(key=lambda c: c[0] * 0.299 + c[1] * 0.587 + c[2] * 0.114)

    return [(0, 0, 0), outline, fills[0], fills[1]]


def find_closest(color, palette):
    best_idx, best_dist = 1, float("inf")
    for i in range(1, len(palette)):
        dist = sum((a - b) ** 2 for a, b in zip(color[:3], palette[i]))
        if dist < best_dist:
            best_dist, best_idx = dist, i
    return best_idx


POSE_WIDTH = 16
POSE_HEIGHT = 24


def extract_region(img, region, bg_color, palette):
    rx, ry, rw, rh = region
    pixels = img.load()

    # Find actual bounds
    min_x, min_y = rw, rh
    max_x, max_y = 0, 0
    for y in range(rh):
        for x in range(rw):
            px, py = rx + x, ry + y
            if px < img.size[0] and py < img.size[1]:
                if not colors_match(pixels[px, py], bg_color):
                    min_x, min_y = min(min_x, x), min(min_y, y)
                    max_x, max_y = max(max_x, x), max(max_y, y)

    if max_x < min_x:
        return []

    sw, sh = max_x - min_x + 1, max_y - min_y + 1
    offset_x = max(0, (POSE_WIDTH - sw) // 2)
    offset_y = max(0, (POSE_HEIGHT - sh) // 2)
    crop_x = max(0, (sw - POSE_WIDTH) // 2)
    crop_y = max(0, (sh - POSE_HEIGHT) // 2)
    draw_w, draw_h = min(sw, POSE_WIDTH), min(sh, POSE_HEIGHT)

    result = []
    for y in range(draw_h):
        for x in range(draw_w):
            src_x = rx + min_x + crop_x + x
            src_y = ry + min_y + crop_y + y
            if src_x < img.size[0] and src_y < img.size[1]:
                c = pixels[src_x, src_y]
                idx = 0 if colors_match(c, bg_color) else find_closest(c[:3], palette)
                dest_x, dest_y = offset_x + x, offset_y + y
                if dest_x < POSE_WIDTH and dest_y < POSE_HEIGHT:
                    result.append((dest_x, dest_y, idx))
    return result


def pack_poses(poses, palette):
    canvas_w, canvas_h = 128, 48
    bg_rgba = palette[0] + (255,)
    out = Image.new("RGBA", (canvas_w, canvas_h), bg_rgba)
    out_pixels = out.load()

    for pose_idx, pose_pixels in enumerate(poses):
        base_x = (pose_idx % 8) * POSE_WIDTH
        base_y = (pose_idx // 8) * 24

        for px, py, color_idx in pose_pixels:
            x, y = base_x + px, base_y + py
            if 0 <= x < canvas_w and 0 <= y < canvas_h:
                out_pixels[x, y] = palette[color_idx] + (255,)
    return out


def show_grid(img, regions, row_bounds):
    out = img.copy()
    draw = ImageDraw.Draw(out)
    colors = [(255, 0, 0), (0, 255, 0), (0, 0, 255), (255, 255, 0),
              (255, 0, 255), (0, 255, 255), (255, 128, 0), (128, 0, 255),
              (0, 128, 255), (255, 255, 255)]

    # Draw row bounds
    for y_start, y_end in row_bounds:
        draw.line([(0, y_start), (img.size[0], y_start)], fill=(128, 128, 128))
        draw.line([(0, y_end), (img.size[0], y_end)], fill=(128, 128, 128))

    # Draw sprite boxes
    for i, (x, y, w, h) in enumerate(regions):
        c = colors[i % len(colors)]
        draw.rectangle([x, y, x + w - 1, y + h - 1], outline=c, width=2)
        draw.text((x + 2, y + 2), str(i), fill=c)
    return out


def main():
    parser = argparse.ArgumentParser(description="Extract run cycle with gap detection")
    parser.add_argument("input", help="Input PNG")
    parser.add_argument("output", help="Output PNG")
    parser.add_argument("--show-grid", action="store_true")
    parser.add_argument("--row-height", type=int, default=32,
                        help="Row height for sprite detection (default: 32)")
    parser.add_argument("--stand", type=int, default=None,
                        help="Override standing pose index")
    parser.add_argument("--list", action="store_true",
                        help="List detected sprites and exit")
    args = parser.parse_args()

    if not Path(args.input).exists():
        print(f"Error: File not found: {args.input}", file=sys.stderr)
        sys.exit(1)

    img = Image.open(args.input).convert("RGBA")
    print(f"Input: {img.size[0]}x{img.size[1]}")

    bg_color = detect_background_color(img)
    print(f"Background: RGB{bg_color[:3]}")

    # Detect sprites row by row
    all_sprites = []
    row_bounds = []
    y = 0
    row_idx = 0

    while y < img.size[1]:
        y_end = min(y + args.row_height, img.size[1])
        sprites = find_sprites_in_row(img, bg_color, y, y_end)

        if sprites:
            row_bounds.append((y, y_end))
            print(f"Row {row_idx} (y={y}-{y_end}): {len(sprites)} sprites")
            for i, (sx, sy, sw, sh) in enumerate(sprites):
                global_idx = len(all_sprites)
                print(f"  #{global_idx}: pos=({sx},{sy}) size={sw}x{sh}")
                all_sprites.append((sx, sy, sw, sh))
            row_idx += 1

        y = y_end

    if args.list:
        sys.exit(0)

    # Select sprites: first 9 for run, find a standing pose
    if len(all_sprites) < 9:
        print(f"Error: Need at least 9 sprites for run cycle, found {len(all_sprites)}")
        sys.exit(1)

    run_sprites = all_sprites[:9]

    # Find standing pose - look for a sprite that's roughly 21x24 in later rows
    stand_idx = args.stand
    if stand_idx is None:
        for i in range(9, len(all_sprites)):
            sx, sy, sw, sh = all_sprites[i]
            if 18 <= sw <= 24 and 22 <= sh <= 26:
                stand_idx = i
                break
        if stand_idx is None:
            stand_idx = 9 if len(all_sprites) > 9 else 0

    stand_sprite = all_sprites[stand_idx]
    selected = [stand_sprite] + run_sprites
    names = ["Stand"] + [f"Run{i}" for i in range(1, 10)]

    print(f"\nSelected:")
    print(f"  Stand: #{stand_idx} at {stand_sprite}")
    for i, s in enumerate(run_sprites):
        print(f"  Run{i+1}: #{i} at {s}")

    if args.show_grid:
        debug_img = show_grid(img, selected, row_bounds)
        debug_path = args.output.replace(".png", "_grid.png")
        debug_img.save(debug_path)
        print(f"Debug: {debug_path}")

    # Build palette and extract
    color_counts = collect_colors(img, selected, bg_color)
    palette = build_palette(color_counts)
    print(f"\nPalette: {palette}")

    poses = []
    for name, region in zip(names, selected):
        pose = extract_region(img, region, bg_color, palette)
        poses.append(pose)
        print(f"  {name}: {len(pose)} px")

    out = pack_poses(poses, palette)
    out.save(args.output)
    print(f"\nOutput: {args.output}")


if __name__ == "__main__":
    main()

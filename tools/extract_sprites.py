#!/usr/bin/env python3
"""
extract_sprites.py - Extract sprites from an irregularly-laid-out spritesheet
and pack them onto an NES-ready 128-pixel-wide grid.

Usage:
    python3 extract_sprites.py input.png output.png

The script will:
    1. Auto-detect the background color
    2. Find all connected sprite regions
    3. Reduce colors to a global 4-color palette
    4. Pack sprites onto a 128-pixel-wide grid aligned to 8x8 tiles
    5. Output a PNG ready for png2chr.py

Requirements:
    pip install Pillow
"""

import sys
from collections import Counter, deque
from pathlib import Path

try:
    from PIL import Image
except ImportError:
    print("Error: Pillow is required. Install it with: pip install Pillow", file=sys.stderr)
    sys.exit(1)


def detect_background_color(img):
    """Detect the background color by sampling the image edges."""
    pixels = img.load()
    w, h = img.size
    edge_colors = []

    # Sample all four edges
    for x in range(w):
        edge_colors.append(pixels[x, 0])
        edge_colors.append(pixels[x, h - 1])
    for y in range(h):
        edge_colors.append(pixels[0, y])
        edge_colors.append(pixels[w - 1, y])

    # Most common edge color is the background
    counter = Counter(edge_colors)
    bg_color = counter.most_common(1)[0][0]
    return bg_color


def colors_match(c1, c2, threshold=30):
    """Check if two RGBA colors are close enough to be considered the same."""
    return all(abs(a - b) <= threshold for a, b in zip(c1[:3], c2[:3]))


def find_sprites(img, bg_color):
    """Find bounding boxes of all connected non-background regions."""
    pixels = img.load()
    w, h = img.size
    visited = [[False] * h for _ in range(w)]
    sprites = []

    for y in range(h):
        for x in range(w):
            if visited[x][y]:
                continue
            if colors_match(pixels[x, y], bg_color):
                visited[x][y] = True
                continue

            # BFS to find connected region
            queue = deque([(x, y)])
            visited[x][y] = True
            min_x, min_y = x, y
            max_x, max_y = x, y

            while queue:
                cx, cy = queue.popleft()
                min_x = min(min_x, cx)
                min_y = min(min_y, cy)
                max_x = max(max_x, cx)
                max_y = max(max_y, cy)

                for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1),
                               (-1, -1), (-1, 1), (1, -1), (1, 1)]:
                    nx, ny = cx + dx, cy + dy
                    if 0 <= nx < w and 0 <= ny < h and not visited[nx][ny]:
                        visited[nx][ny] = True
                        if not colors_match(pixels[nx, ny], bg_color):
                            queue.append((nx, ny))

            bw = max_x - min_x + 1
            bh = max_y - min_y + 1

            # Filter out noise and non-sprite junk (thin lines, small dots)
            if bw >= 8 and bh >= 8:
                sprites.append((min_x, min_y, bw, bh))

    # Sort top-to-bottom, left-to-right
    sprites.sort(key=lambda s: (s[1] // 16, s[0]))
    return sprites


def collect_colors(img, sprites, bg_color):
    """Collect all non-background colors from detected sprite regions.
    Excludes colors that are too close to the background."""
    pixels = img.load()
    color_counts = Counter()

    for sx, sy, sw, sh in sprites:
        for y in range(sy, sy + sh):
            for x in range(sx, sx + sw):
                px = pixels[x, y]
                if not colors_match(px, bg_color):
                    rgb = px[:3]
                    # Also exclude colors very similar to background
                    bg_dist = sum((a - b) ** 2 for a, b in zip(rgb, bg_color[:3]))
                    if bg_dist > 95 ** 2:
                        color_counts[rgb] += 1

    return color_counts


def median_cut(colors_with_counts, n):
    """Reduce colors to n representatives using median cut algorithm."""
    if len(colors_with_counts) <= n:
        return [c for c, _ in colors_with_counts]

    # Start with all colors in one bucket
    buckets = [list(colors_with_counts)]

    while len(buckets) < n:
        # Find the bucket with the widest color range
        best_idx = 0
        best_range = -1
        best_channel = 0

        for i, bucket in enumerate(buckets):
            if len(bucket) <= 1:
                continue
            for ch in range(3):
                vals = [c[0][ch] for c in bucket]
                r = max(vals) - min(vals)
                if r > best_range:
                    best_range = r
                    best_idx = i
                    best_channel = ch

        if best_range <= 0:
            break

        # Split the widest bucket along its widest channel
        bucket = buckets[best_idx]
        bucket.sort(key=lambda c: c[0][best_channel])
        mid = len(bucket) // 2
        buckets[best_idx] = bucket[:mid]
        buckets.append(bucket[mid:])

    # Average each bucket (weighted by count) to get representative colors
    result = []
    for bucket in buckets:
        if not bucket:
            continue
        total_weight = sum(count for _, count in bucket)
        avg = tuple(
            int(sum(c[ch] * count for c, count in bucket) / total_weight)
            for ch in range(3)
        )
        result.append(avg)

    return result


def find_closest(color, palette, skip_bg=True):
    """Find the closest palette color by Euclidean distance.
    If skip_bg is True, never map non-background pixels to index 0."""
    best_idx = 1 if skip_bg else 0
    best_dist = float("inf")
    start = 1 if skip_bg else 0
    for i in range(start, len(palette)):
        dist = sum((a - b) ** 2 for a, b in zip(color[:3], palette[i]))
        if dist < best_dist:
            best_dist = dist
            best_idx = i
    return best_idx


def build_palette(img, sprites, bg_color):
    """Build a global 4-color palette: background + 3 most representative colors.

    Uses a two-pass approach:
    1. Separate colors into dark (outlines) and light (fills/highlights)
    2. Pick the most common dark color and split the lights into 2 groups
    This preserves the outline vs. fill contrast that NES sprites need.
    """
    color_counts = collect_colors(img, sprites, bg_color)

    if not color_counts:
        return [bg_color[:3], (255, 255, 255), (128, 128, 128), (64, 64, 64)]

    # Split into dark (outline) and light (fill) colors
    dark_colors = []
    light_colors = []
    for color, count in color_counts.items():
        bri = color[0] * 0.299 + color[1] * 0.587 + color[2] * 0.114
        if bri < 50:
            dark_colors.append((color, count))
        else:
            light_colors.append((color, count))

    # Pick the dominant dark color (outline)
    if dark_colors:
        dark_colors.sort(key=lambda x: -x[1])
        outline = dark_colors[0][0]
    else:
        outline = (0, 0, 0)

    # Split the light colors into 2 groups
    if len(light_colors) >= 2:
        representatives = median_cut(light_colors, 2)
    elif light_colors:
        representatives = [light_colors[0][0]]
    else:
        representatives = [(128, 128, 128)]

    # Pad if fewer than 2
    while len(representatives) < 2:
        representatives.append(representatives[-1])

    # Sort by brightness
    representatives.sort(key=lambda c: c[0] * 0.299 + c[1] * 0.587 + c[2] * 0.114)

    # Index 0 = background, 1 = dark outline, 2-3 = fill colors (dark to light)
    palette = [bg_color[:3], outline] + representatives
    return palette


def pad_to_8(n):
    """Round up to the next multiple of 8."""
    return ((n + 7) // 8) * 8


def pack_sprites(img, sprites, bg_color, palette):
    """Extract, quantize, and pack sprites onto a 128-wide canvas."""
    src_pixels = img.load()
    canvas_width = 128

    # Calculate padded sizes and plan layout
    packed = []
    for sx, sy, sw, sh in sprites:
        pw = pad_to_8(sw)
        ph = pad_to_8(sh)
        packed.append((sx, sy, sw, sh, pw, ph))

    # Simple row packing: place sprites left-to-right, wrap to next row
    rows = []
    current_row = []
    row_x = 0
    row_height = 0

    for entry in packed:
        _, _, _, _, pw, ph = entry
        if row_x + pw > canvas_width and current_row:
            rows.append((current_row, row_height))
            current_row = []
            row_x = 0
            row_height = 0

        current_row.append((entry, row_x))
        row_x += pw
        row_height = max(row_height, ph)

    if current_row:
        rows.append((current_row, row_height))

    # Calculate total height
    total_height = sum(rh for _, rh in rows)
    canvas_height = pad_to_8(total_height)

    # Clamp to valid NES CHR sizes
    if canvas_height <= 128:
        canvas_height = 128
    elif canvas_height <= 256:
        canvas_height = 256
    else:
        print(
            f"Warning: Packed sprites need {canvas_height}px tall canvas. "
            f"Clamping to 256 (some sprites may be cut off).",
            file=sys.stderr,
        )
        canvas_height = 256

    # Create output image with background color
    bg_rgba = palette[0] + (255,) if len(palette[0]) == 3 else palette[0]
    out = Image.new("RGBA", (canvas_width, canvas_height), bg_rgba)
    out_pixels = out.load()

    # Draw sprites
    dest_y = 0
    sprite_count = 0

    for row_entries, row_height in rows:
        for (sx, sy, sw, sh, pw, ph), dest_x in row_entries:
            if dest_y + ph > canvas_height:
                break

            for y in range(sh):
                for x in range(sw):
                    px = src_pixels[sx + x, sy + y]
                    if colors_match(px, bg_color):
                        idx = 0
                    else:
                        idx = find_closest(px[:3], palette)

                    color = palette[idx]
                    out_pixels[dest_x + x, dest_y + y] = color + (255,)

            sprite_count += 1

        dest_y += row_height

    return out, sprite_count


def main():
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} input.png output.png", file=sys.stderr)
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = sys.argv[2]

    if not Path(input_path).exists():
        print(f"Error: File not found: {input_path}", file=sys.stderr)
        sys.exit(1)

    # Load image
    img = Image.open(input_path).convert("RGBA")
    print(f"Input: {img.size[0]}x{img.size[1]}")

    # Step 1: Detect background
    bg_color = detect_background_color(img)
    print(f"Background color: RGB{bg_color[:3]}")

    # Step 2: Find sprites
    sprites = find_sprites(img, bg_color)
    print(f"Found {len(sprites)} sprite regions")

    if not sprites:
        print("Error: No sprites found in image.", file=sys.stderr)
        sys.exit(1)

    for i, (sx, sy, sw, sh) in enumerate(sprites):
        print(f"  #{i:3d}: pos=({sx},{sy}) size={sw}x{sh} -> padded {pad_to_8(sw)}x{pad_to_8(sh)}")

    # Step 3: Build palette
    palette = build_palette(img, sprites, bg_color)
    print(f"Palette: {['RGB' + str(c) for c in palette]}")

    # Step 4 & 5: Quantize and pack
    out_img, sprite_count = pack_sprites(img, sprites, bg_color, palette)
    print(f"Output: {out_img.size[0]}x{out_img.size[1]} ({sprite_count} sprites packed)")

    # Step 6: Save
    out_img.save(output_path)
    print(f"Saved: {output_path}")


if __name__ == "__main__":
    main()

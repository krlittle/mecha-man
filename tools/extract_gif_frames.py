#!/usr/bin/env python3
"""
extract_gif_frames.py - Extract frames from an animated GIF and pack them
into a 128x128 NES-ready PNG with a known tile layout.

Usage:
    python3 extract_gif_frames.py input.gif output.png

The script extracts all frames from the GIF, reduces colors to 4 (NES limit),
and packs them into a 128-pixel-wide grid suitable for png2chr.py.

For a 16x16 sprite with N frames, output tile layout:
    Each frame occupies 2x2 tiles (16x16 pixels).
    Frames are packed left-to-right, then top-to-bottom.

Requirements:
    pip install Pillow
"""

import sys
from pathlib import Path
from collections import Counter

try:
    from PIL import Image
except ImportError:
    print("Error: Pillow is required. Install it with: pip install Pillow", file=sys.stderr)
    sys.exit(1)


def brightness(color):
    """Perceived brightness of an RGB color."""
    r, g, b = color[0], color[1], color[2]
    return r * 0.299 + g * 0.587 + b * 0.114


def get_gif_frames(gif_path):
    """Extract all frames from an animated GIF."""
    img = Image.open(gif_path)
    frames = []

    try:
        while True:
            # Convert frame to RGBA
            frame = img.convert("RGBA")
            frames.append(frame.copy())
            img.seek(img.tell() + 1)
    except EOFError:
        pass

    return frames


def detect_background_color(frames):
    """Detect background color by finding the most common color at frame edges."""
    edge_colors = Counter()

    for frame in frames:
        pixels = frame.load()
        w, h = frame.size

        # Sample edges
        for x in range(w):
            edge_colors[pixels[x, 0][:3]] += 1
            edge_colors[pixels[x, h-1][:3]] += 1
        for y in range(h):
            edge_colors[pixels[0, y][:3]] += 1
            edge_colors[pixels[w-1, y][:3]] += 1

    return edge_colors.most_common(1)[0][0]


def find_sprite_bounds(frame, bg_color, threshold=30):
    """Find the bounding box of non-background pixels in a frame."""
    pixels = frame.load()
    w, h = frame.size

    min_x, min_y = w, h
    max_x, max_y = 0, 0

    for y in range(h):
        for x in range(w):
            px = pixels[x, y][:3]
            # Check if pixel differs from background
            if not all(abs(a - b) <= threshold for a, b in zip(px, bg_color)):
                min_x = min(min_x, x)
                min_y = min(min_y, y)
                max_x = max(max_x, x)
                max_y = max(max_y, y)

    if min_x > max_x:
        return None

    return (min_x, min_y, max_x - min_x + 1, max_y - min_y + 1)


def collect_all_colors(frames, bg_color, threshold=30):
    """Collect all non-background colors from all frames."""
    color_counts = Counter()

    for frame in frames:
        pixels = frame.load()
        w, h = frame.size

        for y in range(h):
            for x in range(w):
                px = pixels[x, y][:3]
                if not all(abs(a - b) <= threshold for a, b in zip(px, bg_color)):
                    color_counts[px] += 1

    return color_counts


def median_cut(colors_with_counts, n):
    """Reduce colors to n representatives using median cut."""
    if len(colors_with_counts) <= n:
        return [c for c, _ in colors_with_counts]

    buckets = [list(colors_with_counts)]

    while len(buckets) < n:
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

        bucket = buckets[best_idx]
        bucket.sort(key=lambda c: c[0][best_channel])
        mid = len(bucket) // 2
        buckets[best_idx] = bucket[:mid]
        buckets.append(bucket[mid:])

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


def build_palette(frames, bg_color):
    """Build a 4-color palette: background + 3 representative colors."""
    color_counts = collect_all_colors(frames, bg_color)

    if not color_counts:
        return [(0, 0, 0), (64, 64, 64), (128, 128, 128), (255, 255, 255)]

    # Separate dark (outline) and light colors
    dark_colors = []
    light_colors = []

    for color, count in color_counts.items():
        bri = brightness(color)
        if bri < 50:
            dark_colors.append((color, count))
        else:
            light_colors.append((color, count))

    # Get darkest color for outline
    if dark_colors:
        dark_colors.sort(key=lambda x: brightness(x[0]))
        outline = dark_colors[0][0]
        # Ensure it's distinct from pure black
        if brightness(outline) < 20:
            outline = (24, 24, 24)
    else:
        outline = (24, 24, 24)

    # Get 2 representative colors from light colors
    if len(light_colors) >= 2:
        representatives = median_cut(light_colors, 2)
    elif light_colors:
        representatives = [light_colors[0][0], light_colors[0][0]]
    else:
        representatives = [(128, 128, 128), (255, 255, 255)]

    while len(representatives) < 2:
        representatives.append(representatives[-1])

    # Sort by brightness
    representatives.sort(key=brightness)

    # Index 0 = black (transparent), 1 = outline, 2 = mid, 3 = light
    return [(0, 0, 0), outline, representatives[0], representatives[1]]


def find_closest_color(color, palette, bg_color, threshold=30):
    """Find closest palette color, returning 0 for background."""
    # Check if it's background
    if all(abs(a - b) <= threshold for a, b in zip(color, bg_color)):
        return 0

    best_idx = 1
    best_dist = float("inf")

    for i in range(1, len(palette)):
        dist = sum((a - b) ** 2 for a, b in zip(color, palette[i]))
        if dist < best_dist:
            best_dist = dist
            best_idx = i

    return best_idx


def pack_frames(frames, bg_color, palette, sprite_size):
    """Pack all frames onto a 128x128 canvas.

    Layout for 16x16 sprites:
        8 frames per row (128 / 16 = 8)
        8 rows available (128 / 16 = 8)
        = 64 frames max
    """
    canvas_w, canvas_h = 128, 128
    frame_w, frame_h = sprite_size

    frames_per_row = canvas_w // frame_w

    bg_rgba = palette[0] + (255,)
    out = Image.new("RGBA", (canvas_w, canvas_h), bg_rgba)
    out_pixels = out.load()

    for frame_idx, frame in enumerate(frames):
        row = frame_idx // frames_per_row
        col = frame_idx % frames_per_row

        base_x = col * frame_w
        base_y = row * frame_h

        if base_y + frame_h > canvas_h:
            print(f"Warning: Too many frames, stopping at frame {frame_idx}", file=sys.stderr)
            break

        # Find sprite bounds in this frame
        bounds = find_sprite_bounds(frame, bg_color)
        if bounds is None:
            continue

        sx, sy, sw, sh = bounds

        # Center sprite in the frame cell
        offset_x = max(0, (frame_w - sw) // 2)
        offset_y = max(0, (frame_h - sh) // 2)

        frame_pixels = frame.load()

        for y in range(min(sh, frame_h)):
            for x in range(min(sw, frame_w)):
                src_x = sx + x
                src_y = sy + y

                if src_x >= frame.size[0] or src_y >= frame.size[1]:
                    continue

                px = frame_pixels[src_x, src_y][:3]
                color_idx = find_closest_color(px, palette, bg_color)

                dest_x = base_x + offset_x + x
                dest_y = base_y + offset_y + y

                if dest_x < canvas_w and dest_y < canvas_h:
                    out_pixels[dest_x, dest_y] = palette[color_idx] + (255,)

    return out


def main():
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} input.gif output.png", file=sys.stderr)
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = sys.argv[2]

    if not Path(input_path).exists():
        print(f"Error: File not found: {input_path}", file=sys.stderr)
        sys.exit(1)

    print(f"Input: {input_path}")

    # Extract frames
    frames = get_gif_frames(input_path)
    print(f"Extracted {len(frames)} frames")

    if not frames:
        print("Error: No frames found in GIF", file=sys.stderr)
        sys.exit(1)

    # Analyze frames
    frame_size = frames[0].size
    print(f"Frame size: {frame_size[0]}x{frame_size[1]}")

    bg_color = detect_background_color(frames)
    print(f"Background color: RGB{bg_color}")

    # Find actual sprite bounds
    all_bounds = [find_sprite_bounds(f, bg_color) for f in frames]
    valid_bounds = [b for b in all_bounds if b is not None]

    if valid_bounds:
        max_w = max(b[2] for b in valid_bounds)
        max_h = max(b[3] for b in valid_bounds)
        print(f"Max sprite size: {max_w}x{max_h}")

        # Round up to tile boundaries (8px)
        sprite_w = ((max_w + 7) // 8) * 8
        sprite_h = ((max_h + 7) // 8) * 8
    else:
        sprite_w, sprite_h = 16, 16

    print(f"Using sprite cell size: {sprite_w}x{sprite_h} ({sprite_w//8}x{sprite_h//8} tiles)")

    # Build palette
    palette = build_palette(frames, bg_color)
    print(f"\nPalette:")
    for i, c in enumerate(palette):
        print(f"  Index {i}: RGB{c}")

    # Pack frames
    out = pack_frames(frames, bg_color, palette, (sprite_w, sprite_h))
    out.save(output_path)
    print(f"\nOutput: {output_path} (128x128)")

    # Print tile info
    frames_per_row = 128 // sprite_w
    tiles_per_frame_x = sprite_w // 8
    tiles_per_frame_y = sprite_h // 8
    tiles_per_frame = tiles_per_frame_x * tiles_per_frame_y

    print(f"\nTile layout info:")
    print(f"  Frames per row: {frames_per_row}")
    print(f"  Tiles per frame: {tiles_per_frame_x}x{tiles_per_frame_y} = {tiles_per_frame}")
    print(f"  Total frames: {len(frames)}")

    print(f"\nTile indices for each frame (in Pattern Table):")
    for i in range(min(len(frames), 8)):
        base_col = i * tiles_per_frame_x
        tiles = []
        for row in range(tiles_per_frame_y):
            row_tiles = []
            for col in range(tiles_per_frame_x):
                tile_idx = row * 16 + base_col + col  # 16 tiles per row in 128px
                row_tiles.append(f"${tile_idx:02X}")
            tiles.append(",".join(row_tiles))
        print(f"  Frame {i}: {' / '.join(tiles)}")


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""
extract_poses.py - Extract specific Mega Man poses from a source spritesheet
and pack them into a 128x128 NES-ready PNG with a known tile layout.

Usage:
    python3 extract_poses.py input.png output.png [--stand N] [--run N N N]

The script detects all sprites in the source image (same BFS algorithm as
extract_sprites.py), then picks specific poses by sprite index. Each pose
is centered in a 16x24 pixel box (2x3 NES tiles) and packed onto a
128-pixel-wide grid.

Output tile layout (Pattern Table 1):
    Cols:    0-1     2-3     4-5     6-7
    Row 0-2: Stand   Run1    Run2    Run3

    Tile indices (tile = row*16 + col):
    Stand: $00,$01, $10,$11, $20,$21
    Run1:  $02,$03, $12,$13, $22,$23
    Run2:  $04,$05, $14,$15, $24,$25
    Run3:  $06,$07, $16,$17, $26,$27

Requirements:
    pip install Pillow
"""

import sys
import argparse
from collections import Counter, deque
from pathlib import Path

try:
    from PIL import Image
except ImportError:
    print("Error: Pillow is required. Install it with: pip install Pillow", file=sys.stderr)
    sys.exit(1)


# --- Sprite detection (reused from extract_sprites.py) ---

def detect_background_color(img):
    """Detect the background color by sampling image edges."""
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
    """Check if two RGBA colors are close enough."""
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
            if bw >= 8 and bh >= 8:
                sprites.append((min_x, min_y, bw, bh))

    sprites.sort(key=lambda s: (s[1] // 16, s[0]))
    return sprites


# --- Color palette ---

def collect_colors(img, sprites, bg_color):
    """Collect all non-background colors from sprite regions."""
    pixels = img.load()
    color_counts = Counter()
    for sx, sy, sw, sh in sprites:
        for y in range(sy, sy + sh):
            for x in range(sx, sx + sw):
                px = pixels[x, y]
                if not colors_match(px, bg_color):
                    rgb = px[:3]
                    bg_dist = sum((a - b) ** 2 for a, b in zip(rgb, bg_color[:3]))
                    if bg_dist > 95 ** 2:
                        color_counts[rgb] += 1
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


def build_palette(img, sprites, bg_color):
    """Build a global 4-color palette: background + 3 representative colors."""
    color_counts = collect_colors(img, sprites, bg_color)
    if not color_counts:
        return [(0, 0, 0), (0, 0, 0), (128, 128, 128), (255, 255, 255)]

    dark_colors = []
    light_colors = []
    for color, count in color_counts.items():
        bri = color[0] * 0.299 + color[1] * 0.587 + color[2] * 0.114
        if bri < 50:
            dark_colors.append((color, count))
        else:
            light_colors.append((color, count))

    outline = dark_colors[0][0] if dark_colors else (0, 0, 0)
    if dark_colors:
        dark_colors.sort(key=lambda x: -x[1])
        outline = dark_colors[0][0]

    if len(light_colors) >= 2:
        representatives = median_cut(light_colors, 2)
    elif light_colors:
        representatives = [light_colors[0][0]]
    else:
        representatives = [(128, 128, 128)]

    while len(representatives) < 2:
        representatives.append(representatives[-1])

    representatives.sort(key=lambda c: c[0] * 0.299 + c[1] * 0.587 + c[2] * 0.114)

    # Index 0 = black (transparent), 1 = dark outline, 2 = darker fill, 3 = lighter fill
    # Ensure outline is distinct from background (0,0,0) so png2chr.py's
    # per-tile brightness sort produces consistent index mapping.
    # Bump outline brightness to at least 24 if it's too close to black.
    out_bri = outline[0] * 0.299 + outline[1] * 0.587 + outline[2] * 0.114
    if out_bri < 20:
        outline = (24, 24, 24)

    palette = [(0, 0, 0), outline] + representatives
    return palette


def find_closest(color, palette):
    """Find the closest palette color (skipping index 0 = background)."""
    best_idx = 1
    best_dist = float("inf")
    for i in range(1, len(palette)):
        dist = sum((a - b) ** 2 for a, b in zip(color[:3], palette[i]))
        if dist < best_dist:
            best_dist = dist
            best_idx = i
    return best_idx


# --- Pose extraction and packing ---

POSE_WIDTH = 16    # 2 tiles
POSE_HEIGHT = 24   # 3 tiles


def extract_pose(img, sprite_bbox, bg_color, palette):
    """Extract a single sprite and center it in a POSE_WIDTH x POSE_HEIGHT box.
    Returns a list of (x, y, palette_index) for non-background pixels."""
    sx, sy, sw, sh = sprite_bbox
    pixels = img.load()

    # Center the sprite in the pose box
    offset_x = max(0, (POSE_WIDTH - sw) // 2)
    offset_y = max(0, (POSE_HEIGHT - sh) // 2)

    # If sprite is wider than pose box, crop from center
    crop_x = max(0, (sw - POSE_WIDTH) // 2)
    crop_y = max(0, (sh - POSE_HEIGHT) // 2)

    draw_w = min(sw, POSE_WIDTH)
    draw_h = min(sh, POSE_HEIGHT)

    result = []
    for y in range(draw_h):
        for x in range(draw_w):
            src_x = sx + crop_x + x
            src_y = sy + crop_y + y
            px = pixels[src_x, src_y]

            if colors_match(px, bg_color):
                idx = 0
            else:
                idx = find_closest(px[:3], palette)

            dest_x = offset_x + x
            dest_y = offset_y + y
            if dest_x < POSE_WIDTH and dest_y < POSE_HEIGHT:
                result.append((dest_x, dest_y, idx))

    return result


def pack_poses(poses, palette):
    """Pack poses onto a 128x128 canvas in the planned grid layout.

    Layout: 4 poses side by side, each 16x24, starting at column 0.
        Pose 0 (stand): cols 0-1 (pixels 0-15)
        Pose 1 (run1):  cols 2-3 (pixels 16-31)
        Pose 2 (run2):  cols 4-5 (pixels 32-47)
        Pose 3 (run3):  cols 6-7 (pixels 48-63)
    """
    canvas_w, canvas_h = 128, 128
    bg_rgba = palette[0] + (255,)
    out = Image.new("RGBA", (canvas_w, canvas_h), bg_rgba)
    out_pixels = out.load()

    for pose_idx, pose_pixels in enumerate(poses):
        base_x = pose_idx * POSE_WIDTH
        base_y = 0

        for px, py, color_idx in pose_pixels:
            x = base_x + px
            y = base_y + py
            if 0 <= x < canvas_w and 0 <= y < canvas_h:
                out_pixels[x, y] = palette[color_idx] + (255,)

    return out


def main():
    parser = argparse.ArgumentParser(
        description="Extract specific Mega Man poses for NES animation"
    )
    parser.add_argument("input", help="Source spritesheet PNG")
    parser.add_argument("output", help="Output 128x128 PNG")
    parser.add_argument("--stand", type=int, default=45,
                        help="Sprite index for standing pose (default: 45)")
    parser.add_argument("--run", type=int, nargs=3, default=[52, 55, 56],
                        help="Sprite indices for 3 run frames (default: 52 55 56)")
    parser.add_argument("--list", action="store_true",
                        help="List all detected sprites and exit")
    args = parser.parse_args()

    if not Path(args.input).exists():
        print(f"Error: File not found: {args.input}", file=sys.stderr)
        sys.exit(1)

    img = Image.open(args.input).convert("RGBA")
    print(f"Input: {img.size[0]}x{img.size[1]}")

    bg_color = detect_background_color(img)
    print(f"Background color: RGB{bg_color[:3]}")

    sprites = find_sprites(img, bg_color)
    print(f"Found {len(sprites)} sprite regions")

    if args.list:
        for i, (sx, sy, sw, sh) in enumerate(sprites):
            print(f"  #{i:3d}: pos=({sx},{sy}) size={sw}x{sh}")
        sys.exit(0)

    # Validate sprite indices
    pose_indices = [args.stand] + args.run
    for idx in pose_indices:
        if idx < 0 or idx >= len(sprites):
            print(f"Error: Sprite index {idx} out of range (0-{len(sprites)-1})",
                  file=sys.stderr)
            sys.exit(1)

    pose_names = ["Stand", "Run1", "Run2", "Run3"]
    selected_sprites = [sprites[i] for i in pose_indices]

    print(f"\nSelected poses:")
    for name, idx, (sx, sy, sw, sh) in zip(pose_names, pose_indices, selected_sprites):
        print(f"  {name}: sprite #{idx} at ({sx},{sy}) size={sw}x{sh}")

    # Build palette from selected sprites only
    palette = build_palette(img, selected_sprites, bg_color)
    print(f"\nPalette:")
    for i, c in enumerate(palette):
        print(f"  Index {i}: RGB{c}")

    # Extract each pose
    poses = []
    for name, bbox in zip(pose_names, selected_sprites):
        pose = extract_pose(img, bbox, bg_color, palette)
        poses.append(pose)
        print(f"  Extracted {name}: {len(pose)} pixels")

    # Pack onto canvas
    out = pack_poses(poses, palette)
    out.save(args.output)
    print(f"\nOutput: {args.output} ({out.size[0]}x{out.size[1]})")

    # Print tile index reference
    print(f"\nTile indices (Pattern Table 1):")
    print(f"  Stand: $00,$01, $10,$11, $20,$21")
    print(f"  Run1:  $02,$03, $12,$13, $22,$23")
    print(f"  Run2:  $04,$05, $14,$15, $24,$25")
    print(f"  Run3:  $06,$07, $16,$17, $26,$27")


if __name__ == "__main__":
    main()

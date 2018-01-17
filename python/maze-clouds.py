import random
import colorsys   # batteries included!  https://docs.python.org/3/library/colorsys.html


MAZE_WIDTH = 32
MAZE_HEIGHT = 32


def make_dirs():
    dirs = [(0,1), (0,-1), (1,0), (-1,0)]
    random.shuffle(dirs)
    return dirs


def recursively_dig_maze(pf, x, y, n):
    """This was the original implementation of this function.  It is short and elegant,
    but unfortunately it can exhaust the call stack space if the maze size is large."""

    if y < 0 or y >= MAZE_HEIGHT or x < 0 or x >= MAZE_WIDTH or pf[y][x] != 0:
        return
    pf[y][x] = n
    for dx, dy in make_dirs():
        recursively_dig_maze(pf, x+dx, y+dy, n+1)


def iteratively_dig_maze(pf, x, y, n):
    """This is a re-implementation of recursively_dig_maze(), rewritten iteratively,
    so it can handle larger mazes (e.g. 32x32) without exhausing the call stack."""

    stack = []
    stack.append( (x, y, n, make_dirs()) )

    while stack:
        (x, y, n, dirs) = stack.pop()
        pf[y][x] = n
        if dirs:
            (dx, dy) = dirs.pop()
            stack.append( (x, y, n, dirs) )
            nx = x + dx
            ny = y + dy
            if ny < 0 or ny >= MAZE_HEIGHT or nx < 0 or nx >= MAZE_WIDTH or pf[ny][nx] != 0:
                continue
            stack.append( (nx, ny, n+1, make_dirs()) )
        else:
            pf[y][x] = n
            # leave it popped, and continue


tpl = """\
<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg viewBox="0 0 900 900" version="1.1">
    <rect width="100%" height="100%" fill="White" />
    {shapes}
</svg>
"""


if __name__ == '__main__':
    pf = [[0 for n in range(MAZE_WIDTH)] for m in range(MAZE_HEIGHT)]

    iteratively_dig_maze(pf, int(MAZE_WIDTH / 2), int(MAZE_HEIGHT / 2), 1)

    max_value = 0
    for y, row in enumerate(pf):
        for x, cell in enumerate(row):
            if cell > max_value:
                max_value = cell

    elems = []
    CANVAS_WIDTH = 800
    CANVAS_HEIGHT = 800
    CELL_WIDTH = CANVAS_WIDTH / MAZE_WIDTH
    CELL_HEIGHT = CANVAS_HEIGHT / MAZE_HEIGHT
    for y, row in enumerate(pf):
        for x, cell in enumerate(row):
            lightness = float(cell) / float(max_value)
            h = 0
            l = lightness
            s = 1.0
            r, g, b = colorsys.hls_to_rgb(h, l, s)
            colour = "rgb({},{},{})".format(int(r * 255), int(g * 255), int(b * 255))
            elems.append(
                '<rect x="{x}" y="{y}" width="{w}" height="{w}" fill="{colour}"/>\n'.format(
                    w=CELL_WIDTH + 0.5, h=CELL_HEIGHT + 0.5, x=x*CELL_WIDTH + 50, y=y*CELL_HEIGHT + 50, colour=colour
                )
            )

    filename = "out.svg"
    with open(filename, 'w') as out:
        out.write(tpl.format(shapes=''.join(elems)))

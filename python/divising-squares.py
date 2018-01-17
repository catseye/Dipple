import random
import sys


class Cell(object):
    """A Cell is either:
       * a single square of colour (SingleCell), or
       * a 3x3 grid of CellContainers (GridCell)."""

    def __init__(self, x, y, w, h):
        self.x = x
        self.y = y
        self.w = w
        self.h = h

    def render(self):
        raise NotImplementedError


class SingleCell(Cell):
    def __init__(self, x, y, w, h, colour):
        super(SingleCell, self).__init__(x, y, w, h)
        self.colour = colour

    def render(self):
        return '<rect x="{x}" y="{y}" width="{w}" height="{w}" fill="{colour}"/>'.format(
            w=self.w+0.5, h=self.h+0.5, x=self.x, y=self.y, colour=self.colour
        )


class GridCell(Cell):
    def __init__(self, x, y, w, h, widths, heights, c):
        super(GridCell, self).__init__(x, y, w, h)

        self.contents = []

        assert len(c) == len(widths) * len(heights)

        ty = y
        for ht in heights:
            ht *= h
            tx = x
            for wt in widths:
                wt *= w
                colour = c.pop(0)
                print repr((tx, ty, wt, ht, colour))
                cell = SingleCell(tx, ty, wt, ht, colour)
                self.contents.append(CellContainer(cell))
                tx += wt
            ty += ht

    def render(self):
        return ''.join([cell.render() for cell in self.contents])


class CellContainer(object):
    def __init__(self, cell):
        self.cell = cell

    def render(self):
        return self.cell.render()

    def rewrite(self):
        if isinstance(self.cell, SingleCell):
            if self.cell.colour == 'black':
                r = random.choice(['E', 'e', ])
                if r == 'A':
                    self.cell = GridCell(self.cell.x, self.cell.y, self.cell.w, self.cell.h,
                        [1.0/3.0, 1.0/3.0, 1.0/3.0], [1.0/3.0, 1.0/3.0, 1.0/3.0],
                        ['black', 'white', 'black',
                         'white', 'black', 'white',
                         'black', 'white', 'black']
                    )
                elif r == 'B':
                    self.cell = GridCell(self.cell.x, self.cell.y, self.cell.w, self.cell.h,
                        [1.0/3.0, 1.0/3.0, 1.0/3.0], [1.0/3.0, 1.0/3.0, 1.0/3.0],
                        ['black', 'black', 'black',
                         'black', 'white', 'black',
                         'black', 'black', 'black']
                    )
                elif r == 'C':
                    self.cell = GridCell(self.cell.x, self.cell.y, self.cell.w, self.cell.h,
                        [1.0/3.0, 1.0/3.0, 1.0/3.0], [1.0/3.0, 1.0/3.0, 1.0/3.0],
                        ['white', 'black', 'white',
                         'black', 'black', 'black',
                         'white', 'black', 'white']
                    )
                elif r == 'D':
                    self.cell = GridCell(self.cell.x, self.cell.y, self.cell.w, self.cell.h,
                        [1.0/3.0, 1.0/3.0, 1.0/3.0], [1.0/3.0, 1.0/3.0, 1.0/3.0],
                        ['black', 'white', 'black',
                         'white', 'black', 'white',
                         'black', 'black', 'black',]
                    )

                elif r == '1':
                    self.cell = GridCell(self.cell.x, self.cell.y, self.cell.w, self.cell.h,
                        [1.0/3.0, 1.0/3.0, 1.0/3.0], [1.0/3.0, 1.0/3.0, 1.0/3.0],
                        ['black', 'white', 'white',
                         'black', 'white', 'white',
                         'black', 'black', 'white']
                    )
                elif r == '2':
                    self.cell = GridCell(self.cell.x, self.cell.y, self.cell.w, self.cell.h,
                        [1.0/3.0, 1.0/3.0, 1.0/3.0], [1.0/3.0, 1.0/3.0, 1.0/3.0],
                        ['white', 'black', 'white',
                         'white', 'black', 'white',
                         'white', 'black', 'black']
                    )
                elif r == '3':
                    self.cell = GridCell(self.cell.x, self.cell.y, self.cell.w, self.cell.h,
                        [1.0/3.0, 1.0/3.0, 1.0/3.0], [1.0/3.0, 1.0/3.0, 1.0/3.0],
                        ['white', 'white', 'black',
                         'white', 'white', 'black',
                         'white', 'black', 'black']
                    )

                elif r == 'E':
                    self.cell = GridCell(self.cell.x, self.cell.y, self.cell.w, self.cell.h,
                        [1.0/3.0, 1.0/3.0, 1.0/3.0], [1.0/3.0, 1.0/3.0, 1.0/3.0],
                        ['black', 'white', 'white',
                         'black', 'white', 'white',
                         'black', 'black', 'black']
                    )
                elif r == 'e':
                    self.cell = GridCell(self.cell.x, self.cell.y, self.cell.w, self.cell.h,
                        [1.0/2.0, 1.0/2.0], [1.0/2.0, 1.0/2.0],
                        ['black', 'white',
                         'black', 'black',]
                    )

                else:
                    pass
        elif isinstance(self.cell, GridCell):
            for cell in self.cell.contents:
                cell.rewrite()


# ----


tpl = """\
<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg viewBox="0 0 900 900" version="1.1">
    <rect width="100%" height="100%" fill="White" />
    {shapes}
</svg>
"""

LEVELS = (0, 1, 2, 3, 4, 5)
LEVELS = (4,)

for level in LEVELS:
    c = CellContainer(
        SingleCell(100, 100, 700, 700, 'black')
    )
    for z in xrange(0, level):
        c.rewrite()
    filename = "frame%05d.svg" % level
    filename = "out.svg"
    with open(filename, 'w') as out:
        out.write(tpl.format(shapes=c.render()))

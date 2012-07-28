# A square grid, with rotation.  Ostensibly for implementing Nhohnhehr
# more efficiently.

class Room(object):
    """A square grid of characters, which may be virtually rotated.
    orientation is an integer indicating the amount of rotation, in degrees,
    clockwise from the standard orientation.  It must be one of the following
    values: 0, 90, 180, or 270.
    
    >>> r = Room(3, 0)
    >>> r.store(0, 0, 'a')
    >>> r.store(1, 0, 'b')
    >>> r.store(2, 0, 'c')
    >>> r.store(0, 1, '-')
    >>> r.store(0, 2, 'A')
    >>> r.store(2, 2, 'Z')
    >>> print r
    abc
    -  
    A Z

    >>> r.orientation = 90
    >>> print r
    A-a
      b
    Z c

    >>> r.orientation = 180
    >>> print r
    Z A
      -
    cba

    >>> r.orientation = 270
    >>> print r
    c Z
    b  
    a-A

    """
    def __init__(self, size, orientation):
        self.size = size
        self.orientation = orientation
        self.grid = {}

    def rotate(self, x, y):
        if self.orientation == 0:
            return (x, y)
        elif self.orientation == 90:
            return (y, (self.size-1) - x)
        elif self.orientation == 180:
            return ((self.size-1) - x, (self.size-1) - y)
        elif self.orientation == 270:
            return ((self.size-1) - y, x)
        else:
            raise NotImplementedError("Orientation must be 0, 90, 180, or 270")

    def fetch(self, x, y):
        return self.grid.setdefault(self.rotate(x, y), ' ')

    def store(self, x, y, c):
        self.grid[self.rotate(x, y)] = c

    def __str__(self):
        buf = ''
        for y in range(0, self.size):
            line = ''
            for x in range(0, self.size):
                line += self.fetch(x, y)
            buf += line + '\n'
        return buf.rstrip('\n')


if __name__ == '__main__':
    import doctest
    doctest.testmod()

# Generate the co-ordinates of a regular n-gon inscribed in a unit circle
from math import pi, cos, sin
n = 8
print [(cos(2 * k * pi / n), sin(2 * k * pi / n)) for k in xrange(n)]

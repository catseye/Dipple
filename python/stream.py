#!/usr/bin/env python

# a little experiment; try it with redirected input, and without

import sys

print sys.stdin.tell()
line = sys.stdin.readline()
print line
print sys.stdin.tell()
sys.stdin.seek(0, 0)
line = sys.stdin.readline()
print line
print sys.stdin.tell()

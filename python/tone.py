# usage: python tone.py | aplay

import sys

for g in range(1, 10):
    for d in (5.0, 10.0, 15.0, 20.0, 17.5, 15.0, 12.5, 14.1615):
        for x in range(0, int(5.0 * d)):
            v = 0
            while v < 255.0:
                sys.stdout.write(chr(int(v)))
                v += d
            v -= d
            while v > 0.0:
                sys.stdout.write(chr(int(v)))
                v -= d

if False:
    for d in (5.0, 10.0, 15.0, 20.0):
        for x in range(0, int(10.0 * d)):
            v = 0
            while v < 255.0:
                sys.stdout.write(chr(int(v)))
                v += d

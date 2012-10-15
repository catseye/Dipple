# usage: python tone.py | aplay

for d in (5.0, 10.0, 15.0, 20.0):
    for x in range(0, int(5.0 * d)):
        v = 0
        while v < 255.0:
            print chr(int(v))
            v += d
        v -= d
        while v > 0.0:
            print chr(int(v))
            v -= d

for d in (5.0, 10.0, 15.0, 20.0):
    for x in range(0, int(10.0 * d)):
        v = 0
        while v < 255.0:
            print chr(int(v))
            v += d

#!/usr/bin/env python

import sys

def ack(m, n):
    if m == 0:
        return n + 1
    elif n == 0:
        return ack(m-1, 1)
    else:
        return ack(m-1, ack(m, n-1))

sys.setrecursionlimit(12000)

for m in range(0, 4):
  for n in range(0, 10):
    print "ack(%s,%s)=%s" % (m, n, ack(m, n))

m = 4
n = 0
print "ack(%s,%s)=%s" % (m, n, ack(m, n))

# m = 4
# n = 1
# print "ack(%s,%s)=%s" % (m, n, ack(m, n))

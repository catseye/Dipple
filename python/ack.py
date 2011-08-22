def ack(m, n):
    if m == 0:
        return n + 1
    elif n == 0:
        return ack(m-1, 1)
    else:
        return ack(m-1, ack(m, n-1))

for m in range(0, 4):
  for n in range(0, 4):
    print "ack(%s,%s)=%s" % (m, n, ack(m, n))

print ack(4, 1)
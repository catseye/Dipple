import sys

for n, line in enumerate(sys.stdin):
    l = line.rstrip()
    if len(l) < 80:
        continue
    print("{}: |{}|".format(n, l))

import sys

LINE = 1
COL = 182564

lines = []
for line in sys.stdin:
    lines.append(line)
l = lines[LINE]
AT = COL
SIZE = 160

for p in range(-6,6):
    if p == 0:
        print 'vvvvvv'
    print l[AT+(SIZE*p):AT+(SIZE*(p+1))]

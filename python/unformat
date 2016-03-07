#!/usr/bin/env python

import sys
import re

for line in sys.stdin:
    for w in re.finditer(r'(\<.*?\>|[^<\s]+)', line):
        w = w.group(1)
        w = re.sub(r'(\#|\`|\-|\(|\)|\|\=|\*)', '', w)
        if w and not w.startswith('<'):
            print w

#!/usr/bin/env python

# Convert a text file into a Commodore BASIC 2.0 program which displays
# the text file.

# The output program can be tokenized with petcat, e.g.
#     txt2bas.py <doc.txt >doc.bas
#     petcat -w2 -o doc.prg -- doc.bas

import re
import sys
import fileinput
from optparse import OptionParser


if __name__ == '__main__':
    parser = OptionParser()

    (options, args) = parser.parse_args()
    prompt = '[press RETURN]';
    clear = (' ' * (len(prompt) + 1)) + ('{left}' * (len(prompt) + 1))

    print '5 print"{clr}";chr$(14);'
    line_number = 10
    for line in fileinput.input(args):
        line = line.rstrip()
        ender = ''
        if len(line) > 40:
            raise ValueError("line too long: %s" % line)
        if len(line) == 40:
            ender = ';'
        line = re.sub(r'"', '";chr$(34);"', line)
        print '%d print"%s"%s' % (
            line_number, line, ender)
        if line_number % 230 == 0:
            print '%d input"%s";a$' % (line_number + 5, prompt)
            print '%d print"{up}%s";' % (line_number + 6, clear)
        line_number += 10

    sys.exit(0)

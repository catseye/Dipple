#!/usr/bin/env python3

import os

def main(args):

    table = {}
    table['"'] =  '\\"'
    table['\\'] = '\\\\'
    table['\n'] = '\\n'
    for arg in args:
        with open(arg, 'r') as f:
            quoted_lines = []
            for line in f:
                escaped_line = line.translate(line.maketrans(table))
                quoted_lines.append('"{}"'.format(escaped_line))

        print('        examples.add(new ExampleProgram(')
        print('            "{}",'.format(os.path.basename(arg)))
        for quoted_line in quoted_lines[:-1]:
            print("            {} +".format(quoted_line))
        print("            {},".format(quoted_lines[-1]))
        print('            cpProperties')
        print('        ));')

if __name__ == '__main__':
    import sys
    main(sys.argv[1:])

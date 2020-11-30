#!/usr/bin/python

import sys
import re

def main(args):
    words = {}
    for filename in args:
        with open(filename, 'r') as f:
            for line in f:
                line = re.sub('--', ' ', line)
                for word in line.split():
                    match = re.match(r'([a-zA-Z\-]+)', word)
                    if match:
                        word = match.group(1).lower()
                        words.setdefault(word, 0)
                        words[word] += 1
    for word, num in sorted(words.items()):
        print(word, num)


if __name__ == '__main__':
    main(sys.argv[1:])

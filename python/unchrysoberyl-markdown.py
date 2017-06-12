import sys
import re
from urllib.parse import quote


def unchrysoberyl(contents):

    def linker(match):
        text = match.group(1)
        segments = text.split('|')
        if len(segments) == 1:
            phrase = segments[0]
            link = "http://catseye.tc/node/{}".format(quote(phrase))
        elif len(segments) == 2:
            phrase = segments[0]
            link = "http://catseye.tc/node/{}".format(quote(segments[1]))
        else:
            raise NotImplementedError
        return '[{}]({})'.format(phrase, link)

    return re.sub(r'\[\[(.*?)\]\]', linker, contents)


print(unchrysoberyl(sys.stdin.read()))

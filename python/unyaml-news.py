import yaml
from yaml import Loader
import sys
import codecs
from datetime import datetime
from StringIO import StringIO


data = yaml.load(sys.stdin, Loader=Loader)


entries = {}


for item, node in data.iteritems():
    date = None
    out = StringIO()
    out.write(u'### {}\n\n'.format(item))
    for key, value in sorted(node.iteritems()):
        if key in ('commentary', 'type', 'description', 'article-type'):
            continue
        field = key
        if key == 'blurb':
            field = 'summary'
            value = value.rstrip()
        elif key == 'publication-date':
            field = 'date'
            date = datetime.strptime(value, "%a, %d %b %Y %H:%M:%S GMT")

        out.write(u'*   {}: {}\n'.format(field, value))
    out.write('\n')
    out.write(node.get('description', ''))
    out.write('\n')

    assert date is not None
    entries[date] = out.getvalue()


with codecs.open('out.md', 'w', encoding='utf-8') as out:
    for k, v in reversed(sorted(entries.iteritems())):
        out.write(v)

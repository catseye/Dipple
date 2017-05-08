import yaml
from yaml import Loader
import sys
import codecs


data = yaml.load(sys.stdin, Loader=Loader)


out = codecs.open('out.md', 'w', encoding='utf-8')


for item, node in data.iteritems():
    out.write(u'### {}\n\n'.format(item))
    for key, value in sorted(node.iteritems()):
        if key in ('commentary', 'type'):
            continue
        field = key
        if key == 'isbn':
            field = 'ISBN'
        if key == 'authors':
            value = ', '.join(value)
        out.write(u'*   {}: {}\n'.format(field, value))
    out.write('\n')
    out.write(node.get('commentary', ''))
    out.write('\n')

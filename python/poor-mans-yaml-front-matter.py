#!/usr/bin/env python

"""
Poor-man's YAML Front Matter.  Inspired by jekyll's YAML Front Matter:

    https://github.com/mojombo/jekyll/wiki/yaml-front-matter

...except done in a much more chewing-gum-and-baling-wire fashion.

You write your document as a YAML file, but you include its body, as a
Markdown document, under the `body` key in the YAML.

You can do this with a YAML block scalar; the only downside is that you
have to indent the whole Markdown document two spaces.  (Not a huge
tragedy when you have a half-decent text editor at your disposal)

The contents of the `document` key in the YAML is made available as a
template context for rendering the document; you can include Jinja2
("moustache"-style) variables in your Markdown, and these will be replaced
by the various values in the YAML file.

You can also specify a template file under the `template` key; that will
load the template file with that name and render it with the same context,
except with `body` now referring to the HTML that was rendered from the
Markdown.

There are of course scores of variations on this theme; your YAML file
could have a list or dictionary of such documents, the text format need
not be Markdown, the templating engine need not be Jinja2, etc., etc.

Try it:

    % ./poor-mans-yaml-front-matter.py front-matter.yaml

"""

import sys

import markdown
import jinja2
import yaml
try:
    from yaml import CLoader as Loader
except ImportError:
    from yaml import Loader


def render(filename):
    file = open(filename)
    data = yaml.load(file, Loader=Loader)
    file.close()
    html = markdown.markdown(data['body'])
    template = jinja2.Template(html)
    body = template.render(data)
    data['body'] = body
    file = open(data['template'])
    template = jinja2.Template(file.read())
    file.close()
    body = template.render(data)
    return body


if __name__ == '__main__':
    print render(sys.argv[1])

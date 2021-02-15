import yaml
try:
    from yaml import CLoader as Loader
except ImportError:
    from yaml import Loader

with open('LINKS.md', 'r') as file_:
    config = yaml.load(file_, Loader=Loader)

for link in config['links']:
    print("*   [{}]({}) - {}".format(link.get('title', 'LINK'), link.get('url', ''), link.get('description', '')))


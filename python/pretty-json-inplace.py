import os
import json
import sys

filename = sys.argv[1]

with open(filename, 'r') as f:
    data = json.loads(f.read())

with open(filename, 'w') as f:
    f.write(json.dumps(data, indent=4, sort_keys=True))

import re
import os
import sys
import subprocess

dirname = sys.argv[1]
for d in sorted(os.listdir(dirname)):
    match = re.match(r'^(.*?)\.zip$', d)
    if match:
        basename = match.group(1)
        fullname = os.path.join(dirname, d)
        output = subprocess.check_output("unzip -v {}".format(fullname), shell=True)
        lines = output.split('\n')
        lines = lines[3:]
        lines = lines[:-3]
        errors = 0
        for line in lines:
            if basename not in line:
                errors += 1
        if errors > 0:
            print('----------------------')
            print(fullname, basename)
            print('\n'.join(lines))
            print('')

#!/usr/bin/env python3

import os
import subprocess


for fname in os.listdir('.'):
    #print(fname)
    if os.path.islink(fname):
        r = os.readlink(fname)
        subprocess.check_call('ln -s "{}" "{}_"'.format(os.path.relpath(r), fname), shell=True)
        subprocess.check_call('mv "{}_" "{}"'.format(fname, fname), shell=True)

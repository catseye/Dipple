#!/usr/bin/env python
# A gotcha with mkstemp: see e.g. http://www.logilab.org/blogentry/17873

# Contains code from a blog, and is therefore not in the public domain.

import os
from tempfile import mkstemp

fd, filename = mkstemp()
with open(filename, 'w') as file:
    file.write('hello')
    file.close()
os.close(fd) # if you do not do this, you will leak filehandles
print "wrote out %s" % filename

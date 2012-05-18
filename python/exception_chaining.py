#!/usr/bin/env python

# It would be nice if there was a standard way to "chain" these
# exceptions together, so that the IndexError's traceback was
# available from inside the KeyError, and printed on exit.
# You can probably do it, but not in an elegant way.

def a(foo):
    try:
        b(foo)
    except Exception, e:
        # raise
        # raise KeyError(e.args[0])
        raise KeyError(e)


def b(foo):
    c(foo)


def c(foo):
    raise IndexError(foo)


a(17)

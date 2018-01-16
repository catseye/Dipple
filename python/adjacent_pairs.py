

def adjacent_pairs(iterable):
    """Returns an iterable which yields successive pairs of adjacent items in the given
    iterable.  e.g. if given [1,2,3], this will yield (None,1), (1,2), and (2,3)."""
    last_value = None
    for value in iterable:
        yield (last_value, value)
        last_value = value

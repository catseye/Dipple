import random


def shuffled(i):
    j = list(i)
    random.shuffle(j)
    return j


def toposort(depmap):
    result = []

    def add_deps(k1):
        if k1 in result:
            return
        for k2 in shuffled(depmap[k1]):
            add_deps(k2)
        result.append(k1)

    for key in shuffled(depmap.keys()):
        add_deps(key)

    return result


depmap = {
    'shoes': ['socks', 'pants'],
    'pants': ['underpants'],
    'underpants': [],
    'socks': [],
    'suit': ['shirt'],
    'shirt': [],
    'tie': ['shirt'],
}

print(toposort(depmap))

# 1.
# what happens if you try to call a classmethod on an instance?

class AbstractWhatHappens(object):
    def thing(self, y):
        return self.wat(y)

    @classmethod
    def wat(cls, y):
        return cls.FOO[y]

class WhatHappens(AbstractWhatHappens):
    FOO = {
        'x': 23,
    }

w = WhatHappens()
print w.thing('x')

# 2.
# what does this print?

def a1():
    return [1, 2, 3]
def a2():
    for x in [1, 2, 3]:
        yield x
def b1():
    return []
def b2():
    for x in []:
        yield x

if a1():
    print "a1()"
if a2():
    print "a2()"
if b1():
    print "b1()"    
if b2():
    print "b2()"

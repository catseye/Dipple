# Implementation of Marxen and Buntrock's Busy Beaver 5.
# See: H. Marxen and J. Buntrock. Attacking the Busy Beaver 5.
# Bulletin of the EATCS, 40, pages 247-251, February 1990.

# Terminates after 47176870 steps.  Records all of those
# steps, with run-length encoding, in a text file, bbrep.out.

# Could be easily modified to run any given Turing machine.

state = '1'
head = 0
tape = {}

def read():
    global tape, head
    return tape.setdefault(head, 'B')

def write(x):
    global tape, head
    tape[head] = x

trans = {
  '1B': '21L',
  '11': '31R',
  '2B': '31L',
  '21': '21L',
  '3B': '41L',
  '31': '5BR',
  '4B': '11R',
  '41': '41R',
  '5B': 'S1L',
  '51': '1BR',
}

halted = False
step = 0
prev = None
rep = 1
with open('bbrep.out', 'w') as f:
    while not halted:
        value = read()
        r = state + value
        if prev is None:
            prev = r
            rep = 1
        elif r == prev:
            rep += 1
        else:
            f.write('%s*%d\n' % (prev, rep))
            prev = r
            rep = 1
        if state == 'S':
            break
        x = trans[r]
        state = x[0]
        write(x[1])
        dir = x[2]
        if dir == 'L':
          head -= 1
        if dir == 'R':
          head += 1
        step += 1
        #if step % 100000 == 0:
        #    print step
    f.write('%s*%d\n' % (prev, rep))

print step

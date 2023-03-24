# usage:
#   nim r --verbosity:0 stringo.nim

var a : string
let b = 9

a.add("hi")
a.add($b & "\n")
a.add($8)
echo a

# but also,

echo 7

const c = """
This is a long string,
it is very long.
"""
echo c

const d =
   "on the next line."
echo d

const e = (
   discard "yep";
   "no tricky"
); echo e

proc getAlphabet(a: char, z: char): string =
  var acc = ""
  for x in a..z:
    acc.add(x)
  return acc

const alphabet = getAlphabet('a', 'z')
echo alphabet

var digits = getAlphabet('0', '9')
echo digits

var blah1 = alphabet & digits
echo blah1

# const blah2 = alphabet & digits
# Error: cannot evaluate at compile time: digits

# some other types, like... bool

proc boolThings() =
  const a = true
  var b = a and true
  var c = b or false
  echo $c

boolThings()

# echo echo
# required type for x: varargs[typed] but expression 'echo' is of type: proc (x: varargs[typed])

# echo $echo
# ... is no better

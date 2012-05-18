#!/usr/bin/lua

-- Implements an automaton by Elliott Hird.
-- Not sure what it was supposed to do...

m = {}
x = 0
while 1 do
  m[m[0] or 0] = x
  x = m[(m[0] or 0) + 1]
  m[0] = x
end

# have all your functions declared like: def fun(continuation, ...)
# when returning from a function, do: continuation.call()
# when calling a function, do: callcc { |k| fun k }
# when passing control to a function, do: fun continuation

def a(continuation)
  puts "a"
  callcc { |k| b k }
  puts "a"
  continuation.call()
end

def b(continuation)
  puts "b"
  c continuation
  puts "b"
  continuation.call()
end

def c(continuation)
  puts "c"
  continuation.call()
end

callcc { |k| a k }


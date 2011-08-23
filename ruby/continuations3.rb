def rcall(fun)
  callcc do |continuation|
    fun.call(continuation)
  end
end

def a
  puts "a"
  rcall lambda { |k| b k }
  puts "a"
end

def b(continuation)
  puts "b"
  c continuation  #  instead of: rcall lambda { |k| c k }
  puts "b"
  continuation.call()
end

def c(continuation)
  puts "c"
  continuation.call()
end

a

def rcall(name, fun)
  callcc do |continuation|
    puts "calling #{name}"
    fun.call(continuation)
    puts "returning from #{name} (never happens)"
  end
  puts "#{name} did a continue"
end

def a
  puts "a"
  rcall "b", lambda { |k| b k }
  puts "a"
end

def b(continuation)
  puts "b"
  rcall "c", lambda { |k| c k }
  puts "b"
  continuation.call()
end

def c(continuation)
  puts "c"
  continuation.call()
end

a


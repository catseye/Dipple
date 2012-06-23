# A method defined at the toplevel becomes part of Object, you say?
# Technically yes, but it's not as wild as it sounds, because it's private.

def hello x
  puts "Hello #{x}!  From #{self}."
end

hello "there"

class Foo
  def a
    hello "world"
  end
  def b
    self.hello "world"
  end
end

f = Foo.new()
f.a

begin
  f.hello "sailor"
rescue Exception => e
  puts "BZZT! #{e}"
end

begin
  f.b
rescue Exception => e
  puts "BZZT! #{e}"
end

# And lest you think, oho, if it's there but private, that means it's not missing,
# you're wrong.  It's missing.

begin
  f.no_such_method_as_this
rescue Exception => e
  puts "BZZT! #{e}"
end

def f.method_missing(sym, *args, &block)
  puts "method missing! #{sym} #{args} #{block}"
end

f.no_such_method_as_this
f.b
f.hello "fellas"

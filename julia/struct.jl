struct Bleep
  bar
  baz::Int
  quuz::Float64
end

foo = Bleep("Hello, world", 23, 1.5)

println(typeof(foo))
println(foo)
println(foo.bar)
println(foo.baz)
println(foo.quuz)

function baz()
  x = 5
  x = "foo"
  return x
end

print(baz())

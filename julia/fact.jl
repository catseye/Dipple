function factorial(x)
  if x ≤ 1
    return 1
  else
    return x * factorial(x - 1)
  end
end

v = factorial(big(9000))
s = string(v)

for c in s
  println(c)
end

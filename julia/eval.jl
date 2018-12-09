# The quoted version does not run significantly
# slower than the regular version.  In fact, it
# can run faster.  Some sample results:
#
#   1.392471 seconds (585.90 k allocations: 7.955 GiB, 2.63% gc time)
# 421683
#   1.378544 seconds (587.83 k allocations: 7.956 GiB, 3.13% gc time)
# 421683

slow1 = function()
  function factorial(x)
    a = 1
    while x >= 1
      a = a * x
      x = x - 1
    end
    return a
  end
  return factorial(big(93000))
end

@time v = slow1()
println(length(string(v)))

slow2 = quote
  function()
    function factorial(x)
      a = 1
      while x >= 1
        a = a * x
        x = x - 1
      end
      return a
    end
    return factorial(big(93000))
  end
end

@time v = eval(slow2)()
println(length(string(v)))


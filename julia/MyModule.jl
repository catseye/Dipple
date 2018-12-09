# you can
#     push!(LOAD_PATH, ".")
# then
#     using MyModule
# then
#     MyModule.gleep()

module MyModule

using Match

function foop(x)
  return @match x begin
    1 => "one"
    2 => "two"
    3 => "three"
    _ => "another"
  end
end

function gleep()
  for i in 1:8
    println(foop(i))
  end
end

end # module

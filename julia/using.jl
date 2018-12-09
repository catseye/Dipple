# You'll need to run julia from the directory that this
# file and MyModule.jl both reside in, for this to work:

push!(LOAD_PATH, ".")

using MyModule

MyModule.gleep()
println(code_lowered(MyModule.foop))

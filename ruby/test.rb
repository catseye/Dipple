#!/usr/bin/ruby

class City
    def initialize(name, population)
        @name = name
        @population = population
    end
    def to_s
        @name + " (pop. " + @population.to_s + ")"
    end
    def bang!
        @population /= 2
    end
end

c = City.new("Winnipeg", 500000)
puts c
d = c.clone
puts d
d.bang!
puts d
puts c

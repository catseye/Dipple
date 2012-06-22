#!/usr/bin/env ruby

# a basic template to draw from when implementing your favourite
# two-dimensional esolang in Ruby

class Grid
  def initialize
    @store = []
  end

  def load(filename)
    File.open(filename, 'r') do |f|
      y = 0
      while line = f.gets
        @store[y] = []
        line.chomp!
        x = 0
        line.each_char do |c|
          @store[y][x] = c
          x += 1
        end
        y += 1
      end
    end
  end

  # you can, actually, just print the Grid object to
  # get the same effect
  def display
    for y in 0..@store.length-1 do
      for x in 0..@store[y].length-1 do
        print @store[y][x]
      end
      print "\n"
    end
  end

  def to_s
    s = ""
    for y in 0..@store.length-1 do
      s += @store[y].to_s + "\n"
    end
    s
  end
end

### Main ###

grids = []
ARGV.each do |filename|
  g = Grid.new()
  g.load(filename)
  grids.push(g)
end

grids.each do |g|
  g.display
end

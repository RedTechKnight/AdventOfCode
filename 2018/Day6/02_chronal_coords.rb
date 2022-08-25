require 'benchmark'

class Coord
  def initialize(x, y)
    @x = x
    @y = y
  end

  def <=>(x)
    (@x + @y) <=> (x.x + x.y)
  end

  attr_reader :x
  attr_reader :y
  attr_writer :x
  attr_writer :y
end

Nearest = Struct.new(:x, :y, :id)

class Square
  def initialize(x, y)
    @x = x
    @y = y
    @distSum = 0
  end

  def addDistance(x)
    @distSum += x
  end

  attr_reader :x
  attr_reader :y
  attr_reader :distSum
end

puts Benchmark.measure {
  input = File.new('inputs.txt', mode = 'r')

  coords = []

  until input.eof?
    line = input.readline.strip.match(/(\d+), (\d+)/)
    coords.push(Coord.new(line[1].to_i, line[2].to_i))
  end

  input.close

  min = coords.min()

  coords.map! do |coord|
    Coord.new(coord.x - min.x, coord.y - min.y)
  end

  max = coords.max()
  gw = max.x + 1
  gh = max.y + 1

  grid = {}

  (gw * gh).times do |i|
    grid[i] = Square.new(i % gw, i / gw)
  end

  maxDist = 10_000

  grid.each do |_key, value|
    coords.each do |coord|
      dist = (value.x - coord.x).abs + (value.y - coord.y).abs
      value.addDistance(dist)
    end
  end

  grid.delete_if do |_k, v|
    v.distSum > (maxDist - 1)
  end
  puts "The size of the region containing points within #{maxDist} units of all the other points is #{grid.size}"
}

require 'benchmark'
class Coord
  def initialize(x, y)
    @x = x
    @y = y
  end

  def <=>(c)
    (@x + @y) <=> (c.x + c.y)
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
    @nearest = []
  end

  def compareDistance(coord, index)
    xDiff = coord.x - @x
    yDiff = coord.y - @y
    total0 = xDiff.abs + yDiff.abs

    if !@nearest.empty?
      total1 = (@nearest[0].x - @x).abs + (@nearest[0].y - @y).abs
      case total0 <=> total1
      when -1
        @nearest = [Nearest.new(coord.x, coord.y, index)]
      when 0
        @nearest.push(Nearest.new(coord.x, coord.y, index))
      when 1
      end
    else
      @nearest = [Nearest.new(coord.x, coord.y, index)]
    end
  end

  def nearest
    @nearest[0].id if @nearest.size == 1
  end

  attr_reader :x
  attr_reader :y
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

  coords.each_index  do |index|
    (gw * gh).times  do |i|
      grid[i].compareDistance(coords[index], index)
    end
  end

  areas = {}

  grid.each do |ind, _val|
    x = ind % gw
    y = ind / gw
    unless grid[ind].nearest.nil?
      if areas[grid[ind].nearest].nil?
        if (x == 0) || (x == max.x) || (y == 0) || (y == max.y)
          areas[grid[ind].nearest] = 'INF'
        else
          areas[grid[ind].nearest] = 1
          end
      else
        if (x == 0) || (x == max.x) || (y == 0) || (y == max.y)
          areas[grid[ind].nearest] = 'INF'
        else
          areas[grid[ind].nearest] += 1 unless areas[grid[ind].nearest] == 'INF'
          end
            end
    end
  end
  areas.delete_if do |_k, v|
    v == 'INF'
  end
  puts "The largest non-infinite area among the points is: #{areas.max[1]}"
}

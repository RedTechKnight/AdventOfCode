require 'benchmark'

class Coord
    def initialize(x,y)
        @x = x
        @y = y
    end

    def <=>(x)
        return (@x+@y) <=> (x.x+x.y)
    end

    attr_reader :x
    attr_reader :y
    attr_writer :x
    attr_writer :y
end

Nearest = Struct.new(:x,:y,:id)

class Square
    def initialize(x,y)
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

puts Benchmark.measure(){
    input = File.new("inputs.txt",mode="r")

    coords = Array.new()

    until(input.eof?())
        line = input.readline().strip().match(/(\d+), (\d+)/)
        coords.push(Coord.new(line[1].to_i(),line[2].to_i()))
    end

    input.close()

    min = coords.min()
        
    coords.map!(){
        |coord|
        Coord.new(coord.x-min.x,coord.y-min.y)
    }
    
    max = coords.max()
    gw,gh = max.x+1,max.y+1

    grid = Hash.new()
        
    (gw*gh).times(){
        |i|
        grid[i] = Square.new(i % gw,i / gw)
    }

    maxDist = 10000

    grid.each(){
        |key,value|
        coords.each(){
            |coord|
            dist = (value.x-coord.x).abs() + (value.y-coord.y).abs()
            value.addDistance(dist)
        }
    }
        
    grid.delete_if(){
        |k,v|
        v.distSum > (maxDist - 1)
    }
    puts "The size of the region containing points within #{maxDist} units of all the other points is #{grid.size()}"
}
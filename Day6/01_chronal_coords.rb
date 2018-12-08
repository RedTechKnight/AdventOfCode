require 'benchmark'
class Coord
    def initialize(x,y)
        @x = x
        @y = y
    end

    def <=>(c)
        return (@x+@y) <=> (c.x+c.y)
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
        @nearest = Array.new()
    end

    def compareDistance(coord,index)
        xDiff = coord.x - @x
        yDiff = coord.y - @y
        total0 = xDiff.abs() + yDiff.abs()

        if(@nearest.size() > 0)
            total1 = (@nearest[0].x - @x).abs() + (@nearest[0].y - @y).abs()
            case total0 <=> total1
            when -1
                @nearest = [Nearest.new(coord.x,coord.y,index)]
            when 0
                @nearest.push(Nearest.new(coord.x,coord.y,index))
            when 1
            end
        else
            @nearest = [Nearest.new(coord.x,coord.y,index)]
        end
    end

    def nearest()
        if(@nearest.size() == 1)
            return @nearest[0].id
        end
    end

    attr_reader :x
    attr_reader :y
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

        coords.each_index(){
            |index|
            (gw*gh).times(){
                |i|
                grid[i].compareDistance(coords[index],index)
            }
        }

        areas = Hash.new()

        grid.each{
            |ind,val|
            x = ind % gw
            y = ind / gw
            unless(grid[ind].nearest().nil?())
                unless(areas[grid[ind].nearest()].nil?())
                    if((x == 0) or (x == max.x) or (y == 0) or (y == max.y))
                        areas[grid[ind].nearest()] = "INF"
                    else
                        areas[grid[ind].nearest()] += 1 unless areas[grid[ind].nearest()] == "INF"
                    end
                else
                    if((x == 0) or (x == max.x) or (y == 0) or (y == max.y))
                        areas[grid[ind].nearest()] = "INF"
                    else
                        areas[grid[ind].nearest()] = 1
                    end
                end
            end
        }
        areas.delete_if(){
            |k,v|
            v == "INF"
        }
        puts "The largest non-infinite area among the points is: #{areas.max()[1]}"
    
}
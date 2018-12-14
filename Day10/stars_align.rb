require 'gosu'
require 'benchmark'

class Vec2
    def initialize(x = 0,y = 0)
        @x = x
        @y = y
    end

    def *(scalar = 1)
        return Vec2.new(@x*scalar,@y*scalar)
    end 

    def +(vec1)
        return Vec2.new(@x+vec1.x,@y+vec1.y)
    end

    def -(vec1)
        return Vec2.new(@x-vec1.x,@y-vec1.y)
    end

    def to_s()
        return "[#{@x},#{@y}]"
    end

    def inspect()
        return "[#{@x},#{@y}]"
    end

    attr_reader :x
    attr_writer :x
    attr_reader :y
    attr_writer :y
end

class Star
    def initialize(pos = Vec2.new(0,0),vel = Vec2.new(0,0))
        @position = pos
        @velocity = vel
    end

    def update(rate = 1)
        @position = @position + (@velocity*rate)
    end

    def to_s()
        return "(#{@position.x},#{@position.y}) + [#{@velocity.x},#{@velocity.y}]"
    end

    def inspect()
        return "(#{@position.x},#{@position.y}) + [#{@velocity.x},#{@velocity.y}]"
    end

    attr_reader :position
    attr_reader :velocity
    attr_writer :position
    attr_writer :velocity
end

class Window < Gosu::Window
    def initialize(w,h,fullscreen)
        super w,h
        @width,@height = w,h
        self.caption = "Day 10 - Stars Align"
        @star = Gosu::Image.new("star.png",:tileable => true)
        @scale = 1
        input = File.new("inputs.txt",mode="r")
        @time = 0
        @stars = Array.new()
        until(input.eof?())
            line = input.readline()
            pos = line.match(/position=<\s*(-\d+|\d+),\s*(-\d+|\d+)/).to_a()
            vel = line.match(/velocity=<\s*(-\d+|\d+),\s*(-\d+|\d+)/).to_a()
            star = Star.new(Vec2.new(pos[1].to_i(),pos[2].to_i()),Vec2.new(vel[1].to_i(),vel[2].to_i()))
            @stars.push(star)
        end
        
        minmaxX = @stars.minmax_by(){
            |star|
            star.position.x
        }
        minmaxY = @stars.minmax_by(){
            |star|
            star.position.y
        }

        @minX,@maxX = minmaxX[0].position.x,minmaxX[1].position.x
        @minY,@maxY = minmaxY[0].position.y,minmaxY[1].position.y

        @stars.map!(){
            |star|
            star.position.x += @maxX
            star.position.y += @maxY
            star
        }

        xDiff = @maxX - @minX
        yDiff = @maxY - @minY
        
        @xScale = w.to_f()/xDiff
        @yScale = h.to_f()/yDiff

        input.close()
    end

    def update()
        if(Gosu.button_down?(Gosu::KB_LEFT))
            @time -= 10
            @stars.map!(){
                |star|
                star.update(-10)
                star
            }
        end
        if(Gosu.button_down?(Gosu::KB_RIGHT))
            @time += 100
            @stars.map!(){
                |star|
                star.update(100)
                star
            }
        end
        
        if(Gosu.button_down?(Gosu::KB_UP))
            @scale += 0.1
        end
        if(Gosu.button_down?(Gosu::KB_DOWN))
            @scale -= 0.1
        end

        minmaxX = @stars.minmax_by(){
            |star|
            star.position.x
        }
        minmaxY = @stars.minmax_by(){
            |star|
            star.position.y
        }
        @minX,@maxX = minmaxX[0].position.x,minmaxX[1].position.x
        @minY,@maxY = minmaxY[0].position.y,minmaxY[1].position.y

        @stars.map!(){
            |star|
            star.position.x -= @minX
            star.position.y -= @minY
            star
        }
        
        xDiff = @maxX - @minX
        yDiff = @maxY - @minY
        
        @xScale = @width.to_f()/(xDiff)
        @yScale = @height.to_f()/(yDiff)
    end

    def draw() 
        font = Gosu::Font.new(19)
        font.draw_text("T+ #{@time}s",10,10,0)
        @stars.each(){
        |star|
            x = (star.position.x*@xScale)*@scale            
            y = (star.position.y*@yScale)*@scale
            @star.draw(x,y,0)
        }
    end

    def button_down(id)
        if(id == Gosu::KB_ESCAPE)
            close()
        elsif(id == Gosu::KB_A)
            
            @time -= 1
            @stars.map!(){
                |star|
                star.update(-1)
                star
            }
        elsif(id == Gosu::KB_D)
            
            @time += 1
            @stars.map!(){
                |star|
                star.update(1)
                star
            }
        else
            super
        end
    end 
end

window = Window.new(1280,720,false)
window.show()

input = File.new("inputs.txt",mode="r")

GridWidth = 1000
GridHeight = 1000

class Fabric
    def initialize()
        @fabric = Hash.new()
    end
    
    def addClaims(x,y,w,h)
        ind = 0
        while(ind < (w * h)) do
            fabricSquare =  (x + (ind % w)) + GridHeight * (y + (ind / w))

            unless(@fabric[fabricSquare].nil?()) 
                @fabric[fabricSquare] += 1
            else
                @fabric[fabricSquare] = 1
            end

            ind += 1
        end
    end

    def removeNonOverlapping()
        @fabric.delete_if() {
            |key,value|
            value < 2
        }
    end

    def overlapping()
        return @fabric.size()
    end
end

fabric = Fabric.new()

startTime = Time.now()

until input.eof?() do
    val = input.readline.split(/\D+/)
    fabric.addClaims(val[2].to_i(),val[3].to_i(),val[4].to_i(),val[5].to_i())
end

fabric.removeNonOverlapping()

puts "Total square inches of overlapping claims: #{fabric.overlapping()}"

endTime = Time.now()

timeTaken = endTime - startTime

puts "Time taken to find solution #{timeTaken}"
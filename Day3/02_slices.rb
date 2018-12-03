startTime = Time.now()

input = File.new("inputs.txt",mode="r")
GridWidth = 1000
GridHeight = 1000

class Fabric
    def initialize()
        @fabric = Hash.new()
        @overlapping = Hash.new()
    end
    
    def addClaims(id,x,y,w,h)
        ind = 0
        while(ind < (w * h)) do

            fabricSquare =  (x + (ind % w)) + GridHeight * (y + (ind / w))

            unless(@fabric[fabricSquare].nil?())
                @fabric[fabricSquare].push(id)

                for square in @fabric[fabricSquare] do
                    @overlapping[ square ] = true
                end

            else
                @fabric[fabricSquare] = Array.new()
                @fabric[fabricSquare].push(id)

                unless(@overlapping[id] == true)
                    @overlapping[id] = false
                end
            end
            ind += 1

        end
    end

    def nonOverlapping()
        nonOverlapping = @overlapping.find_all(){
            |key,value|
            value == false
        }
        return nonOverlapping[0][0]
    end
end


fabric = Fabric.new()

until(input.eof?())
    val = input.readline.split(/\D+/)
    fabric.addClaims(val[1].to_i(),val[2].to_i(),val[3].to_i(),val[4].to_i(),val[5].to_i())
end

puts "The ID of the square of fabric that is not overlapping with any others is: #{fabric.nonOverlapping()}"

endTime = Time.now()

timeTaken = endTime - startTime

puts "Time taken to find solution #{timeTaken}"
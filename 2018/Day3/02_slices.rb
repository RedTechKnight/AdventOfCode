startTime = Time.now

input = File.new('input_day3', mode = 'r')
GridWidth = 1000
GridHeight = 1000

class Fabric
  def initialize
    @fabric = {}
    @overlapping = {}
  end

  def addClaims(id, x, y, w, h)
    ind = 0
    while ind < (w * h)

      fabricSquare = (x + (ind % w)) + GridHeight * (y + (ind / w))

      if @fabric[fabricSquare].nil?
        @fabric[fabricSquare] = []
        @fabric[fabricSquare].push(id)

        @overlapping[id] = false unless @overlapping[id] == true
      else
        @fabric[fabricSquare].push(id)

        @fabric[fabricSquare].each do |square|
          @overlapping[square] = true
        end

        end
      ind += 1

    end
  end

  def nonOverlapping
    nonOverlapping = @overlapping.find_all do |_key, value|
      value == false
    end
    nonOverlapping[0][0]
  end
end

fabric = Fabric.new

until input.eof?
  val = input.readline.split(/\D+/)
  fabric.addClaims(val[1].to_i, val[2].to_i, val[3].to_i, val[4].to_i, val[5].to_i)
end

puts "The ID of the square of fabric that is not overlapping with any others is: #{fabric.nonOverlapping}"

endTime = Time.now

timeTaken = endTime - startTime

puts "Time taken to find solution #{timeTaken}"

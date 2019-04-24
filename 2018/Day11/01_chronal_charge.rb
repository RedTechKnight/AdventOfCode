require 'benchmark'

def cellPower(serialNum, x, y)
  rackID = x + 10
  power = rackID * y
  power += serialNum
  power *= rackID
  power = if power / 100 > 0
            power.to_s[-3].to_i
          else
            0
          end
  power -= 5
  power
end
puts Benchmark.measure {
  maxSquare = [1, 1, 0]
  serialNumber = 8141
  charges = []

  (300 * 300).times do |i|
    charge = cellPower(serialNumber, (i % 300) + 1, (i / 300) + 1)
    charges.push(charge)
  end

  10_000.times do |i|
    c0 = charges[(i * 3)]
    c1 = charges[(i * 3) + 1]
    c2 = charges[(i * 3) + 2]

    c3 = charges[(i * 3) + 300]
    c4 = charges[(i * 3) + 301]
    c5 = charges[(i * 3) + 302]

    c6 = charges[(i * 3) + 600]
    c7 = charges[(i * 3) + 601]
    c8 = charges[(i * 3) + 602]
    totalPower = c0 + c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8
    if totalPower > maxSquare[0]
      maxSquare = [totalPower, 1 + (i * 3) % 300, 1 + (i * 3) / 300]

    end
  end
  puts "The 3x3 square with the largest charge is starting at #{maxSquare[1]},#{maxSquare[2]}, with total power of #{maxSquare[0]}"
}

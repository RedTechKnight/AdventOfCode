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
  maxSquare = [1, 1, 1, 1]
  serialNumber = 8141
  charges = []

  (300 * 300).times do |i|
    charge = cellPower(serialNumber, (i % 300) + 1, (i / 300) + 1)
    charges.push(charge)
  end
  total = 0

  maxPositive = 0
  totalPositive = 0
  (300 * 300).times do |i|
    if i % 300 == 0
      maxPositive = [totalPositive, maxPositive].max
      totalPositive = 0
    end
    totalPositive += 1 if charges[i] > 0
    if charges[i] <= 0
      maxPositive = [totalPositive, maxPositive].max
      totalPositive = 0
    end
  end

  (3..maxPositive).each do |size|
    (0..(300 - size)).each do |x|
      (0..(300 - size)).each do |y|
        total = 0
        size.times do |b|
          total += charges[x + (y + b) * 300, size].sum
        end
        maxSquare = [total, x + 1, y + 1, size] if total > maxSquare[0]
      end
    end
  end

  puts "The square with the largest charge is starting at #{maxSquare[1]},#{maxSquare[2]}, with total power of #{maxSquare[0]} and size of #{maxSquare[3]}X#{maxSquare[3]}"
}

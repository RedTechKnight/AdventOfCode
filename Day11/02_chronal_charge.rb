require 'benchmark'

def cellPower(serialNum,x,y)
    rackID = x + 10
    power = rackID * y
    power += serialNum
    power *= rackID
    if(power/100 > 0)
        power = power.to_s()[-3].to_i()
    else
        power = 0
    end 
    power -= 5
    return power
end
puts Benchmark.measure(){


maxSquare = [1,1,1,1]
serialNumber = 8141
charges = Array.new()

(300*300).times(){
    |i|
    charge = cellPower(serialNumber,(i%300)+1,(i/300)+1)
    charges.push(charge)
}
total = 0

maxPositive = 0
totalPositive = 0
(300*300).times(){
    |i|
    if(i%300 == 0)
        maxPositive = [totalPositive,maxPositive].max()
        totalPositive = 0
    end
    if(charges[i] > 0)
        totalPositive += 1
    end
    if(charges[i] <= 0)
        maxPositive = [totalPositive,maxPositive].max()
        totalPositive = 0
    end
}

(3..maxPositive).each(){
    |size|
    (0..(300-size)).each(){
        |x|
        (0..(300-size)).each(){
            |y|
            total = 0
            size.times(){
                |b|
                total += charges[x+(y+b)*300,size].sum()

            }
            if(total > maxSquare[0])
                maxSquare = [total,x+1,y+1,size]
            end
        }
    }
}


puts "The square with the largest charge is starting at #{maxSquare[1]},#{maxSquare[2]}, with total power of #{maxSquare[0]} and size of #{maxSquare[3]}X#{maxSquare[3]}"
}
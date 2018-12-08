require 'benchmark'

puts Benchmark.measure(){

    input = File.new("inputs.txt",mode="r")

    steps = Hash.new()

    until(input.eof?())
        line = input.readline()
        before = line.match(/Step (\w)/)[1]
        after = line.match(/before step (\w)/)[1]
        
        unless(steps[after].nil?())
            steps[after].push(before)
        else
            steps[after] = Array.new()
            steps[after].push(before)
        end

        if(steps[before].nil?())
            steps[before] = Array.new()
        end
    end
    input.close()
    
    order = Array.new

    until(order.size() == steps.size())

        nextStep = steps.select(){
            |key,val|
            val.size() < 1 and order.none?(key)
        }.to_a()

        nextStep.sort!()

        steps.transform_values(){
            |val|
            val.delete(nextStep[0][0])
        }

        order.push(nextStep[0][0])
    end

    str = String.new()
    
    order.each(){
        |s|
        str.concat(s)
    }

    puts "Order steps should be executed in: #{str}"
}
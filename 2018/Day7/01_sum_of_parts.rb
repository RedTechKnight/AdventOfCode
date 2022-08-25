require 'benchmark'

puts Benchmark.measure {
  input = File.new('inputs.txt', mode = 'r')

  steps = {}

  until input.eof?
    line = input.readline
    before = line.match(/Step (\w)/)[1]
    after = line.match(/before step (\w)/)[1]

    if steps[after].nil?
      steps[after] = []
      steps[after].push(before)
    else
      steps[after].push(before)
      end

    steps[before] = [] if steps[before].nil?
  end
  input.close

  order = []

  until order.size == steps.size

    nextStep = steps.select do |key, val|
      val.empty? && order.none?(key)
    end.to_a

    nextStep.sort!

    steps.transform_values do |val|
      val.delete(nextStep[0][0])
    end

    order.push(nextStep[0][0])
  end

  str = ''

  order.each do |s|
    str.concat(s)
  end

  puts "Order steps should be executed in: #{str}"
}

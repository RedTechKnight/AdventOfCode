require 'benchmark'
input = File.new('input_day1', 'r')

def find_duplicate_frequency(data)
  frequencies = {}
  current_freq = 0
  data.readlines.cycle do |val|
    current_freq += val.to_i
    break if frequencies[current_freq]

    frequencies[current_freq] = true
  end
  current_freq
end

puts Benchmark.measure('Time taken: ') {
  puts("The duplicate frequency is: #{find_duplicate_frequency(input)}")
}

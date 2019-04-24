require 'benchmark'
def calculate_checksum(data)
  two_letter = 0
  three_letter = 0
  data.readlines.each do |line|
    chars = line.chars
    two_letter += 1 unless chars.filter { |char| chars.count(char) == 2 }.empty?
    three_letter += 1 unless chars.filter { |char| chars.count(char) == 3 }.empty?
  end
  two_letter * three_letter
end

puts Benchmark.measure {
  input = File.new('input_day2', mode = 'r')
  puts("Checksum value is #{calculate_checksum(input)}")
}

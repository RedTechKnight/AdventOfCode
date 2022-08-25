require 'benchmark'
puts Benchmark.bmbm { |benchmarker|
  benchmarker.report('Reduction') do
    startTime = Time.now
    input = File.new('inputs.txt', mode = 'r')
    polymer = ''
    polymer.concat(input.readline) until input.eof?
    input.close

    polymer.strip!
    units = 'abcdefghijklmnopqrstuvwxyz'

    noMatches = false
    until noMatches
      noMatches = true
      units.each_char do |c|
        str0 = polymer.gsub!(/[#{c}][#{c.upcase}]/, '')
        noMatches = false unless str0.nil?
        str1 = polymer.gsub!(/[#{c.upcase}][#{c}]/, '')
        noMatches = false unless str1.nil?
      end
    end

    puts "After full reduction, #{polymer.size} units remain!"
    endTime = Time.now
  end
}

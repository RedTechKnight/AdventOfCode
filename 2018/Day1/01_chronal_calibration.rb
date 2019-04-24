input = File.new('input_day1', mode = 'r')
print "The final frequency is: #{input.readlines.map(&:to_i).sum}"
input.close

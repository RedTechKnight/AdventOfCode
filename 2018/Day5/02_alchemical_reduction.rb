startTime = Time.now
input = File.new('inputs.txt', mode = 'r')
polymer = ''
polymer.concat(input.readline) until input.eof?
input.close
polymer.strip!

units = 'abcdefghijklmnopqrstuvwxyz'
problematicUnit = 'a'
shortestLength = polymer.size

units.each_char  do |unwanted|
  moddedPolymer = polymer.gsub(/[#{unwanted}#{unwanted.upcase}]/, '')

  noMatches = false
  until noMatches
    noMatches = true
    units.each_char do |c|
      str0 = moddedPolymer.gsub!(/[#{c}][#{c.upcase}]/, '')
      noMatches = false unless str0.nil?
      str1 = moddedPolymer.gsub!(/[#{c.upcase}][#{c}]/, '')
      noMatches = false unless str1.nil?
    end
  end
  if moddedPolymer.size < shortestLength
    shortestLength = moddedPolymer.size
    problematicUnit = unwanted
  end
end
endTime = Time.now

puts "The shortest length is #{shortestLength} and the most problematic unit is #{problematicUnit}!"
puts "Time taken to solve this: #{endTime - startTime}!"

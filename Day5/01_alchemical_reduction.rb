startTime = Time.now()
input = File.new("inputs.txt",mode="r")
polymer = ""
until(input.eof?())
    polymer.concat(input.readline())
end
input.close()

polymer.strip!()
units = "abcdefghijklmnopqrstuvwxyz"

noMatches = false
until(noMatches) do
    noMatches = true
    units.each_char(){
        |c|
        str0 = polymer.gsub!(/[#{c}][#{c.upcase()}]/,'')
        noMatches = false unless str0.nil?()
        str1 = polymer.gsub!(/[#{c.upcase()}][#{c}]/,'')
        noMatches = false unless str1.nil?()
    }
end

puts "After full reduction, #{polymer.size()} units remain!"
endTime = Time.now()

puts "Time taken to solve this: #{endTime - startTime}!"
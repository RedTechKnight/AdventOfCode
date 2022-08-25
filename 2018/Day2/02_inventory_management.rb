startTime = Time.now

input = File.new('input_day2', mode = 'r')

boxIDs = []
until input.eof?

  line = input.readline.strip
  boxIDs.push(line)
end

boxesFound = false

until (boxesFound == true) || boxIDs.empty?
  currentID = boxIDs.delete_at(0)

  matches = boxIDs.find_all do |id|
    currentID.delete(id).size < 2
  end

  next if matches.empty?

  matches.each do |match|
    charInd = 0
    mismatched = 0
    similar = ''

    while charInd < match.size

      if match[charInd] != currentID[charInd]
        mismatched += 1
      else
        similar.concat(match[charInd])
        end

      charInd += 1
    end

    next unless mismatched == 1

    output = File.new('output.txt', mode = 'w')
    output.write(similar)
    output.close
    puts("Found boxes #{match} and #{currentID}. Similar Characters in ID: #{similar}")
    boxesFound = true
  end

end

endTime = Time.now
time = endTime - startTime

puts "Completed in #{time} seconds!"

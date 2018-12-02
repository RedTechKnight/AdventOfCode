startTime = Time.now()

input = File.new("inputs.txt",mode = "r")

boxIDs = Array.new()
until(input.eof?) do

    line = input.readline.strip()
    boxIDs.push(line)
end

boxesFound = false
commonLetters = ""

until((boxesFound == true) or (boxIDs.size < 1)) do
    currentID = boxIDs.delete_at(0)

    matches = boxIDs.find_all(){
        |id|
        currentID.delete(id).size < 2
    }

    unless(matches.empty?())
        
        for match in matches do
            
            charInd = 0
            mismatched = 0
            similar = String.new()
            
            while((charInd < match.size()))  do
                
                if(match[charInd] != currentID[charInd])
                    mismatched += 1
                else
                    similar.concat(match[charInd])
                end

                charInd += 1
            end

            if(mismatched == 1)
                output = File.new("output.txt",mode="w")
                output.write(similar)
                output.close()
                puts("Found boxes #{match} and #{currentID}. Similar Characters in ID: #{similar}")
                boxesFound = true
            end
        end
    end

end

endTime = Time.now()
time = endTime - startTime

puts "Completed in #{time} seconds!"
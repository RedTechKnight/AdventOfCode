startTime = Time.now()

input = File.new("inputs.txt",mode="r")

twoLetters = 0
threeLetters = 0

until(input.eof?()) do
    line = input.readline()
    
    uniqueChars = Hash.new()

    charCount = 0
    while(charCount < line.size) do
        uniqueChars[line[charCount]] = true
        charCount += 1;
    end

    charCount = 0
    twoFound = false
    threeFound = false
    for char in uniqueChars do
        if(line.count(char[0]) == 2)
            twoLetters += 1 unless twoFound
            twoFound = true
        end
        if(line.count(char[0]) == 3)
            threeLetters += 1 unless threeFound
            threeFound = true
        end
    end

end

checksum = twoLetters * threeLetters
puts("Checksum value is #{checksum}")
endTime = Time.now()

timeTaken = endTime - startTime

puts "Results achieved after #{timeTaken} seconds."
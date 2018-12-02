startTime = Time.now

input = File.new("inputs.txt",mode="r")
frequencies = Hash.new()
freqChanges = Array.new()

currentFreq = 0

until(input.eof?())
    freqChanges.push(input.readline.to_i)
end

input.close

duplicateFound = false
duplicateFreq = 0

freqIndex = 0
until((duplicateFound) or (freqIndex >= freqChanges.size)) do
    frequencies[currentFreq] = true

    currentFreq += freqChanges[freqIndex]

    if(frequencies[currentFreq] == true)
        duplicateFound = true
        duplicateFreq = currentFreq 
    end
    freqIndex += 1
end

iterations = 0

freqIndex = 0
until (duplicateFound == true) do
    currentFreq += freqChanges[freqIndex]

    if(frequencies[currentFreq] == true)
        duplicateFound = true
        duplicateFreq = currentFreq
    end
    
    freqIndex += 1
    if(freqIndex >= freqChanges.size)
        freqIndex = 0
        iterations += 1
    end
end

if(duplicateFound)
    print "Duplicate found in first frequency list after ",iterations," loops! Frequency: ",duplicateFreq,"\n\n"
end

endTime = Time.now
time = endTime - startTime
print "Time Taken: ",time,"! \n\n"
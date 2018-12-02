startTime = Time.now

input = File.new("inputs.txt",mode="r")
frequencies = Array.new
freqChanges = Array.new

currentFreq = 0

until input.eof?
    freqChanges.push(input.readline.to_i)
end

input.close

duplicateFound = false
duplicateFreq = 0

freqIndex = 0

until (duplicateFound) or (freqIndex >= freqChanges.size) do
    frequencies.push(currentFreq)
    currentFreq += freqChanges[freqIndex]

    repeated = frequencies.find {
        |freq| 
        currentFreq == freq
    }

    unless repeated.nil?
        duplicateFreq = repeated
        duplicateFound = true
    end
    freqIndex += 1
end

frequencies = frequencies.sort
iterations = 0

freqIndex = 0
until (duplicateFound == true) do
    currentFreq += freqChanges[freqIndex]

    repeaeted = frequencies.find{
        |freq|
        freq == currentFreq
    }

    unless repeaeted.nil?
        duplicateFreq = repeaeted
        duplicateFound = true
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
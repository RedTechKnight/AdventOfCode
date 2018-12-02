input = File.new("inputs.txt",mode="r")

finalFreq = 0

until(input.eof?()) do
    finalFreq += input.readline.to_i()
end

print "The final frequency is: ",finalFreq
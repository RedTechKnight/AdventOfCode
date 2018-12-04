startTime = Time.now()
input = File.new("inputs.txt",mode="r")

class Event 

    def initialize(time,event)
        @time = time
        @event = event
    end
    attr_reader :time
    attr_reader :event

end

events = Array.new()

until(input.eof?())
    i = input.readline()
    a,y,mon,d,h,min = i.match(/(\d+)-(\d+)-(\d+) (\d+):(\d+)/).to_a()
    e = i.split(/] /)
    time = Time.new(y,mon,d,h,min)
    events.push(Event.new(time,e[1]))
end

activeGuard = 0
guards = Hash.new()

events.sort!(){
    |a,b|
    a.time <=> b.time
}

Guard = Struct.new(:id,:asleepTime)
GuardEvent = Struct.new(:state,:time)

for event in events do
    guard = event.event.match(/#(\d+)/) 

    unless(guard.nil?())
        activeGuard = guard[1].to_i()
        guards[activeGuard] = Array.new() if guards[activeGuard].nil?()
    end

    state = event.event.match(/\w+/)
    state = state.to_s() unless state.nil?()
    if(state == "falls")
        guards[activeGuard].push(GuardEvent.new("asleep",event.time))
    elsif(state == "wakes")
        guards[activeGuard].push(GuardEvent.new("awoken",event.time))
    end
end

guardTimes = Array.new()

guards.each(){
    |key,value|
    guard = Guard.new()
    guard.id = key
    guard.asleepTime = Hash.new()
    index = 0
    while(index < value.size()) do
        (value[index].time.min...value[index+1].time.min).each(){
            |min|
            unless(guard.asleepTime[min].nil?())
                guard.asleepTime[min] += 1
            else
                guard.asleepTime[min] = 1
            end
        }
        index += 2
    end
    guardTimes.push(guard)
}

sleepiestMinute = 0
timesSlept = 0
sleepiestGuard = 0

guardTimes.each(){
    |guardTime|

    min = guardTime.asleepTime.find(){
        |key,value|
        value > timesSlept
    }

    unless(min.nil?())
        sleepiestMinute = min[0]
        timesSlept = min[1]
        sleepiestGuard = guardTime.id
    end
}

consistentSleeper = guardTimes.find(){
    |guard|
    guard.id == sleepiestGuard
}

consistentSleeper.asleepTime.each(){
    |key,value|
    
    unless(value < timesSlept)
        sleepiestMinute = key
        timesSlept = value
    end
}

puts "The most consistently sleepy guard is \##{sleepiestGuard}, on minute #{sleepiestMinute} for a total of #{timesSlept} times!"
endTime = Time.now()
puts "Solution found in #{endTime - startTime} seconds!"
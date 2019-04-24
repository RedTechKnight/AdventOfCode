startTime = Time.now
input = File.new('input_day4', mode = 'r')

class Event
  def initialize(time, event)
    @time = time
    @event = event
  end
  attr_reader :time
  attr_reader :event
end

events = []

until input.eof?
  i = input.readline
  a, y, mon, d, h, min = i.match(/(\d+)-(\d+)-(\d+) (\d+):(\d+)/).to_a
  e = i.split(/] /)
  time = Time.new(y, mon, d, h, min)
  events.push(Event.new(time, e[1]))
end

activeGuard = 0
guards = {}

events.sort! do |a, b|
  a.time <=> b.time
end

Guard = Struct.new(:id, :asleepTime)
GuardEvent = Struct.new(:state, :time)

events.each do |event|
  guard = event.event.match(/#(\d+)/)

  unless guard.nil?
    activeGuard = guard[1].to_i
    guards[activeGuard] = [] if guards[activeGuard].nil?
  end

  state = event.event.match(/\w+/)
  state = state.to_s unless state.nil?
  if state == 'falls'
    guards[activeGuard].push(GuardEvent.new('asleep', event.time))
  elsif state == 'wakes'
    guards[activeGuard].push(GuardEvent.new('awoken', event.time))
  end
end

guardTimes = []

guards.each  do |key, value|
  guard = Guard.new
  guard.id = key
  guard.asleepTime = []
  index = 0
  while index < value.size
    guard.asleepTime.push(value[index].time.min...value[index + 1].time.min)
    index += 2
  end
  guardTimes.push(guard)
end

highestTimeSleeping = 0
sleepiest = 0

guardTimes.each do |time|
  totalTime = 0
  time.asleepTime.each do |t|
    totalTime += (t.end - 1 - t.begin)
  end
  if totalTime > highestTimeSleeping
    sleepiest = time.id
    highestTimeSleeping = totalTime
  end
end

sleepiestGuard = guardTimes.find do |guard|
  guard.id == sleepiest
end

asleepMinutes = {}

sleepiestGuard.asleepTime.each do |time|
  time.each  do |min|
    if asleepMinutes[min].nil?
      asleepMinutes[min] = 1
    else
      asleepMinutes[min] += 1
      end
  end
end

sleepiestMinute = 0
timesSlept = 0
asleepMinutes.each do |key, val|
  if val > timesSlept
    sleepiestMinute = key
    timesSlept = val
  end
end

puts "The sleepiest guard of all is \##{sleepiestGuard.id} and he is asleep on the #{sleepiestMinute}th minute of his watch the most!"
endTime = Time.now

puts "Solution found in #{endTime - startTime} seconds!"

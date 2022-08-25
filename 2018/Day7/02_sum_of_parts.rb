require 'benchmark'

class Worker
  def initialize
    @task = ''
    @timeLeft = 0
  end
  attr_reader :task
  attr_writer :task
  attr_reader :timeLeft
  attr_writer :timeLeft
end

puts Benchmark.measure {
  input = File.new('inputs.txt', mode = 'r')

  steps = {}

  until input.eof?
    line = input.readline
    before = line.match(/Step (\w)/)[1]
    after = line.match(/before step (\w)/)[1]

    if steps[after].nil?
      steps[after] = []
      steps[after].push(before)
    else
      steps[after].push(before)
      end

    steps[before] = [] if steps[before].nil?
  end
  input.close

  stepDurations = {}
  index = 1
  ('A'..'Z').each do |letter|
    stepDurations[letter] = index
    index += 1
  end

  totalTime = 0
  workers = {}

  availableWorkers = 5
  availableWorkers.times do |i|
    workers[i] = Worker.new
  end

  done = []
  inProgress = []

  until done.size == steps.size

    lowestTime = 100_000

    workers.each do |_key, val|
      lowestTime = [val.timeLeft, lowestTime].min unless val.task == ''
    end

    transformed = false
    workers.transform_values! do |val|
      unless val.task == ''
        val.timeLeft -= lowestTime

        if val.timeLeft <= 0
          done.push(val.task)
          inProgress.delete(val.task)
          steps.transform_values do |value|
            value.delete(val.task)
          end
          val.task = ''
          val.timeLeft = 0
          transformed = true
        end

      end

      val
    end

    totalTime += lowestTime if transformed

    nextSteps = steps.select do |key, val|
      val.empty? && done.none?(key) && inProgress.none?(key)
    end.to_a
    nextSteps.sort!

    step = 0
    availableWorkers = workers.count do |val|
      val[1].task == ''
    end

    until (availableWorkers < 1) || (step >= nextSteps.size)
      nextStep = nextSteps[step][0]
      freeWorker = workers.find do |_key, val|
        val.task == ''
      end

      inProgress.push(nextStep)
      workers[freeWorker[0]].task = nextStep
      workers[freeWorker[0]].timeLeft = 60 + stepDurations[nextStep]
      step += 1
      availableWorkers -= 1
    end
  end

  puts "Total time taken to complete tasks: #{totalTime}"
}

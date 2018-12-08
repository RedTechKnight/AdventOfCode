require 'benchmark'

class Worker
    def initialize()
        @task = ""
        @timeLeft = 0
    end
    attr_reader :task
    attr_writer :task
    attr_reader :timeLeft
    attr_writer :timeLeft
end

puts Benchmark.measure(){

    input = File.new("inputs.txt",mode="r")

    steps = Hash.new()

    until(input.eof?())
        line = input.readline()
        before = line.match(/Step (\w)/)[1]
        after = line.match(/before step (\w)/)[1]
        
        unless(steps[after].nil?())
            steps[after].push(before)
        else
            steps[after] = Array.new()
            steps[after].push(before)
        end

        if(steps[before].nil?())
            steps[before] = Array.new()
        end
    end
    input.close()

    stepDurations = Hash.new()
    index = 1
    ('A'..'Z').each(){
        |letter|
        stepDurations[letter] = index
        index += 1
    }

    totalTime = 0
    workers = Hash.new()
    
    availableWorkers = 5
    availableWorkers.times(){
        |i|
        workers[i] = Worker.new()
    }

    done = Array.new()
    inProgress = Array.new()

    until(done.size() == steps.size())

        lowestTime = 100000

        workers.each(){
            |key,val|
            unless(val.task == "")
                lowestTime = [val.timeLeft,lowestTime].min()
            end
        }

        transformed = false
        workers.transform_values!(){
            |val|

            unless(val.task == "")
                val.timeLeft -= lowestTime

                if(val.timeLeft <= 0)
                    done.push(val.task)
                    inProgress.delete(val.task)
                    steps.transform_values(){
                        |value|
                        value.delete(val.task)
                    }
                    val.task = ""
                    val.timeLeft = 0
                    transformed = true
                end

            end

            val
        }

        if(transformed)
            totalTime += lowestTime
        end

        nextSteps = steps.select(){
            |key,val|
            val.size() < 1 and done.none?(key) and inProgress.none?(key)
        }.to_a()
        nextSteps.sort!()

        step = 0
        availableWorkers = workers.count(){
            |val|
            val[1].task == ""
        }

        until(availableWorkers < 1 or step >= nextSteps.size())
            nextStep = nextSteps[step][0]
            freeWorker = workers.find(){
                |key,val|
                val.task == ""
            }

            inProgress.push(nextStep)
            workers[freeWorker[0]].task = nextStep
            workers[freeWorker[0]].timeLeft = 60 + stepDurations[nextStep]
            step += 1
            availableWorkers -= 1
        end
    end


    puts "Total time taken to complete tasks: #{totalTime}"
}
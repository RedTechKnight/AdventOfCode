require 'benchmark'

class Node
  def initialize(data)
    @next = nil
    @prev = nil
    @data = data
  end

  def to_s
    "[#{@data}]"
  end
  attr_reader :next
  attr_reader :prev
  attr_reader :data
  attr_writer :next
  attr_writer :prev
  attr_writer :data
end

class List
  def initialize
    @current = nil
    @front = Node.new(-1)
    @front.next = @current
    @back = Node.new(-2)
    @back.next = @front
    @front.prev = @back
    @size = 0
  end

  def forward
    @current = if @current.next.data == -2
                 @front.next
               else
                 @current.next
               end
  end

  def backward
    @current = if current.prev.data == -1
                 @back.prev
               else
                 @current.prev
               end
  end

  def insert(node)
    if @current.nil?
      @current = node
      @current.next = @back
      @current.prev = @front
      @front.next = @current
      @front.prev = @back
      @back.prev = @current
      @back.next = @front
      @size += 1
    else
      @current.next.prev = node
      temp = @current.next
      @current.next = node
      node.next = temp
      node.prev = @current
      @current = @current.next
      @size += 1
      end
  end

  def remove
    unless @size < 1
      @size -= 1
      rem = @current
      @current.prev.next = @current.next
      @current.next.prev = @current.prev
      @current = @current.next
      rem
    end
  end

  def to_s
    node = @front
    str = "[#{node.data}] -> "
    node = node.next
    until node.data == -1
      str.concat("[#{node.data}] -> ")
      node = node.next
    end
    str.concat("[#{node.data}]")
    str
  end
  attr_reader :front
  attr_reader :back
  attr_reader :current
end
puts Benchmark.measure {
  input = File.new('inputs.txt', mode = 'r').readline

  playerCount = input.match(/(\d+) players/).to_a[1].to_i
  lastMarble = input.match(/(\d+) points/).to_a[1].to_i * 100

  players = {}

  playerCount.times  do |x|
    players[x + 1] = 0
  end

  marbles = List.new
  marbles.insert(Node.new(0))
  current = 1
  player = 1

  until current > lastMarble
    player = 1 if player > playerCount
    if (current % 23) == 0
      8.times do
        marbles.backward
      end
      players[player] += marbles.current.data + current
      marbles.remove
      marbles.forward
      player += 1
      current += 1
    else
      marbles.insert(Node.new(current))
      current += 1
      player += 1
      marbles.forward
      end
  end
  puts "The player with the maximum score has #{players.max_by { |pl| pl[1] }[1]} points"
}

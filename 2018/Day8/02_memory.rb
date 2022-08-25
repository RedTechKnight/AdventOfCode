require 'benchmark'

puts Benchmark.measure {
  input = File.new('inputs.txt', mode = 'r')

  root = input.readline.split(/ /).map(&:to_i)

  def getTree(node)
    children, metadata = node[0, 2]
    res = node[2, node.size]
    tree = {}
    if children > 0
      children.times do |x|
        res, t = getTree(res)
        tree[x + 1] = t
      end
    end
    arr = []
    metadata.times do |_x|
      arr.push(res[0])
      res.shift
    end
    tree[-1] = arr
    [res, tree]
  end

  tree = getTree(root)[1]

  def calcRoot(tree, val)
    indices = tree[-1]
    val = 0
    if tree.size > 1
      indices.each do |ind|
        unless tree[ind].nil?
          b = calcRoot(tree[ind], val)[1]
          val += b
          end
      end
    else
      indices.each do |ind|
        val += ind
      end
    end
    [tree, val]
  end

  puts "The value of the root node is: #{calcRoot(tree, 0)[1]}"
}

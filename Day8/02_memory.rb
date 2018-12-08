require 'benchmark'

puts Benchmark.measure(){
    input = File.new("inputs.txt",mode="r")

    root = input.readline().split(/ /).map(){|x| x.to_i()}
    
    def getTree(node)
        children,metadata = node[0,2]
        res = node[2,node.size()]
        tree = Hash.new()
        if(children > 0)
            children.times(){
                |x|
                res,t = getTree(res)
                tree[x+1] = t
            }
        end
        arr = Array.new()
        metadata.times(){
           |x|
            arr.push(res[0])
            res.shift()
        }
        tree[-1] = arr
        return res,tree
    end

    tree = getTree(root)[1]

    def calcRoot(tree,val)
        indices = tree[-1]
        val = 0
        if(tree.size() > 1)
            indices.each(){
                |ind|
                unless(tree[ind].nil?())
                    b = calcRoot(tree[ind],val)[1]
                    val += b
                end
            }
        else
            indices.each(){
                |ind|
                val += ind
            }
        end
        return tree,val
    end

    puts "The value of the root node is: #{calcRoot(tree,0)[1]}"
}
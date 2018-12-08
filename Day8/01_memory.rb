require 'benchmark'

puts Benchmark.measure(){
    input = File.new("inputs.txt",mode="r")

    root = input.readline().split(/ /).map(){|x| x.to_i()}
    input.close()

    data = Array.new()
    
    def traverse(node,total)
        children,metadata = node[0,2]
        res = node[2,node.size()]
        if(children > 0)
            children.times(){
                res,total = traverse(res,total)
            }
        end
        metadata.times(){
           |x|
            total += res[0]
            res.shift()
        }
        return res,total
    end
    
    print "The total of the metadata in all nodes is: #{traverse(root,0)[1]}"
}
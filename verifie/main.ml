module Graph = GraphStructure.Graph
module Util = InstanceConverter

open Graph

let () = 

  let graph = Graph.create 9 in 
  Graph.add graph 0 {adj = [1 ; 2 ; 3 ; 4] ; color = 0 ; degree = 4 ; dsat = 0};
  Graph.add graph 1 {adj = [0 ; 2 ; 3 ; 5] ; color = 0 ; degree = 4 ; dsat = 0};
  Graph.add graph 2 {adj = [0 ; 1] ; color = 0 ; degree = 2 ; dsat = 0};
  Graph.add graph 3 {adj = [0 ; 1] ; color = 0 ; degree = 2 ; dsat = 0};
  Graph.add graph 4 {adj = [0 ; 6 ; 7] ; color = 0 ; degree = 3 ; dsat = 0};
  Graph.add graph 5 {adj = [1 ; 6 ; 7] ; color = 0 ; degree = 3 ; dsat = 0};
  Graph.add graph 6 {adj = [4 ; 5 ; 7] ; color = 0 ; degree = 3 ; dsat = 0};
  Graph.add graph 7 {adj = [4 ; 5 ; 6] ; color = 0 ; degree = 3 ; dsat = 0};

  Util.save_graph "instances/demo.ig" graph 
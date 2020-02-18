functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex
  (*定义graph类型为邻接表*边的数量*节点的数量*)
  type graph = (((Set.set) table)*int*int)
  (*定义asp,用来储存每个节点指向父节点的边*)
  type asp = (vertex seq table)

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph =
  let
    (*该句主要为构造邻接表，利用collect操作得到使得每一个元素指向其相邻元素的table
      主要为collect的复杂度，即O(|E|log|E|) work 和 O(log平方|E|) span*)
    val table_graph = Table.map (Set.fromSeq) (Table.collect E)
    (*要求节点的集合，即将table_graph的key和value(这构成图中所有节点)进行union，得到结果
      主要为union的复杂度，union复杂度为O(mlog(1+n/m)) work 和 O(log2 (1+n/m))
      在这里m肯定小于|E|，复杂度不会超过O(|E|log|E|) work 和 O(log2 |E|) span *)
    val vertices = Set.union(domain table_graph, Set.fromSeq (map #2 E))
  in
    (*返回值为：第一个：指向邻居的table_graph，第二个：边的个数，第三个：节点的个数*)
    (table_graph, length E, Set.size vertices)
  end

  (* Task 2.2 *)
  (*边的数量即为graph的第二个元素*)
  fun numEdges (G : graph) : int = #2 G
  (*节点的数量即为graph的第三个元素*)
  fun numVertices (G : graph) : int = #3 G

  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
    (*如果v元素指向的元素为空，则它邻居为空
      否则它指向的就是它的outneighbors 
      有O(|Vout|+log|V|) work 和 O(log|V|) span*)
    case find (#1 G) v of
        NONE => Seq.empty()
       |SOME s => Set.toSeq s

  (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp =
    (*构造一个由子节点指向父节点的table，即我们所求的asp，开始节点为v*)
    case find (#1 G) v of 
      (*如果v节点没有邻接的边，即返回空的table*)
       NONE => Table.empty()
      |SOME _ =>
        let
          fun BFS (X,F) =
            if size F = 0 then X else
            let
              (*这个函数将s指向它的父节点(outNeighbors)*)
              fun mkparentpair s = Seq.map (fn v => (v,s)) (outNeighbors G s)
              (*这一步将所有F中的元素指向好的pairs进行flatten之后collect合并，最终变为F中的元素每个都指向其父元素的table*)
              val table_parent_pair = Table.collect (flatten (map mkparentpair (Set.toSeq (domain F))))
              (*这一步将X和F合并，形成新的X，已访问过的节点*)
              val update_X = Table.merge (fn(a,b) => a) (X,F)
              (*这一步将新的将新的Frontier，即从集合中去掉已经访问过的X*)
              val update_F = Table.erase (table_parent_pair, domain update_X)
            in
              (*新的X和F，递归进行BFS操作*)
              BFS(update_X, update_F)
            end
        in
          (*初始状态，已访问过节点集合X为空，Frontier为v*)
          BFS(Table.empty(), Table.singleton(v,Seq.empty()))
        end
  (*复杂度分析：这实际上实现了一个从子节点指向父节点的BFS，对于这个算法，最大复杂度体现在第二行即table_parent_pair上，
    我们对于图中的每个边都走了两遍，(首先，我们由父节点指向子节点走了一次，用子节点指向父节点又走了一次)，
    图的节点数为V，整个work保持在O(|E| log |V |)
    又由于树深度为D,整个span能保持在O(Dlog2 |V|*)

  (* Task 2.5 *)
fun report (A : asp) (v : vertex) : vertex seq seq =
 let 
  fun DFS (v : vertex) : vertex list seq =
      (*寻找节点v是否在A中，如果不在，依照题目要求返回空串，如果为孤立点，它的path为它自己*)
      case Table.find A v of
         NONE => Seq.empty ()
        |SOME parents => if (Seq.length parents) = 0 then Seq.singleton [v]
          else
          (*其他情况，从v开始，递归调用DFS，倒过来沿着每条路径去找初始节点*)  
           let
              fun path_to_parent u = DFS u
              fun add_v vpath = [v] @ vpath
              val parent_vpath = Seq.flatten (Seq.map path_to_parent parents)
              val vpath = Seq.map add_v parent_vpath
            in
              vpath
            end          
  in
  (*将串翻转，得到u到v的路径*)
    let
      fun reverse_seq_vpath vpath = Seq.rev (Seq.fromList vpath)
      val all_vpath = DFS v
      val afterRev_vpath = Seq.map (reverse_seq_vpath) (all_vpath)
    in
      afterRev_vpath
    end
  end
  (*复杂度分析：首先注意到find操作复杂度为O(log|V|),接下来，我们沿着每条路去找，由于路长为|L|,路径数为|P|,
    所以整体复杂度不超过O(|P||L| log |V |)*)
end

functor MkAStarCore(structure Table : TABLE
                    structure PQ : PQUEUE
                      where type Key.t = real) : ASTAR =
struct
  structure Set = Table.Set
  structure Seq = Set.Seq

  exception NYI

  type weight = real
  type vertex = Set.key
  type edge = vertex * vertex * weight
  type heuristic = vertex -> real
  type 'a table = 'a Table.table


  (* Define this type yourself *)
  type graph = unit
  
  fun makeGraph (E : edge Seq.seq) : graph =
    raise NYI
  (*A Simple modification of the Dijkstra algorithm*)
  fun findPath h G (S, T) = 
    raise NYI
end
functor MkBridges(structure STSeq : ST_SEQUENCE) : BRIDGES =
struct
  structure Seq = STSeq.Seq
  type 'a stseq = 'a STSeq.stseq
  open Seq

  exception NYI

  type vertex = int
  type edge = vertex * vertex
  type edges = edge seq

  type ugraph = unit

  fun makeGraph (E : edge seq) : ugraph = 
  raise NYI

  fun findBridges (G : ugraph) : edges = 
    raise NYI
end

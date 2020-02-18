functor MkThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq
  open ASP


  type thesaurus = graph

  (* Task 3.1 *)
  fun make (S : (string * string seq) seq) : thesaurus =
    (**)
    let
      (*这里对于一个w，作出由word指向synonyms的pair*)
      fun single_mkedges (w, syn) = Seq.map (fn s => (w, s)) syn
      (*对于S中的每一个w，都作single_mkedges操作，构造所有的pairs*)
      val mkedges = flatten (map single_mkedges S)
    in
      (*将edges数对变成图*)
      makeGraph mkedges
    end


  (* Task 3.2 *)
  (*word的数量就是graph中节点的数量*)
  fun numWords (T : thesaurus) : int = numVertices T

  (*每个w的近义词就是它的outneighbors*)
  fun synonyms (T : thesaurus) (w : string) : string seq = outNeighbors T w

  (* Task 3.3 *)
  (*要返回w1到w2的all shortest path，只需要对其进行report操作
    这里我们以w1为原点，构造一个asp，report w1到w2的all shortest path*)
  fun query (T : thesaurus) (w1 : string) (w2 : string) : string seq seq =
     report (makeASP T w1) w2

end

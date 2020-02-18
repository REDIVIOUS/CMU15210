functor MkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  (* Include all the functionalities of the standard Table *)
  open Table

  (* This is defined after "open" so it doesn't get overwritten *)
  structure Key = Tree.Key
  type key = Key.t

  (* Remember, type 'a table = 'a Tree.bst *)

(*first求解思路：1.如果树为NONE，则返回NONE
 2.若不为NONE，则沿着左子树一直找到叶子结点，递归完成这个函数，返回叶子结点的值*)
  fun first (T : 'a table) : (key * 'a) option =
    case (Tree.expose T) of 
      NONE => NONE
     |SOME {key, value, left, right} =>
        case (Tree.expose left) of 
          NONE => SOME(key,value)
          | _ => first (left)

(*last求解思路：1.如果树为NONE，则返回NONE
 2.若不为NONE，则沿着右子树一直找到叶子结点，递归完成，返回last的值*)
  fun last (T : 'a table) : (key * 'a) option =
    case (Tree.expose T) of 
      NONE => NONE
     |SOME {key, value, left, right} =>
        case Tree.expose right of 
          NONE => SOME(key,value)
          | _ => last (right)

	(*详见库中Mktreap.sml，splitAt(T,K)，将T分成三个部分，key比k小的一个table，一个pair(k,v)，和一个key比k大的table
   1.previous取splitAt后的第一个元素，即key比k小的table，取这个table的last，即为k之前key最大的元素
   2.next取splitAt后的第二个元素，即key比k大的table，取这个table的first，即为k之后key最小的元素
   *)	      
  fun previous (T : 'a table) (k : key) : (key * 'a) option =
    last (#1 (Tree.splitAt (T, k)))

  fun next (T : 'a table) (k : key) : (key * 'a) option =
    first (#3 (Tree.splitAt (T, k)))

  (*详见库中Mktreap.sml，要把两个table进行join，我们直接调用库中的join函数即可*)
  fun join (L : 'a table, R : 'a table) : 'a table =
    Tree.join (L, R)
    
  (*详见库中Mktreap.sml，要将table进行split，获得key比k小的一个table，一个pair(k,v)，和一个key比k大的table，直接调用函数splitAt(T,k)即可*)
  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
    Tree.splitAt(T,k)
  
  (*我们要做的是获取key的值在low和high之间的一个table(含low和high)，步骤如下：
    1.首先我们从lowerbound(low)进行split，取右子树，我们得到了key大于等于low的树，并且注意，如果low是这棵树的key，我们要把low这个节点加入
    2.对于我们已经取出的key大于等于low的树，对其upperbound进行界定，在high进行split，取左子树，同样，如果high是这棵树的key，则加入high这个节点
    3.返回截取好的树*)
  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
    let
     val lower_bound = case split(T, low) of 
        (L, NONE, R) => R
       |(L, SOME v, R ) => join(Tree.singleton(low,v),R)
     val upper_bound = case split(lower_bound,high) of
        (L, NONE, R) => L 
       |(L, SOME v, R) => join(L,Tree.singleton(high,v))
    in
      upper_bound
    end
end
functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare

  (* Define this yourself *)
  type countTable = (Key.t table * Key.t) table

  (*算法思路实现：
    我们构造一个table，table的key是x的值，table的value是一个table和另一个数的数对，我们用这个table来存储比当前x小于或等于的x所对应的y值，而这另一个数就是当前的y值，实现如下：
   1.我们按照x对整个table进行排序。
   2.定义一个函数table_insert,参数值为((count_table, y_table),(x,y))，其中count_table是我们要求的table，y_table用来存储比当前x小的x所对应的y值，(x,y)为当前要加入的table的数对
   3.用iterate调用该函数，构造该table，将(x,y)一个个加入，知道iterate完成*)
  fun makeCountTable (S : point seq) : countTable =
    if Seq.length S = 0 then empty()
    else
      let
        fun compare_x ((x1,_),(x2,_)) = compareKey(x1,x2)
        val sorted_x = Seq.sort (compare_x) (S)
        fun table_insert ((count_table, y_table),(x,y)) =
          let 
            val new_y_table = insert (fn (v,v') => v) ((y,y)) (y_table)
            val new_count_table = insert (fn (v,v') => v)((x,(new_y_table, y))) (count_table)        
          in
            (new_count_table, new_y_table)
          end
        val (final_table, _) = Seq.iter(table_insert)((empty(),empty()))(sorted_x)
      in
        final_table
      end
  (*算法求解思路：
    我们用小于xRght且y范围在yLo和yHi之间的点减去小于xLeft且y范围在yLo和yHi之间的点，然后加上x值为xLefty范围在yLo和yHi之间的点，得到范围内的点
    1.如果table的大小为0，则返回0
    2.大小不为0情况下，首先看小于xLeft且y范围在yLo和yHi之间的点，由于我们构造的countTable的value值为一个key为所有在当前x之前的点的y值和当前y值组成的数对，所以我们先
      寻找xLeft是不是在该countTable中，如果在，返回所有在该点及其之前的对应的y值(那个table)，如果不在，则找xLeft前一最大的x所对应的ytable
    3.对于截取出来的那个y值table进行getrange，命名为leftrange
    4.寻找x=xLeft这一条线上有没有区域内的点。如果xLeft上面有对应的y值且这个y值在yLo和yHi之间，则我们可以判断这个点是被重复减去的，命名为repeat_left
    5.再看小于等于xRght且y范围在yLo和yHi之间的点，若xRght在该countTable中返回所有在该点及其之前的对应的y值(那个table)，如果不在，则找xLeft前一最大的x所对应的ytable
    6.对于这回截取出来的那个y值table进行getrange,命名为rightrange
    7.将rightrange的大小减去leftrange的大小加上repeat_left(因为repeat_left本应该属于区域但被减去)，得到区域内点的数量*)

  fun count (T : countTable)
                   ((xLeft, yHi) : point, (xRght, yLo) : point) : int  =
      if size T = 0 then 0
      else 
        let
          val SOME(_,(left_y,y1)) = case (find(T)(xLeft)) of
                                     NONE => previous(T)(xLeft)
                                   | SOME(v) => SOME(xLeft,v)

          val leftrange = getRange(left_y)(yLo,yHi)

          fun whether_cross y1 = case (compareKey(y1,yHi),compareKey(y1,yLo)) of
                                 (GREATER,_) => 0
                                | (_,LESS) => 0
                                | (_,_) => 1

          val repeat_left = case (find(T)(xLeft)) of
                              NONE => 0
                             | _ => whether_cross y1


          val SOME(_,(right_y, y2)) = case (find(T)(xRght)) of
                                      NONE => previous(T)(xRght)
                                    |SOME y_pair => SOME(xRght,y_pair)

          val rightrange = getRange(right_y)(yLo,yHi)

        in
          OrdTable.size(rightrange) - OrdTable.size(leftrange) + repeat_left
        end
end
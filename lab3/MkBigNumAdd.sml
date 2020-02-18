functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  infix 6 ++
  exception NotYetImplemented
  datatype carry = GEN | PROP | STOP

  fun x ++ y =
    let
      val min = Int.min(length x, length y)
      val max = Int.max(length x, length y)

      (*定义一个pairs，模拟二进制数每一位的加法，得到的结果也是一个数对，第一个是本位的结果，第二个判断是否进位*)
      fun pairs (ZERO,ZERO) = (ZERO,STOP)
        | pairs (ONE,ONE) = (ZERO,GEN)
        | pairs (ZERO,ONE) = (ONE,PROP)
        | pairs (ONE,ZERO) = (ONE,PROP)

      (*定义first和second函数，分别用于取pairs数对的第一个和第二个，用于后续分成两部分计算本位值和进位值*)
      fun first (i,_) = i
      fun second (_,i) = i

      (*定义addbits函数，遍历两个bignum，使得加法的每一位都形成一个pairs，较小的数高位用ZERO补齐*)
      fun addbits a =
        if(a < min) then pairs ((nth x a),(nth y a))
        else if (a<length x) then pairs ((nth x a),ZERO)
        else if (a<length y) then pairs (ZERO,(nth y a))
        else (ZERO,STOP)

      (*计算x,y经过addbits的结果*)
      val addresult0 = tabulate addbits max

      (*下面我们首先来计算进位，判断整个加法完成时候到底有哪些位置会产生进位
        首先定义一个carrystep判断进位关系的产生、传递和停止关系
        然后carryseq取用每一位单独的进位结果，即addresult0每一项的第二位进行map
        然后执行scan操作，判断carry之后到底哪些位产生了真实进位
        最后很关键的一步，由于产生进位是从第二位开始的，是对下一位产生进位，所以进位关系是对于下一位而言，这里要在最低位补充一个PROP或者STOP
        (代码中最低位补充的是PROP)
        *)
      fun carrystep (a,PROP) = a
        | carrystep (_,GEN) = GEN
        | carrystep (_,STOP) = STOP
      val carryseq = map second addresult0
      val carryresult0 = scani carrystep PROP carryseq
      val carryresult = append(singleton(PROP), carryresult0)

      (*下面来计算每一位本位产生的值
        首先先取addresult0的每一项的第一个，即为本位值，同时为防止最高位向下一位进位，这里在给最高位留出一个位子，用ZERO补充
        *)
      val addresult1 = map first addresult0
      val addresult2 = append(addresult1, singleton(ZERO))

      (*下面将本位和进位加起来，        
        定义一个反转函数，如果碰到进位就调用这个函数，ZERO变为ONE，ONE变为ZERO(因为这一位多加了一个ONE)
        然后是主要的addtwoparts函数，对于本位的每一个数字，如果相应的碰到GEN，就产生位翻转，否则不变
        最后对每一位使用这个函数，用tabulate并行实现
        *)
      fun reverse ZERO = ONE
        | reverse ONE = ZERO

      fun addtwoparts i = 
        if (i >= length addresult2) then ZERO
        else if (nth carryresult i = GEN) then reverse (nth addresult2 i)
        else (nth addresult2 i)
        
      val result = tabulate addtwoparts (length addresult2)

    in
    (*最后判断最高位是否为ONE，若不为ONE，则为ZERO，去掉多余的零*)
      if(nth result ((length result)-1) = ONE) then result
      else subseq result (0, length result - 1)
    end
    
  val add = op++
end

functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  exception NotYetImplemented
  infix 6 ++ --
  fun x ++ y = BNA.add (x, y)
  fun x -- y = 
    let
      (*编写反转函数，将串y中的元素0变成1,1变成0，其他位用0补齐*)
      fun reverse y i =
        if i < 0 orelse i >= length y orelse nth y i = ZERO
          then ONE
          else ZERO
      (*将y的值翻转加一得到负数的值，用x加上y的负数，然后舍弃最高位得到减法的值*)
      val revadd = (singleton ONE) ++ (tabulate (reverse y) (length x)) ++ x
      val result0 = take (revadd , length x) 

    in
      result0
    end
      
  val sub = op--
end

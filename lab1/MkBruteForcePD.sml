functor MkBruteForcePD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq
  open Option210



  fun parenDist (parens : paren seq) : int option =
    let
    (*定义函数判断括号是否匹配*)
    fun parenMatch p =
      let
        fun pm (( NONE , _) | ( SOME 0, CPAREN )) = NONE
          | pm ( SOME c, CPAREN ) = SOME (c -1)
          | pm ( SOME c, OPAREN ) = SOME (c +1)
      in
        (iter pm (SOME 0) p) = (SOME 0)
      end
    in
    if(parenMatch parens = false orelse length parens = 0) then NONE
      else
        let
          (*这个函数的功能是计算第m位开始的(第m位为左括号时)闭括号的长度*)
          fun MatchDist parens m =
          (*如果第m位超过了整个串的长度或者是右括号，就返回长度0*)
          if((m >= length parens) orelse (nth parens m = CPAREN)) then 0 
            else 
              let 
                (*判断第n位是否可以匹配，若可以返回长度，若不行则到下一位*)
                fun ending n = if (n > length parens) then 0 
                                else if(parenMatch (subseq parens (m,n-m+1))) then (n-m+1)
                                else (ending (n+1)) 
              in 
                (*从第m+1位开始判断*)
                ending (m+1) 
              end 
          (*列出每个位置开始的闭括号的长度，寻找出最大值，即为我们所求的值*)
          val parensdists = tabulate (MatchDist parens) (length parens)
          val max = nth parensdists (argmax Int.compare parensdists)
        in
          SOME max
        end
    end
end


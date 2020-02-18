functor MkDivideAndConquerPD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq
  open Option210

(*m:闭括号中最大括号数
 *l:没有匹配的左括号数
 *r:没有匹配的右括号数
 *ld:最左边的左括号到串末尾的距离
 *rd:最右边右括号到串开始处的距离
*)
  fun parenDist (parens : paren seq) : int option =
    let
    fun mp (s0) =
      case showt s0
        of EMPTY => (SOME 0, 0, 0, 0, 0)
         | ELT OPAREN => (SOME 0, 1, 0, 1, 0)
         | ELT CPAREN => (SOME 0, 0, 1, 0, 1)
         | NODE (s1, s2) =>
           let
             val ((m1, l1, r1, ld1, rd1), (m2, l2, r2, ld2, rd2)) =
               par(fn () => mp s1, fn () => mp s2)
            (*如果左边没匹配左括号与右边没匹配右括号相等，则计算中间的闭括号长度m0便于后续与m1和m2比较
             *如果左边没匹配左括号比右边没匹配右括号要少，合并之后的l0就是右边的左括号数，合并之后的r0是左右两边括号总数减去已匹配的左边左括号数
             *如果左边没匹配左括号不比右边没匹配右括号要少，合并之后l0就是两边括号数之和减去右边已经匹配的右括号，合并之后的r0就是左边的右括号数
             *重新计算左边最左到末尾以及右边最右到开始的距离
             *如果左边没匹配左括号数大于右边没匹配右括号数，则左边左括号没匹配完，新的ld就是ld1加上右边的总长度，否则就是右边的左括号数
             *如果右边没匹配右括号数大于左边没匹配左括号数，则右边右括号没匹配完，新的的rd就是rd2加上左边的总长度，否则就是左边的右括号数
             *)
             val m0 = if l1 = r2 then rd2 + ld1 else 0
             val l0 = if l1 < r2 then l2 else l1+l2-r2
             val r0 = if l1 < r2 then r1+r2-l1 else r1
             val ld0 = if l1 > r2 then ld1 + length s2 else ld2
             val rd0 = if r2 > l1 then rd2 + length s1 else rd1
           in
             (Option210.intMax (Option210.intMax (m1, m2), SOME m0), l0, r0, ld0, rd0)
           end
      val (max, oparen, cparen, _, _) = mp parens
    in
    (*如果左右括号都完全匹配，则返回max，否则没有匹配返回NONE*)
    if oparen = 0 andalso cparen = 0 andalso length parens > 0 then max else NONE
    end
  end

functor MkBigNumMultiply(structure BNA : BIGNUM_ADD
                         structure BNS : BIGNUM_SUBTRACT
                         sharing BNA.Util = BNS.Util) : BIGNUM_MULTIPLY =
struct
  structure Util = BNA.Util
  open Util
  open Seq
  open Primitives
  exception NotYetImplemented

  infix 6 ++ --
  infix 7 **

  fun x ++ y = BNA.add (x, y)
  fun x -- y = BNS.sub (x, y)
  fun x ** y =
    let 
      (*我们的思路是先把x,y变得同样长且长度为偶数，方便后续divide
        不够的位数我们用0补，定义addzero往高位补n个0
        makeaqual函数把两个bignum变得一样长，即若谁短，谁补差值个0
        makeiteven函数即在两个穿长度相同情况下，若长度为奇数，则高位加0补为偶数
        *)
      fun addzero (x,n) = append(x,(tabulate (fn i => ZERO) n))
      fun makeequal (x,y) =
        case Int.compare(length x, length y) of
            LESS =>((addzero(x,length y - length x)),y)
          | GREATER => (x,addzero(y,length x - length y))
          | _ => (x,y)
      fun makeiteven (x,y) = 
        case(length x) mod 2 of
            0 => (x,y)
          | 1 => (addzero(x,1),addzero(y,1))
      (*定义pow2意思为x乘以2的n次方，实际上就是往左移动n位，低位用0补齐*)
      fun pow2 (x,n) = 
          case length x of 
                0 => empty()
              | _ => append((tabulate(fn i => ZERO) n), x)
    in
      let
        (*先把两个串变得一样长，若长度是0，就是空，若长度是1且两个都是ONE，证明为ONE，否则为ZERO
          其余的，执行divide操作，先把串长n变成偶数，定义p、q、r、s分别为x1的高n/2位，低n/2位，x2的高n/2位，低n/2位
          并行计算p ** r，q ** s，(p ++ q) ** (r ++ s)
          计算ps+rq=(p ++ q)(r ++ s)-pr-qs
          根据公式计AB=pr*2^n+(ps+qr)*2^(n/2)+qs算出两大数乘法*)
        val (x1,x2) = makeequal(x,y)
      in
        case length x1 of
            0 => empty()
          | 1=> (case (nth x1 0, nth x2 0) of
                    (ONE, ONE) => singleton(ONE)
                  | _ => empty())
          | _ => let 
                   val (x0,y0) = makeiteven(x1,x2)
                   val n = length x0
                   val p = drop(x0, n div 2)
                   val q = take(x0, n div 2)
                   val r = drop(y0, n div 2)
                   val s = take(y0, n div 2)

                   val (pr,qs,pqrs) = par3(fn() => p ** r, fn() => q ** s, fn() => ((p ++ q) ** (r ++ s)))
                   val psrq = pqrs -- pr --qs
                   val AB = pow2(pr, n) ++ pow2(psrq, n div 2) ++ qs
                 in
                   AB
                 end
      end
    end
  val mul = op**
end

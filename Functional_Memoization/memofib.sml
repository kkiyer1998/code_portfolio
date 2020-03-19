(* andrew Id: kkiyer *)

functor MemoFib (D: EPHDICT where K = IntInfKey): FIB =
struct
  type t = IntInf.int

  val d: IntInf.int D.dict = D.new()

  (* fib (n) ==> x
     ENSURES: x is the nth fibonacci number
   *)
  fun fib (0: IntInf.int): IntInf.int = 0
  	| fib (1) = 1
  	| fib n = (case D.lookup(d,n) of
  	SOME y => y
  | NONE => let 
  		val y = fib(n-1) + fib(n-2)
  	in 
  		D.insert(d,(n,y));y
  	end)

end (* functor MemoFib *)

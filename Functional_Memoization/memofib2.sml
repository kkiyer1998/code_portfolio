(* andrew Id: kkiyer *)

functor MemoFib2 (M: MEMOIZER where D.K = IntInfKey): FIB =
struct
  type t = IntInf.int

  (* fib' (f) (n) ==> x
  	 REQUIRES: f calls fib on its input
     ENSURES: x is the nth fibonacci number
   *)
  fun fib' f (0: t): t = 0
  	| fib' f (1) = 1
  	| fib' f (n) = f(n-1) + f(n-2)

  (* fib (n) ==> x
     ENSURES: x is the nth fibonacci number
   *)
  fun fib n = M.memo fib' n

end (* MemoFib2 *)

(* andrew Id: kkiyer *)

functor MemoLCS (M: MEMOIZER where D.K = DNA2Key): LCS =
struct
open DNA
  
  (* lcs' (f)(s,s') ==> res
  	 REQUIRES: f is a function that calls lcs on its input
     ENSURES: res is the 
     		longest common subsequence
   *)
  fun lcs' (f)([]: DNA, _: DNA): DNA = []
    | lcs' (f)(_, []) = []
    | lcs' (f)(s1 as x1::s1', s2 as x2::s2') = 
        if Base.eq (x1, x2)
          then x1 :: f (s1', s2')
          else longer (f (s1, s2'),f (s1', s2))

  (* lcs (s,s') ==> res
     ENSURES: res is the longest common 
     		subsequence between s and s'
   *)
  fun lcs (s1:DNA,s2:DNA) = M.memo lcs' (s1,s2)

end (* functor MemoLCS *)

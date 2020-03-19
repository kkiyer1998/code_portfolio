functor TestAll (P : MLISA) =
struct
  structure M = P.M
  structure S = P.S

  (* Include you tests here ... *)

  fun program (n) = let
    val l = List.tabulate(3*n,fn i=>if i<n then P.PUSHI (i+1) 
                              else if (i<2*n-1) then P.SS ("*",op* )
                              else if i=2*n then P.STOP else P.NOP)
  in
    M.fromList(l)
  end 


  (* fact: int->int
   * REQUIRES inp>=0
   * ENSURES result is factorial of inp 
   *)
  fun fact (0)=1
    | fact (n: int): int = let
    val pc = 0
    val stack = S.empty()
    val dmem = M.new(1,0)
    val iMem = program(n)
    val s = P.connect(iMem)(pc)(stack)(dmem)
  in
    (case P.simulate(s) of (stack',dmem') => (case S.pop(stack') of (x,_)=>x))
  end


  val 120 = fact(5)
  val 1 = fact(0)


  fun fib  _ = raise Fail "Unimplemented"

end (* functor TestAll *)


structure Test =
struct
  structure P = Mlisa (structure Stream = IStream
                       structure Memory = Memory
                       structure Stack = Stack (IStream))
  structure T = TestAll (P)
  open T

  (* ... or here *)

end (* structure Test *)

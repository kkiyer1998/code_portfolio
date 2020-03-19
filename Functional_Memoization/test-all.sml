structure Test =
struct
(* Use this structure for outside-the-box testing.  There is one
   section for each structure or functor you are asked to implement.
   Functors are pre-instantiated with provided parameters, but feel
   free to try more instances.  Use the nickname defined in each
   section to write your tests, but do not open any of these structures.
 *)


  structure D = TreeDict (IntInfKey)   (* any dictionary/key will do *)
  structure E = Ephemerize (D)
  val x: string E.dict = E.new()
  val _ = E.insert(x,(1,"Uno"))
  val _ = E.remove(x,1)
  val NONE = E.lookup(x,1)
  (* Insert tests for functor Ephemerize here *)


  structure H = HashDict(structure Key = IntInfKey
                         val nBuckets = 23)
  val x: string H.dict = H.new()
  val _ = H.insert(x,(1,"Uno"))
  val _ = H.insert(x,(2,"Dos"))
  val _ = H.insert(x,(3,"Tres"))
  val _ = H.insert(x,(4,"Quattro"))
  val _ = H.remove(x,1)
  val NONE = H.lookup(x,1)
  val SOME "Quattro" = H.lookup(x,4)
  (* Insert tests for functor HashDict here *)


  structure PM = PoorMemoizer(E)       (* any EPHDICT will do *)
  (* Play with PoorMemoizer here *)


  structure F1 = MemoFib (H)
  val 8 = F1.fib(6)
  val 13 = F1.fib(7)
  (* Insert tests for functor MemoFib here *)


  structure M = Memoizer (H)

  (* Insert tests for functor Memoizer here *)


  structure F2 = MemoFib2 (M)
  val 0 = F2.fib(0)
  val 21 = F2.fib(8)
  val 1 = F2.fib(2)
  (* Insert tests for functor MemoFib2 here *)


  structure DDNA = HashDict (structure Key = DNA2Key
                             val nBuckets = 37)
  structure L = MemoLCS (Memoizer(DDNA))
  val seq1 = [Base.A,Base.G,Base.T,Base.C]
  val seq2 = [Base.C,Base.T,Base.C,Base.G]
  val seq3 = [Base.G,Base.C,Base.C,Base.C]
  val [Base.T,Base.C] = L.lcs(seq1,seq2)
  val [Base.C,Base.C] = L.lcs(seq3,seq2)
  (* Insert tests for functor MemoLCS here *)


end (* structure Test *)

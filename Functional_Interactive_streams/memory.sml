structure Memory :> MEMORY =
struct
  type 'a mem = 'a Seq.seq

  exception InvalidMem

  (* new      : int * 'a -> 'a mem 
   * REQUIRES n>=0
   * ENSURES memory space filled with a is created
   *)
  fun new   (n,a) = Seq.tabulate(fn x => a)(n)

  (* read     : 'a mem -> int -> 'a
   * REQUIRES x<length(mem)
   * ENSURES value returned is at mem index x*)
  fun read  (m: 'a mem)(x) = if Seq.length(m)<=x then raise InvalidMem else Seq.nth(m)(x)

  (* write    : 'a mem -> int * 'a -> 'a mem
   * REQUIRES x<length(mem)
   * ENSURES mem returned is the same, but updated at index x
   *)
  fun write (m:'a mem)(x,a) =if Seq.length(m)<=x then raise InvalidMem else Seq.tabulate (fn n=> if n=x then a else Seq.nth(m)(n))(Seq.length(m))


  (* toString: ('a -> string) -> 'a mem -> string
     toString ts M = s
     ENSURES: s is a string representation of M
   *)
  fun toString (ts: 'a -> string) (M: 'a mem): string =
     Seq.toString (fn (i,x) => Int.toString i ^ ":" ^ ts x) (Seq.enum M)

  (* eq: ('a * 'a -> bool) -> ('a mem * 'a mem) -> bool
     eq a_eq (M1,M2) = b
     ENSURES: b == true iff M1 and M2 contain the same elements
              in the same order
   *)
  val eq = Seq.equal

  (* fromList: 'a list -> 'a mem
     fromList l = M
     ENSURES: M is a memory of the same size as l and containing
              the elements of l in the same order as l
   *)
  val fromList = Seq.fromList

  (* toList: 'a mem -> 'a list
     toList M = l
     ENSURES: l is the list of the elements of M in the same order
              as thei occur in M
   *)
  val toList = Seq.toList

end (* structure Memory *)

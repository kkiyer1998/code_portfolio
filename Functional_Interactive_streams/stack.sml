functor Stack (S: ISTREAM) :> STACK =
struct
  type 'a stack = (unit,'a) S.stream

  exception StackUnderflow


  (* empty    : unit -> 'a stack 
   * REQUIRES true
   * ENSURES stack generated is empty
   *)
  fun empty () = S.delay (fn _=> S.End)
  
  (* push     : 'a * 'a stack -> 'a stack 
   * REQUIRES true
   * ENSURES resulting stack has the input on top
   *)
  fun push  (a: 'a, s: 'a stack) = S.delay (fn () => S.Gen(a,s))
  
  (* pop      : 'a stack -> 'a * 'a stack 
   * REQUIRES stack isnt empty
   * ENSURES returns the first element and the remaining stack
   *)
  fun pop   (s: 'a stack) = let
    val S.Gen(a,s) = case (S.expose (s)(())) of S.End=>raise StackUnderflow | x=>x
  in
    (a,s)
  end 


  (* toString: ('a -> string) -> 'a stack -> string
     toString ts st = s
     REQUIRES: the stack st is finite
     ENSURES: s is a string representation of s
   *)
  fun toString (ts: 'a -> string) (st: 'a stack): string =
        toString' ts (S.expose st ())
  and toString' ts (S.Gen (x, st)) = ts x ^ "," ^ toString ts st
    | toString' _ S.End = "."

  (* eq: ('a * 'a -> bool) -> ('a stack * 'a stack) -> bool
     eq a_eq (st1,st2) = b
     REQUIRES: the stacks st1 and st2 are finite
     ENSURES: b == true iff st1 and st2 contain the same elements
   *)
  fun eq (a_eq: 'a * 'a -> bool) (st1: 'a stack, st2: 'a stack): bool = 
        eq' a_eq (S.expose st1 (), S.expose st2 ())
  and eq' a_eq (S.Gen (x1, st1), S.Gen (x2, st2)) =
        a_eq (x1,x2) andalso eq a_eq (st1, st2)
    | eq' _ (S.End, S.End) = true
    | eq' _ _ = false

  (* fromList: 'a list -> 'a stack
     fromList l = st
     ENSURES: st is the stack with the same elements as s, with the first
              element of s on top
   *)
  fun fromList ([]: 'a list): 'a stack = S.delay (fn () => S.End)
    | fromList (x::l) = S.delay (fn () => S.Gen (x, fromList l))

  (* toList: 'a stack -> 'a list
     toList st = l
     REQUIRES: the stack st is finite
     ENSURES: l is the list of the elements in st, from top to bottom
   *)
  fun toList  (st: 'a stack): 'a list = toList' (S.expose st ())
  and toList' (S.Gen (x, st)) = x :: toList st
    | toList' S.End = []

end (* functor Stack *)

structure IStream :> ISTREAM =
struct
  datatype ('a,'b) stream = Stream of 'a -> ('a,'b) front
  and      ('a,'b) front  = End
                          | Gen of 'b * ('a,'b) stream

  (* delay   : ('a -> ('a,'b) front) -> ('a,'b) stream 
   * REQUIRES true
   * ENSURES result is a stream which uses the inputted function on delay to generate
   *)
  fun delay  (f: ('a -> ('a,'b) front)): ('a,'b) stream = Stream f
  
  (* expose  : ('a,'b) stream -> 'a -> ('a,'b) front 
   * REQUIRES true
   * ENSURES result is the first member of the stream generated from a
   *)
  fun expose (Stream(f): ('a,'b) stream)(alpha: 'a): ('a,'b) front = f(alpha)


  (* map     : ('a -> 'b -> 'c) -> ('a,'b) stream -> ('a,'c) stream 
   * REQUIRES true
   * ENSURES the resulting stream gives a unique 'c for each 'a 'b input resulting from the funtion
   *)
  fun map (g: ('a->'b->'c))(s:('a,'b) stream): ('a,'c) stream = 
    delay (fn a => map' (g) (expose (s)(a))(a))
  and map' (g:('a->'b->'c))(End: ('a,'b) front)(a: 'a): ('a,'c) front = End
    | map' (g:('a->'b->'c))(Gen(b,s): ('a,'b) front)(a: 'a): ('a,'c) front = Gen (g(a)(b),map g s)


  (* filter  : ('a * 'b -> bool) -> ('a,'b) stream -> ('a,'b) stream 
   * REQUIRES true
   * ENSURES the resulting stream only contains the stuff that map to true
   *)
  fun filter (f: 'a*'b->bool)(s:('a,'b) stream): ('a,'b) stream = 
    delay ( fn a => filter' (f) (expose (s)(a))(a))
  and filter'(f: 'a*'b->bool)(End:('a,'b) front)(a:'a): ('a,'b) front = End
    | filter'(f: 'a*'b->bool)(Gen(b,s):('a,'b) front)(a:'a): ('a,'b) front = 
    if f(a,b) then Gen(b,filter f s) else filter' (f)(expose (s)(a))(a)


  (* take    : ('a,'b) stream -> 'a list -> 'b list
   * REQUIRES true
   * ENSURES the resulting list contains all stream elements that are in the input list
   *)
  fun take (s: ('a,'b) stream)(nil: 'a list) = nil
    | take (s)(x::l') = take'(expose(s)(x))(l')
  and take'(End)(_) = []
    | take'(Gen(b,s'))(l') = b::take(s')(l')

  (* drop    : ('a,'b) stream -> 'a list -> ('a,'b) stream
   * REQUIRES true
   * ENSURES the resulting stream contains all stream elements that are not in the input list
   *)
  fun drop (s: ('a,'b) stream)(nil: 'a list) = s
    | drop (s)(x::l) = drop'(expose(s)(x))(l)
  and drop'(End)(_) = delay (fn _ => End)
    | drop'(Gen(b,s'))(l') = drop (s')(l')

  (* fromFun : ('a -> 'b) -> ('a,'b) stream
   * REQUIRES true
   * ENSURES the resulting stream is one that maps 'a to 'b using the input fn
   *)
  fun fromFun (f:'a->'b): ('a,'b) stream = delay(fn a => Gen(f(a),fromFun(f)))

  (* Bonus *)

  (* compose : ('a,'b) stream -> ('b -> 'c) -> ('a,'c) stream
   * REQUIRES true
   * ENSURES the resulting stream is one that uses f to change each 'b to a 'c
   *)
  fun compose (s: ('a,'b) stream)(f: 'b->'c): ('a,'c) stream = 
    delay (fn a => compose' (expose (s)(a))(f))
  and compose' (End: ('a,'b) front)(f:'b->'c) = End
    | compose' (Gen(b,s'))(f) = Gen(f(b),compose(s')(f))

  (* iterate : ('a -> 'a) -> 'a -> ('a,'a) stream
   * REQUIRES true
   * ENSURES the resulting stream is one that uses f i times on a for its ith element
   *)
  fun iterate (f:'a->'a)(a: 'a):('a,'a) stream = delay (fn _ => Gen(a,iterate(f)(f(a))))



 (* toString (a_ts,b_ts) S l ==> s
    ENSURES: s is a string representation of exposing stream S with
             list of values l
  *)
 fun toString (_: 'a -> string, _: 'b -> string) ([]: 'a list)
	      (_: ('a,'b) stream): string = "."
   | toString (a_ts, b_ts) (a::l) S =
       a_ts a ^ " > " ^ toString' (a_ts, b_ts) l (expose S a)
 and toString'(a_ts, b_ts) l (Gen (b,S)) =
       b_ts b ^ "; " ^ toString (a_ts,b_ts) l S
   | toString' _ _ End = "!"

 (* eq b_ts (S1,S2) l ==> b
    ENSURES: b == true iff exposing both streams S1 and S2 with the
	     same list of values l yields the same values in the same
             order.
  *)
 fun eq (_: 'b * 'b -> bool) ([]: 'a list)
	(_: ('a,'b) stream, _: ('a,'b) stream): bool = true
   | eq b_eq (a::l) (S1,S2) = eq' b_eq l (expose S1 a, expose S2 a)
 and eq' b_eq l (Gen (b1,S1), Gen (b2,S2)) =
          b_eq (b1,b2) andalso eq b_eq l (S1,S2)
   | eq' _ _ (End, End) = true
   | eq' _ _ _ = false
end (* structure IStream *)

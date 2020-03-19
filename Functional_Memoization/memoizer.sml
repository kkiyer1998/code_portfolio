(* andrew Id: kkiyer *)

functor Memoizer (D: EPHDICT): MEMOIZER =
struct
  structure D = D

  (* memo (f) ==> f'
     ENSURES: f is a function that memorizes the 
     			results and uses them when needed.
   *)
  fun memo (f: (D.K.t -> 'a) -> D.K.t -> 'a): D.K.t -> 'a =
    let
      val memoTable: 'a D.dict = D.new ()

      fun f_memoed (x: D.K.t): 'a =
            case D.lookup (memoTable, x)
              of SOME y => y
               | NONE   => let
                             val y = f f_memoed x
                             val _ = D.insert (memoTable, (x,y))
                           in y end
    in
      f f_memoed
    end

end (* functor Memoizer *)

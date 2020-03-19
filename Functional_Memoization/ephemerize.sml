functor Ephemerize (D: DICT) :> EPHDICT where K = D.K =
struct
  structure K = D.K
  type key = K.t
  type 'a entry = key * 'a
  type 'a dict = 'a D.dict ref

  val entry_toString = D.entry_toString
  val entry_eq       = D.entry_eq

  fun toString (f)(dictref: 'a dict): string = D.toString (f)(!dictref)
  fun eq (f)(d1: 'a dict,d2: 'a dict): bool = D.eq(f)(!d1,!d2)

  (* new () ==> d
     ENSURES: d is a new empty dictionary
   *)
  fun new () = ref (D.empty)

  (* insert (d, e) ==> ()
     ENSURES: d is updated to contain entry e
   *)
  fun insert (d:'a dict,x: 'a entry) = (d := D.insert(!d,x))

  (* remove (d, k) ==> ()
     ENSURES: d is updated with any entry with key k removed
   *)
  fun remove (d:'a dict,k: key) = (d := D.remove(!d,k))

  (* lookup (d, k) ==> opt
     ENSURES:
      - opt = SOME x if entry (k,x) is in d
      - opt = NONE otherwise
   *)
  fun lookup (d: 'a dict,k: key) = D.lookup (!d,k)

end (* functor Ephemerize *)

functor HashDict (structure Key: KEY
                        val nBuckets: int) :> EPHDICT where K = Key =
struct
  structure K = Key
  type key = K.t
  type 'a entry = key * 'a

  (* Representation invariant: duplicate keys are allowed in each
     bucket, but only the first is returned on lookup; remove scrubs
     the bucket *)
  type 'a bucket = 'a entry list
  type 'a dict = 'a bucket ref Seq.seq ref

  fun entry_toString (ts: 'a -> string) (k: key, x: 'a): string =
    K.toString k ^ "->" ^ ts x

  fun entry_eq (a_eq: 'a * 'a -> bool) ((k1,x1): 'a entry, (k2,x2): 'a entry): bool =
      K.eq (k1,k2) andalso a_eq (x1,x2)


  fun toString _ = raise Fail "Unimplemented" (* use for testing *)
  fun eq       _ = raise Fail "Unimplemented" (* use for testing *)

  (* new () ==> d
     ENSURES: d is a new empty dictionary
   *)
  fun new () = (ref (Seq.tabulate (fn i => ref [])(nBuckets)))

  fun hashit (k: key) = (K.toInt(k) mod nBuckets)

  (* insert (d, e) ==> ()
     ENSURES: d is updated to contain entry e
   *)
  fun insert (d:'a dict,(k,v): 'a entry) = let
    val hashval = hashit (k)
    val listref = Seq.nth(!d)(hashval)
  in
    listref := ((k,v)::(!listref))
  end

  (* removefroml (l,k) = l'
     ENSURES: l' does not contain k
   *)
  fun removefroml (nil: 'a bucket,k: key) = nil
    | removefroml ((k',v)::l,k) = if K.eq(k,k') then removefroml(l,k) else (k',v)::removefroml(l,k)

  (* remove (d, k) ==> ()
     ENSURES: d is updated with any entry with key k removed
   *)
  fun remove (d: 'a dict,k: key) = let
    val hashval = hashit(k)
    val listref = Seq.nth(!d)(hashval)
  in
    listref := removefroml (!listref,k)
  end

  (* getval (l, k) ==> v
     ENSURES: v is the value corresponding to k in l
   *)
  fun getval (nil,k) = NONE
    | getval ((k',v)::l,k) = if K.eq(k,k') then SOME v else getval(l,k)

  (* lookup (d, k) ==> opt
     ENSURES:
      - opt = SOME x if entry (k,x) is in d
      - opt = NONE otherwise
   *)
  fun lookup (d: 'a dict,k: key) = let
    val hashval = hashit(k)
    val listref = Seq.nth(!d)(hashval)
  in
    getval(!listref,k)
  end

end (* structure HashDict *)

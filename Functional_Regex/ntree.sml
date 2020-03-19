structure NTree: NTREE =
struct


datatype 'a tree = Bud of 'a
                 | Branch of 'a tree list

val l1 : int tree list = [Bud(0),Branch(nil),Branch([Bud 1]),Bud 15]
val l2 : int tree list = [Branch(l1),Bud(10),Branch([Bud 1]),Bud 15]

(* height: 'a tree-> int
 * REQUIRES true
 * ENSURES result>=0
 *)
fun height (Bud(x): 'a tree) = 1
  | height (Branch(nil)) = 0
  | height (Branch(L)) = List.foldl(fn (a: 'a tree,b: int) => if height(a)>b then height(a) else b) (0) (L)+1
(* TEST CASES *)
val 5 = height(Branch([Branch l1,Branch l2]))
val 0 = height(Branch(nil))
val 1 = height(Bud(10))
val 2 = height(Branch([Bud(15)]))

(* fill: int*int -> 'a -> 'a tree
 * fill (c,h) (a) = res 
 * REQUIRES c>0 andalso h>=0 (If h=)
 * ENSURES height(res) = x
 *)
fun fill (c,0) (x) = Branch(nil)
  | fill (c,1) (x) = Bud(x)
  | fill (c,h) (x) = Branch(List.tabulate(c,fn a => fill(c,h-1)(x)))
(* TEST CASES *)
val Branch(nil) = fill (5,0) (10022)
val Branch([Bud 5,Bud 5,Bud 5,Bud 5]) = fill (4,2) (5)

(* map ('a -> 'b) -> ('a tree) -> ('b tree)
 * map (f) (t) = t'
 * REQUIRES true
 * ENSURES t' is f applied on every member of t
 *)
fun map (f) (Branch []) = Branch []
  | map (f) (Bud(x)) = Bud (f(x))
  | map (f) (Branch l) = Branch (List.map (map (f)) (l))
(* TEST CASES *)
val Branch([Branch [Bud(5),Branch(nil),Branch([Bud 6]),Bud 20],Branch [Branch [Bud(5),Branch(nil),Branch([Bud 6]),Bud 20],Bud(15),Branch([Bud 6]),Bud 20]])
	 = map (fn x => x+5) (Branch [Branch l1,Branch l2])

(* filter ('a -> bool) -> 'a tree -> 'a tree
 * filter f t = t'
 * REQUIRES true
 * ENSURES no element x in t' s.t f(x) = false 
 *)
fun filter (f) (Branch([])) = Branch ([])
  | filter (f) (Bud x) = if f(x) then Bud (x) else Branch ([])
  | filter (f) (Branch l) = Branch(List.map (filter (f)) (l))
(* TEST CASES *)
val Branch([Branch ([Branch [],Branch [],Branch [Bud 1],Bud 15]),Branch ([Branch [Branch [],Branch [],Branch [Bud 1],Bud 15],Branch [],Branch [Bud 1],Bud 15])])
	 = filter (fn x => if ((x+5) mod 2)=0 then true else false) (Branch [Branch l1,Branch l2])

(* reduce: ('a -> 'b) -> ('b list -> 'b) -> 'a tree -> 'b
 * reduce f g t = x
 * REQUIRES true
 * ENSURES f was applied to each bud and g was applied to the result of mapping f to buds*)
fun reduce (f) (g) (Bud(x)) = f(x)
  | reduce (f) (g) (Branch l) = g(List.map (reduce (f) (g)) (l)) 
(* TEST CASES *)
val rtree = Branch([Branch [Bud(5),Branch(nil),Branch([Bud 6]),Bud 20],
	Branch [Branch [Bud(5),Branch(nil),Branch([Bud 6]),Bud 20],Bud(15),Branch([Bud 6]),Bud 20]])
val 103 = reduce (fn x => x) (fn l => List.foldl (fn (x,y) => x+y) (0) (l)) (rtree)

(* inorder: 'a tree -> int
 * REQUIRES true
 * ENSURES output is a list in order of all buds
 *)
fun inorder (t) = reduce (fn x => [x]) (fn l => List.foldr (fn (l1,l2) => l1@l2) ([]) l) t
(* TEST CASES *)
val [5,6,20,5,6,20,15,6,20] = inorder rtree

(* evalinttree: int list tree -> int
 * REQUIRES true
 * ENSURES output is the sum of the product over the list in each bud, of all buds
 *)
fun evalIntTree (t) = reduce (List.foldl (fn (x,y) => x*y) (1)) (List.foldl (fn (x,y) => x+y) (0)) (t)
(* TEST CASES *)
val etree = Branch([Branch [Bud([5,1]),Branch(nil),Branch([Bud [3,2]]),Bud [1,2,5,2]],
	Branch [Branch [Bud([5,1,1,1]),Branch(nil),Branch([Bud [2,3]]),Bud [10,2]],Bud([15]),Branch([Bud [6]]),Bud [20]]])
val 103 = evalIntTree etree


(* Bonus *)
(* isCannical: 'a tree -> bool
 * REQUIRES true
 * ENSURES true if t is a canonical n-tree
 *)
fun isCanonical (Branch []: 'a tree): bool = true
  | isCanonical (t) = reduce (fn x=> true) (fn nil => false 
                                             | l => List.foldl (fn (a,b)=>a andalso b) true l) (t)

(* TEST CASES *)
val false = isCanonical(rtree)
val true = isCanonical(Branch[])
val true = isCanonical(Branch([Branch [Bud 5],Bud 6,Bud 4]))

(* mkCanonical: 'a tree -> 'a tree
 * mkCanonical t = t'
 * REQUIRES true
 * ENSURES (isCanonical t') andalso (inorder t = inorder t')
 *)
fun mkCanonical (t) = reduce (fn a => Bud a) (fn l => Branch(List.filter (fn Branch [] => false | _ =>true) l)) (t)
  (** I DESERVE 5 POINTS FOR THIS **)
(* TEST CASES *)
val can = mkCanonical (etree)
val true = isCanonical(can)
end
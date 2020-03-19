(* A simple tree containing only characters at the leaves *)
(* THAT IS NOT A HUFFMAN TREE *)
datatype tree = Node of tree * tree
              | Leaf of char

datatype htree = 
	Hnode of htree*(string*string*int)*htree 
	| Hleaf of (string*int)

(* Encoding is done with the following steps:
 * 1. Generating a list of frequencies.
 * 2. Sorting this list of frequencies
 * 3. Making a huffman tree
 * 4. Creating an encoding table from the huffman tree
 * 5. Converting huffman tree to the given tree format
 * 6. Iterating through the string and replacing chars with their respective encoding*)


(* Function List:
 * addin
 * isin
 * getFreq
 * merge
 * split
 * msort
 * makeHuff
 * member
 * findit
 * make_table
 * turntree
 * findCode
 * encode'
 * encode
 * decodeHelper'
 * decodeHelper
 * decode
 *)

(* This part generates a list of tuples containing each char and their frequency.
 * The characters are stored as strings in hleaves for future ease *)
(* addin: string * htree list -> htree list
 * REQUIRES isin (s,l)
 * ENSURES frequency of s is increased by 1
 *)
fun addin(s:string,nil: htree list): htree list = raise Fail "REQUIRES spec not met"
	|addin(s,Hleaf(a,b)::l) = if a = s then [Hleaf(a,b+1)]@l else addin(s,l@[Hleaf(a,b)])

(* isin: string * htree list -> bool
 * REQUIRES true
 * ENSURES true if s is in the list of frequencies
 *)
fun isin(s:string,[]:(htree)list):bool = false
	|isin(s,Hleaf(a,b)::l) = if s = a then true else isin(s,l)

(* getFreq: string * htree list -> htree list
 * REQUIRES true
 * ENSURES the returned list has all the characters in the 
 * 		string uniquely stored with their frequencies
 *)
fun getFreq("":string,l:(htree) list): (htree) list = l
	|getFreq(s,l)= if isin(String.str(String.sub(s,0)),l) then
					getFreq(String.extract(s,1,NONE),addin(String.str(String.sub(s,0)),l))
				else
					getFreq(String.extract(s,1,NONE),l@[Hleaf(String.str(String.sub(s,0)),1)])

(* Test cases for these frequency list generation *)
val [Hleaf ("s",6),Hleaf ("h",2),Hleaf ("l",4),Hleaf ("a",1),Hleaf ("-",1),
   Hleaf ("e",4),Hleaf (" ",2)] = getFreq("she sells sea-shells",[])
val [] = getFreq("",[])
val [Hleaf ("e",2),Hleaf ("b",1),Hleaf ("r",1),Hleaf ("x",1),Hleaf ("o",2),
   Hleaf ("w",1),Hleaf ("n",1),Hleaf ("f",1),Hleaf ("j",1),Hleaf (" ",4),
   Hleaf ("q",1),Hleaf ("m",1),Hleaf ("p",1),Hleaf ("u",2),Hleaf ("i",1),
   Hleaf ("c",1),Hleaf ("k",1),Hleaf ("T",1),Hleaf ("h",1),Hleaf ("d",1),
   Hleaf (".",1)] = getFreq("The quick brown fox jumped.",[])
(* Freq list generated *)



(* This part is for sorting my freq list *)(* Source(ish) => www.gisellereis.com (Check her out she's awesome)*)
(* merge: htree list * htree list -> htree list
   merge (l1, l2) = l where
   REQUIRES 
   - l1 is sorted
   - l2 is sorted
   ENSURES 
   - l is sorted
   - l contains every member of l1 and l2
*)
fun merge ([]: (htree) list, []: (htree) list): (htree) list = []
  | merge (l1, []) = l1
  | merge ([], l2) = l2
  | merge (Hleaf(a,b)::l1, Hleaf(c,d)::l2) = if b < d then Hleaf(a,b) :: merge (l1, Hleaf(c,d)::l2)
			     else Hleaf(c,d) :: merge (Hleaf(a,b)::l1, l2)
  | merge (Hnode(tl1,(s1,p1,x1),tr1)::l1, Hnode(tl2,(s2,p2,x2),tr2)::l2) = if x1 < x2 then Hnode(tl1,(s1,p1,x1),tr1) :: merge (l1, Hnode(tl2,(s2,p2,x2),tr2)::l2)
			     else Hnode(tl2,(s2,p2,x2),tr2) :: merge (Hnode(tl1,(s1,p1,x1),tr1)::l1, l2)	     
  | merge (Hnode(tl1,(s1,p1,x1),tr1)::l1, Hleaf(c,d)::l2) = if x1 < d then Hnode(tl1,(s1,p1,x1),tr1) :: merge (l1, Hleaf(c,d)::l2)
			     else Hleaf(c,d) :: merge (Hnode(tl1,(s1,p1,x1),tr1)::l1, l2)
  | merge (Hleaf(c,d)::l1, Hnode(tl1,(s1,p1,x1),tr1)::l2) = if d < x1 then Hleaf(c,d) :: merge (l1, Hnode(tl1,(s1,p1,x1),tr1)::l2)
			     else Hnode(tl1,(s1,p1,x1),tr1) :: merge (Hleaf(c,d)::l1, l2)
  
(* split: htree list -> htree list * htree list
   REQUIRES true
   ENSURES split l = (l1, l2) where
   - l contains every member of l1 and l2
   - length l1 = (length l) div 2 + (length l) mod 2
     (if l has odd length, the first list is one element longer)
   - length l2 = (length l) div 2
*)
fun split ([]: (htree) list): ((htree) list * (htree) list) = ([], [])
  | split [x] = ([x], [])
  | split (x1::x2::l) = let
      val (l1, l2) = split l
  in
      (x1::l1, x2::l2)
  end

(* msort: htree list -> htree list
   REQUIRES true
   ENSURES msort l = l' where
   - l' contains all elements from l
   - l' is sorted by frquency.
*)
fun msort ([]: (htree) list): (htree) list = []
  | msort [x] = [x]
  | msort l = let
      val (l1, l2) = split l
  in
      merge (msort l1, msort l2)
  end

(* Test cases for sort functions: *)
val [Hleaf ("a",1),Hleaf ("-",1),Hleaf ("h",2),Hleaf (" ",2),Hleaf ("e",4),
   Hleaf ("l",4),Hleaf ("s",6)] = msort(getFreq("she sells sea-shells",[]))
val [Hleaf ("x",1),Hleaf ("F",1),Hleaf ("b",1),Hleaf ("n",1),Hleaf ("h",1),
   Hleaf ("d",1),Hleaf ("w",1),Hleaf ("t",1),Hleaf ("o",2),Hleaf ("v",2),
   Hleaf ("a",2),Hleaf ("y",3),Hleaf ("s",3),Hleaf (".",3),Hleaf ("r",4),
   Hleaf (" ",6),Hleaf ("e",6)] = msort(getFreq("Foxes are very very brown these days...",[]))
(* Freq list sorted *)


(* Create a huffman tree- This part works by sorting the list and making
 * a node of the smallest two trees in the list. We then add this tree 
 * back into the list and repeat *)

(* makeHuff: htree list -> htree
 * REQUIRES l <> nil
 * ENSURES t contains all the htrees in l*)
fun makeHuff (x::nil:(htree) list): htree = x
	|makeHuff (Hleaf(s,x)::Hleaf(p,y)::l) = makeHuff(msort(Hnode
												(Hleaf(s,x),(s,p,x+y),Hleaf(p,y))::l))
	|makeHuff (Hnode(t1,(s,s1,x),t2)::Hleaf(p,y)::l) = makeHuff(msort(Hnode
												(Hnode(t1,(s,s1,x),t2),(s^s1,p,x+y),Hleaf(p,y))::l))
	|makeHuff (Hleaf(p,y)::Hnode(t1,(s,s1,x),t2)::l) = makeHuff(msort(Hnode
												(Hleaf(p,y),(p,s^s1,x+y),Hnode(t1,(s,s1,x),t2))::l))
	|makeHuff (Hnode(t1,(s,s1,x),t2)::Hnode(t3,(p,p1,y),t4)::l) = makeHuff(msort(Hnode
									(Hnode(t1,(s,s1,x),t2),(s^s1,p^p1,x+y),Hnode(t3,(p,p1,y),t4))::l))
(* Test cases for makeHuff*)
val Hnode
    (Hnode
       (Hnode
          (Hnode
             (Hnode (Hleaf ("x",1),("x","F",2),Hleaf ("F",1)),("xF","hw",4),
              Hnode (Hleaf ("h",1),("h","w",2),Hleaf ("w",1))),
           ("xFhw","vtd",8),
           Hnode
             (Hleaf ("v",2),("v","td",4),
              Hnode (Hleaf ("t",1),("t","d",2),Hleaf ("d",1)))),
        ("xFhwvtd","aor",16),
        Hnode
          (Hnode (Hleaf ("a",2),("a","o",4),Hleaf ("o",2)),("ao","r",8),
           Hleaf ("r",4))),("xFhwvtdaor","nbys. e",39),
     Hnode
       (Hnode
          (Hnode
             (Hnode (Hleaf ("n",1),("n","b",2),Hleaf ("b",1)),("nb","y",5),
              Hleaf ("y",3)),("nby","s.",11),
           Hnode (Hleaf ("s",3),("s",".",6),Hleaf (".",3))),("nbys."," e",23),
        Hnode (Hleaf (" ",6),(" ","e",12),Hleaf ("e",6)))) = makeHuff(msort(getFreq("Foxes are very very brown these days...",[])))
val Hnode
    (Hnode
       (Hleaf ("e",4),("e","h ",8),
        Hnode (Hleaf ("h",2),("h"," ",4),Hleaf (" ",2))),("eh ","sa-l",20),
     Hnode
       (Hleaf ("s",6),("s","a-l",12),
        Hnode
          (Hnode (Hleaf ("a",1),("a","-",2),Hleaf ("-",1)),("a-","l",6),
           Hleaf ("l",4)))) = makeHuff(msort(getFreq("she sells sea-shells",[])))
(* Htree created *)

(* Now I'm gonna make a table (list) of the encodings vs the characters *)
(* member: string*string -> bool
 * REQUIRES true
 * ENSURES true if s is in the string a
 	*)
fun member (s:string,"":string): bool = false
  |member(s,a) = if String.sub(s,0) = String.sub(a,0) then true else (member(s,String.extract(a,1,NONE))) 

(* findit: string*htree -> string
 * REQUIRES s is present in a leaf in t
 * ENSURES res is the encoding of the character stored in s*)
fun findit (s:string,Hleaf(a,x):htree):string = if s = a then "" else raise Fail "Messed up..."
  |findit(s,Hnode(t1,(a,b,x),t2)) = if member(s,a) then "0"^findit(s,t1) else "1"^findit(s,t2) 

(* make_table htree * htree list -> (string*string) list
 * REQUIRES all members of l are in t
 * ENSURES all members of res are characters matched with their encodings
 *)
fun make_table (t: htree,[]: htree list):(string*string) list = []
  |make_table(t,Hleaf (s,x)::l) = (s,findit(s,t))::make_table(t,l)
(* Test cases for make_table *)
val [("a","1100"),("-","1101"),("h","010"),(" ","011"),("e","00"),("l","111"),
   ("s","10")] = make_table(makeHuff(msort(getFreq("she sells sea-shells",[]))),
   								msort(getFreq("she sells sea-shells",[])))


(*Final set: main encode helpers*)
(* turntree htree -> tree
 * REQUIRES true
 * ENSURES htraverseleaves ht = traverse t *)
fun turntree(Hleaf(s,f):htree): tree = Leaf(String.sub(s,0))
	|turntree(Hnode(t1,(s,s1,f),t2)) = Node(turntree(t1),turntree(t2))
(* Test cases for turntree*)
val Node
    (Node (Leaf #"e",Node (Leaf #"h",Leaf #" ")),
     Node (Leaf #"s",Node (Node (Leaf #"a",Leaf #"-"),Leaf #"l")))
      = turntree(makeHuff(msort(getFreq("she sells sea-shells",[]))))

(* findcode string*(string*string)list -> string
 * REQUIRES s is a single character in l
 * ENSURES y is the encoding for character s*)
fun findcode(s,nil) = raise Fail "Not Happening..."
	|findcode(s,(x,y)::tab) = if String.sub(s,0) = String.sub(x,0) then y else findcode(s,tab)
(* Test cases for findcode *)
val "111" = findcode("l",[("a","1100"),("-","1101"),("h","010"),(" ","011"),("e","00"),("l","111"),
	("s","10")])

(* encode' string*(string*string)list*string -> string
 * REQUIRES every character in s has an encoding, ie. is on the table
 * ENSURES res is the encoded version of s, the input string *)
fun encode'("":string,tab:(string*string) list,acc:string):string = acc
	|encode'(s,tab,acc) = let
		val b = findcode(s,tab)
	in
		encode'(String.extract(s,1,NONE),tab,acc^b)
	end
(* Test cases for encode' *)
val "1001000011100011111110011100011001101100100011111110" = 
			encode'("she sells sea-shells",[("a","1100"),("-","1101"),("h","010"),(" ","011")
				,("e","00"),("l","111"),("s","10")],"")


(* encode string -> tree * string
 * REQUIRES true
 * ENSURES decode(t,code) = s*)
fun encode (s: string): tree * string = let
	val freqlist = getFreq(s,[])
	val sorted = msort(freqlist)
	val hufftree = makeHuff(sorted)
	val table = make_table(hufftree,sorted)
in
	(turntree(hufftree),encode'(s,table,""))
end


(* decode_help' tree*string*string -> (string*string)
 * REQUIRES true
 * ENSURES There is one character more in the output string  *)
fun decode_help' (Leaf x,s,res) = (s,res^String.str(x))
	|decode_help' (Node(t1,t2),s,res) = if String.sub(s,0) = #"0" 
										then decode_help'(t1,String.extract(s,1,NONE),res) 
										else decode_help'(t2,String.extract(s,1,NONE),res)

(* decode_help tree*string*string -> string
 * REQUIRES t is the tree corresponding to s
 * ENSURES the result is the decoded string *)
fun decode_help (t,s,res) = let
	val (x,y) = decode_help'(t,s,res)
in
	if size(x) = 0 then y else decode_help(t,x,y)
end


(* decode tree*string -> string
 * REQUIRES t is the tree corresponding to s
 * ENSURES the result is the decoded string *)
fun decode (t: tree, s: string): string = decode_help(t,s,"")


(* ENCODE AND DECODE TEST CASES*)
val (inp1,inp2) = encode("zHPICChrYUdBPuOWWWmmjqGBsx ")
val "zHPICChrYUdBPuOWWWmmjqGBsx " = decode(inp1,inp2)
(* andrew Id: kkiyer *)

functor MakeTicTacToe (val size: int): TICTACTOE =
struct
  datatype player  = Minnie
		   | Maxie

  datatype outcome = Winner of player
		   | Draw

  datatype status  = Over of outcome
		   | In_play

  datatype est     = Definitely of outcome
	           | Guess of int

  datatype position = Filled of player
		    | Empty

  (* (0,0) is bottom-left corner *)
  datatype c4state = Unimplemented (* use in starter code only *)
		   | S of (position Matrix.matrix) * player
  type state = c4state

  type move = int * int (* cols and rows are numbered 0 ... num_cols-1 *)

  val num_cols = size
  val num_rows = size

  (* printing functions *)
  fun player_toString Maxie  = "Maxie"
    | player_toString Minnie = "Minnie"

  fun outcome_toString (Winner P) = player_toString P ^ " wins!"
    | outcome_toString Draw       = "It's a draw!"

  fun status_toString (Over O) = "Over ("^ outcome_toString O ^")"
    | status_toString In_play  = "Still playing"

  fun est_toString (Definitely O) = outcome_toString O
    | est_toString (Guess i)      = "Guess: " ^ Int.toString i

  fun move_toString (x,y) = "("^(Int.toString x)^","^(Int.toString y)^")"

  fun pos_toString (Filled Minnie) = "O"
    | pos_toString (Filled Maxie)  = "X"
    | pos_toString Empty           = " "

  fun state_toString (board as (S (m, _))) =
      let
        val rows = Matrix.rows m
        val ts : string Seq.seq -> string = Seq.reduce op^ ""
        fun print_row s =
            "|" ^ ts (Seq.map (fn x => pos_toString x ^ "|") s) ^ "\n"
      in
          "-" ^ ts (Seq.tabulate (fn _ => "--") num_cols) ^ "\n" ^
          Seq.mapreduce print_row "" (fn (x,y) => y^x) rows ^ "\n"
      end
    | state_toString Unimplemented = raise Fail "Incomplete implementation"


  (* equality functions *)
  val player_eq  = (op=)
  val outcome_eq = (op=)
  val status_eq  = (op=)
  val est_eq     = (op=)
  val move_eq    = (op=)

  fun pos_eq (Filled p1, Filled p2) = p1=p2
    | pos_eq (Empty, Empty) = true
    | pos_eq _ = false

  fun state_eq (S (m1,p1), S (m2,p2)) =
    Matrix.eq pos_eq (m1,m2) andalso p1=p2


  (* parsing functions *)
  fun parse_move (S(st,p)) input =
    let
        val [x,y] = String.tokens (fn #"," => true | _ => false) (input)
        val SOME(x) = Int.fromString(String.extract (x,1,NONE))
        val SOME(y) = Int.fromString(y)
    in
      if x >= 0 andalso x < num_cols andalso y >= 0 andalso y < num_rows
      then (case (Matrix.sub (st) (x,y))
              of Filled(_) => NONE
               | _ => SOME(x,y))
      else NONE
    end

  fun flip Maxie  = Minnie
    | flip Minnie = Maxie

  (* initial state - A completely empty matrix *)
  val start = S(Matrix.repeat (Empty) (num_rows,num_cols),Maxie)

  (* make_move (s,m) ==> s'
     REQUIRES: m is a valid move in s
     ENSURES: s' is the state resulting from making move m in s
   *)
  fun make_move (S(mat,p): state,(i,j):move) = S (Matrix.update(mat)((i,j),Filled(p)),flip p)

  (* moves s ==> M
     REQUIRES: status s <> Over _
     ENSURES: - M is the collection of valid moves from state s
              - Always generates at least one move
   *)
  fun moves (S(mat,p):state) = Matrix.matching_subs (fn Empty=> true | _ => false) (mat)

  (* status: state->status
       status s ==> u
       REQUIRES: 
       ENSURES: u is the status of the game in state s
  *)
  fun status (S(mat,p):state) = let
    val rows = Matrix.rows(mat)
    val cols = Matrix.cols(mat)
    val diag1 = Seq.nth(num_rows-1)(Matrix.diags1(mat))
    val diag2 = Seq.nth(num_rows-1)(Matrix.diags2(mat))
    val maxiewinsr = Seq.exists (fn a=>a) 
                                (Seq.map (fn rowi => ((Seq.all (fn Filled(Maxie) => true 
                                                                       | _ => false) 
                                                                     (rowi)))) 
                                          (rows))
    val minniewinsr = Seq.exists (fn a=>a) 
                                (Seq.map (fn rowi => ((Seq.all (fn Filled(Minnie) => true 
                                                                       | _ => false) 
                                                                     (rowi)))) 
                                          (rows))
    val maxiewinsc = Seq.exists (fn a=>a) 
                                (Seq.map (fn coli => ((Seq.all (fn Filled(Maxie) => true 
                                                                       | _ => false) 
                                                                     (coli)))) 
                                          (cols))
    val minniewinsc = Seq.exists (fn a=>a) 
                                (Seq.map (fn coli => ((Seq.all (fn Filled(Minnie) => true 
                                                                       | _ => false) 
                                                                     (coli)))) 
                                          (cols))
    val maxiewinsd = (Seq.all (fn Filled(Maxie) => true | _ => false) (diag1)) orelse
                     (Seq.all (fn Filled(Maxie) => true | _ => false) (diag2))
    val minniewinsd = (Seq.all (fn Filled(Minnie) => true | _ => false) (diag1)) orelse
                     (Seq.all (fn Filled(Minnie) => true | _ => false) (diag2))
    val filled = not (Seq.exists (fn a=>a)(Seq.map (fn rowi => (Seq.exists(fn Empty =>true | _ => false)(rowi)))(rows)))
  in
    if maxiewinsc orelse maxiewinsd orelse maxiewinsr then Over(Winner(Maxie))
         else if minniewinsd orelse minniewinsc orelse minniewinsr then Over(Winner(Minnie))  
         else if filled then Over(Draw)
    else In_play
  end


  (* player: state->player
     player s ==> P
     REQUIRES: true
     ENSURES: P is who's turn it is to play in state s
   *)
  fun player (S(mat,p)) = (p)


  (* estimate the value of the state, which is assumed to be In_play *)
  fun estimate (S(mat,p)) = let
    val rows = Matrix.rows(mat)
    val cols = Matrix.cols(mat)
    val diag1 = Seq.nth(num_rows-1)(Matrix.diags1(mat))
    val diag2 = Seq.nth(num_rows-1)(Matrix.diags2(mat))

    val rowcount = Seq.map (fn rowi => LookAndSay.lookAndSay(op=)(rowi)) (rows)
    val colcount = Seq.map (fn coli => LookAndSay.lookAndSay(op=)(coli)) (cols)
    val diag1c = LookAndSay.lookAndSay(op=)(diag1)
    val diag2c = LookAndSay.lookAndSay(op=)(diag2)

    val all = Seq.append(Seq.append(Seq.append(rowcount)(colcount))(Seq.singleton(diag1c)))(Seq.singleton(diag2c))

    val maxiecounts = Seq.map (fn i => Seq.reduce(fn ((n,Filled Maxie),(m,Filled Maxie)) => (m+n,Filled Maxie) | ((n,Filled Maxie),(_,Empty))=>(n,Filled Maxie) | ((_,Empty),(n,Filled Maxie))=>(n,Filled Maxie) | _ => (0,Filled Minnie)) 
                                                      (0,Filled Maxie) (i))(all)
    val minniecounts = Seq.map (fn i => Seq.reduce(fn ((n,Filled Minnie),(m,Filled Minnie)) => (m+n,Filled Minnie) | ((n,Filled Minnie),(_,Empty))=>(n,Filled Minnie)  | ((_,Empty),(n,Filled Minnie))=>(n,Filled Minnie) | _ => (0,Filled Maxie)) 
                                                      (0,Filled Minnie) (i))(all)

    val maxmaxie = Seq.reduce (fn ((i,Filled Maxie),(j,Filled Maxie)) => if i>j then (i,Filled Maxie) else (j,Filled Maxie) | (_,(j,Filled Maxie))=>(j,Filled Maxie) | ((j,Filled Maxie),_)=>(j,Filled Maxie)) (0,Filled Maxie) (maxiecounts)
    val maxminnie = Seq.reduce (fn ((i,Filled Minnie),(j,Filled Minnie)) => if i>j then (i,Filled Minnie) else (j,Filled Minnie)| (_,(j,Filled Minnie))=>(j,Filled Minnie) | ((j,Filled Minnie),_)=>(j,Filled Minnie)) (0,Filled Minnie) (minniecounts)

    val capmaxie = Seq.reduce (fn ((i,Filled Maxie),(j,Filled Maxie)) => (i+j,Filled Maxie) | (_,(j,Filled Maxie))=>(j,Filled Maxie) | ((j,Filled Maxie),_)=>(j,Filled Maxie)) (0,Filled Maxie) (maxiecounts)
    val capminnie = Seq.reduce (fn ((i,Filled Minnie),(j,Filled Minnie)) => (i+j,Filled Minnie) | (_,(j,Filled Minnie))=>(j,Filled Minnie) | ((j,Filled Minnie),_)=>(j,Filled Minnie)) (0,Filled Minnie) (minniecounts)

    val difference = case (capmaxie,capminnie) of
      ((i,Filled Maxie),(j,Filled Minnie)) => (i-j)
    val maxiee = case maxmaxie of (i,Filled Maxie)=>i
    val minniee = case maxminnie of (i,Filled Minnie)=>i

    val winningno = num_rows-1

    val maxiecountsr = Seq.map (fn i => Seq.reduce(fn ((n,Filled Maxie),(m,Filled Maxie)) => (m+n,Filled Maxie) | ((n,Filled Maxie),(_,Empty))=>(n,Filled Maxie) | ((_,Empty),(n,Filled Maxie))=>(n,Filled Maxie) | _ => (0,Filled Minnie)) 
                                                      (0,Filled Maxie) (i))(rowcount)
    val maxiecountsc = Seq.map (fn i => Seq.reduce(fn ((n,Filled Maxie),(m,Filled Maxie)) => (m+n,Filled Maxie) | ((n,Filled Maxie),(_,Empty))=>(n,Filled Maxie) | ((_,Empty),(n,Filled Maxie))=>(n,Filled Maxie) | _ => (0,Filled Minnie)) 
                                                      (0,Filled Maxie) (i))(colcount)
    val minniecountsr = Seq.map (fn i => Seq.reduce(fn ((n,Filled Minnie),(m,Filled Minnie)) => (m+n,Filled Minnie) | ((n,Filled Minnie),(_,Empty))=>(n,Filled Minnie)  | ((_,Empty),(n,Filled Minnie))=>(n,Filled Minnie) | _ => (0,Filled Maxie)) 
                                                      (0,Filled Minnie) (i))(rowcount)
    val minniecountsc = Seq.map (fn i => Seq.reduce(fn ((n,Filled Minnie),(m,Filled Minnie)) => (m+n,Filled Minnie) | ((n,Filled Minnie),(_,Empty))=>(n,Filled Minnie)  | ((_,Empty),(n,Filled Minnie))=>(n,Filled Minnie) | _ => (0,Filled Maxie)) 
                                                      (0,Filled Minnie) (i))(colcount)
    val maxmaxier = Seq.reduce (fn ((i,Filled Maxie),(j,Filled Maxie)) => if i>j then (i,Filled Maxie) else (j,Filled Maxie) | (_,(j,Filled Maxie))=>(j,Filled Maxie) | ((j,Filled Maxie),_)=>(j,Filled Maxie)) (0,Filled Maxie) (maxiecountsr)
    val maxminnier = Seq.reduce (fn ((i,Filled Minnie),(j,Filled Minnie)) => if i>j then (i,Filled Minnie) else (j,Filled Minnie)| (_,(j,Filled Minnie))=>(j,Filled Minnie) | ((j,Filled Minnie),_)=>(j,Filled Minnie)) (0,Filled Minnie) (minniecountsr)
    val maxmaxiec = Seq.reduce (fn ((i,Filled Maxie),(j,Filled Maxie)) => if i>j then (i,Filled Maxie) else (j,Filled Maxie) | (_,(j,Filled Maxie))=>(j,Filled Maxie) | ((j,Filled Maxie),_)=>(j,Filled Maxie)) (0,Filled Maxie) (maxiecountsc)
    val maxminniec = Seq.reduce (fn ((i,Filled Minnie),(j,Filled Minnie)) => if i>j then (i,Filled Minnie) else (j,Filled Minnie)| (_,(j,Filled Minnie))=>(j,Filled Minnie) | ((j,Filled Minnie),_)=>(j,Filled Minnie)) (0,Filled Minnie) (minniecountsc)
    val maxmaxied1 = Seq.reduce (fn ((i,Filled Maxie),(j,Filled Maxie)) => if i>j then (i,Filled Maxie) else (j,Filled Maxie) | (_,(j,Filled Maxie))=>(j,Filled Maxie) | ((j,Filled Maxie),_)=>(j,Filled Maxie)) (0,Filled Maxie) (diag1c)
    val maxminnied1 = Seq.reduce (fn ((i,Filled Minnie),(j,Filled Minnie)) => if i>j then (i,Filled Minnie) else (j,Filled Minnie)| (_,(j,Filled Minnie))=>(j,Filled Minnie) | ((j,Filled Minnie),_)=>(j,Filled Minnie)) (0,Filled Minnie) (diag1c)
    val maxmaxied2 = Seq.reduce (fn ((i,Filled Maxie),(j,Filled Maxie)) => if i>j then (i,Filled Maxie) else (j,Filled Maxie) | (_,(j,Filled Maxie))=>(j,Filled Maxie) | ((j,Filled Maxie),_)=>(j,Filled Maxie)) (0,Filled Maxie) (diag2c)
    val maxminnied2 = Seq.reduce (fn ((i,Filled Minnie),(j,Filled Minnie)) => if i>j then (i,Filled Minnie) else (j,Filled Minnie)| (_,(j,Filled Minnie))=>(j,Filled Minnie) | ((j,Filled Minnie),_)=>(j,Filled Minnie)) (0,Filled Minnie) (diag2c)
    val maxiewins = (case (maxmaxier,maxmaxiec,maxmaxied1,maxmaxied2) of 
      ((a,Filled Maxie),x,(b,Filled Maxie),y)=> if a=b andalso b=num_rows-2 then true else false
    | ((a,Filled Maxie),x,y,(b,Filled Maxie))=> if a=b andalso b=num_rows-2 then true else false
    | (x,(a,Filled Maxie),(b,Filled Maxie),y)=> if a=b andalso b=num_rows-2 then true else false
    | (x,(a,Filled Maxie),y,(b,Filled Maxie))=> if a=b andalso b=num_rows-2 then true else false
    | _=>false)
    val minniewins = (case (maxminnier,maxminniec,maxminnied1,maxminnied2) of 
      ((a,Filled Minnie),x,(b,Filled Minnie),y)=> if a=b andalso b=num_rows-2 then true else false
    | ((a,Filled Minnie),x,y,(b,Filled Minnie))=> if a=b andalso b=num_rows-2 then true else false
    | (x,(a,Filled Minnie),(b,Filled Minnie),y)=> if a=b andalso b=num_rows-2 then true else false
    | (x,(a,Filled Minnie),y,(b,Filled Minnie))=> if a=b andalso b=num_rows-2 then true else false
    | _=>false)
  in
    if (p=Maxie) andalso (maxiee = num_rows-1) then Definitely(Winner(Maxie))
    else if (p=Maxie) andalso (maxiewins) then Guess(50000)
    else if (p=Minnie) andalso (minniee = num_rows-1) then Definitely(Winner(Minnie))
    else if (p=Minnie) andalso minniewins then Guess(~50000)
    else Guess(difference*5)
  end

end (* functor TicTacToe *)

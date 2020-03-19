(* andrew Id: kkiyer *)

functor MakeAlphaBeta (structure G : GAME
                       val search_depth : int)
        : ALPHABETA where type Game.move = G.move
                      and type Game.state = G.state =
struct
  structure Game = G
  structure EstOrd = OrderedExt(EstOrdered(Game))

  type edge = Game.move * Game.est
  datatype value = BestEdge of edge
		 | Pruned

  type alphabeta = value * value (* invariant: alpha < beta *)

  datatype result = Value of value
		  | ParentPrune   (* an evaluation result *)


  val search_depth = search_depth

  (* printing functions *)
  fun edge_toString (m,e) = "(" ^ G.move_toString m ^ ", " ^ G.est_toString e ^ ")"

  fun value_toString (Pruned: value): string = "Pruned"
    | value_toString (BestEdge (_,e)) = "Value(" ^ G.est_toString e ^ ")"

  fun alphabeta_toString (a,b) =
      "(" ^ value_toString a ^ "," ^ value_toString b ^ ")"

  fun result_toString (Value v) = value_toString v
    | result_toString ParentPrune = "ParentPrune"

  (* equality functions *)
  fun edge_eq ((m1,est1), (m2,est2)) =
    Game.move_eq (m1,m2) andalso Game.est_eq(est1,est2)

  fun value_eq (BestEdge e1, BestEdge e2): bool = edge_eq (e1,e2)
    | value_eq (Pruned, Pruned) = true
    | value_eq _ = false

  fun alphabeta_eq ((a1,b1): alphabeta, (a2,b2): alphabeta): bool =
      value_eq(a1,a2) andalso value_eq(b1,b2)

  fun result_eq (Value v1: result, Value v2: result): bool = value_eq (v1,v2)
    | result_eq (ParentPrune, ParentPrune) = true
    | result_eq _ = false


  (* for alpha, we want max(alpha,Pruned) to be alpha, i.e.
     Pruned <= alpha for any alpha;
     otherwise order by the estimates on the edges
     *)
  fun alpha_is_less_than (Pruned: value, v: Game.est): bool = true
    | alpha_is_less_than (BestEdge(_,alphav), v) = EstOrd.lt(alphav,v)

  fun maxalpha (Pruned, v2): value = v2
    | maxalpha (v1, Pruned) = v1
    | maxalpha (v1 as BestEdge(_,e1), v2 as BestEdge(_,e2)) =
       if EstOrd.lt (e1,e2) then v2 else v1

  (* for beta, we want min(beta,Pruned) to be beta, i.e.
     beta <= Pruned for any beta;
     otherwise order by the estimates on the edges
     *)
  fun beta_is_greater_than (v: Game.est, Pruned: value): bool = true
    | beta_is_greater_than (v, BestEdge(_,betav)) = EstOrd.lt(v,betav)

  fun minbeta (Pruned, v2): value = v2
    | minbeta (v1, Pruned) = v1
    | minbeta (v1 as BestEdge(_,e1), v2 as BestEdge(_,e2)) =(if EstOrd.lt (e1,e2) then v1 else v2)


  (* updateAB: state->alphabeta->value
   * Updates the alpha or beta 
   * depending on whether I'm maxie or minnie 
   *)
  fun updateAB (s: Game.state)((a,b): alphabeta)(v: value) = 
    if Game.player_eq(Game.player(s),Game.Maxie) 
    then (maxalpha(a,v),b)
  else (a,minbeta(b,v))


  (* valuefor: state->alphabeta->value
   * Finds whether the value is alpha or beta depending on the player 
   *)
  fun value_for (s: Game.state)((a,b): alphabeta) = if Game.player_eq(Game.player(s),Game.Maxie) 
    then a 
    else b

  (* check_bounds: alphabeta->state->move->est
   * compares the estimate to alpha and beta and accordingly passes up a result 
   *)
  fun check_bounds ((a,b): alphabeta)(s: Game.state)(m: Game.move)(x: Game.est) = if not(alpha_is_less_than(a,x))
    then if Game.player_eq(Game.player(s),Game.Maxie) then ParentPrune
         else Value(Pruned)
  else if not(beta_is_greater_than(x,b))
    then if Game.player_eq(Game.player(s),Game.Minnie) then ParentPrune
         else Value(Pruned)
    else Value(BestEdge(m,x))

  (* evaluate: int->alphabeta->state->move->result
   * search: int->alphabeta->state->move Seq.seq->value
   * evaluate evaluates the current node, and search evaluates all 
   * the child states of the node, and parses them.
   *)
  fun evaluate (0: int)((a,b): alphabeta)(s: Game.state)(m: Game.move) = check_bounds((a,b))(s)(m)(Game.estimate(s))
    | evaluate (n)((a,b))(s)(m) = let
      val moveseq = Game.moves(s)
    in
      if not(Game.status_eq(Game.status(s),Game.In_play)) then check_bounds((a,b))(s)(m)(Game.estimate(s))
      else (case search (n)(a,b)(s)(Game.moves(s)) of
              Pruned => Value(Pruned)
            | BestEdge(m',e) => check_bounds((a,b))(s)(m)(e))
    end
  and search (n: int)((a,b): alphabeta)(s: Game.state)(mseq: Game.move Seq.seq) = if Seq.length(mseq)=0 
    then value_for(s)((a,b))
    else
      let
        val firstmove = Seq.nth(0)(mseq)
        val remaining = Seq.drop(1)(mseq)
      in
        (case (evaluate (n-1)((a,b))(Game.make_move(s,firstmove))(firstmove)) of
            ParentPrune => Pruned
          | Value(v) => search (n)(updateAB(s)((a,b))(v))(s)(remaining))
      end
    
  fun next_move (s:Game.state) = (case (search (search_depth)((Pruned,Pruned))(s)(Game.moves s)) of
      BestEdge(m,x) => m)

end (* functor MakeAlphaBeta *)

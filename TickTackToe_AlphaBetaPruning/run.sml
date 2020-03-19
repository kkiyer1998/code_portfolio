(* andrew Id: kkiyer *)

structure T3 = TicTacToe(val size = 3)

(* Players *)
structure HumanT3 = HumanPlayer(T3)
structure MM5T3   = MiniMax(structure G = T3
                            (* search depth 4 is relatively instantaneous;
                               5 takes about 5 seconds per move *)
                            val search_depth = 5)

structure AB5T3 = MakeAlphaBeta(structure G = T3 val search_depth = 5) (* AlphaBeta *)
structure J5T3  = MakeJamboree(structure G = T3 val search_depth = 5 val prune_percentage = 0.5) (* Jamboree  *)

(* Plays *)
structure T3_HvMM = Referee(structure Maxie = HumanT3
                            structure Minnie = MM5T3)

structure T3_HvAB = Referee(structure Maxie = AB5T3 structure Minnie = MM5T3) (* AlphaBeta vs. human *)
structure T3_HvJ  = Referee(structure Maxie = J5T3 structure Minnie = MM5T3) (* Jamboree  vs. human *)


(* FOR THE TOURNAMENT I WILL BE USING MINMAX WITH A SEARCH DEPTH OF 5*)
structure MM5T3   = MiniMax(structure G = T3
                            (* search depth 4 is relatively instantaneous;
                               5 takes about 5 seconds per move *)
                            val search_depth = 5)
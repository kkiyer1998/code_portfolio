functor Mlisa (structure Stream : ISTREAM
               structure Memory : MEMORY
	       structure Stack  : STACK) :> MLISA where I = Stream
                                                    and M = Memory
						    and S = Stack    =
struct
  structure S = Stack     (* Abbreviations *)
  structure M = Memory
  structure I = Stream

  datatype instruction = NOP                (* MLISA instructions *)
		       | STOP
		       | SS    of string * (int * int -> int)
		       | PUSHI of int
		       | PUSH  of int
		       | POP   of int
		       | BR    of string * (int * int -> bool) * int
		       | JUMP  of int
  
  datatype memcode = MNOP                   (* memory micro-codes *)
		   | LOAD
		   | STORE of int

  datatype stacode = STNOP                  (* stack micro-codes *)
		   | STPUSH of int

  type operation = int * stacode * memcode  (* micro-operations *)

  type instMem = instruction M.mem          (* instruction memory *)
  type stack   = int S.stack                (* stack *)
  type dataMem = int M.mem                  (* data memory *)
  type data    = stack * dataMem            (* data storage *)

  type state = data * (data, data) I.stream (* processor state *)

  exception Invalid       (* invalid operation -- should never happen *)
  exception Halt          (* end of execution -- used internally *)

  (* fetch    : instMem -> int -> (data, instruction * data) I.stream
   * REQUIRES pc>=0
   * ENSURES stream returned contains all instructions along contril flow
   *)
  fun fetch (imem: instMem)(pc: int): (data, instruction * data) I.stream = let
    val instr = M.read (imem)(pc)
    val fetch' = case instr of 
                    BR(st,b,addr) =>(fn (s,dmem): data => let 
                                    val (x,s') = S.pop(s)
                                    val (y,s'') = S.pop(s')
                                  in
                                    if b(x,y) then I.Gen((NOP,(s'',dmem)), fetch (imem)(addr))
                                    else I.Gen((NOP,(s'',dmem)),fetch (imem)(pc+1))
                                    end)
                  | JUMP x =>(fn (s,dmem)=>I.Gen((NOP,(s,dmem)),fetch (imem)(x)))
                  | STOP => (fn (s,dmem)=> I.End)
                  | x => (fn (s,dmem)=>I.Gen((x,(s,dmem)),fetch (imem)(pc+1)))
  in
    I.delay (fetch')
  end 

  (* decode   : (data, instruction * data) I.stream
              -> (data, operation * data) I.stream
   * REQUIRES true
   * ENSURES instructions are decomposed into operations mapping them
   *)
  fun decode (idstream: (data, instruction * data) I.stream): (data, operation * data) I.stream = let
    val decode' = fn (s1,dmem1) => (case (I.expose (idstream)(s1,dmem1)) of 
      I.Gen((SS(st,f),(s,dmem)),idstream') => let 
                                    val (x,s') = S.pop(s)
                                    val (y,s'') = S.pop(s')
                                  in
                                    I.Gen(((0,STPUSH(f(x,y)),MNOP),(s'',dmem)),decode idstream') 
                                  end
    | I.Gen(((PUSH addr),(s,dmem)),idstream') => I.Gen(((addr,STNOP,LOAD),(s,dmem)), decode idstream')
    | I.Gen((PUSHI x,(s,dmem)),idstream') => I.Gen(((0,STPUSH(x),MNOP),(s,dmem)),decode idstream')
    | I.Gen((NOP,(s,dmem)),idstream') => I.Gen(((0,STNOP,MNOP),(s,dmem)),decode idstream')
    | I.Gen((POP addr,(s,dmem)),idstream') => let val (x,s') = S.pop(s) in I.Gen(((addr,STNOP,STORE x),(s',dmem)), decode idstream') end
    | I.End => I.End)
  in
    I.delay(decode')
  end

  (* mmu      : (data, operation * data) I.stream -> (data, data) I.stream
   * REQUIRES true
   * ENSURES grows out resulting data after each operation as a stream
   *)
  fun mmu  (oS: (data,operation*data) I.stream): (data,data) I.stream = let
    val mmu' = fn d => (case I.expose(oS)(d) of
        I.Gen(((0,STNOP,MNOP): operation,(s,dmem): data),oS') => I.Gen((s,dmem),mmu oS')
      | I.Gen(((addr,STNOP,STORE x): operation,(s,dmem): data),oS') => let
        val dmem' = M.write (dmem) (addr,x)
      in
        I.Gen((s,dmem'),mmu oS')
      end
      | I.Gen(((0,STPUSH(x),MNOP): operation,(s,dmem): data),oS') => let
        val s' = S.push(x,s)
      in
        I.Gen((s',dmem),mmu oS')
      end
      | I.Gen(((addr,STNOP,LOAD): operation,(s,dmem): data),oS') => let
        val x = M.read(dmem)(addr)
        val s'= S.push(x,s)
      in
        I.Gen((s',dmem),mmu oS')
      end
      | I.End => I.End)
  in
    I.delay(mmu')
  end

  (* connect  : instMem -> int -> stack -> dataMem -> state 
   * REQUIRES true
   * ENSURES state returned is the eq to corresponding program
   *)
  fun connect  (imem: instMem)(pc: int)(st)(dmem: dataMem): state = let
    val fetched = fetch (imem)(pc)
    val decoded = decode (fetched)
    val mmued = mmu (decoded)
  in
    ((st,dmem),mmued)
  end


  fun step (((st,dmem),dstream): state): state = let
    val next = I.expose (dstream) (st,dmem)
  in
    (case next of I.End => raise Halt 
      | I.Gen((st',dmem'),dstream') => ((st',dmem'),dstream'))
  end

  fun simulate (s:state) = simulate( step(s) ) handle Halt => (case (s) of (d,dstream')=>d)



  (* Printing and equality functions *)
  fun instruction_toString NOP          = "NOP"
    | instruction_toString STOP         = "STOP"
    | instruction_toString (SS (s,_))   = "SS "    ^ s
    | instruction_toString (PUSHI x)    = "PUSHI " ^ Int.toString x
    | instruction_toString (PUSH a)     = "PUSH "  ^ Int.toString a
    | instruction_toString (POP a)      = "POP "   ^ Int.toString a
    | instruction_toString (BR (s,_,i)) = "BR "    ^ s ^ " " ^ Int.toString i
    | instruction_toString (JUMP i)     = "JUMP "  ^ Int.toString i

  fun instruction_eq (NOP,          NOP)          = true
    | instruction_eq (STOP,         STOP)         = true
    | instruction_eq (SS (s1,_),    SS (s2,_))    = s1=s2
    | instruction_eq (PUSHI x1,     PUSHI x2)     = x1=x2
    | instruction_eq (PUSH a1,      PUSH a2)      = a1=a2
    | instruction_eq (POP a1,       POP a2)       = a1=a2
    | instruction_eq (BR (s1,_,i1), BR (s2,_,i2)) = s1=s2 andalso i1=i2
    | instruction_eq (JUMP i1,      JUMP i2)      = i1=i2
    | instruction_eq _ = false

  fun memcode_toString MNOP      = "MNOP"
    | memcode_toString LOAD      = "LOAD"
    | memcode_toString (STORE x) = "STORE " ^ Int.toString x

  fun memcode_eq (MNOP,     MNOP)     = true
    | memcode_eq (LOAD,     LOAD)     = true
    | memcode_eq (STORE x1, STORE x2) = x1=x2
    | memcode_eq _ = false

  fun stacode_toString STNOP      = "STNOP"
    | stacode_toString (STPUSH x) = "STPUSH " ^ Int.toString x

  fun stacode_eq (STNOP,     STNOP)     = true
    | stacode_eq (STPUSH x1, STPUSH x2) = x1=x2
    | stacode_eq _ = false

  fun operation_toString (addr, sc, mc) = "("  ^ Int.toString addr
					^ ", " ^ stacode_toString sc
					^ ", " ^ memcode_toString mc
					^ ")"

  fun operation_eq ((a1,sc1,mc1), (a2,sc2,mc2)) =
        a1=a1                andalso
	stacode_eq (sc1,sc2) andalso
	memcode_eq (mc1,mc2)

  fun data_toString (st,mem) =   "S: " ^ S.toString Int.toString st
                             ^ "; M: " ^ M.toString Int.toString mem

  fun data_eq ((st1,m1), (st2,m2)) =
        S.eq (op=) (st1,st2) andalso M.eq (op=) (m1,m2)

  fun state_toString l (d, dS) = "["  ^ data_toString d
			       ^ "; " ^ I.toString (data_toString, data_toString) l dS
			       ^ "]"

  fun state_eq (l: data list) ((d1,dS1): state, (d2,dS2): state): bool =
        data_eq (d1,d2) andalso I.eq data_eq l (dS1, dS2)

end (* functor Mlisa *)

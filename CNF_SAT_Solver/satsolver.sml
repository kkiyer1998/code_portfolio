functor SatSolver (P : PROP) : SATSOLVER =
struct

structure P = P

datatype form = PVar of P.t
	      | And of form * form
	      | Or of form * form
	      | Not of form
	      | Impl of form * form;

datatype 'a cnf = 
  L of 'a 
  | Neg of 'a cnf
  | Andl of 'a cnf list
  | Orl of 'a cnf list


type assignment = (P.t * bool);



(* removeimpl: form -> form
 * removeimpl(f) = f'
 * REQUIRES true
 * ENSURES Impl is not contained in f'
 *)
fun removeimpl (PVar(x) :form):form = PVar(x)
  | removeimpl (And(a,b)) = And(removeimpl(a),removeimpl(b))
  | removeimpl (Or(a,b)) = Or(removeimpl(a),removeimpl(b))
  | removeimpl (Not(x)) = Not(removeimpl(x))
  | removeimpl (Impl(a,b)) = Or(Not(removeimpl(a)),removeimpl(b))

(* pushnegs: form -> form
 * pushnegs(f) = f'
 * REQUIRES f has no Impl
 * ENSURES Not operates only on PVar's in f'
 *)
fun pushnegs (PVar(x):form):form = PVar x
  | pushnegs (Not(PVar x)) = Not(PVar x)
  | pushnegs (Not(Not(f))) = pushnegs(f)
  | pushnegs (Not(And(a,b))) = Or(pushnegs(Not(a)),pushnegs(Not(b)))
  | pushnegs (Not(Or(a,b))) = And(pushnegs(Not a),pushnegs(Not b))
  | pushnegs (Or(a,b)) = Or(pushnegs(a),pushnegs(b))
  | pushnegs (And(a,b)) = And(pushnegs(a),pushnegs(b))

(* convert: form -> form
 * convert f = f'
 * REQUIRES Impl removed, Nots pushed to bottom level
 * ENSURES f' is a formula of the CNF form
 *)
fun convert (PVar x:form) = PVar x
  | convert (Or(x,And(a,b))) = And(convert(Or(x,a)),convert(Or(x,b)))
  | convert (Or(And(a,b),x)) = And(convert(Or(a,x)),convert(Or(b,x)))
  | convert (Or(x,y)) = let
    val x' = convert x
    val y' = convert y
  in
    case (x',y') of
      (And(a,b),z) => convert(Or(x',y'))
      | (z,And(a,b)) => convert(Or(x',y'))
      | (a,b) => Or(a,b)
  end
  | convert (And(a,b)) = And(convert a,convert b)
  | convert (Not(x)) = Not(x)

(* cnfify: form -> 'a cnf
 * cnfify(f) = cn
 * REQUIRES f is a valid cnf formula
 * ENSURES true
 *)
fun cnfify (PVar(x): form):P.t cnf = L(x)
  | cnfify (Not(PVar x)) = Neg(L x)
  | cnfify (And(a,b)) = (case (cnfify(a),cnfify(b)) of
    (Andl(l),Andl(l')) => Andl(l@l')
    | (Andl(l),x) => Andl(l@[x])
    | (x,Andl(l')) => Andl(x::l')
    | (x,x') => Andl([x,x']))
  | cnfify (Or(a,b)) = (case (cnfify(a),cnfify(b)) of
    (Orl(l),Orl(l')) => Orl(l@l')
    | (Orl(l),x) => Orl(l@[x])
    | (x,Orl(l')) => Orl(x::l')
    | (x,x') => Orl([x,x']))

(* cnfify: form -> 'a cnf
 * cnfify(cn) = f
 * REQUIRES true
 * ENSURES cn is a valid cnf form
 *)
fun uncnfify (L(x): P.t cnf): form = PVar(x)
  | uncnfify (Neg(L(x))) = Not(PVar(x))
  | uncnfify (Orl([x])) = uncnfify x
  | uncnfify (Orl(x::l)) = Or(uncnfify x,uncnfify(Orl l))
  | uncnfify (Andl([x])) = uncnfify x
  | uncnfify (Andl(x::l)) = And(uncnfify x,uncnfify(Andl l))

(* toCNF: form -> form
 * toCNF(f) = f'
 * REQUIRES true
 * ENSURES f' is a valid cnf form
 *)
fun toCNF (f:form):form = convert(pushnegs(removeimpl(f)))

(* getvlist: 'a cnf -> 'a cnf list
 * getvlist(f) = fl
 * REQUIRES f is a valid cnf formula
 * ENSURES fl contains all the unique literals in f
 *)
fun getvlist (Andl(x::l): P.t cnf): P.t cnf list = getvlist(x)@getvlist(Andl(l))
  | getvlist (Andl(nil)) = []
  | getvlist (Orl(L(x)::l)) = L(x)::getvlist(Orl(l))
  | getvlist (Orl(Neg(L(x))::l)) = L(x)::getvlist(Orl(l))
  | getvlist (Orl(nil)) = []
  | getvlist (L(x)) = [L(x)]
  | getvlist (Neg(L(x))) = [L(x)]

(* isin: 'a cnf * 'a cnf list -> bool
 * isin(f,fl) = n
 * REQUIRES f is a literal
 * ENSURES true if f is present in the list of 'a cnfs
 *)
fun isin (x:P.t cnf,nil: P.t cnf list) = false
  | isin (L(x),L(a)::l) = if P.eq(x,a) then true else isin(L(x),l)
  | isin (L(x),Neg(L(a))::l) = isin(L(x),l)
  | isin (Neg(L(x)),Neg(L(a))::l) = if P.eq(x,a) then true else isin(Neg (L(x)),l)
  | isin (Neg(L(x)),L(a)::l) = isin(Neg(L(x)),l)
  | isin (x,a::l) = isin(x,l)
  
(* removeduplicates: 'a cnf -> 'a cnf list
 * removeduplicates(f) = fl
 * REQUIRES f is a valid cnf formula
 * ENSURES fl contains all the unique literals in f
 *)
fun removeduplicates (nil: P.t cnf list) = nil
  | removeduplicates (x::l) = if isin(x,l) then removeduplicates(l) else (x::removeduplicates(l))

(* reduce: 'a cnf * 'a cnf -> 'a cnf
 * reduce(f,var) = newf
 * REQUIRES f is a valid cnf formula, var is a literal truth value
 * ENSURES newf is the result of setting var to true in f
 *)
fun reduce (Andl([]): P.t cnf,_: P.t cnf): P.t cnf = Andl([])
  | reduce (Andl(Orl(li)::l), L(x)) = if (isin(L(x),li)) 
                                    then reduce(Andl(l),L(x))
                                    else (case reduce(Andl(l),L(x)) of
                                          Andl(lx) => Andl(Orl(li)::lx))
  | reduce (Andl(Orl(li)::l), Neg(L(x))) = if (isin(Neg(L(x)),li)) 
                                          then reduce(Andl(l),Neg(L(x)))
                                          else (case (reduce(Andl(l),Neg(L(x)))) of
                                                Andl(lx) => Andl(Orl(li)::lx))
  | reduce (Andl(L(x)::l),L(a)) = if P.eq(x,a) 
                                  then reduce(Andl(l),L(a))
                                  else (case (reduce(Andl(l),L(a))) of
                                        Andl(lx) => Andl(L(x)::lx))
  | reduce (Andl(Neg(L(x))::l),Neg(L(a))) = if P.eq(x,a) 
                                  then reduce(Andl(l),Neg(L(a)))
                                  else (case (reduce(Andl(l),Neg(L(a)))) of
                                        Andl(lx) => Andl(Neg(L(x))::lx))
  | reduce (Andl(x::l),a) = (case (reduce(Andl(l),a)) of
                                        Andl(lx) => Andl(x::lx))

(* fixcnf: 'a cnf -> 'a cnf
 * fixcnf x = y
 * REQUIRES x is in CNF form
 * ENSURES Ors act on more than one literal
 *)
fun fixcnf (Andl (x::l): P.t cnf): P.t cnf = (case (fixcnf(Andl(l))) of 
  Andl(l) => Andl(fixcnf(x)::l))
  | fixcnf (Andl(nil)) = Andl(nil)
  | fixcnf (Orl (x::nil)) = x
  | fixcnf (Orl l) = Orl l
  | fixcnf x = x


(* satsolver: 'a cnf * 'a cnf list -> assignment list option
 * satsolver(f,vlist) = x
 * REQUIRES f is a valid cnf formula, vlist is a unique list of variables in f
 * ENSURES x=NONE if no assignment satisfies f and SOME l 
 * where l is a list of assignments that satisfies f
 *)

fun satsolver (Andl([]): P.t cnf,nil: P.t cnf list): assignment list option = SOME []
  | satsolver (Andl(l),nil) = NONE
  | satsolver (Andl([]),L(x)::l) = SOME [](*(case (satsolver(Andl([]),l)) of 
        SOME (li) => SOME((x,true)::li))*)
  | satsolver (Andl(ll), L(x)::l) = let
    val Andl(redl) = fixcnf(reduce(Andl(ll),L(x)))
  in
    if isin(Neg(L(x)),redl) then satsolver(Andl(ll),Neg(L(x))::l) else
    (case (satsolver(Andl(redl),l)) of 
      SOME (li) => SOME((x,true)::li)
    | NONE => satsolver(Andl(ll),Neg(L(x))::l))
  end
  | satsolver (Andl([]),Neg(L(x))::l) = (case (satsolver(Andl([]),l)) of 
      SOME (li) => SOME((x,false)::li))
  | satsolver (Andl(ll), Neg(L(x))::l) = let
    val Andl(redl) = fixcnf(reduce(Andl(ll),Neg(L(x))))
  in
    if isin(L(x),redl) then NONE else
    (case (satsolver(Andl(redl),l)) of 
      SOME (li) => SOME((x,false)::li)
    | NONE => NONE)
  end     
  | satsolver (Orl(l),L(x)::li) = (case satsolver(fixcnf(reduce(Andl([Orl(l)]),L(x))),li) of
    SOME lx => SOME((x,true)::lx)
    |NONE => satsolver(Orl(l),L(x)::li))
  | satsolver (Orl(l),Neg(L(x))::li) = (case satsolver(fixcnf(reduce(Andl([Orl(l)]),Neg(L(x)))),li) of
    SOME lx => SOME((x,false)::lx)
    |NONE => NONE)
  | satsolver (L(i),L(x)::lx) = SOME([(i,true)])
  | satsolver (Neg(L(i)),L(x)::lx) = SOME([(i,false)])

(* sat: form -> assignment list option
 * sat(f) = x
 * REQUIRES true
 * ENSURES x=NONE if no assignment satisfies f and SOME l 
 * where l is a list of assignments that satisfies f
 *)
fun sat (f:form): assignment list option = let
  val newf = cnfify (toCNF(f))
  val vlist = removeduplicates (getvlist (newf))
in
  satsolver (newf,vlist)
end

(* isUnsat: form -> bool
 * isUnsat(f) = x
 * REQUIRES true
 * ENSURES x=true if no satisfiable assignment exists for f, false if there exists an assignment
 *)
fun isUnsat (f:form) = (case sat(f) of
  NONE => true
  | SOME x => false)

(* isValid: form -> bool
 * isValid(f) = x
 * REQUIRES true
 * ENSURES x=true if every assignment possible satisfies f
 *)
fun isValid (f: form) = if isUnsat(Not(f)) then true else false

end;

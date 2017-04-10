(*
Type checker for an imaginary language with Hindley-Milner type inference
Includes a full implementation of algorithm W
*)
datatype prim = Add | Neg | Mult | Div | And | Or | Not | Eq | Lt | Gt

datatype expr = Var of string
                | Abs of string * expr
                | App of expr * expr
                | If of expr * expr * expr
                | Let of string * expr * expr
                | Fix of string * expr
                | Int of int
                | Bool of bool
                | Prim of prim
                | Letex of string * expr
                | Raise of expr
                | Handle of expr * expr * expr 


val num = ref 0
fun freshvar () = "Z" ^ Int.toString(!num before num := !num + 1)


datatype primtypes = TInt | TBool | TExc

datatype mtype = Exception | Primitive of primtypes | TVar of string | Arrow of mtype * mtype

type substitution = (mtype * mtype)
type substitutions = substitution list

type entry = (expr * mtype * (mtype list))
type tenv = entry list


(* Pretty print a monotype *)
(* mtype -> string *)
fun pptype (TVar a) = "'" ^ a
|	pptype Exception = "Exception"
|	pptype (Primitive TInt) = "int"
|	pptype (Primitive TBool) = "bool"
|	pptype (Primitive TExc) = "exc"
|	pptype (Arrow (a, b)) = "(" ^ pptype(a) ^ "->" ^ pptype(b) ^ ")"

(* expr -> string *)
fun pp (Var a) = a
|	pp (Prim Add) = "Add"
|	pp (Prim Neg) = "Neg"
|	pp (Prim Mult) = "Mult"
|	pp (Prim Div) = "Div"
|	pp (Prim And) = "And"
|	pp (Prim Or) = "Or"
|	pp (Prim Not) = "Not"
|	pp (Prim Eq) = "Eq"
|	pp (Prim Lt) = "Lt"
|	pp (Prim Gt) = "Gt"
|	pp (Int x) = Int.toString(x)
|	pp (Bool x) = if x then "#t" else "#f" 
|	pp (Abs (x, e)) = "(L" ^ x ^ "." ^ (pp e) ^ ")"
|	pp (App (a, b)) = (pp a) ^ " " ^ (pp b)
|	pp (If (a, b, c)) = "if (" ^ (pp a) ^ ") then (" ^ (pp b) ^ ") else (" ^ (pp c) ^ ")"
|	pp (Let (x, e1, e2)) = "<" ^ x ^ ", " ^ (pp e1) ^ "> " ^ (pp e2)
|	pp (Fix (x, e)) = "fix " ^ (pp (Abs (x, e)))
|	pp (Letex (x, e)) = "<" ^ x ^ "> " ^ (pp e)
|	pp (Raise (e)) = "Raise(" ^ (pp e) ^ ")"
|	pp (Handle (e1, e2, e3)) = (pp e2) ^ "\n\tHandle " ^ (pp e1) ^ " => " ^ pp (e3)


(* Expression is not well-typed *)
exception TypeError of string

(* Tried to lookup a variable which was not in the type environment *)
exception ReferenceError of string

(* A bug in the type checker *)
exception ProgrammingError of string

exception AssertionError


(* Given a subtitution and a single type, preform the type substitution on the type*)
(* substitution * mtype -> mtype *)
fun sub ((t, TVar b),    (TVar a)) = if a = b then t else (TVar a)
|	sub ((t, Exception), Exception) = t
|	sub ((t, Exception), b) = b
|	sub (_,              Exception) = Exception
|   sub (_,              (P as Primitive a)) = P
|   sub (s,              (Arrow (t1, t2))) = Arrow (sub (s, t1), sub (s, t2))
|	sub (s, a) = raise ProgrammingError ("[" ^ (pptype (#1 s)) ^ "/" ^ (pptype (#2 s)) ^ "] " ^ (pptype a))

(* Given a type and a list of substitutions, perform all of the substitutions on the type *)
(* mtype -> substitutions -> mtype *)
val subs = foldl sub

(* Given a substitution and a single entry in the environment, peform the type substitution on the type *)
(* subsitutions -> entry -> entry *)
fun subEntry s (expr, t, quants) = (expr, (subs t s), quants)

(* Given a sub and an environment, peform the type substitution on all entries in the env  (definition 4.7) *)
(* substitutions -> tenv -> tenv *)
fun subEnv s = map (subEntry s)

(* Check if type occurs in another type, or the types are equal*)
(* mtype -> mtype -> bool *)
fun occursHelp (TVar a) (Primitive t) = false
|	occursHelp (TVar a) Exception = false
|	occursHelp (TVar a) (TVar b) = a = b
|	occursHelp (TVar a) (Arrow (t1, t2)) = occursHelp (TVar a) t1 orelse occursHelp (TVar a) t2
|	occursHelp a b = raise ProgrammingError "occursHelp"

(* Check if type occurs in another type, but the types aren't equal*)
(* mtype -> mtype -> bool *)
fun occurs (TVar a) (TVar b) = false
|	occurs a t = occursHelp a t

(* Given two types, find a substitution which makes them equal (definition 4.6)*)
(* mtype -> mtype -> substitutions *)
fun unify (Primitive t1) (Primitive t2) = if t1 = t2 then [] else raise TypeError "Type mismatch"
|	unify Exception Exception = []
|	unify Exception t = [(t, Exception)]
|	unify t Exception  = [(t, Exception)]
|	unify (Arrow (_,_)) (Primitive _) = raise TypeError "Kind mismatch"
|	unify (Arrow (t1, t2)) (Arrow (t3, t4)) = let
		val s1 = unify t1 t3
		val s2 = unify (subs t2 s1) (subs t4 s1)
	in
		s1 @ s2
	end
|	unify (TVar a) (TVar b) = if a = b then [] else [(TVar a, TVar b)]
|	unify (TVar a) t = if occurs (TVar a) t then raise TypeError (a ^ " strict subterm of " ^ (pptype t)) else [(t, TVar a)]
|	unify a b = unify b a (* unify is symmetric so if no pattern matches then try the other way *)

(* mtype -> tenv -> bool*)
fun freeVarIn (TVar a) [] = false
|	freeVarIn (TVar a) ((e, t, quants)::es) = (TVar a) = t orelse freeVarIn (TVar a) es
|	freeVarIn _ _ = raise ProgrammingError "freeVarIn" (* freeVarIn should only be called on TVars *)

(* mtype -> tenv -> mtype list*)
fun toQuantify (Primitive _) env = []
|	toQuantify (TVar a) env = if freeVarIn (TVar a) env then [] else [TVar a]
|	toQuantify (Arrow (a, b)) env = (toQuantify a env) @ (toQuantify b env)
|	toQuantify Exception env = []

(* Given a monotype and a list of quantifiers, returns the monotype with all of
	the TVars replaced with fresh type variables *)
(* mtype -> mtype list -> mtype *)
fun replace t [] = t
|	replace t (q::qs) = subs (replace t qs) [(TVar (freshvar()), q)]

(* Given a type environment and a Var, looks up the type of the var *)
(* tenv -> expr -> mtype *)
fun lookup [] m = raise ReferenceError ((pp m) ^ " undefined")
|	lookup ((e, t, quants)::es) expr = if expr = e then replace t quants else lookup es expr  

(* tenv -> expr -> substitutions * mtype *)
(* (expr * mtype * mtype list) list -> expr -> (mtype * mtype) list * mtype *)
fun w a (Int _) = ([], Primitive TInt)
|	w a (Bool _) = ([], Primitive TBool)
|	w a (Prim Add) = ([], Arrow (Primitive TInt, Arrow (Primitive TInt, Primitive TInt)))
|	w a (Prim Neg) = ([], Arrow (Primitive TInt, Primitive TInt))
|	w a (Prim Mult) = ([], Arrow (Primitive TInt, Arrow (Primitive TInt, Primitive TInt)))
|	w a (Prim Div) = ([], Arrow (Primitive TInt, Arrow (Primitive TInt, Primitive TInt)))
|	w a (Prim And) = ([], Arrow (Primitive TBool, Arrow (Primitive TBool, Primitive TBool)))
|	w a (Prim Or) = ([], Arrow (Primitive TBool, Arrow (Primitive TBool, Primitive TBool)))
|	w a (Prim Not) = ([], Arrow (Primitive TBool, Primitive TBool))
|	w a (Prim Eq) = ([], Arrow (Primitive TInt, Arrow (Primitive TInt, Primitive TBool)))
|	w a (Prim Lt) = ([], Arrow (Primitive TInt, Arrow (Primitive TInt, Primitive TBool)))
|	w a (Prim Gt) = ([], Arrow (Primitive TInt, Arrow (Primitive TInt, Primitive TBool)))
|	w a (Var x) = ([], lookup a (Var x))
|	w a (Abs (x, e)) = let
		val alpha = TVar (freshvar())
		val temp = w ((Var x, alpha, [])::a) e
		val sx = #1 temp
		val tau = #2 temp
	in
		(sx, Arrow (subs alpha sx, tau))
	end
|	w a (App (e1, e2)) = let
		val alpha = TVar (freshvar())
		val temp1 = w a e1
		val sx1 = #1 temp1
		val tau1 = #2 temp1
		val temp2 = w (subEnv sx1 a) e2
		val sx2 = #1 temp2
		val tau2 = #2 temp2
		val sx3 = unify (subs tau1 sx2) (Arrow (tau2, alpha))
	in
		(sx1 @ sx2 @ sx3, subs alpha sx3)
	end
|	w a (If (e1, e2, e3)) = let
		val temp1 = w a e1
		val sx1 = #1 temp1
		val tau1 = #2 temp1
		val sx2 = unify (Primitive TBool) tau1
		val sx1sx2 = sx1 @ sx2
		val temp3 = w (subEnv sx1sx2 a) e2
		val sx3 = #1 temp3
		val tau3 = #2 temp3
		val sx1sx2sx3 = sx1sx2 @ sx3
		val temp4 = w (subEnv sx1sx2sx3 a) e3
		val sx4 = #1 temp4
		val tau4 = #2 temp4
		val sx5 = unify (subs tau3 sx4) tau4
	in
		(sx1sx2sx3 @ sx4 @ sx5, subs tau4 sx5)
	end
|	w a (Let (x, e1, e2)) = let
		val temp1 = w a e1
		val sx1 = #1 temp1
		val tau1 = #2 temp1
		val quants = toQuantify tau1 (subEnv sx1 a)
		val temp2 = w (subEnv sx1 ((Var x, tau1, quants)::a)) e2
		val sx2 = #1 temp2
		val tau2 = #2 temp2
	in
		(sx1 @ sx2, tau2)
	end
|	w a (Fix (x, e)) = let
		val alpha = TVar (freshvar())
		val temp1 = w ((Var x, alpha, [])::a) e
		val sx1 = #1 temp1
		val tau1 = #2 temp1
		val sx2 = unify (subs alpha sx1) tau1
		val sx1sx2 = sx1 @ sx2
	in
		(sx1sx2, (subs alpha sx1sx2))
	end
|	w a (Raise e) = let
		val temp1 = w a e
		val sx1 = #1 temp1
		val tau1 = #2 temp1
		val sx2 = unify (Primitive TExc) tau1
		val sx1sx2 = sx1 @ sx2
	in
		(sx1sx2, Exception)
	end
|	w a (Letex (var, e)) = w ((Var var, Primitive TExc, [])::a) e
|	w a (Handle (e1, e2, e3)) = let
		val temp1 = w a e1
		val sx1 = #1 temp1
		val tau1 = #2 temp1
		val sx2 = unify (Primitive TExc) tau1
		val sx1sx2 = sx1 @ sx2
		val temp3 = w (subEnv sx1sx2 a) e2
		val sx3 = #1 temp3
		val tau3 = #2 temp3
		val sx1sx2sx3 = sx1sx2 @ sx3
		val temp4 = w (subEnv sx1sx2sx3 a) e3
		val sx4 = #1 temp4
		val tau4 = #2 temp4
		val sx5 = unify (subs tau3 sx4) tau4
	in
		(sx1sx2sx3 @ sx4 @ sx5, subs tau4 sx5)
	end


(* Test helpers *)
fun assert expr expected = let
	val actual = #2 (w [] expr)
in
	if expected = actual then "pass" else (pptype actual) ^ " =/= " ^ (pptype expected)  
end

fun assertRaises expr = ((w [] expr); pp expr)
	handle TypeError msg => "pass"
	| ReferenceError msg => "pass"

fun run expr = (pp expr) ^ " : " ^ (pptype (#2 (W expr initenv))) ^ "\n"
	handle TypeError msg => (pp expr) ^ " : TypeError(" ^ msg ^ ")\n"
	| ReferenceError msg => (pp expr) ^ " : ReferenceError(" ^ msg ^ ")\n"
	| ProgrammingError msg => (pp expr) ^ " : ProgrammingError(" ^ msg ^ ")\n"
;

(* Miscellaneous tests *)
assert (If(Fix("x",Var"x"),Int 1,Int 2))
	(Primitive TInt);
assert (Let("y",Fix("x",Var"x"),If(Var"y",Int 1,Int 2)))
	(Primitive TInt);
assert (Let("y",Fix("x",Var"x"),If(Var"y",Var"y",Int 2)))
	(Primitive TInt);
assert (Abs("x",If(Var"x",Int 1,Int 0)))
	(Arrow (Primitive TBool, Primitive TInt));
assert (Abs("x",Raise(Var"x")))
	(Arrow(Primitive TExc, Exception));
assert (Letex("err",Raise(Raise(Var"err"))))
	Exception;
assert (Let("f",Abs("x",Var"x"),If(App(Var"f",Bool true),App(Var"f",Int 1),Int 0)))
	(Primitive TInt);
assert (Letex("err",Let("x",Raise(Var"err"),If(Bool true,Var"x",Int 0))))
	(Primitive TInt);
assert (Letex("err",Abs("x",If(Bool true, Raise(Var "x"), Int 2))))
	(Arrow (Primitive TExc, Primitive TInt));
assert (Let("x",Abs("x", Fix("x", Var"x")),Let("y",App(Var"x",Int 5),If(Var"y",Int 1,Int 2))))
	(Primitive TInt);
assert (App(Abs("x",Let("x",Abs("x",Fix("x", Var"x")),Let("y",App(Var"x",Int 5),If(Var"y",Int 1,Int 2)))),Int 0))
	(Primitive TInt);

assertRaises (Abs("x",If(Var"x",App(Prim Neg,Var"x"),Int 0)));

(* ==== Exception Tests ==== *)

(* TExc's are primitives so they can be passed around as such *)
assert (Letex ("err", App (Abs ("x", Var "x"), Var "err"))) (Primitive TExc);

(* Calling Raise on a TExc produces mtype "Exception" which unifies with anything *)
assert (Letex("err", If (Bool true, Raise (Var "err"), Int 1))) (Primitive TInt);

(* A TExc is NOT an Exception type so if you forget to Raise it,
	it will not unify with other primitives *)
assertRaises (Letex("err", If (Bool true, Var "err", Int 1)));

(* An exception must be defined before it can be raised *)
assertRaises (Letex("err1", Raise (Var "err2")));

(* In Handle, the pattern (e1) can be any expr,
	so long as it has type TExc *)
assert (
	Letex("divzero",
	Letex("indexerror",
	Handle(
		If (Bool true, Var "divzero", Var "indexerror"),
		Raise (Var "divzero"),
		Int 1
	))))
	(Primitive TInt);

(* In Raise, the param can be any expr, so long as it has type TExc *)
assert (
	Letex("divzero",
	Letex("indexerror",
	Handle(
		Var "divzero",
		Raise (If (Bool true, Var "divzero", Var "indexerror")),
		Int 1
	))))
	(Primitive TInt);


(* Giving the wrong type as an argument *)
print(run (App(Prim Add, Bool true))); (* Add #t : TypeError(Type mismatch) *)

(* Applying a function where a primitive type is expected *)
print(run (App(Prim Add, Prim Add))); (* Add Add : TypeError(Kind mismatch) *)

(* Trying to apply a term to itself will result in a
	positive occurs check*)
print(run (Abs("x", App(Var"x", Var"x")))); (* (Lx.x x) : TypeError(Z45 strict subterm of ('Z45->'Z46)) *)

(* Free variables will raise a reference error because they have
	no type info in the environment *)
print(run (Var "x")); (* x : ReferenceError(x undefined) *)


(* Multiple invocations of a let binding (Apply id to itself) *)
print(run (
	Let (
		"id",
		Abs("x", Var("x")),
		App(Var("id"), Var("id"))
	)
)); (* <id, (Lx.x)> id id : ('Z41->'Z41) *)

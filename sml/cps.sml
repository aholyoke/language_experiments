

(*
fun prodPrimes n = 
	if n = 1 then 1 
	else if isPrime n then n * prodPrimes (n - 1)
	else prodPrimes (n - 1)
*)

(*
fun dividable n d =
	if d = 1 then false
	else if (n mod d) = 0 then true
	else dividable n (d - 1)
*)


(*  divides : int -> int -> (bool -> 'a) -> 'a *)
fun divides n d k = k ((n mod d) = 0)

(* int -> int -> (bool -> 'a) -> 'a *)
fun dividable n d k1 =
	(* k1 : bool -> 'b *)
	if d = 1 then k1 false
	else let
		fun k2 b =
			if b then k1 true
			else let
				(* m : bool *)
				(* k3 : bool -> 'b *)
				fun k3 m = k1 m
				(* p : int *)
				val p = d - 1
			in
				(* int -> int -> (bool -> 'b) -> 'a *)
				dividable n p k3
			end
	in
		(* n : int *)
		(* d : int *)
		(* k2 : (bool -> 'a) *)
		divides n d k2
	end

(* isPrime : int -> (bool -> 'a) -> 'a *)
fun isPrime (n, k1) =

	if n = 2 then k1 true
	else let
		(* bool -> bool *)
		fun k2 b = k1 b
		(* p : int *)
		val p = (Real.floor (Math.sqrt (Real.fromInt n))) + 1 
	in
		not (dividable n p k2)
	end

(*
fun showBool true = "T"
|	showBool false = "F"


val x = isPrime (5, showBool)


fun prodPrimes (n, k1) = 
	if n = 1 then k1 1
	else let
		fun k2 b =
			if b then
				let
					fun k3 m = let
						val n2 = n * m
					in
						k1 n2
					end
					val p = n - 1
				in
					prodPrimes (p, k3)
				end
			else
				let
					fun k4 m = k1 m
					val p = n - 1
				in
					prodPrimes (p, k4)
				end

	in
		isPrime (n, k2)
	end
*)
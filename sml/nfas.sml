(*
Playing around with functors
*)
signature DFASIG = sig
	eqtype Q
	eqtype Sigma
	val init : Q
	val delta : Q * Sigma -> Q
	val accepting : Q list
end

functor DFA (Spec:DFASIG):sig
	(* Given the spec for a DFA, return an object that can run the DFA on some input *)
	val run : Spec.Sigma list -> bool
end
= struct
	fun run l =
		let
			fun run' s [] = s
			|	run' s (x::xs) = run' (Spec.delta (s, x)) xs
			val final = run' Spec.init l
			fun member _ [] = false
			|	member x (y::ys) = x = y orelse member x ys
		in
			member final Spec.accepting
		end
end

structure Even0sOdd1s = struct
	exception Bad
	type Sigma = int
	datatype QAux = ODD | EVEN
	type Q = QAux * QAux
	val init = (EVEN, EVEN)
	val accepting = [(EVEN, ODD)]
	fun toggle ODD = EVEN
	|	toggle _ = ODD

	fun delta ((x, y), 0) = (toggle x, y)
	|	delta ((x, y), 1) = (x, toggle y)
	|	delta _ = raise Bad
end

structure Even0sOdd1sInterp = DFA(Even0sOdd1s)

signature NFASIG = sig
	eqtype Q
	eqtype Sigma
	val init : Q list
	val delta : Q * Sigma -> Q list
	val accepting : Q list
end

functor NFA (Spec:NFASIG) : sig
	val run : Spec.Sigma list -> bool
end
= struct
	fun run l =
		let
			(* Q list -> Sigma -> Q list *)
			fun move [] _ = []
			|	move (s::ss) x = Spec.delta(s, x) @ move ss x

			(* Q list -> Sigma list -> Q list *)
			fun run' s [] = s
			|	run' s (x::xs) = run' (move s x) xs

			(* Q list *)
			val final = run' Spec.init l

			(* Q -> Q list -> bool *)
			fun member' _ [] = false
			|	member' x (y::ys) = x = y orelse member' x ys

			(* Q list -> Q List -> bool*)
			fun member [] acc = false
			|	member (x::xs) acc = member' x acc orelse member xs acc
		in
			member final Spec.accepting
		end
end

functor DFAtoNFA (Spec:DFASIG) : NFASIG =
	struct
		type Q = Spec.Q
		type Sigma = Spec.Sigma
		val init = [Spec.init]
		fun delta (q, s) = [Spec.delta(q, s)]
		val accepting = Spec.accepting
	end

structure pNFA = DFAtoNFA(Even0sOdd1s)

structure pNFAInterp = NFA(pNFA)

functor Intersection(structure Spec1:DFASIG and Spec2:DFASIG
					 sharing type Spec1.Sigma = Spec2.Sigma):DFASIG = struct
	(* Given 2 DFA specs, return a DFA spec that recognizes the intersection of both languages *)
	type Sigma = Spec1.Sigma
	type Q = Spec1.Q * Spec2.Q
	val init = (Spec1.init, Spec2.init)
	fun delta ((q1, q2), s) = (Spec1.delta (q1, s), Spec2.delta (q2, s))
	val accepting = let
		fun cartesian' x [] = []
		|	cartesian' x (y::ys) = (x, y)::cartesian' x ys

		fun cartesian x [] = []
		|	cartesian [] y = []
		|	cartesian (x::xs) ys = (cartesian' x ys) @ (cartesian xs ys)
	in
		cartesian Spec1.accepting Spec2.accepting
	end
end


structure BeginEqualsEnd = struct
	type Sigma = int
	datatype Q = A | B | C | D | E
	val init = A
	val accepting = [B, D]
	fun delta (A, 1) = B
	|	delta (A, 0) = D
	|	delta (B, 1) = B
	|	delta (B, 0) = C
	|	delta (C, 1) = B
	|	delta (C, 0) = C
	|	delta (D, 1) = E
	|	delta (D, 0) = D
	|	delta (E, 1) = E
	|	delta (E, 0) = D
end


structure testCombined = DFA(Intersection(structure Spec1 = BeginEqualsEnd; structure Spec2 = Even0sOdd1s))

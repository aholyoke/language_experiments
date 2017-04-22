(*
Lazy stream implementation using thunking
*)

signature THUNK = sig
	type 'a promise
	val force: 'a promise -> 'a
	val delay: (unit -> 'a) -> 'a promise
end

structure Thunk : THUNK = struct
	type 'a promise = unit -> 'a
	fun force p = p ()
	fun delay p = p
end


functor DefineStream (t : THUNK) : sig
	(* Given a thunking implementation, return a stream implementation *)
	type 'a stream
	val from: ('a * ('a -> 'a)) -> 'a stream
	val cons : 'a * 'a stream t.promise -> 'a stream
	val force: 'a stream -> 'a * 'a stream
	val car: 'a stream -> 'a
	val cdr: 'a stream -> 'a stream
end = struct
    datatype 'a stream = Cons of ('a * 'a stream) t.promise
    fun from (current, succ) = Cons (t.delay(fn () => (current, from (succ current, succ))))
	fun cons (current, rest) = Cons (t.delay(fn () => (current, t.force rest)))
    fun force (Cons p) = t.force p
    fun car p = case (force p) of (a, s) => a
    fun cdr p = case (force p) of (a, s) => s
end

structure Stream = DefineStream(Thunk)

fun lazymap f s = Stream.cons(f (Stream.car s), fn () => lazymap f (Stream.cdr s))

val nats = Stream.from (1, fn x => 1 + x)

fun take 0 s = []
|	take n s = (Stream.car s)::(take (n - 1) (Stream.cdr s))

val byTwos = lazymap (fn x => x * 2) nats

val t0 = take 4 nats
val t1 = take 5 byTwos

structure MaybeMonad :
          sig
            datatype 'a maybe = Some of 'a | None
            val >>= : 'a maybe -> ('a -> 'b maybe) -> 'b maybe
            val return : 'a -> 'a maybe
          end
  =
struct
  datatype 'a maybe = Some of 'a | None

  fun >>= (Some a) f = f a
    | >>= None _ = None

  fun return a = Some a
end

open MaybeMonad
infix >>=

structure StateMonad
          (* : *)
          (* sig *)
          (* end *)
  =
struct
  datatype ('t, 'tstate) state = STATE of 'tstate -> 't * 'tstate
  val get = STATE (fn ts => (ts, ts))

  fun putState (STATE state) ts =
      STATE
      (fn ts' =>
          let
            val (t, _) = state ts'
          in
            (t, ts)
          end)
  fun putValue (STATE state) t =
      STATE
      (fn ts =>
          let
            val (_, newTs) = state ts
          in
            (t, newTs)
          end)

  fun calcState (STATE state) f =
      STATE
      (fn ts =>
          let
            val (t, newTs) = state ts
          in
            (t, f newTs)
          end)
  fun calcValue (STATE state) f =
      STATE
      (fn ts =>
          let
            val (t, newTs) = state ts
          in
            (f t, newTs)
          end)

  (* type 'value 'state state = 'value * 'state *)
  (* fun get (value, state) = value *)
  (* fun calc (value, state) f = (f value, state) *)
  (* fun put (value, state) f = (value, f state) *)
end

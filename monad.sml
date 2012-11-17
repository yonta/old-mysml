structure Monad :>
          sig
            type 'a maybe
            val >>= : 'a maybe -> ('a -> 'b maybe) -> 'b maybe
            val return : 'a -> 'a maybe
          end
  =
struct
  datatype 'a maybe = Some of 'a | None

  fun >>= (Some a) f = f a
    | >>= None _ = None
  infix >>=

  fun return a = Some a
end
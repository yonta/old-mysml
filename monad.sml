structure Monad :
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

open Monad
infix >>=

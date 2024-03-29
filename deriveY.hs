import Data.Function (fix)  -- lazy, fixed-point combinator
import Unsafe.Coerce

-- Y Combinator (unsafeCoerce is needed when passing a function
--      to itself. This is because Haskell is strictly typed)
yComb =
    \fn ->
        (\f -> fn (unsafeCoerce f f))
        (\f -> fn (unsafeCoerce f f))

almostFactorial =
    \f n ->
        if n == 0
        then 1
        else n * (f (n - 1))


-- Fully derived, but needs unsafeCoerce
factorial = yComb almostFactorial

-- Using Haskell's fixed-point combinator (fix)
-- No need for unsafeCoerce
factorial' = fix almostFactorial

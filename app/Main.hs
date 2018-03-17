module Main where

import Prelude (($), (.), (==), Show, Eq, show, print, IO, Bool(..))
import qualified Prelude


data Natural
  = Zero
  | Succ Natural


instance Show Natural where
  show x = Prelude.concat ["[", show $ go x 0, "]"]
    where
      go Zero acc = acc
      go (Succ a) acc = go a (acc Prelude.+ 1)


instance Eq Natural where
  Zero == Zero = True
  Zero == (Succ _) = False
  (Succ _) == Zero = False
  (Succ a) == (Succ b) = a == b


one   = Succ Zero
two   = Succ one
three = Succ two
four  = Succ three
five  = Succ four
six   = Succ five
seven = Succ six


infixl 3 +
Zero + x = x
(Succ a) + b = Succ (a + b)

infixl 4 *
Zero * x = Zero
(Succ a) * b = a * b + b


main :: IO ()
main = do
  print $ two + five
  print $ two * five
  print $ two == two
  print $ two == three
  print $ Zero == Zero
  print $ Zero == one

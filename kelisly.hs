import Data.Monoid (Sum(..))
import Control.Monad.Writer (Writer)

writerIsomorphism :: Isomorphism (Kleisli (Writer (Sum Integer))) () ()
writerIsomorphism = Isomorphism{ forward, backward }
  where
    forward :: Kleisli (Writer (Sum Integer)) () ()
    forward = Kleisli (\_ -> tell (Sum 1))

    backward :: Kleisli (Writer (Sum Integer)) () ()
    backward = Kleisli (\_ -> tell (Sum (-1)))

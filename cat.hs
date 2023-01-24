import Control.Category (Category(..))
import Control.Monad ((<=<))

-- Note: This type and instance already exists in the `Control.Arrow` module
newtype Kleisli m a b = Kleisli{ runKleisli :: a -> m b }

instance Monad m => Category (Kleisli m) where
    id = Kleisli return

    Kleisli f . Kleisli g = Kleisli (f <=< g)

import Prelude hiding ((.), id)

-- Note: This is not how the lens package works, but it's still a useful example
data Lens a b = Lens{ view :: a -> b, over :: (b -> b) -> (a -> a) }

instance Category Lens where
    id = Lens{ view = id, over = id }

    Lens{ view = viewL, over = overL } . Lens{ view = viewR, over = overR } =
        Lens{ view = viewL . viewR, over = overR . overL }

lensIsomorphism :: Isomorphism Lens Bool Bool
lensIsomorphism = Isomorphism{ forward, backward }
  where
    forward :: Lens Bool Bool
    forward = Lens{ view = not, over = \f -> not . f . not }

    -- There is no rule that the two morphisms can't be the same
    backward :: Lens Bool Bool
    backward = forward

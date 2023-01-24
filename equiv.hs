{-# LANGUAGE NamedFieldPuns #-}

import Control.Lens (AnIso', Iso', cloneIso, iso, review, view)

data Isomorphism a b = Isomorphism
    { forward  :: a -> b
    , backward :: b -> a
    }

-- | We have to use `AnIso'` here instead of `Iso'` for reasons I won't go into
isomorphismIsomorphism :: Isomorphism (Isomorphism a b) (AnIso' a b)
isomorphismIsomorphism = Isomorphism{ forward, backward }
  where
    forward :: Isomorphism a b -> AnIso' a b
    forward (Isomorphism f b) = iso f b

    backward :: AnIso' a b -> Isomorphism a b
    backward iso =
        Isomorphism
            { forward  = view (cloneIso iso)
            , backward = review (cloneIso iso)
            }

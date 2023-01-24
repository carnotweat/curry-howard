import Control.Lens (Iso', iso)

exampleIso :: Iso' ((a, b) -> c) (a -> b -> c)
exampleIso = iso curry uncurry

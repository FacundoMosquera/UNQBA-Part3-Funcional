type Miles = Float 

newtype Distance = MkDistance Miles

instance Eq Distance where
    (MkDistance m1) == (MkDistance m2) = let distanceApart = abs (m1 - m2)
                                            in distanceApart < 10

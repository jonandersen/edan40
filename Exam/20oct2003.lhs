1

> applyPlusOne :: (Num b) => (a -> b) -> a -> b
> applyPlusOne f = (+) 1 . f

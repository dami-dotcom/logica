{--
x      y      f x y
True   True   True
True   False  False
False  True   True
False  False  True
--}
f :: Bool -> Bool -> Bool
f True True = True
f True False = False
f False True = True
f False False = True

{-- 
Aplico forma normal conjuntiva.
Me quedo con el caso False.
Niego las variables que tienen True, las uno con and

--}
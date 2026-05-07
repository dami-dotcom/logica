import Prelude(Show)
 
data Bool = True | False
    deriving Show
 
not:: Bool -> Bool
not False = True
not True = False
 
conec:: Bool -> Bool -> Bool
conec False y = False
conec x y = not y
 
conec2 :: Bool -> Bool -> Bool
conec2 x y = not y
 
terna:: Bool -> Bool -> Bool -> Bool
terna True False True = False
terna False True False = False
terna False False True = False
terna x y z = True
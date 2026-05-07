andor :: Bool -> Bool -> Bool -> Bool
andor _ _ True = True
andor True True _ = True
andor _ _ _ = False

unoodos :: Bool -> Bool -> Bool -> Bool
unoodos True True True = False
unoodos False False False = False
unoodos _ _ _ = True
-- Some tests for case expressions

swap l = case l of
  [x,y] -> [y,x]
  _     -> l

-- swap []      -> []
-- swap [1]     -> [1]
-- swap [1,2]   -> [2,1]
-- swap [1,2,3] -> [1,2,3]


f l = case l of
        []   -> 0
        _:xs -> 100 + case xs of
                          [y] -> y
                      + 50

-- f []    -> 0
-- f [1]   -> undefined
-- f [1,2] -> 152

g x = case x of
        Just "abc" -> True
        Just "xyz" -> True
        _          -> False

-- g (Just "xyz") -> True
-- g (Just "ab")  -> False

h x = case x of
        [1,2] -> True
        _     -> False

-- h [1,2] -> True
-- h [1,3] -> False
-- h [2,div 1 0] -> False


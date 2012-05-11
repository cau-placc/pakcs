-- Pretty printing library
-- Mostly identical with Wadler's pretty printer.
-- With extensions, some based on Andy Gill's pretty printer in Hood.

module CoosyPrettier (pretty,prettyFlat,DOC,nil,(<>),nest,text,
                      line,sep,break,group,
                     (<+>),(</>),folddoc,spread,stack,bracket,(<+/>)) where

import Maybe

infixr 6 <>,<+>,</>,<+/>

data DOC = NIL          -- nil 
         | BESIDE DOC DOC  -- beside
         | NEST Int DOC
         | TEXT String
         | LINE         -- always "\n"
         | SEP          -- " " or "\n"
         | BREAK        -- ""  or "\n"
         | CHOOSE DOC DOC -- choose one

data Doc = Nil
         | Text     Int String Doc
         | Line     Int Int Doc

mkText :: String -> Doc -> Doc
mkText s d = Text (toplen d + length s) s d

mkLine :: Int -> Doc -> Doc
mkLine i d = Line (toplen d + i) i d

toplen :: Doc -> Int
toplen Nil = 0
toplen (Text w _ _) = w
toplen (Line _ _ _) = 0

nil = NIL
x <> y = BESIDE x y
nest i x = NEST i x
text s = TEXT s
line = LINE
sep = SEP
break = BREAK

x <+> y = x <> text " " <> y
x </> y = x <> sep <> y

folddoc :: (DOC -> DOC -> DOC) -> [DOC] -> DOC
folddoc f = foldr f nil
-- folddoc f [] = nil
-- folddoc f [x] = x
-- folddoc f (x:xs) = f x (folddoc f xs)

spread = folddoc (<+>)
stack = folddoc (</>)

bracket l x r =
  group (text l <> nest 2 (sep <> x) <> sep <> text r)

x <+/> y = x <> (CHOOSE (text " ") sep) <> y

{-
fill :: [DOC] -> DOC
fill [] = nil
fill [x] = x
fill (x:y:zs) = 
  CHOOSE (flatten x <+> fill (flatten y : zs))               
    (x </> fill (y : zs))
-}

fold x = group (break <> x)

group :: DOC -> DOC
group x = case flatten x of
  Just x' -> CHOOSE x' x
  Nothing -> x

flatten :: DOC -> Maybe DOC
flatten NIL = Just NIL
flatten (BESIDE x y)= 
   flatten x >>- \x' ->
   flatten y >>- \y' ->
   Just (BESIDE x' y')
flatten (NEST i x) =
   flatten x >>- \x' ->
   Just (NEST i x')
flatten (TEXT s) = Just (TEXT s)
flatten LINE = Nothing           -- abort
flatten SEP = Just (TEXT " ")  -- SEP is space
flatten BREAK = Just NIL       -- BREAK is nil
flatten (CHOOSE x _) = flatten x

layout :: Doc -> String
layout Nil = ""
layout (Text _ s x) = s ++ layout x
layout (Line _ i x) = '\n' : replicate i ' ' ++ layout x

best w k doc = be w k [(0,doc)]

be :: Int -> Int -> [(Int,DOC)] -> Doc
be _ _ []= Nil
be w k ((_,NIL):z)= be w k z
be w k ((i,BESIDE x y):z)= be w k ((i,x):(i,y):z)
be w k ((_,NEST j x):z) = be w k ((k+j,x):z)
be w k ((_,TEXT s):z)= mkText s (be w (k+length s) z)
be w _ ((i,LINE):z)= mkLine i (be w i z)
be w _ ((i,SEP):z)= mkLine i (be w i z)
be w _ ((i,BREAK):z)= mkLine i (be w i z)
be w k ((i,CHOOSE x y):z) = better w k (be w k ((i,x):z)) (be w k ((i,y):z))

better :: Int -> Int -> Doc -> Doc -> Doc
better w k x y = if (w-k) >= toplen x then x else y

pretty :: Int -> DOC -> String
pretty w x = layout (best w 0 x)


-- this version never makes optional line breaks in groups but puts
-- everything into a single line except where line breaks are forced.
prettyFlat :: DOC -> String
prettyFlat d = go 0 d ""
  
go :: Int -> DOC -> String -> String
go _ NIL = id
go i (BESIDE d1 d2) = go i d1 . go i d2
go i (NEST n d1) = go (i+n) d1
go _ (TEXT s) = (s++)
go i LINE = ('\n':) . rep i ' '
go _ SEP = (' ':)
go _ BREAK = id
go i (CHOOSE d1 _) = go i d1

rep :: Int -> a -> [a] -> [a]
rep n x xs = if n <=0 then xs else x : rep (n-1) x xs

------------------------------------------------------------------------------
--- This library provides pretty printing combinators.
--- The interface is that of 
--- [Daan Leijen's library](<http://www.cs.uu.nl/~daan/download/pprint/pprint.html)
--- (`fill`, `fillBreak` and `indent` are missing) with a
--- [linear-time, bounded implementation](http://www.cs.kent.ac.uk/pubs/2006/2381/index.html)
---  by Olaf Chitil.
---
--- @author Sebastian Fischer
--- @version June 2011
------------------------------------------------------------------------------

module Pretty (

  -- pretty printer and document type
  pretty, Doc, 

  -- basic document combinators
  empty, isEmpty, text, linesep, line, linebreak, group, softline, softbreak,

  -- alignment combinators
  nest, hang, align, --indent??,

  -- composition combinators
  combine, (<>), (<+>), (<$>), (</>), (<$$>), (<//>),

  -- list combinators
  compose, hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat, 
  punctuate, encloseSep, hEncloseSep, fillEncloseSep, list, tupled, semiBraces,

  -- bracketing combinators
  enclose, squotes, dquotes, bquotes, parens, angles, braces, brackets,

  -- primitve type documents
  char, string, int, float,

  -- character documents
  lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket,
  squote, dquote, semi, colon, comma, space, dot, backslash, equals

  ) where

import Dequeue as Q

infixl 1 <>, <+>, <$>, </>, <$$>, <//>

--- The abstract data type Doc represents pretty documents.
data Doc = Doc (Tokens -> Tokens)

deDoc (Doc d) = d

--- The empty document is, indeed, empty. Although empty has no content,
--- it does have a 'height' of 1 and behaves exactly like `(text "")` 
--- (and is therefore not a unit of `&lt;$&gt;`).
--- @return an empty document
empty :: Doc
empty = text ""

--- Is the document empty?
isEmpty :: Doc -> Bool
isEmpty (Doc d) = d Empty == Text "" Empty

--- The document `(text s)` contains the literal string `s`. 
--- The string shouldn't contain any newline ('\n') characters. 
--- If the string contains newline characters, 
--- the function `string` should be used.
--- @param s - a string without newline (`'\n'`) characters
--- @return a document which contains the literal string
text :: String -> Doc
text s = Doc (Text s)

--- The document `(linesep s)` advances to the next line and indents
--- to the current nesting level. Document `(linesep s)`
--- behaves like `(text s)` if the line break is undone by group.
--- @param s - a string
--- @return a document which advances to the next line or behaves like `(text s)`
linesep :: String -> Doc
linesep = Doc . Line

--- The line document advances to the next line and indents to the current
--- nesting level. Document line behaves like `(text " ")` if the line break 
--- is undone by group.
--- @return a document which advances to the next line or behaves
---         like `(text " ")`
line :: Doc
line = linesep " "

--- The linebreak document advances to the next line and indents to 
--- the current nesting level. Document linebreak behaves like empty 
--- if the line break is undone by group.
--- @return a document which advances to the next line or behaves like
---         `(text "")`
linebreak :: Doc
linebreak = linesep ""

--- The document softline behaves like `space` if the resulting output 
--- fits the page, otherwise it behaves like `line`.<br><br>
--- `softline  = group line`
--- @return a document which behaves like `space` or `line`
softline :: Doc
softline = group line

--- The document softbreak behaves like `empty` if the resulting output 
--- fits the page, otherwise it behaves like `line`.<br><br>
--- `softbreak  = group linebreak`
--- @return a document which behaves like `empty` or `line`
softbreak :: Doc
softbreak = group linebreak

--- The group combinator is used to specify alternative layouts. 
--- The document `(group x)` undoes all line breaks in document `x`. 
--- The resulting line is added to the current line if that fits the page. 
--- Otherwise, the document x is rendered without any changes.
--- @param d - a document
--- @return document d without line breaks if that fits the page.
group :: Doc -> Doc
group d = Doc (Open . deDoc d . Close)

--- The document `(nest i d)` renders document `d` with the current 
--- indentation level increased by `i` (See also `hang`, 
--- `align` and `indent`).
---
---     nest 2 (text "hello" <$> text "world") <$> text "!"
---
--- outputs as:
---
---     hello
---       world
---     !
---
--- @param i - an integer which increases the indentation level
--- @param d - a document
--- @return document d with an indentation level increased by i
nest :: Int -> Doc -> Doc
nest i d = Doc (OpenNest (\ms@(m:_) _ _ -> (m+i):ms) . deDoc d . CloseNest)

--- The hang combinator implements hanging indentation. 
--- The document `(hang i d)` renders document `d` with a nesting level set 
--- to the current column plus `i`. The following example uses hanging 
--- indentation for some text:
---
---     test = hang 4
---              (fillSep
---                 (map text 
---                      (words "the hang combinator indents these words !")))
---
--- Which lays out on a page with a width of 20 characters as:
---
---     the hang combinator
---         indents these
---         words !
---
--- The hang combinator is implemented as:
---
---     hang i x  = align (nest i x)
---
--- @param i - an integer which increases the indentation level
--- @param d - a document
--- @return document d with an indentation level set to the current column plus i
hang :: Int -> Doc -> Doc
hang i d = Doc (OpenNest (\ms r w -> (w-r+i):ms) . deDoc d . CloseNest)

--- The document `(align d)` renders document `d with the nesting level 
--- set to the current column. It is used for example to implement hang.
---
--- As an example, we will put a document right above another one, 
--- regardless of the current nesting level:
---
---     x $$ y  = align (x <$> y)
---     test    = text "hi" <+> (text "nice" $$ text "world")
---
--- which will be layed out as:
---
---     hi nice
---        world
---
--- @param d - a document
--- @return document d with the nesting level set to the current column
align :: Doc -> Doc
align = hang 0

--- The document `(combine x l r)` encloses document `x` between 
--- documents `l` and `r` using `(&lt;&gt;)`.
---
---     combine x l r   = l <> x <> r
---
--- @param x - the middle document
--- @param l - the left document
--- @param r - the right document
--- @return concatenation of l, x and r
combine :: Doc -> Doc -> Doc -> Doc
combine s d1 d2 = enclose d1 d2 s

--- The document `(x &lt;&gt; y)` concatenates document `x` and document `y`. 
--- It is an associative operation having empty as a left and right unit.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y without seperator
(<>) :: Doc -> Doc -> Doc
d1 <> d2 = Doc (deDoc d1 . deDoc d2)

--- The document `(x &lt;+&gt; y)` concatenates document `x` and `y` with a 
--- `space` in between.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a `space` in between
(<+>) :: Doc -> Doc -> Doc
(<+>) = combine space

--- The document `(x &lt;$&gt; y)` concatenates document x and y with a 
--- `line` in between.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a `line` in between
(<$>) :: Doc -> Doc -> Doc
(<$>) = combine line

--- The document (x &lt;/&gt; y) concatenates document x and y with 
--- a `softline` in between. This effectively puts x and y either 
--- next to each other (with a `space` in between) 
--- or underneath each other.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a `softline` in between
(</>) :: Doc -> Doc -> Doc
(</>) = combine softline

--- The document (x &lt;$$&gt; y) concatenates document x and y with a 
--- `linebreak` in between.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a `linebreak` in between
(<$$>) :: Doc -> Doc -> Doc
(<$$>) = combine linebreak

--- The document (x &lt;//&gt; y) concatenates document x and y with a 
--- `softbreak` in between. This effectively puts x and y either 
--- right next to each other or underneath each other.
--- @param x - the first document
--- @param y - the second document
--- @return concatenation of x and y with a `softbreak` in between
(<//>) :: Doc -> Doc -> Doc
(<//>) = combine softbreak

--- The document (compose f xs) concatenates all documents xs with function f.
--- Function f should be like `(&lt;+&gt;)`, `(&lt;$&gt;)` and so on.
--- @param f - a combiner function
--- @param xs - a list of documents
--- @return concatenation of documents
compose :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
--compose op = foldr op empty
compose _ [] = empty
compose op ds@(_:_) = foldr1 op ds -- no seperator at the end

--- The document (hsep xs) concatenates all documents xs 
--- horizontally with `(&lt;+&gt;)`.
--- @param xs - a list of documents
--- @return horizontal concatenation of documents
hsep :: [Doc] -> Doc
hsep = compose (<+>)

--- The document `(vsep xs)` concatenates all documents `xs` vertically with 
--- `(&lt;$&gt;)`. If a group undoes the line breaks inserted by `vsep`, 
--- all documents are separated with a `space`.
---
---     someText = map text (words ("text to lay out"))
---     test     = text "some" <+> vsep someText
---
--- This is layed out as:
---
---     some text
---     to
---     lay
---     out
---
--- The `align` combinator can be used to align the documents
--- under their first element:
---
---     test     = text "some" <+> align (vsep someText)
---
--- This is printed as:
---
---     some text
---          to
---          lay
---          out
---
--- @param xs - a list of documents
--- @return vertical concatenation of documents
vsep :: [Doc] -> Doc
vsep = compose (<$>)

--- The document (fillSep xs) concatenates documents xs horizontally with 
--- `(&lt;+&gt;)` as long as its fits the page, than inserts a
--- `line` and continues doing that for all documents in xs.<br><br>
--- `fillSep xs  = foldr (&lt;/&gt;) empty xs`
--- @param xs - a list of documents
--- @return horizontal concatenation of documents
fillSep :: [Doc] -> Doc
fillSep = compose (</>)

--- The document (sep xs) concatenates all documents xs either horizontally 
--- with `(&lt;+&gt;)`, if it fits the page, or vertically 
--- with `(&lt;$&gt;)`.<br><br>
--- `sep xs  = group (vsep xs)`
--- @param xs - a list of documents
--- @return horizontal concatenation of documents, if it fits the page, 
--- or vertical concatenation else
sep :: [Doc] -> Doc
sep = group . vsep

--- The document (hcat xs) concatenates all documents xs horizontally 
--- with `(&lt;&gt;)`.
--- @param xs - a list of documents
--- @return horizontal concatenation of documents
hcat :: [Doc] -> Doc
hcat = compose (<>)

--- The document (vcat xs) concatenates all documents xs vertically 
--- with `(&lt;$$&gt;)`. If a `group` undoes the line 
--- breaks inserted by `vcat`, all documents are directly 
--- concatenated.
--- @param xs - a list of documents
--- @return vertical concatenation of documents
vcat :: [Doc] -> Doc
vcat = compose (<$$>)

--- The document (fillCat xs) concatenates documents xs horizontally 
--- with `(&lt;&gt;)` as long as its fits the page, than inserts 
--- a `linebreak` and continues doing that for all documents in xs.
--- <br><br>
--- `fillCat xs  = foldr (&lt;//&gt;) empty xs`
--- @param xs - a list of documents
--- @return horizontal concatenation of documents
fillCat :: [Doc] -> Doc
fillCat = compose (<//>)

--- The document (cat xs) concatenates all documents xs either horizontally 
--- with `(&lt;&gt;)`, if it fits the page, or vertically with 
--- `(&lt;$$&gt;)`.<br><br>
--- `cat xs  = group (vcat xs)`
--- @param xs - a list of documents
--- @return horizontal concatenation of documents
cat :: [Doc] -> Doc
cat = group . vcat


--- `(punctuate p xs)` concatenates all documents `xs` with document `p` except 
--- for the last document.
---
---     someText = map text ["words","in","a","tuple"]
---     test     = parens (align (cat (punctuate comma someText)))
--- 
--- This is layed out on a page width of 20 as:
---
---     (words,in,a,tuple)
---
--- But when the page width is 15, it is layed out as:
---
---     (words,
---      in,
---      a,
---      tuple)
---
--- (If you want put the commas in front of their elements instead of at the
--- end, you should use `tupled` or, in general, 
--- `encloseSep`.)
--- @param p - a document as seperator
--- @param xs - a list of documents
--- @return concatenation of documents with p in between
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate d ds@(_:_) = go ds
 where
  go [x] = [x]
  go (x:xs@(_:_)) = (x <> d) : go xs

--- The document (encloseSep l r sep xs) concatenates the documents xs 
--- seperated by sep and encloses the resulting document by l and r.<br>
--- The documents are rendered horizontally if that fits the page. Otherwise 
--- they are aligned vertically. All seperators are put in front of the 
--- elements.
---
--- For example, the combinator `list` can be defined with `encloseSep`:
---
---     list xs  = encloseSep lbracket rbracket comma xs
---     test     = text "list" <+> (list (map int [10,200,3000]))
---
--- Which is layed out with a page width of 20 as:
---
---     list [10,200,3000]
---
--- But when the page width is 15, it is layed out as:
---
---     list [10
---          ,200
---          ,3000]
---
--- @param l - left document
--- @param r - right document
--- @param sep - a document as seperator
--- @param xs - a list of documents
--- @return concatenation of l, xs (with p in between) and r
encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep l r _ [] = l <> r
encloseSep l r s (d:ds) = align (enclose l r (cat (d:map (s<>) ds)))

--- The document `(hEncloseSep l r sep xs)` concatenates the documents `xs`
--- seperated by `sep` and encloses the resulting document by `l` and `r`.
---
--- The documents are rendered horizontally.
--- @param l - left document
--- @param r - right document
--- @param sep - a document as seperator
--- @param xs - a list of documents
--- @return concatenation of l, xs (with p in between) and r
hEncloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
hEncloseSep l r _ [] = l <> r
hEncloseSep l r s (d:ds) = align (enclose l r (hcat (d:map (s<>) ds)))

--- The document `(hEncloseSep l r sep xs)` concatenates the documents `xs`
--- seperated by `sep` and encloses the resulting document by `l` and `r`.
---
--- The documents are rendered horizontally if that fits the page. 
--- Otherwise they are aligned vertically. 
--- All seperators are put in front of the elements.
--- @param l - left document
--- @param r - right document
--- @param sep - a document as seperator
--- @param xs - a list of documents
--- @return concatenation of l, xs (with p in between) and r
fillEncloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
fillEncloseSep l r _ [] = l <> r
fillEncloseSep l r s (d:ds)
  = align (enclose l r (hcat (d:withSoftBreaks (map (s<>) ds))))
 where
  withSoftBreaks [] = []
  withSoftBreaks [x] = [group (linebreak <> x)]
  withSoftBreaks (x:xs@(_:_))
    = (group (linebreak <> (group (x <> linebreak))) : withSoftBreaks xs)

--- The document (list xs) comma seperates the documents xs and encloses 
--- them in square brackets. The documents are rendered horizontally if 
--- that fits the page. Otherwise they are aligned vertically. 
--- All comma seperators are put in front of the elements.
--- @param xs - a list of documents
--- @return comma seperated documents xs and enclosed 
--- in square brackets
list :: [Doc] -> Doc
list = fillEncloseSep lbracket rbracket comma

--- The document (tupled xs) comma seperates the documents xs and encloses
--- them in parenthesis. The documents are rendered horizontally if that fits 
--- the page. Otherwise they are aligned vertically. 
--- All comma seperators are put in front of the elements.
--- @param xs - a list of documents
--- @return comma seperated documents xs and enclosed 
--- in parenthesis
tupled :: [Doc] -> Doc
tupled = fillEncloseSep lparen rparen comma

--- The document (semiBraces xs) seperates the documents xs with semi colons
--- and encloses them in braces. The documents are rendered horizontally 
--- if that fits the page. Otherwise they are aligned vertically. 
--- All semi colons are put in front of the elements.
--- @param xs - a list of documents
--- @return documents xs seperated with semi colons and enclosed 
--- in braces
semiBraces :: [Doc] -> Doc
semiBraces = fillEncloseSep lbrace rbrace semi

--- The document (enclose l r x) encloses document x between 
--- documents l and r using (&lt;&gt;).<br><br>
--- `enclose l r x   = l &lt;&gt; x &lt;&gt; r`
--- @param l - the left document
--- @param r - the right document
--- @param x - the middle document
--- @return concatenation of l, x and r
enclose :: Doc -> Doc -> Doc -> Doc
enclose l r d = l <> d <> r

--- Document (squotes x) encloses document x with single quotes `"'"`.
--- @param x - a document
--- @return document x enclosed by single quotes
squotes :: Doc -> Doc
squotes = enclose squote squote

--- Document (dquotes x) encloses document x with double quotes `'"'`.
--- @param x - a document
--- @return document x enclosed by double quotes
dquotes :: Doc -> Doc
dquotes = enclose dquote dquote

--- Document (bquotes x) encloses document x with `'`'` quotes.
--- @param x - a document
--- @return document x enclosed by `'`'` quotes
bquotes  :: Doc -> Doc
bquotes = enclose bquote bquote

--- Document (parens x) encloses document x in parenthesis, 
--- `"("` and `")"`.
--- @param x - a document
--- @return document x enclosed in parenthesis
parens :: Doc -> Doc
parens = enclose lparen rparen

--- Document (angles x) encloses document x in angles, 
--- `"<"` and `">"`.
--- @param x - a document
--- @return document x enclosed in angles
angles :: Doc -> Doc
angles = enclose langle rangle

--- Document (braces x) encloses document x in braces, 
--- `"{"` and `"}"`.
--- @param x - a document
--- @return document x enclosed in braces
braces :: Doc -> Doc
braces = enclose lbrace rbrace

--- Document (brackets x) encloses document x in square brackets, 
--- `"["` and `"]"`.
--- @param x - a document
--- @return document x enclosed in square brackets
brackets :: Doc -> Doc
brackets = enclose lbracket rbracket

--- The document (char c) contains the literal character c. 
--- The character shouldn't be a newline ('\n'), 
--- the function `line` should be used for line breaks.
--- @param c - a character
--- @return a document which contains the literal character c
char :: Char -> Doc
char c = text [c]

--- The document (string s) concatenates all characters in s using
--- `line` for newline characters and `char` for all 
--- other characters. It is used instead of `text` whenever the 
--- text contains newline characters.
--- @param s - a string
--- @return a document which contains the string s
string :: String -> Doc
string = hcat . map (\c -> if elem c ['\n','\r'] then line else char c)

--- The document (int i) shows the literal integer i using `text`.
--- @param i - an integer
--- @return a document which contains the integer i
int :: Int -> Doc
int n = text (show n)

--- The document (float f) shows the literal float f using `text`.
--- @param f - a float
--- @return a document which contains the float f
float :: Float -> Doc
float x = text (show x)

--- The document lparen contains a left parenthesis, `"("`.
--- @return a document which contains a left parenthesis
lparen :: Doc
lparen = char '('

--- The document rparen contains a right parenthesis, `")"`.
--- @return a document which contains a right parenthesis
rparen :: Doc
rparen = char ')'

--- The document langle contains a left angle, `"<"`.
--- @return a document which contains a left angle
langle :: Doc
langle = char '<'

--- The document rangle contains a right angle, `">"`.
--- @return a document which contains a right angle
rangle :: Doc
rangle = char '>'

--- The document lbrace contains a left brace, `"{"`.
--- @return a document which contains a left brace
lbrace :: Doc
lbrace = char '{'

--- The document rbrace contains a right brace, `"}"`.
--- @return a document which contains a right brace
rbrace :: Doc
rbrace = char '}'

--- The document lbracket contains a left square bracket, `"["`.
--- @return a document which contains a left square bracket
lbracket :: Doc
lbracket = char '['

--- The document rbracket contains a right square bracket, `"]"`.
--- @return a document which contains a right square bracket
rbracket :: Doc
rbracket = char ']'

--- The document squote contains a single quote, `"'"`.
--- @return a document which contains a single quote
squote :: Doc
squote = char '\''

--- The document dquote contains a double quote, `'"'`.
--- @return a document which contains a double quote
dquote :: Doc
dquote = char '\"'

--- The document dquote contains a `'`'` quote.
--- @return a document which contains a `'`'` quote
bquote :: Doc
bquote = char '`'

--- The document semi contains a semi colon, `";"`.
--- @return a document which contains a semi colon
semi :: Doc
semi = char ';'

--- The document colon contains a colon, `":"`.
--- @return a document which contains a colon
colon :: Doc
colon = char ':'

--- The document comma contains a comma, `","`.
--- @return a document which contains a comma
comma :: Doc
comma = char ','

--- The document space contains a single space, `" "`.
---
---     x <+> y   = x <> space <> y
---
--- @return a document which contains a single space
space :: Doc
space = char ' '

--- The document dot contains a single dot, `"."`.
--- @return a document which contains a single dot
dot :: Doc
dot = char '.'

--- The document backslash contains a back slash, `"\\"`.
--- @return a document which contains a back slash
backslash :: Doc
backslash = char '\\'

--- The document equals contains an equal sign, `"="`.
--- @return a document which contains an equal
equals :: Doc
equals = char '='


type Layout = String
type Horizontal = Bool
type Remaining = Int
type Width = Int
type Position = Int
type StartPosition = Int
type EndPosition = Int
type Out = Remaining -> Margins -> String
type OutGroupPrefix = Horizontal -> Out -> Out
type Margins = [Int]

data Tokens = Text String Tokens
            | Line String Tokens
            | Open Tokens
            | Close Tokens
            | Empty
            | OpenNest (Margins -> Remaining -> Width -> Margins) Tokens
            | CloseNest Tokens

normalise :: Tokens -> Tokens
normalise = go id
  where
  go co Empty = co Empty
    -- there should be no deferred opening brackets
  go co (Open ts) = go (co . open) ts
  go co (Close ts) = go (co . Close) ts
  go co (Line s ts) = co . Line s . go id $ ts
  go co (Text s ts) = Text s (go co ts)
  go co (OpenNest f ts) = OpenNest f (go co ts)
  go co (CloseNest ts) = CloseNest (go co ts)

  open t = case t of Close ts -> ts; _ -> Open t

doc2Tokens (Doc d) = normalise (d Empty)


--- `(pretty w d)` pretty prints document `d` with a page width of `w` characters
--- @param w - width of page
--- @param d - a document
--- @return pretty printed document
pretty :: Width -> Doc -> String
pretty w d = noGroup (doc2Tokens d) w 1 w [0]


length = Prelude.length . filter (not . (`elem` ([5,6,7]++[16..31])) . ord)

noGroup :: Tokens -> Width -> Position -> Out
noGroup Empty _ _ _ _ = ""
noGroup (Text t ts) w p r ms = t ++ noGroup ts w (p+l) (r-l) ms
  where
  l = length t
noGroup (Line _ ts) w p _ ms@(m:_) = 
  '\n' : replicate m ' ' ++ noGroup ts w (p+1) (w-m) ms
noGroup (Open ts) w p r ms = oneGroup ts w p (p+r) (\_ c -> c) r ms
noGroup (Close ts) w p r ms = noGroup ts w p r ms -- may have been pruned
noGroup (OpenNest f ts) w p r ms = noGroup ts w p r (f ms r w)
noGroup (CloseNest ts) w p r ms = noGroup ts w p r (tail ms)

oneGroup :: Tokens -> Width -> Position -> EndPosition -> OutGroupPrefix -> Out
oneGroup (Text t ts) w p e outGrpPre = 
  pruneOne ts w (p+l) e (\h c -> outGrpPre h (outText c))
  where
  l = length t
  outText c r ms = t ++ c (r-l) ms
oneGroup (Line s ts) w p e outGrpPre = 
  pruneOne ts w (p + lens) e (\h c -> outGrpPre h (outLine h c))
  where
  lens = length s
  outLine h c r ms@(m:_) = 
    if h then s ++ c (r-lens) ms else '\n' : replicate m ' ' ++ c (w-m) ms
oneGroup (Open ts) w p e outGrpPre =
  multiGroup ts w p e outGrpPre Q.empty p (\_ c -> c)
oneGroup (Close ts) w p e outGrpPre = outGrpPre (p<=e) (noGroup ts w p) 
oneGroup (OpenNest f ts) w p e outGrpPre =
  oneGroup ts w p e (\h c -> outGrpPre h (\r ms -> c r (f ms r w)))
oneGroup (CloseNest ts) w p e outGrpPre =
  oneGroup ts w p e (\h c -> outGrpPre h (\r ms -> c r (tail ms)))

multiGroup :: Tokens -> Width -> Position -> EndPosition -> OutGroupPrefix 
              -> Queue (StartPosition,OutGroupPrefix) 
              -> StartPosition -> OutGroupPrefix -> Out
multiGroup (Text t ts) w p e outGrpPreOuter qs s outGrpPreInner =
  pruneMulti ts w (p+l) e outGrpPreOuter qs s 
    (\h c -> outGrpPreInner h (outText c))
  where
  l = length t
  outText c r ms = t ++ c (r-l) ms
multiGroup (Line s ts) w p e outGrpPreOuter qs si outGrpPreInner =
  pruneMulti ts w (p + lens) e outGrpPreOuter qs si 
    (\h c -> outGrpPreInner h (outLine h c))
  where
  lens = length s
  outLine h c r ms@(m:_) = 
    if h then s ++ c (r-lens) ms else '\n': replicate m ' ' ++ c (w-m) ms
multiGroup (Open ts) w p e outGrpPreOuter qs si outGrpPreInner =
  multiGroup ts w p e outGrpPreOuter (cons (si,outGrpPreInner) qs) p (\_ c -> c)
multiGroup (Close ts) w p e outGrpPreOuter qs si outGrpPreInner =
  case matchHead qs of
    Nothing -> oneGroup ts w p e 
                 (\h c -> outGrpPreOuter h 
                            (\ri -> outGrpPreInner (p<=si+ri) c ri))
    Just ((s,outGrpPre),qs') ->
      multiGroup ts w p e outGrpPreOuter qs' s
        (\h c -> outGrpPre h (\ri -> outGrpPreInner (p<=si+ri) c ri))
multiGroup (OpenNest f ts) w p e outGrpPreOuter qs si outGrpPreInner =
  multiGroup ts w p e outGrpPreOuter qs si 
    (\h c -> outGrpPreInner h (\r ms -> c r (f ms r w)))
multiGroup (CloseNest ts) w p e outGrpPreOuter qs si outGrpPreInner =
  multiGroup ts w p e outGrpPreOuter qs si
    (\h c -> outGrpPreInner h (\r ms -> c r (tail ms)))


pruneOne :: Tokens -> Width -> Position -> EndPosition -> OutGroupPrefix -> Out
pruneOne ts w p e outGrpPre = 
  if p <= e then oneGroup ts w p e outGrpPre 
            else outGrpPre False (noGroup ts w p)

pruneMulti :: Tokens -> Width -> Position -> EndPosition -> OutGroupPrefix 
              -> Queue (StartPosition,OutGroupPrefix) 
              -> StartPosition -> OutGroupPrefix -> Out
pruneMulti ts w p e outGrpPreOuter qs si outGrpPreInner =
  if p <= e then multiGroup ts w p e outGrpPreOuter qs si outGrpPreInner
            else outGrpPreOuter False (\r ->
                   (case matchLast qs of
                      Nothing -> pruneOne ts w p (si+r) outGrpPreInner
                      Just ((s,outGrpPre),qs') ->
                        pruneMulti ts w p (s+r) outGrpPre qs' si outGrpPreInner)
                          r)

--- Provides type-based combinators to construct XML converters.
--- Arbitrary XML data can be represented as algebraic datatypes and vice versa.
--- See <a href="http://www-ps.informatik.uni-kiel.de/~sebf/projects/xmlconv/">
--- here</a> for a description of this library.
---
--- @author Sebastian Fischer
--- @version March 2006
module XmlConv (

  -- converter types
  XElemConv, XAttrConv, XPrimConv, XOptConv, XRepConv,

  -- Reads and Shows types for XML trees
  XmlReads, XmlShows,

  -- read and show functions
  xmlRead, xmlShow, xmlReads, xmlShows,

  -- converter for primitive values
  int, float, char, string,

  -- combinators for complex XML data
  (!), element, empty, attr, adapt, opt, rep,

  -- attribute converter for primitive values and booleans
  aInt, aFloat, aChar, aString, aBool,

  -- element converter 
  eInt, eFloat, eChar, eString, eBool, eEmpty, eOpt, eRep,

  -- converter for sequences
  seq1, seq2, seq3, seq4, seq5, seq6,

  -- converter for repeated sequences
  repSeq1, repSeq2, repSeq3, repSeq4, repSeq5, repSeq6,

  -- element converter for sequences
  eSeq1, eSeq2, eSeq3, eSeq4, eSeq5, eSeq6,

  -- element converter for repeated sequences
  eRepSeq1, eRepSeq2, eRepSeq3, eRepSeq4, eRepSeq5, eRepSeq6

  ) where

import XML
import Read ( readInt )
import ReadShowTerm ( readQTerm )

infixr 0 !
infixl 1 />=

--- Phantom type for XML data that may be part of a repetition
data Repeatable

--- Phantom type for XML data that must not be part of a repetition
data NotRepeatable

--- Phantom type for single elements
data Elem

--- Phantom type for primitive values, multiple elements and/or attributes
data NoElem

type Attrs = [(String,String)]
type Childs = (Attrs,[XmlExp])

--- Type of functions that consume some XML data to compute a result
type XmlReads a = Childs -> (a,Childs)

--- Type of functions that extend XML data corresponding to a given value
type XmlShows a = a -> Childs -> Childs

data XmlConv _ _ a = Conv (XmlReads a) (XmlShows a)
type ValConv a = (String -> a,a -> String)

--- Type of converters for XML elements
type XElemConv a = XmlConv Repeatable Elem a

--- Type of converters for attributes
type XAttrConv a = XmlConv NotRepeatable NoElem a

--- Type of converters for primitive values
type XPrimConv a = XmlConv NotRepeatable NoElem a

--- Type of converters for optional values
type XOptConv a = XmlConv NotRepeatable NoElem a

--- Type of converters for repetitions
type XRepConv a = XmlConv NotRepeatable NoElem a

--- Type of converters for sequences
type XSeqConv a = XmlConv NotRepeatable NoElem a

-- Monadic bind for XML parser
(/>=) :: XmlReads a -> (a -> XmlReads b) -> XmlReads b
rd />= f = \childs -> case rd childs of (a,childs') -> f a childs'

-- Monadic return for XML parser
ret :: a -> XmlReads a
ret = (,)

--- Takes an XML converter and returns a function that consumes XML data
--- and returns the remaining data along with the result.
---
--- @param conv XML converter
--- @return XmlReads function
xmlReads :: XmlConv _ _ a -> XmlReads a
xmlReads (Conv rd _) = rd

--- Takes an XML converter and returns a function that extends XML data
--- with the representation of a given value.
---
--- @param conv XML converter
--- @return XmlShows function
xmlShows :: XmlConv _ _ a -> XmlShows a
xmlShows (Conv _ sh) = sh

--- Takes an XML converter and an XML expression and returns a
--- corresponding Curry value.
---
--- @param conv XML converter
--- @return XML read function
xmlRead :: XmlConv _ Elem a -> XmlExp -> a
xmlRead xa x = a
 where
  (a,([],[])) = xmlReads xa ([],[x])

--- Takes an XML converter and a value and returns a corresponding
--- XML expression.
---
--- @param conv XML converter
--- @return XML show function
xmlShow :: XmlConv _ Elem a -> a -> XmlExp
xmlShow xa a = x
 where
  ([],[x]) = xmlShows xa a ([],[])

int_ :: ValConv Int
int_ = (readInt,show)

float_ :: ValConv Float
float_ = (readQTerm,show)

char_ :: ValConv Char
char_ = (head,(:[]))

string_ :: ValConv String
string_ = (id,id)  

bool_ :: String -> String -> ValConv Bool
bool_ true false = (readBool,showBool)
 where
  fromJust (Just x) = x
  readBool s = fromJust (lookup s [(true,True),(false,False)])
  showBool b = if b then true else false

val_ :: ValConv a -> XPrimConv a
val_ (rda,sha) = Conv rd sh
 where
  rd childs = (rda a,(attrs,elems))
   where
    (attrs,XText a : elems) = childs

  sh a childs = (attrs,XText (sha a) : elems)
   where
    (attrs,elems) = childs

--- Creates an XML converter for integer values. Integer values must not be
--- used in repetitions and do not represent XML elements.
---
--- @return Int converter
int :: XPrimConv Int
int = val_ int_

--- Creates an XML converter for float values. Float values must not be
--- used in repetitions and do not represent XML elements.
---
--- @return Float converter
float :: XPrimConv Float
float = val_ float_

--- Creates an XML converter for character values. Character values must not be
--- used in repetitions and do not represent XML elements.
---
--- @return Char converter
char :: XPrimConv Char
char = val_ char_

--- Creates an XML converter for string values. String values must not be
--- used in repetitions and do not represent XML elements.
---
--- @return String converter
string :: XPrimConv String
string = Conv rd sh
 where
  rd childs = ("",childs)
            ? case elems of XText s : elems' -> (s,(attrs,elems'))
   where
    (attrs,elems) = childs

  sh "" childs = childs
  sh s@(_:_) childs = (attrs,XText s : elems)
   where
    (attrs,elems) = childs

--- Parallel composition of XML converters.
---
--- @return Nondeterministic choice of XML converters
(!) :: XmlConv rep elem a -> XmlConv rep elem a -> XmlConv rep elem a
Conv rd1 sh1 ! Conv rd2 sh2 = Conv rd sh
 where
  rd childs = rd1 childs ? rd2 childs
  sh x = sh1 x ? sh2 x

--- Takes an arbitrary XML converter and returns a converter representing
--- an XML element that contains the corresponding data. XML elements may be
--- used in repetitions.
---
--- @param name Tag name of the XML element
--- @param conv XML converter for the childs of the XML element
--- @return XML converter representing an XML element
element :: String -> XmlConv _ _ a -> XElemConv a
element name xa = Conv rd sh
 where
  rd childs
    | myName==name
      = case xmlReads xa (myAttrs,myElems) of
         (a,([],[])) -> (a,(attrs,elems))
   where
    (attrs,XElem myName myAttrs myElems : elems) = childs

  sh a childs
    = case xmlShows xa a ([],[]) of
        (myAttrs,myElems) -> (attrs,XElem name myAttrs myElems : elems)
   where
    (attrs,elems) = childs

--- Takes a value and returns an XML converter for this value which is not
--- represented as XML data. Empty XML data must not be used in repetitions
--- and does not represent an XML element.
---
--- @param val Value without an XML representation
--- @return Empty XML converter
empty :: a -> XPrimConv a
empty val = Conv rd sh
 where
  rd = ret val
  sh v childs | v=:=val = childs

--- Takes a name and string conversion functions and returns an XML converter
--- that represents an attribute. Attributes must not be used in repetitions
--- and do not represent an XML element.
---
--- @param name Attribute name
--- @param readShow functions that convert between values and strings
--- @return Attribute converter
attr :: String -> ValConv a -> XAttrConv a
attr name (rda,sha) = Conv rd sh
 where
  rd childs = (rda value,(attrs',elems))
   where
    (attrs,elems) = childs
    ((_,value),attrs') = exposeBy ((name==) . fst) attrs

  sh a childs = ((name,sha a) : attrs,elems)
   where
    (attrs,elems) = childs

-- Fetch a list element that satisfies a given predicate.
exposeBy :: (a -> Bool) -> [a] -> (a,[a])
exposeBy p (x:xs) = if p x then (x,xs) else (y,x:ys)
 where
  (y,ys) = exposeBy p xs

--- Converts between arbitrary XML converters for different types.
---
--- @param a2b_b2a functions that convert between values of types a and b
--- @param conv XML converter for type a
--- @return XML converter for type b
adapt :: (a -> b,b -> a) -> XmlConv rep e a -> XmlConv rep e b
adapt (a2b,b2a) (Conv rda sha) = Conv rd sh
 where
  rd = rda />= ret . a2b
  sh = sha . b2a

--- Creates a converter for arbitrary optional XML data. Optional XML data
--- must not be used in repetitions and does not represent an XML element.
---
--- @param conv XML converter
--- @return XML converter for optional data represented by the given converter
opt :: XmlConv _ _ a -> XOptConv (Maybe a)
opt xa = Conv rd sh
 where
  rd childs = ret Nothing childs
            ? (xmlReads xa />= ret . Just) childs

  sh Nothing = id
  sh (Just a) = xmlShows xa a

--- Takes an XML converter representing repeatable data and returns an
--- XML converter that represents repetitions of this data. Repetitions
--- must not be used in other repetitions and do not represent XML elements.
---
--- @param conv XML converter representing repeatable data
--- @return XML converter representing repetitions
rep :: XmlConv Repeatable _ a -> XRepConv [a]
rep xa = Conv rd sh
 where
  rd childs = ret [] childs
            ? ( xmlReads xa />= \x ->
                rd />= \xs ->
                ret (x:xs)) childs

  sh = foldr (.) id . map (xmlShows xa)

--- Creates an XML converter for integer attributes. Integer attributes
--- must not be used in repetitions and do not represent XML elements.
---
--- @param name Attribute name
--- @return Int attribute converter
aInt :: String -> XAttrConv Int
aInt name = attr name int_

--- Creates an XML converter for float attributes. Float attributes
--- must not be used in repetitions and do not represent XML elements.
---
--- @param name Attribute name
--- @return Float attribute converter
aFloat :: String -> XAttrConv Float
aFloat name = attr name float_

--- Creates an XML converter for character attributes. Character attributes
--- must not be used in repetitions and do not represent XML elements.
---
--- @param name Attribute name
--- @return Char attribute converter
aChar :: String -> XAttrConv Char
aChar name = attr name char_

--- Creates an XML converter for string attributes. String attributes
--- must not be used in repetitions and do not represent XML elements.
---
--- @param name Attribute name
--- @return String attribute converter
aString :: String -> XAttrConv String
aString name = attr name string_

--- Creates an XML converter for boolean attributes. Boolean attributes
--- must not be used in repetitions and do not represent XML elements.
---
--- @param name Attribute name
--- @param true String representing True
--- @param false String representing False
--- @return Bool attribute converter
aBool :: String -> String -> String -> XAttrConv Bool
aBool name true false = attr name (bool_ true false)

--- Creates an XML converter for integer elements. Integer elements may be
--- used in repetitions.
---
--- @param name Tag name of the XML element containing the integer value
--- @return Int element converter
eInt :: String -> XElemConv Int
eInt name = element name int

--- Creates an XML converter for float elements. Float elements may be
--- used in repetitions.
---
--- @param name Tag name of the XML element containing the float value
--- @return Float element converter
eFloat :: String -> XElemConv Float
eFloat name = element name float

--- Creates an XML converter for character elements. Character elements may be
--- used in repetitions.
---
--- @param name Tag name of the XML element containing the character value
--- @return Char element converter
eChar :: String -> XElemConv Char 
eChar name = element name char

--- Creates an XML converter for string elements. String elements may be
--- used in repetitions.
---
--- @param name Tag name of the XML element containing the string value
--- @return String element converter
eString :: String -> XElemConv String
eString name = element name string

--- Creates an XML converter for boolean elements. Boolean elements may be
--- used in repetitions.
---
--- @param true Tag name of the XML element representing True
--- @param false Tag name of the XML element representing False
--- @return Bool element converter
eBool :: String -> String -> XElemConv Bool
eBool true false = eEmpty true True ! eEmpty false False

--- Takes a name and a value and creates an empty XML element that represents
--- the given value. The created element may be used in repetitions.
---
--- @param name Tag name of the empty element
--- @param val Value represented by the empty element
--- @return XML converter representing an empty XML element
eEmpty :: String -> a -> XElemConv a
eEmpty name a = element name (empty a)

--- Creates an XML converter that represents an element containing
--- optional XML data. The created element may be used in repetitions.
---
--- @param name Tag name of the element containing optional XML data
--- @return XML converter for an element enclosing optional XML data
eOpt :: String -> XmlConv _ _ a -> XElemConv (Maybe a)
eOpt name xa = element name (opt xa)

--- Creates an XML converter that represents an element containing
--- repeated XML data. The created element may be used in repetitions.
---
--- @param name Tag name of the element containing repeated XML data
--- @return XML converter for an element enclosing repeated XML data
eRep :: String -> XmlConv Repeatable _ a -> XElemConv [a]
eRep name xa = element name (rep xa)

--- Creates an XML converter representing a sequence of arbitrary XML data.
--- The sequence must not be used in repetitions and does not represent an
--- XML element.
---
--- @param f Invertable function (constructor) that combines the sequence
--- @param conv(s) XML converter for the data contained in the sequence
--- @return XML converter representing a sequence
seq1 :: (a -> b) -> XmlConv rep _ a -> XmlConv rep NoElem b
seq1 cons xa = Conv rd sh
 where
  cf a = cons a
  rd = xmlReads xa />= ret . cons
  sh (cf a) = xmlShows xa a

--- Creates an XML converter that represents a repetition of a sequence
--- of repeatable XML data. The repetition may be used in other
--- repetitions but does not represent an XML element. This combinator is
--- provided because converters for repeatable sequences cannot be 
--- constructed by the seq combinators.
---
--- @param f Invertable function (constructor) that combines the sequence
--- @param conv(s) XML converter for the data contained in the sequence
--- @return XML converter representing a repetition of a sequence
repSeq1 :: (a -> b) -> XmlConv Repeatable _ a -> XRepConv [b]
repSeq1 cons xa = rep (seq1 cons xa)

--- Creates an XML converter for compound values represented as an
--- XML element with children that correspond to the values components.
--- The element can be used in repetitions.
---
--- @param name Tag name of the element
--- @param cons constructor of the compound value
--- @param conv(s) XML converter for the components
--- @return XML element converter for a compound value  
eSeq1 :: String -> (a -> b) -> XmlConv _ _ a -> XElemConv b
eSeq1 name cons xa = element name (seq1 cons xa)

--- Creates an XML converter for repetitions of sequences represented as an
--- XML element that can be used in repetitions.
---
--- @param name Tag name of the element
--- @param cons constructor of the sequence
--- @param conv(s) XML converter for the components
--- @return XML element converter for a repeated sequence
eRepSeq1 :: String -> (a -> b) -> XmlConv Repeatable _ a -> XElemConv [b]
eRepSeq1 name cons xa = element name (repSeq1 cons xa)

seq2_ :: (a -> b -> c)
      -> XmlConv _ _ a -> XmlConv _ _ b
      -> XmlConv _ NoElem c
seq2_ cons xa xb = Conv rd sh
 where
  cf a b = cons a b
  rd = xmlReads xa />= \a ->
       xmlReads xb />= \b ->
       ret (cons a b)
  sh (cf a b) = xmlShows xa a . xmlShows xb b

--- Creates an XML converter representing a sequence of arbitrary XML data.
--- The sequence must not be used in repetitions and does not represent an
--- XML element.
---
--- @param f Invertable function (constructor) that combines the sequence
--- @param conv(s) XML converter for the data contained in the sequence
--- @return XML converter representing a sequence
seq2 :: (a -> b -> c)
     -> XmlConv _ _ a -> XmlConv _ _ b
     -> XSeqConv c
seq2 = seq2_

--- Creates an XML converter that represents a repetition of a sequence
--- of repeatable XML data. The repetition may be used in other
--- repetitions and does not represent an XML element. This combinator is
--- provided because converters for repeatable sequences cannot be 
--- constructed by the seq combinators.
---
--- @param f Invertable function (constructor) that combines the sequence
--- @param conv(s) XML converter for the data contained in the sequence
--- @return XML converter representing a repetition of a sequence
repSeq2 :: (a -> b -> c) 
        -> XmlConv Repeatable _ a -> XmlConv Repeatable _ b
        -> XRepConv [c]
repSeq2 cons xa xb = rep (seq2_ cons xa xb)

--- Creates an XML converter for compound values represented as an
--- XML element with children that correspond to the values components.
--- The element can be used in repetitions.
---
--- @param name Tag name of the element
--- @param cons constructor of the compound value
--- @param conv(s) XML converter for the components
--- @return XML element converter for a compound value  
eSeq2 :: String -> (a -> b -> c)
      -> XmlConv _ _ a -> XmlConv _ _ b
      -> XElemConv c
eSeq2 name cons xa xb = element name (seq2 cons xa xb)

--- Creates an XML converter for repetitions of sequences represented as an
--- XML element that can be used in repetitions.
---
--- @param name Tag name of the element
--- @param cons constructor of the sequence
--- @param conv(s) XML converter for the components
--- @return XML element converter for a repeated sequence
eRepSeq2 :: String -> (a -> b -> c)
         -> XmlConv Repeatable _ a -> XmlConv Repeatable _ b
         -> XElemConv [c]
eRepSeq2 name cons xa xb = element name (repSeq2 cons xa xb)

seq3_ :: (a -> b -> c -> d)
      -> XmlConv _ _ a -> XmlConv _ _ b -> XmlConv _ _ c
      -> XmlConv _ NoElem d
seq3_ cons xa xb xc = Conv rd sh
 where
  cf a b c = cons a b c
  rd = xmlReads xa />= \a ->
       xmlReads xb />= \b ->
       xmlReads xc />= \c ->
       ret (cons a b c)
  sh (cf a b c) = xmlShows xa a . xmlShows xb b . xmlShows xc c

--- Creates an XML converter representing a sequence of arbitrary XML data.
--- The sequence must not be used in repetitions and does not represent an
--- XML element.
---
--- @param f Invertable function (constructor) that combines the sequence
--- @param conv(s) XML converter for the data contained in the sequence
--- @return XML converter representing a sequence
seq3 :: (a -> b -> c -> d)
     -> XmlConv _ _ a -> XmlConv _ _ b -> XmlConv _ _ c
     -> XSeqConv d
seq3 = seq3_

--- Creates an XML converter that represents a repetition of a sequence
--- of repeatable XML data. The repetition may be used in other
--- repetitions and does not represent an XML element. This combinator is
--- provided because converters for repeatable sequences cannot be 
--- constructed by the seq combinators.
---
--- @param f Invertable function (constructor) that combines the sequence
--- @param conv(s) XML converter for the data contained in the sequence
--- @return XML converter representing a repetition of a sequence
repSeq3 :: (a -> b -> c -> d)
        -> XmlConv Repeatable _ a -> XmlConv Repeatable _ b
        -> XmlConv Repeatable _ c
        -> XRepConv [d]
repSeq3 cons xa xb xc = rep (seq3_ cons xa xb xc)

--- Creates an XML converter for compound values represented as an
--- XML element with children that correspond to the values components.
--- The element can be used in repetitions.
---
--- @param name Tag name of the element
--- @param cons constructor of the compound value
--- @param conv(s) XML converter for the components
--- @return XML element converter for a compound value  
eSeq3 :: String -> (a -> b -> c -> d)
      -> XmlConv _ _ a -> XmlConv _ _ b -> XmlConv _ _ c
      -> XElemConv d
eSeq3 name cons xa xb xc = element name (seq3 cons xa xb xc)

--- Creates an XML converter for repetitions of sequences represented as an
--- XML element that can be used in repetitions.
---
--- @param name Tag name of the element
--- @param cons constructor of the sequence
--- @param conv(s) XML converter for the components
--- @return XML element converter for a repeated sequence
eRepSeq3 :: String -> (a -> b -> c -> d)
         -> XmlConv Repeatable _ a -> XmlConv Repeatable _ b
         -> XmlConv Repeatable _ c
         -> XElemConv [d]
eRepSeq3 name cons xa xb xc = element name (repSeq3 cons xa xb xc)

seq4_ :: (a -> b -> c -> d -> e)
      -> XmlConv _ _ a -> XmlConv _ _ b -> XmlConv _ _ c -> XmlConv _ _ d
      -> XmlConv _ NoElem e
seq4_ cons xa xb xc xd = Conv rd sh
 where
  cf a b c d = cons a b c d
  rd = xmlReads xa />= \a ->
       xmlReads xb />= \b ->
       xmlReads xc />= \c ->
       xmlReads xd />= \d ->
       ret (cons a b c d)
  sh (cf a b c d) = xmlShows xa a . xmlShows xb b . xmlShows xc c
                  . xmlShows xd d

--- Creates an XML converter representing a sequence of arbitrary XML data.
--- The sequence must not be used in repetitions and does not represent an
--- XML element.
---
--- @param f Invertable function (constructor) that combines the sequence
--- @param conv(s) XML converter for the data contained in the sequence
--- @return XML converter representing a sequence
seq4 :: (a -> b -> c -> d -> e)
     -> XmlConv _ _ a -> XmlConv _ _ b -> XmlConv _ _ c -> XmlConv _ _ d
     -> XSeqConv e
seq4 = seq4_

--- Creates an XML converter that represents a repetition of a sequence
--- of repeatable XML data. The repetition may be used in other
--- repetitions and does not represent an XML element. This combinator is
--- provided because converters for repeatable sequences cannot be 
--- constructed by the seq combinators.
---
--- @param f Invertable function (constructor) that combines the sequence
--- @param conv(s) XML converter for the data contained in the sequence
--- @return XML converter representing a repetition of a sequence
repSeq4 :: (a -> b -> c -> d -> e)
        -> XmlConv Repeatable _ a -> XmlConv Repeatable _ b
        -> XmlConv Repeatable _ c -> XmlConv Repeatable _ d
        -> XRepConv [e]
repSeq4 cons xa xb xc xd = rep (seq4_ cons xa xb xc xd)

--- Creates an XML converter for compound values represented as an
--- XML element with children that correspond to the values components.
--- The element can be used in repetitions.
---
--- @param name Tag name of the element
--- @param cons constructor of the compound value
--- @param conv(s) XML converter for the components
--- @return XML element converter for a compound value  
eSeq4 :: String -> (a -> b -> c -> d -> e)
      -> XmlConv _ _ a -> XmlConv _ _ b -> XmlConv _ _ c -> XmlConv _ _ d
      -> XElemConv e
eSeq4 name cons xa xb xc xd = element name (seq4 cons xa xb xc xd)

--- Creates an XML converter for repetitions of sequences represented as an
--- XML element that can be used in repetitions.
---
--- @param name Tag name of the element
--- @param cons constructor of the sequence
--- @param conv(s) XML converter for the components
--- @return XML element converter for a repeated sequence
eRepSeq4 :: String -> (a -> b -> c -> d -> e)
         -> XmlConv Repeatable _ a -> XmlConv Repeatable _ b
         -> XmlConv Repeatable _ c -> XmlConv Repeatable _ d
         -> XElemConv [e]
eRepSeq4 name cons xa xb xc xd = element name (repSeq4 cons xa xb xc xd)

seq5_ :: (a -> b -> c -> d -> e -> f)
      -> XmlConv _ _ a -> XmlConv _ _ b -> XmlConv _ _ c -> XmlConv _ _ d
      -> XmlConv _ _ e
      -> XmlConv _ NoElem f
seq5_ cons xa xb xc xd xe = Conv rd sh
 where
  cf a b c d e = cons a b c d e
  rd = xmlReads xa />= \a ->
       xmlReads xb />= \b ->
       xmlReads xc />= \c ->
       xmlReads xd />= \d ->
       xmlReads xe />= \e ->
       ret (cons a b c d e)
  sh (cf a b c d e) = xmlShows xa a . xmlShows xb b . xmlShows xc c
                    . xmlShows xd d . xmlShows xe e

--- Creates an XML converter representing a sequence of arbitrary XML data.
--- The sequence must not be used in repetitions and does not represent an
--- XML element.
---
--- @param f Invertable function (constructor) that combines the sequence
--- @param conv(s) XML converter for the data contained in the sequence
--- @return XML converter representing a sequence
seq5 :: (a -> b -> c -> d -> e -> f)
     -> XmlConv _ _ a -> XmlConv _ _ b -> XmlConv _ _ c -> XmlConv _ _ d
     -> XmlConv _ _ e
     -> XSeqConv f
seq5 = seq5_

--- Creates an XML converter that represents a repetition of a sequence
--- of repeatable XML data. The repetition may be used in other
--- repetitions and does not represent an XML element. This combinator is
--- provided because converters for repeatable sequences cannot be 
--- constructed by the seq combinators.
---
--- @param f Invertable function (constructor) that combines the sequence
--- @param conv(s) XML converter for the data contained in the sequence
--- @return XML converter representing a repetition of a sequence
repSeq5 :: (a -> b -> c -> d -> e -> f)
        -> XmlConv Repeatable _ a -> XmlConv Repeatable _ b
        -> XmlConv Repeatable _ c -> XmlConv Repeatable _ d
        -> XmlConv Repeatable _ e
        -> XRepConv [f]
repSeq5 cons xa xb xc xd xe = rep (seq5_ cons xa xb xc xd xe)

--- Creates an XML converter for compound values represented as an
--- XML element with children that correspond to the values components.
--- The element can be used in repetitions.
---
--- @param name Tag name of the element
--- @param cons constructor of the compound value
--- @param conv(s) XML converter for the components
--- @return XML element converter for a compound value  
eSeq5 :: String -> (a -> b -> c -> d -> e -> f)
      -> XmlConv _ _ a -> XmlConv _ _ b -> XmlConv _ _ c -> XmlConv _ _ d
      -> XmlConv _ _ e
      -> XElemConv f
eSeq5 name cons xa xb xc xd xe = element name (seq5 cons xa xb xc xd xe)

--- Creates an XML converter for repetitions of sequences represented as an
--- XML element that can be used in repetitions.
---
--- @param name Tag name of the element
--- @param cons constructor of the sequence
--- @param conv(s) XML converter for the components
--- @return XML element converter for a repeated sequence
eRepSeq5 :: String -> (a -> b -> c -> d -> e -> f)
         -> XmlConv Repeatable _ a -> XmlConv Repeatable _ b
         -> XmlConv Repeatable _ c -> XmlConv Repeatable _ d
         -> XmlConv Repeatable _ e
         -> XElemConv [f]
eRepSeq5 name cons xa xb xc xd xe = element name (repSeq5 cons xa xb xc xd xe)

seq6_ :: (a -> b -> c -> d -> e -> f -> g)
      -> XmlConv _ _ a -> XmlConv _ _ b -> XmlConv _ _ c -> XmlConv _ _ d
      -> XmlConv _ _ e -> XmlConv _ _ f
      -> XmlConv _ NoElem g
seq6_ cons xa xb xc xd xe xf = Conv rd sh
 where
  cf a b c d e f = cons a b c d e f
  rd = xmlReads xa />= \a ->
       xmlReads xb />= \b ->
       xmlReads xc />= \c ->
       xmlReads xd />= \d ->
       xmlReads xe />= \e ->
       xmlReads xf />= \f ->
       ret (cons a b c d e f)
  sh (cf a b c d e f)  = xmlShows xa a . xmlShows xb b . xmlShows xc c
                       . xmlShows xd d . xmlShows xe e . xmlShows xf f

--- Creates an XML converter representing a sequence of arbitrary XML data.
--- The sequence must not be used in repetitions and does not represent an
--- XML element.
---
--- @param f Invertable function (constructor) that combines the sequence
--- @param conv(s) XML converter for the data contained in the sequence
--- @return XML converter representing a sequence
seq6 :: (a -> b -> c -> d -> e -> f -> g)
     -> XmlConv _ _ a -> XmlConv _ _ b -> XmlConv _ _ c -> XmlConv _ _ d
     -> XmlConv _ _ e -> XmlConv _ _ f
     -> XSeqConv g
seq6 = seq6_

--- Creates an XML converter that represents a repetition of a sequence
--- of repeatable XML data. The repetition may be used in other
--- repetitions and does not represent an XML element. This combinator is
--- provided because converters for repeatable sequences cannot be 
--- constructed by the seq combinators.
---
--- @param f Invertable function (constructor) that combines the sequence
--- @param conv(s) XML converter for the data contained in the sequence
--- @return XML converter representing a repetition of a sequence
repSeq6 :: (a -> b -> c -> d -> e -> f -> g)
        -> XmlConv Repeatable _ a -> XmlConv Repeatable _ b
        -> XmlConv Repeatable _ c -> XmlConv Repeatable _ d
        -> XmlConv Repeatable _ e -> XmlConv Repeatable _ f
        -> XRepConv [g]
repSeq6 cons xa xb xc xd xe xf = rep (seq6_ cons xa xb xc xd xe xf)

--- Creates an XML converter for compound values represented as an
--- XML element with children that correspond to the values components.
--- The element can be used in repetitions.
---
--- @param name Tag name of the element
--- @param cons constructor of the compound value
--- @param conv(s) XML converter for the components
--- @return XML element converter for a compound value  
eSeq6 :: String -> (a -> b -> c -> d -> e -> f -> g)
      -> XmlConv _ _ a -> XmlConv _ _ b -> XmlConv _ _ c -> XmlConv _ _ d
      -> XmlConv _ _ e -> XmlConv _ _ f
      -> XElemConv g
eSeq6 name cons xa xb xc xd xe xf = element name (seq6 cons xa xb xc xd xe xf)

--- Creates an XML converter for repetitions of sequences represented as an
--- XML element that can be used in repetitions.
---
--- @param name Tag name of the element
--- @param cons constructor of the sequence
--- @param conv(s) XML converter for the components
--- @return XML element converter for a repeated sequence
eRepSeq6 :: String -> (a -> b -> c -> d -> e -> f -> g)
         -> XmlConv Repeatable _ a -> XmlConv Repeatable _ b
         -> XmlConv Repeatable _ c -> XmlConv Repeatable _ d
         -> XmlConv Repeatable _ e -> XmlConv Repeatable _ f
         -> XElemConv [g]
eRepSeq6 name cons xa xb xc xd xe xf 
  = element name (repSeq6 cons xa xb xc xd xe xf)


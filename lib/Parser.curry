------------------------------------------------------------------------------
--- Library with functional logic parser combinators.
---
--- Adapted from: Rafael Caballero and Francisco J. Lopez-Fraguas:
---               A Functional Logic Perspective of Parsing.
---               In Proc. FLOPS'99, Springer LNCS 1722, pp. 85-99, 1999
---
--- @author Michael Hanus
--- @version March 2000
------------------------------------------------------------------------------

module Parser where

-- Operator declarations for the parser combinators:

infixr 4 <*>
infixr 3 >>>
infixr 2 <|>, <||>

-- We distinguish two kind of parsers:

-- A parser without a representation has type "[token] -> [token]":
-- it parses a list of tokens and returns the remaining unparsed tokens

type Parser token = [token] -> [token]

-- A parser with representation has type "rep -> [token] -> [token]":
-- in addition to the input tokens, it has the representation as an argument
-- (which is usually a free variable bound to the representation after parsing)

type ParserRep rep token = rep -> Parser token


-- Now we can define the basic combinators for parsers:

--- Combines two parsers without representation in an alternative manner.
(<|>)  :: Parser t -> Parser t -> Parser t
p <|> _ = \sentence -> p sentence
_ <|> q = \sentence -> q sentence


--- Combines two parsers with representation in an alternative manner.
(<||>)  :: ParserRep r t -> ParserRep r t -> ParserRep r t
p <||> q = \rep -> p rep <|> q rep


--- Combines two parsers (with or without representation) in a
--- sequential manner.
(<*>)    :: Parser t -> Parser t -> Parser t
p1 <*> p2 = seq
 where seq sentence | p1 sentence =:= sent1 = p2 sent1  where sent1 free


--- Attaches a representation to a parser without representation.
(>>>) :: Parser token -> rep -> ParserRep rep token
parser >>> repexp = attach
  where attach rep sentence | parser sentence =:= rest &> repexp =:= rep
                            = rest              where rest free


-- Finally, we define some useful basic parsers and derived combinators:

--- The empty parser which recognizes the empty word.
empty :: Parser _
empty sentence = sentence

--- A parser recognizing a particular terminal symbol.
terminal :: token -> Parser token
terminal sym (token:tokens) | sym=:=token = tokens

--- A parser (with representation) recognizing a terminal satisfying
--- a given predicate.
satisfy :: (token->Bool) -> ParserRep token token
satisfy pred sym (token:tokens) | pred token =:= True & sym=:=token = tokens

--- A star combinator for parsers. The returned parser
--- repeats zero or more times a parser p with representation and
--- returns the representation of all parsers in a list.
star :: ParserRep rep token -> ParserRep [rep] token
star p =    p x <*> (star p) xs >>> (x:xs)
       <||> empty               >>> []         where x,xs free

--- A some combinator for parsers. The returned parser
--- repeats the argument parser (with representation) at least once.
some :: ParserRep rep token -> ParserRep [rep] token
some p = p x <*> (star p) xs >>> (x:xs)        where x,xs free


{-----------------------------------------------------------------------

As a simple example we define a parser for arithmetic expressions
over natural numbers. The presentation of this parser is the value
of the expression.

expression   =  term t <*> plus_minus op <*> expression e  >>> (op t e)
           <||> term
 where op,t,e free

term         =  factor f <*> prod_div op <*> term t        >>> (op f t)
           <||> factor
 where op,f,t free

factor       =  terminal '(' <*> expression e <*> terminal ')'  >>> e
           <||> num
 where e free

plus_minus   =  terminal '+'  >>> (+)
           <||> terminal '-'  >>> (-)

prod_div     =  terminal '*'  >>> (*)
           <||> terminal '/'  >>> div

num = some digit l >>> numeric_value l
  where l free
        numeric_value ds = foldl1 ((+).(10*)) (map (\c->ord c - ord '0') ds)

digit = satisfy isDigit


-- example application: expression val "(10+5*2)/4" =:= []

-----------------------------------------------------------------------}


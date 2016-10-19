--- Functional implementation of parser combinators.
---
--- @author Sebastian Fischer
module Parse where

infixr 3 <|>
infixr 4 <*>, <.>, <:>

--- A parser maps a list of tokens into parsed values with remaining tokens.
type Parser t a = [t] -> [(a,[t])]

--- Combines two parsers in an alternative manner.
(<|>) :: Parser t a -> Parser t a -> Parser t a
p <|> q = \ts -> p ts ++ q ts

--- Combines two parsers in a sequential manner.
(<*>) :: Parser t a -> (a -> Parser t b) -> Parser t b
p <*> f = \ts -> concat [ f x ts' | (x,ts') <- p ts ]

--- Updates all parsed values of a parser
update :: (a -> b) -> Parser t a -> Parser t b
update f p = map (\ (x,ts) -> (f x,ts)) . p

--- Combines two parsers in a sequential manner.
--- Ignores the result of the second parser.
(<.>) :: Parser t a -> Parser t _ -> Parser t a
p <.> q = p <*> \x -> update (const x) q

--- Combines two parser in a sequential manner.
--- Ignores the result of the first parser.
(<:>) :: Parser t _ -> Parser t a -> Parser t a
p <:> q = p <*> const q

--- The empty parser which recognizes the empty word.
empty :: a -> Parser t a
empty x ts = [(x,ts)]

--- A parser recognizing a terminal satisfying a given predicate.
satisfy :: (t -> Bool) -> Parser t t
satisfy _ [] = []
satisfy p (t:ts) = if p t then [(t,ts)] else []

--- A parser recognizing a particular terminal symbol.
terminal :: Eq t => t -> Parser t t
terminal s = satisfy (s==)

--- A star combinator for parsers. The returned parser
--- repeats zero or more times a parser p and
--- returns the result of all parsers in a list.
star :: Parser t a -> Parser t [a]
star p = empty [] <|> p <*> \x -> update (x:) (star p)

--- A some combinator for parsers. The returned parser
--- repeats the argument parser at least once.
some :: Parser t a -> Parser t [a]
some p = p <*> \x -> update (x:) (star p)

------------------------------------------------------------------------------
--- Library with some functions for reading special tokens.
---
--- This library is included for backward compatibility.
--- You should use the library ReadNumeric which provides a better interface
--- for these functions.
---
--- @author Michael Hanus
--- @version January 2000
------------------------------------------------------------------------------

module Read(readNat,readInt,readHex)  where

import Char

--- Read a natural number in a string.
--- The string might contain leadings blanks and the the number is read
--- up to the first non-digit.
readNat :: String -> Int   -- result >= 0
readNat l = readNatPrefix (dropWhile (\c->c==' ') l) 0
 where
  readNatPrefix [] n = n
  readNatPrefix (c:cs) n =
   let oc = ord c in
     if oc>=ord '0' && oc<=ord '9' then readNatPrefix cs (n*10+oc-(ord '0'))
                                   else n


--- Read a (possibly negative) integer in a string.
--- The string might contain leadings blanks and the the integer is read
--- up to the first non-digit.
readInt :: String -> Int   -- result >= 0
readInt l = readIntPrefix (dropWhile (\c->c==' ') l)
 where
  readIntPrefix []     = 0
  readIntPrefix (c:cs) = if c=='-' then - (readNat cs) else readNat (c:cs)


--- Read a hexadecimal number in a string.
--- The string might contain leadings blanks and the the integer is read
--- up to the first non-heaxdecimal digit.
readHex :: String -> Int   -- result >= 0
readHex l = readHexPrefix (dropWhile (\c->c==' ') l) 0
 where
  readHexPrefix [] n = n
  readHexPrefix (c:cs) n =
   let cv = hex2int c in
     if cv>=0 then readHexPrefix cs (n*16+cv)
              else n

  hex2int c = if isDigit c then ord c - ord '0'
                           else if ord c >= ord 'A' && ord c <= ord 'F'
                                then ord c - ord 'A' + 10
                                else -1

-- end of library Read


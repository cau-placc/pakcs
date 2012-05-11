------------------------------------------------------------------------------
--- This library provides functions to categorize a list of entities
--- into a HTML page with an index access (e.g., "A-Z") to these entities.
------------------------------------------------------------------------------

module CategorizedHtmlList (list2CategorizedHtml,
                            categorizeByItemKey,
                            stringList2ItemList)  where

import List
import HTML
import Char

--- General categorization of a list of entries.
---
--- The item will occur in every category for which the boolean function
--- categoryFun yields True.
--- @param itemL the list of key-item pairs which are supposed to be
--- categorized with respect to key
--- @param categoryL list of key-category pairs to which the items can be
--- sorted in
--- @param categoryFun uses the keys of the items and the keys of the
--- categories to distribute the items among the categories.
--- @return Html containing inner links between the categories
list2CategorizedHtml :: [(a,[HtmlExp])] -> [(b,String)] -> (a -> b -> Bool)
                        -> [HtmlExp]
list2CategorizedHtml itemL categoryL categoryFun =
   categories2LinkList categoryL :
   map (\ (categoryKey,categoryString) ->
          anchor (string2urlencoded (show categoryKey))
                 (h2 [htxt categoryString] :
                  concatMap (\ (_,item)->item++[breakline])
                            (filter (\ (itemKey,_) -> 
                                            categoryFun itemKey categoryKey)
                                     itemL)
                     ++ [categories2LinkList categoryL])
         )
        categoryL

-- the navigation list
categories2LinkList :: [(_,String)] -> HtmlExp
categories2LinkList categoryL =
  par
  [center
    (concatMap (\ (categoryKey,categoryString) ->
                     [href ('#':(string2urlencoded (show categoryKey)))
                           [htxt categoryString], nbsp])
               categoryL)]

--- Categorize a list of entries with respect to the inial keys.
---
--- The categories are named as all initial characters of the keys of the items.
--- @param itemL  the list of key-item pairs which are supposed to be
--- categorized with respect to key
--- @return Html containing inner links between the categories
categorizeByItemKey :: [(String,[HtmlExp])] -> [HtmlExp]
categorizeByItemKey itemL =
   list2CategorizedHtml
       itemL 
       (map (\c -> (toUpper c,[toUpper c])) (listHeads (map fst itemL)))
       categorizeStringHead

--- Convert a string list into an key-item list
--- The strings are used as keys and for the simple text layout.
stringList2ItemList :: [String] -> [(String,[HtmlExp])]
stringList2ItemList = map (\str -> (str,[htxt str]))

-- yields every listHead only once
listHeads :: [String] -> [Char]
listHeads =
  nubBy isUpperEqual . foldr (\xs ys -> if xs==[] then ys else head xs:ys) []

-- categoryFun for categorizeByItemKey
categorizeStringHead :: String -> Char -> Bool
categorizeStringHead [] _ = False
categorizeStringHead (c:_) c' = isUpperEqual c c'

isUpperEqual c c' = toUpper c == toUpper c'


-- just for testing ----------------------------------------

main = return $ form "CatTest"
                     (categorizeByItemKey (stringList2ItemList testList))

testList = ["bbcd",
            "acde",
            "ab",
            "cde",
            "b",
            "xt",
            "gbbcd",
            "uacde",
            "Oab",
            "Qcde",
            "Tb",
            "mxt",
            "mxtr"]

-- makecurrycgi -o ~/public_html/cat.cgi CategorizedHtmlList

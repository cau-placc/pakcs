----------------------------------------------------------------------
--- Functions to generate documentation in HTML format.
---
--- @author Michael Hanus
----------------------------------------------------------------------

module CurryDocHtml where

import CurryDocParams
import CurryDocRead
import FlatCurry
import FlexRigid
import HTML
import List
import Char
import AnaCompleteness
import Sort
import Time
import Distribution
import CategorizedHtmlList
import Markdown

-- Name of style sheet for documentation files:
currydocCSS = "currydoc.css"

--------------------------------------------------------------------------
-- Generates the documentation of a module in HTML format where the comments
-- are already analyzed.
generateHtmlDocs :: String -> CalendarTime -> DocParams -> AnaInfo -> String
                 -> String -> [(SourceLine,String)] -> IO ([String],[HtmlExp])
generateHtmlDocs cdversion time docparams anainfo progname modcmts progcmts = do
  let fcyname = flatCurryFileName progname
  putStrLn $ "Reading FlatCurry program \""++fcyname++"\"..."
  (Prog _ imports types functions ops) <- readFlatCurryFile fcyname
  return $
    (imports,
     [h1 [href (getLastName progname++"_curry.html")
               [htxt (getLastName progname++".curry")]]] ++
     genHtmlModule docparams modcmts ++
     [HtmlStruct "table" [("border","1"),("width","100%")]
       [HtmlStruct "tr" []
         [HtmlStruct "td" [("valign","top"),("width","20%")]
           ([h2 [htxt "Exported names:"]] ++
            genHtmlExportIndex (getExportedTypes types)
                               (getExportedCons types)
                               (getExportedFuns functions) ++
            [h2 [htxt "Imported modules:"],
             par (concatMap (\i->[href (getLastName i++".html") [htxt i],
                                  breakline]) imports)]),
          HtmlStruct "td" []
           ([h2 [htxt "Summary of exported functions:"],
             HtmlStruct "table" [("border","1"),("width","100%")]
             (map (\ht->HtmlStruct "tr" [] [HtmlStruct "td" [] [ht]])
                (concatMap (genHtmlFuncShort docparams progcmts anainfo)
                           functions))] ++
            (if null types
             then []
             else [h2 [htxt "Exported datatypes:"], hrule] ++
                  concatMap (genHtmlType docparams progcmts) types) ++
            [h2 [htxt "Exported functions:"], hrule] ++
            concatMap (genHtmlFunc docparams progname progcmts anainfo ops)
                      functions
                 )]]] ++
       curryDocEpilog cdversion time)

--- Translate a documentation comment to HTML and use markdown translation
--- if necessary.
docComment2HTML :: DocParams -> String -> [HtmlExp]
docComment2HTML docparams cmt =
  if withMarkdown docparams
  then markdownText2HTML (replaceIdLinks cmt)
  else [HtmlText (replaceIdLinks cmt)]

-- replace identifier hyperlinks in a string (i.e., enclosed in single quotes)
-- by HTML hyperrefences:
replaceIdLinks :: String -> String
replaceIdLinks str = case str of
  [] -> []
  ('\\':'\'':cs) -> '\'' : replaceIdLinks cs
  (c:cs) -> if c=='\'' then tryReplaceIdLink [] cs
                       else c : replaceIdLinks cs
 where
  tryReplaceIdLink ltxt [] = '\'' : reverse ltxt
  tryReplaceIdLink ltxt (c:cs)
   | isSpace c = '\'' : reverse ltxt ++ c : replaceIdLinks cs -- no space in id
   | c == '\'' = checkId (reverse ltxt) ++ replaceIdLinks cs
   | otherwise = tryReplaceIdLink (c:ltxt) cs

  checkId s =
    if ' ' `elem` s
    then '\'' : s ++ ['\'']
    else let (md,dotfun) = break (=='.') s
          in "<code><a href=\"" ++
             (if null dotfun then '#':s else md++".html#"++tail dotfun) ++
             "\">"++s++"</a></code>"

-- generate HTML index for all exported names:
genHtmlExportIndex exptypes expcons expfuns =
  concatMap (\ (htmlnames,cattitle) ->
                if null htmlnames
                then []
                else [par ([bold [htxt cattitle], breakline] ++
                           intersperse breakline htmlnames)])
            [(htmltypes,"Datatypes:"),
             (htmlcons ,"Constructors:"),
             (htmlfuns ,"Functions:")]
 where
  htmltypes = map (\n->href ('#':n) [htxt n])
                  (nub (sortStrings exptypes))
  htmlcons  = map (\n->href ('#':n++"_CONS") [htxt n])
                  (nub (sortStrings expcons))
  htmlfuns  = map (\n->href ('#':n++"_SHORT") [htxt n])
                  (nub (sortStrings expfuns))

-- extract all exported types
getExportedTypes :: [TypeDecl] -> [String]
getExportedTypes types = concatMap getExpType types
 where
   getExpType (Type (_,name) vis _ _) = if vis==Public then [name] else []
   getExpType (TypeSyn (_,name) vis _ _) = if vis==Public then [name] else []

-- extract all exported constructors
getExportedCons :: [TypeDecl] -> [String]
getExportedCons types =
   map (\(Cons (_,name) _ _ _)->name)
       (filter (\(Cons _ _ vis _)->vis==Public) (concatConsDecls types))
 where
   concatConsDecls [] = []
   concatConsDecls (TypeSyn _ _ _ _ : ts) = concatConsDecls ts
   concatConsDecls (Type _ _ _ cdcls : ts) = cdcls ++ concatConsDecls ts

-- extract all exported functions
getExportedFuns :: [FuncDecl] -> [String]
getExportedFuns funs = map (\(Func (_,name) _ _ _ _)->name)
                           (filter (\(Func _ _ vis _ _)->vis==Public) funs)


--- generate HTML documentation for a module:
genHtmlModule :: DocParams -> String -> [HtmlExp]
genHtmlModule docparams modcmts =
  let (maincmt,avcmts) = splitComment modcmts
   in [par (docComment2HTML docparams maincmt)] ++
      map (\a->par [bold [htxt "Author: "], htxt a])
          (getCommentType "author" avcmts) ++
      map (\a->par [bold [htxt "Version: "], htxt a])
          (getCommentType "version" avcmts)

--- generate HTML documentation for a datatype if it is exported:
genHtmlType docparams progcmts (Type (_,tcons) tvis tvars constrs) =
  if tvis==Public
  then
   let (datacmt,conscmts) = splitComment (getDataComment tcons progcmts)
    in [h3 [anchor tcons [htxt tcons]],
        par (docComment2HTML docparams datacmt),
        par [explainCat "Constructors:", breakline,
             dlist (concatMap
                      (genHtmlCons (getCommentType "cons" conscmts))
                      constrs)],
        hrule]
  else []
 where
  genHtmlCons conscmts (Cons (cmod,cname) _ cvis argtypes) =
    if cvis==Public
    then [([opnameDoc [anchor (cname++"_CONS") [htxt cname]],
            code [HtmlText
             (" :: " ++
              concatMap (\t->" "++showType cmod True t++" -> ") argtypes ++
              tcons ++ concatMap (\i->[' ',chr (97+i)]) tvars)]],
           (maybe []
                  (\ (call,cmt) ->
                     [par ([code [htxt call], htxt " : "] ++
                           removeTopPar (docComment2HTML docparams
                                                         (removeDash cmt)))])
                  (getConsComment conscmts cname))
          )]
    else []

genHtmlType docparams progcmts (TypeSyn (tcmod,tcons) tvis tvars texp) =
  if tvis==Public
  then let (typecmt,_) = splitComment (getDataComment tcons progcmts) in
       [h3 [anchor tcons [htxt tcons]],
        par (docComment2HTML docparams typecmt),
        par [explainCat "Type synonym:", nbsp,
             if tcons=="String" && tcmod=="Prelude"
             then code [htxt "String = [Char]"]
             else code [HtmlText
                   (tcons ++ concatMap (\i->[' ',chr (97+i)]) tvars ++ " = " ++
                    showType tcmod False texp)]],
        hrule]
  else []

-- generate short HTML documentation for a function if it is exported:
genHtmlFuncShort docparams progcmts anainfo
                 (Func (fmod,fname) _ fvis ftype rule) =
  if fvis==Public
  then [table
         [[[opnameDoc
                  [anchor (fname++"_SHORT")
                          [href ('#':fname) [htxt (showId fname)]]],
            code [HtmlText (" :: " ++ showType fmod False ftype)],
            nbsp, nbsp]
            ++ genFuncPropIcons anainfo (fmod,fname) rule],
          [removeTopPar
             (docComment2HTML docparams
                (firstSentence (fst (splitComment
                                       (getFuncComment fname progcmts)))))]]
       ]
  else []

-- generate HTML documentation for a function if it is exported:
genHtmlFunc docparams progname progcmts anainfo ops
            (Func (fmod,fname) _ fvis ftype rule) =
  if fvis==Public
  then let (funcmt,paramcmts) = splitComment (getFuncComment fname progcmts)
        in [par $
             [opnameDoc
                    [anchor fname
                            [href (getLastName progname++"_curry.html#"++fname)
                                  [htxt (showId fname)]]],
              code [HtmlText (" :: "++ showType fmod False ftype)],
              nbsp, nbsp] ++
             genFuncPropIcons anainfo (fmod,fname) rule] ++
           docComment2HTML docparams funcmt ++
           genParamComment paramcmts ++
           -- show further infos for this function, if present:
           (if furtherInfos == []
            then []
            else [dlist [([explainCat "Further infos:"],
                          [ulist furtherInfos])]] ) ++
           [hrule]
  else []
 where
  furtherInfos = genFuncPropComments anainfo (fmod,fname) rule ops

  genParamComment paramcmts =
    let params = map (span isIdChar) (getCommentType "param" paramcmts)
     in (if params==[]
         then []
         else [par [explainCat "Example call:", nbsp,
                    code [htxt (showCall fname (map fst params))]],
               dlist ([([explainCat "Parameters:"],[])] ++
                      map (\(parid,parcmt)->
                            ([],[code [htxt parid], htxt " : "] ++
                                removeTopPar (docComment2HTML docparams
                                                       (removeDash parcmt))))
                          params)
              ]) ++
         [dlist (map (\rescmt ->
                         ([explainCat "Returns:"],
                          removeTopPar (docComment2HTML docparams rescmt)))
                     (getCommentType "return" paramcmts))
         ]

  showCall f params =
    if isAlpha (head f) || length params /= 2
    then "(" ++ showId f ++ concatMap (" "++) params ++ ")"
    else "(" ++ params!!0 ++ " " ++ f ++ " " ++ params!!1 ++ ")"

-- remove initial dash sign (of a parameter comment)
removeDash s = let ds = dropWhile isSpace s in
  if take 2 ds == "- " then dropWhile isSpace (drop 2 ds)
                       else ds

-- remove a single top-level paragraph in HTML expressions:
removeTopPar hexps = case hexps of
  [HtmlStruct "p" [] hs] -> hs
  _ -> hexps

--------------------------------------------------------------------------
--- Generates icons for particular properties of functions.
genFuncPropIcons anainfo fname rule =
   [detIcon, nbsp, flexRigidIcon rule]
 where
   --(non)deterministically defined property:
   detIcon =
    if getOverlappingInfo anainfo fname
    then href "index.html#nondet_explain"
              [addIconParams $ image "nondet.gif" "non-deterministic"]
    else href "index.html#det_explain"
              [addIconParams $ image "det.gif" "deterministic"]

   -- icon for rigid/flexible:
   flexRigidIcon (External _) = htxt ""
   flexRigidIcon (Rule _ rhs) = imageEvalAnnot (getFlexRigid rhs)
    where
      imageEvalAnnot ConflictFR = -- mixed rigid flexible
          href "index.html#flexrigid_explain"
               [addIconParams $ image "flexrigid.gif" "flexible+rigid"]
      imageEvalAnnot UnknownFR  = htxt ""
      imageEvalAnnot KnownRigid =
          href "index.html#rigid_explain"
               [addIconParams $ image "rigid.gif" "rigid"]
      imageEvalAnnot KnownFlex  =
          href "index.html#flex_explain"
               [addIconParams $ image "flex.gif" "flexible"]

addIconParams hicon = hicon `addAttr` ("align","middle")
                            `addAttr` ("border","0")

--------------------------------------------------------------------------
--- Generates further textual infos about particular properties
--- of a function. The result is a list of HTML expressions to be
--- formatted (if not empty) as some HTML list.
genFuncPropComments anainfo fname rule ops =
   filter (/=[]) [genFixityInfo fname ops,
                  completenessInfo,
                  indeterminismInfo,
                  opcompleteInfo,
                  externalInfo rule]
 where
   -- comment about the definitional completeness of a function:
   completenessInfo =
      let ci = getCompleteInfo anainfo fname
       in if ci==Complete
          then []
          else [htxt (if ci==InComplete
                      then "incompletely defined"
                      else
           "incompletely defined in each disjunction (but might be complete)")]

   -- comment about the indeterminism of a function:
   indeterminismInfo = if getIndetInfo anainfo fname
                       then [htxt "might behave indeterministically"]
                       else []

   -- comment about the indeterminism of a function:
   opcompleteInfo =
      if getOpCompleteInfo anainfo fname
      then [htxt "solution complete, i.e., able to compute all solutions"]
      else []

   -- comment about the external definition of a function:
   externalInfo (External _) = [htxt "externally defined"]
   externalInfo (Rule _ _)   = []


--- Generates a comment about the associativity and precedence
--- if the name is defined as an infix operator.
genFixityInfo fname ops =
    concatMap (\(Op n fix prec)->
                  if n==fname
                  then [htxt ("defined as "++showFixity fix++
                              " infix operator with precedence "++show prec)]
                  else [])
              ops
 where
  showFixity InfixOp  = "non-associative"
  showFixity InfixlOp = "left-associative"
  showFixity InfixrOp = "right-associative"


--------------------------------------------------------------------------
-- Pretty printer for types in Curry syntax:
-- second argument is True iff brackets must be written around complex types
showType :: String -> Bool -> TypeExpr -> String
showType _ _ (TVar i) = [chr (97+i)]
showType mod nested (FuncType t1 t2) =
   brackets nested
    (showType mod (isFunctionType t1) t1 ++ " -&gt; " ++ showType mod False t2)
showType mod nested (TCons tc ts)
 | ts==[]  = showTypeCons mod tc
 | tc==("Prelude","[]") && (head ts == TCons ("Prelude","Char") [])
   = "String"
 | tc==("Prelude","[]")
   = "[" ++ showType mod False (head ts) ++ "]" -- list type
 | take 2 (snd tc) == "(,"                      -- tuple type
   = "(" ++ concat (intersperse "," (map (showType mod False) ts)) ++ ")"
 | otherwise
   = brackets nested
      (showTypeCons mod tc ++ " " ++
       concat (intersperse " " (map (showType mod True) ts)))

showTypeCons mod (mtc,tc) =
  if mtc == "Prelude"
  then tc --"<a href=\"Prelude.html#"++tc++"\">"++tc++"</a>"
  else
    if mod == mtc
    then "<a href=\"#"++tc++"\">"++tc++"</a>"
    else "<a href=\""++mtc++".html#"++tc++"\">"++tc++"</a>"


--------------------------------------------------------------------------
-- translate source file into HTML file with syntax coloring
translateSource2ColoredHtml :: String -> String -> IO ()
translateSource2ColoredHtml docdir progname = do
    let output = docdir++"/"++getLastName progname++"_curry.html"         
    putStrLn ("Writing source file as HTML to \""++output++"\"...") 
    callFrontendWithParams HTML
      (setQuiet True (setOutfile output defaultParams)) progname

-- translate source file into HTML file with anchors for each function:
translateSource2AnchoredHtml :: String -> String -> IO ()
translateSource2AnchoredHtml docdir progname =
 do putStrLn ("Writing source file as HTML to \""++docdir++"/"++getLastName progname++"_curry.html\"...")
    prog <- readFile (progname++".curry")
    writeFile (docdir++"/"++getLastName progname++"_curry.html")
              (showDocCSS (progname++".curry")
                        [HtmlStruct "PRE" []
                              [HtmlText (addFuncAnchors [] (lines prog))]])

-- add the anchors to the classified lines and translate back:
-- first argument: list of already added anchors
-- second argument: list of source lines
addFuncAnchors :: [String] -> [String] -> String
addFuncAnchors _ [] = ""
addFuncAnchors ancs (sl : sls) = let id1 = getFirstId sl in
  if id1=="" ||
     id1 `elem` ["data","type","import","module","infix","infixl","infixr"]
  then htmlQuote (sl++"\n") ++ addFuncAnchors ancs sls
  else if id1 `elem` ancs
       then (sl++"\n") ++ addFuncAnchors ancs sls
       else "<A NAME=\""++id1++"\"></A>"
            ++ htmlQuote (sl++"\n")
            ++ addFuncAnchors (id1:ancs) sls


--------------------------------------------------------------------------
-- generate the index page for the documentation directory:
genMainIndexPage cdversion time docdir modnames =
 do putStrLn ("Writing index page to \""++docdir++"/index.html\"...")
    writeFile (docdir++"/index.html")
              (showDocCSS ("Documentation of Curry modules")
                          (htmlIndex modnames ++
                           curryDocEpilog cdversion time))

htmlIndex modnames =
  (if length modnames == 1
   then [h1 [htxt "Documentation of the Curry program ",
            href (head modnames++".html") [htxt (head modnames++".curry")]]]
   else [h1 [htxt "Documentation of the Curry programs:"],
         ulist (map (\m->[href (m++".html") [htxt (m++".curry ")]])
                    (mergeSort leqStringIgnoreCase modnames))]
  ) ++
  [ulist [[href "findex.html" [htxt "All functions"]],
          [href "cindex.html" [htxt "All constructors"]]],
   bold [htxt "Explanations of the icons used in the documentation:"],
   par [anchor "det_explain" [image "det.gif" "deterministic"],
        htxt " Function is deterministically defined, i.e.,",
        htxt " patterns are pairwise exclusive"],
   par [anchor "nondet_explain" [image "nondet.gif" "non-deterministic"],
        htxt " Function is non-deterministically defined, i.e.,",
        htxt " contains overlapping patterns"],
   par [anchor "rigid_explain" [image "rigid.gif" "rigid"],
        htxt " Function is rigid"],
   par [anchor "flex_explain" [image "flex.gif" "flexible"],
        htxt " Function is flexible"],
   par [anchor "flexrigid_explain" [image "flexrigid.gif" "flexible+rigid"],
        htxt " Function is partially flexible and partially rigid"]
   --par [image "impl.gif" "implementation",
   --     htxt " Reference to the implementation of the module or function"]
  ]
   

--------------------------------------------------------------------------
-- generate the function index page for the documentation directory:
genFunctionIndexPage cdversion time docdir funs = do
  putStrLn ("Writing function index page to \""++docdir++"/findex.html\"...")
  writeFile (docdir++"/findex.html")
     (showDocCSS "Index to all functions"
         (htmlFuncIndex (sortNames expfuns) ++
          curryDocEpilog cdversion time))
 where
   expfuns = map (\(Func name _ _ _ _)->name)
                 (filter (\(Func _ _ vis _ _)->vis==Public) funs)

htmlFuncIndex :: [(String,String)] -> [HtmlExp]
htmlFuncIndex qnames =
   [h1 [htxt "Index to all functions"]] ++
   categorizeByItemKey (map showModNameRef qnames)
   
showModNameRef :: (String,String) -> (String,[HtmlExp])
showModNameRef (modname,name) =
  (name,
   [href (modname++".html#"++name) [htxt name], nbsp, nbsp,
    htxt "(", href (getLastName modname++".html") [htxt modname], htxt ")"]
  )

sortNames names = mergeSort (\(_,n1) (_,n2)->leqStringIgnoreCase n1 n2) names


--------------------------------------------------------------------------
-- generate the constructor index page for the documentation directory:
genConsIndexPage cdversion time docdir types = do
  putStrLn ("Writing constructor index page to \""++docdir++"/cindex.html\"...")
  writeFile (docdir++"/cindex.html")
    (showDocCSS "Index to all constructors"
         (htmlConsIndex (sortNames expcons) ++
          curryDocEpilog cdversion time))
 where
   expcons = map (\(Cons name _ _ _)->name)
                 (filter (\(Cons _ _ vis _)->vis==Public)
                         (concatMap getCons types))

   getCons (Type _ _ _ cdecls) = cdecls
   getCons (TypeSyn _ _ _ _) = []

htmlConsIndex qnames =
   [h1 [htxt "Index to all constructors"]] ++
   categorizeByItemKey (map showModNameRef qnames)


--------------------------------------------------------------------------
-- auxiliaries:

-- style for explanation categories, like "Constructors:", "Parameters:",...
explainCat :: String -> HtmlExp
explainCat s = textstyle "explaincat" s

-- style for function/constructor name shown in the documentation part:
opnameDoc :: [HtmlExp] -> HtmlExp
opnameDoc = style "opname"

-- show HTML doc with standard style sheet:
showDocCSS title hexps = 
  showHtmlPage (page title hexps `addPageParam` pageCSS currydocCSS)

-- Sorts a list of strings.
sortStrings :: [String] -> [String]
sortStrings strings = mergeSort leqStringIgnoreCase strings

-- Returns the first sentence in a string:
firstSentence s = let (fs,ls) = break (=='.') s in
  if ls==""
  then fs
  else if tail ls /= "" && isWhiteSpace (head (tail ls))
       then fs ++ "."
       else fs ++ "." ++ firstSentence (tail ls)


-- standard epilog for all generated web pages:
curryDocEpilog cdversion time =
  [hrule,
   italic [htxt "Generated by ",
           bold [htxt "CurryDoc"],
           htxt (" ("++cdversion++") at "),
           htxt (calendarTimeToString time)]]

--------------------------------------------------------------------------

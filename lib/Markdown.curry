------------------------------------------------------------------------------
--- Library to translate
--- [markdown documents](http://en.wikipedia.org/wiki/Markdown)
--- into HTML or LaTeX.
--- The slightly restricted subset of the markdown syntax recognized by
--- this implementation is
--- [documented in this page](http://www.informatik.uni-kiel.de/~pakcs/markdown_syntax.html).
---
--- @author Michael Hanus
--- @version December 2011
------------------------------------------------------------------------------

module Markdown(MarkdownDoc,MarkdownElem(..),fromMarkdownText,
                removeEscapes,
                markdownText2HTML,markdownText2CompleteHTML,
                markdownText2LaTeX,markdownText2LaTeXWithFormat,
                markdownText2CompleteLaTeX,
                formatMarkdownFileAsPDF,formatMarkdownInputAsPDF)
 where

import List
import Char
import HTML
import HtmlParser
import System
import IO(getContents)

-----------------------------------------------------------------------
--- A markdown document is a list of markdown elements.
type MarkdownDoc = [MarkdownElem]

--- The data type for representing the different elements
--- occurring in a markdown document.
--- @cons Text s - a simple text in a markdown document
--- @cons Emph s - an emphasized text in a markdown document
--- @cons Strong s - a strongly emphaszed text in a markdown document
--- @cons Strong s - a code string in a markdown document
--- @cons Code s - a code string in a markdown document
--- @cons HRef s u - a reference to URL `u` with text `s` in a markdown document
--- @cons Par md - a paragraph in a markdown document
--- @cons CodeBlock s - a code block in a markdown document
--- @cons UList mds - an unordered list in a markdown document
--- @cons OList mds - an ordered list in a markdown document
--- @cons Quote md - a quoted paragraph in a markdown document
--- @cons HRule - a hoirzontal rule in a markdown document
--- @cons Header l s - a level `l` header with title `s`
---                    in a markdown document
data MarkdownElem = Text String
                  | Emph String
                  | Strong String
                  | Code String
                  | HRef String String
                  | Par MarkdownDoc
                  | CodeBlock String
                  | UList [MarkdownDoc]
                  | OList [MarkdownDoc]
                  | Quote MarkdownDoc
                  | HRule
                  | Header Int String

-----------------------------------------------------------------------
--- The data type for representing the different elements
--- of a source markdown document. Basically, it is the same
--- as the final markdown document execept for single list items
--- that occur in the source but are combined into a list in
--- the final document.
data SourceMDElem = SMDText String
                  | SMDEmph String
                  | SMDStrong String
                  | SMDCode String
                  | SMDHRef String String
                  | SMDPar MarkdownDoc
                  | SMDCodeBlock String
                  | SMDUItem String
                  | SMDOItem String
                  | SMDQuote MarkdownDoc
                  | SMDHRule
                  | SMDHeader Int String

isSMDUItem md = case md of SMDUItem _ -> True
                           _          -> False

isSMDOItem md = case md of SMDOItem _ -> True
                           _          -> False

textOfItem (SMDUItem txt) = txt
textOfItem (SMDOItem txt) = txt

-----------------------------------------------------------------------
--- Parse markdown document from its textual representation.
fromMarkdownText :: String -> MarkdownDoc
fromMarkdownText = groupMarkDownElems . markdownText

-- Group adjacent item elements together in a markdown list.
groupMarkDownElems :: [SourceMDElem] -> MarkdownDoc
groupMarkDownElems [] = []
groupMarkDownElems (SMDUItem itxt :mds) = joinItems UList isSMDUItem [itxt] mds
groupMarkDownElems (SMDOItem itxt :mds) = joinItems OList isSMDOItem [itxt] mds
groupMarkDownElems (SMDText s : mds) = Text s : groupMarkDownElems mds
groupMarkDownElems (SMDEmph s : mds) = Emph s : groupMarkDownElems mds
groupMarkDownElems (SMDStrong s : mds) = Strong s : groupMarkDownElems mds
groupMarkDownElems (SMDCode s : mds) = Code s : groupMarkDownElems mds
groupMarkDownElems (SMDHRef s u : mds) = HRef s u : groupMarkDownElems mds
groupMarkDownElems (SMDPar md : mds) = Par md : groupMarkDownElems mds
groupMarkDownElems (SMDCodeBlock s : mds) = CodeBlock s : groupMarkDownElems mds
groupMarkDownElems (SMDQuote md : mds) = Quote md : groupMarkDownElems mds
groupMarkDownElems (SMDHRule : mds) = HRule : groupMarkDownElems mds
groupMarkDownElems (SMDHeader l s : mds) = Header l s : groupMarkDownElems mds

joinItems mdlcons _ items [] = [mdlcons (reverse (map fromMarkdownText items))]
joinItems mdlcons isitem items (md:mds) =
  if isitem md
  then joinItems mdlcons isitem (textOfItem md : items) mds
  else mdlcons (reverse (map fromMarkdownText items))
        : groupMarkDownElems (md:mds)

-- Basic reader for a markdown text.
markdownText :: String -> [SourceMDElem]
markdownText [] = []
markdownText txt@(_:_) = markdownLine fstline (dropFirst remtxt)
 where (fstline,remtxt) = break (=='\n') txt

-- Analyze the first line of a markdown text:
markdownLine :: String -> String -> [SourceMDElem]
markdownLine fstline remtxt
 | all isSpace fstline   = markdownText remtxt
 | isLevel1Line = SMDHeader 1 fstline : markdownText (dropFirst furtherlines)
 | isLevel2Line = SMDHeader 2 fstline : markdownText (dropFirst furtherlines)
 | take 1 fstline == "#" = tryMDHeader fstline remtxt
 | isHRule fstline       = SMDHRule : markdownText remtxt
 | take 2 fstline == "> " -- start of a quoted text
  = markdownQuote (drop 2 fstline) remtxt
 | blanklen > 0 -- four space indent for code
  = markdownCodeBlock blanklen (removeEscapes (drop blanklen fstline)) remtxt
 | uitemlen > 0 -- start of an unordered item
  = markdownItem SMDUItem uitemlen (drop uitemlen fstline) remtxt
 | nitemlen > 0 -- start of a numbered item
  = markdownItem SMDOItem nitemlen (drop nitemlen fstline) remtxt
 | otherwise = markdownPar fstline remtxt
 where
  (sndline,furtherlines) = break (=='\n') remtxt
  isLevel1Line = not (null sndline) && all (=='=') sndline
  isLevel2Line = not (null sndline) && all (=='-') sndline
  nitemlen = isNumberedItemLine fstline
  uitemlen = isUnorderedItemLine fstline
  blanklen = isCodeLine fstline

dropFirst s = if null s then [] else tail s

-- translate a header line
tryMDHeader s rtxt =
  let (sharps,htxt) = break (==' ') s
      level = length sharps
   in if null htxt || level>6
      then markdownPar s rtxt
      else SMDHeader level (dropFirst htxt) : markdownText rtxt

-- is a line a horizontal rule:
isHRule l =
  (all (\c -> isSpace c || c=='-') l && length (filter (=='-') l) > 3) ||
  (all (\c -> isSpace c || c=='*') l && length (filter (=='*') l) > 3)

-- check whether a line starts with an unordered item indicator ("* ")
-- and return indent:
isUnorderedItemLine s =
  let (blanks,nonblanks) = span (==' ') s
   in if take 2 nonblanks `elem` ["* ","- ","+ "] then length blanks+2 else 0

-- check whether a line starts with an indented number and return indent value:
isNumberedItemLine s =
  let (blanks,nonblanks) = span (==' ') s
      numblanks = length blanks
   in checkNumber numblanks nonblanks
 where
  checkNumber indt numtxt =
    let (ns,brt) = break (==' ') numtxt
        (blanks,rtxt) = break (/=' ') brt
        nsl = length ns
     in if nsl>0 && all isDigit (take (nsl-1) ns) && ns!!(nsl-1)=='.' &&
           not (null blanks) && not (null rtxt)
        then indt+nsl+length blanks
        else 0

-- check whether a line starts with at least four blanks and return indent value:
isCodeLine s =
  let (blanks,nonblanks) = span (==' ') s
      numblanks = length blanks
   in if not (null nonblanks) && numblanks >= 4 then numblanks else 0

-- parse a paragraph (where the initial part of the paragraph is given
-- as the first argument):
markdownPar :: String -> String -> [SourceMDElem]
markdownPar ptxt txt
 | null txt || head txt `elem` ['\n'] ||
   uitemlen>0 || nitemlen>0
  = SMDPar (groupMarkDownElems (outsideMarkdownElem "" ptxt)) : markdownText txt
 | null remtxt
  = [SMDPar (groupMarkDownElems (outsideMarkdownElem "" (ptxt++'\n':fstline)))]
 | otherwise = markdownPar (ptxt++'\n':fstline) (tail remtxt)
 where
  (fstline,remtxt) = break (=='\n') txt
  nitemlen = isNumberedItemLine fstline
  uitemlen = isUnorderedItemLine fstline

-- parse a quoted section:
markdownQuote qtxt alltxt =
  let txt = if take 2 alltxt == ">\n" -- allow empty quote lines
            then "> " ++ drop 1 alltxt
            else alltxt
  in if take 2 txt == "> "
     then let (fstline,remtxt) = break (=='\n') (drop 2 txt)
           in if null remtxt
              then [SMDQuote (fromMarkdownText (qtxt++'\n':fstline))]
              else markdownQuote (qtxt++'\n':fstline) (tail remtxt)
     else SMDQuote (fromMarkdownText qtxt) : markdownText txt

-- parse a program block (where the indent and the initial code block is given):
markdownCodeBlock :: Int -> String -> String -> [SourceMDElem]
markdownCodeBlock n ctxt txt =
  if take n txt == "    "
  then
   let (fstline,remtxt) = break (=='\n') (drop n txt)
    in if null remtxt
       then [SMDCodeBlock (ctxt++'\n':removeEscapes fstline)]
       else markdownCodeBlock n (ctxt++'\n':removeEscapes fstline)
                                (tail remtxt)
  else SMDCodeBlock ctxt : markdownText txt

-- parse a markdown list item:
markdownItem icons n itxt txt =
  if take n txt == take n (repeat ' ')
  then let (fstline,remtxt) = break (=='\n') (drop n txt)
        in if null remtxt
           then [icons (itxt++'\n':fstline)]
           else markdownItem icons n (itxt++'\n':fstline) (tail remtxt)
  else let (fstline,remtxt) = break (=='\n') txt
        in if all isSpace fstline
           then if null remtxt
                then [icons itxt]
                else markdownItem icons n (itxt++"\n") (tail remtxt)
           else icons itxt : markdownText txt

--- Remove the backlash of escaped markdown characters in a string.
removeEscapes :: String -> String
removeEscapes s = case s of
  []          -> []
  ('\\':c:cs) -> if c `elem` markdownEscapeChars
                 then c : removeEscapes cs
                 else '\\' : removeEscapes (c:cs)
  (c:cs)      -> c : removeEscapes cs

--- Escape characters supported by markdown.
markdownEscapeChars =
  ['\\','`','*','_','{','}','[',']','(',')','#','+','-','.',' ','!']

-- Analyze markdown text outside an element like emphasis, code, strong:
outsideMarkdownElem :: String -> String -> [SourceMDElem]
outsideMarkdownElem txt s = case s of
  [] -> addPrevious txt []
  ('\\':c:cs)  -> if c `elem` markdownEscapeChars
                  then outsideMarkdownElem (c:'\\':txt) cs
                  else outsideMarkdownElem ('\\':txt) (c:cs)
  ('*':'*':cs) -> addPrevious txt $ insideMarkdownElem "**" [] cs
  ('_':'_':cs) -> addPrevious txt $ insideMarkdownElem "__" [] cs
  ('*':cs)     -> addPrevious txt $ insideMarkdownElem "*" [] cs
  ('_':cs)     -> addPrevious txt $ insideMarkdownElem "_" [] cs
  ('`':cs)     -> addPrevious txt $ insideMarkdownElem "`" [] cs
  ('[':cs) -> addPrevious txt $ tryParseLink cs
  ('<':cs) -> if take 4 cs == "http"
              then addPrevious txt $ markdownHRef cs
              else outsideMarkdownElem ('<':txt) cs
  (c:cs)   -> outsideMarkdownElem (c:txt) cs

addPrevious ptxt xs = if null ptxt then xs else SMDText (reverse ptxt) : xs

-- Try to parse a link of the form [link test](url)
tryParseLink txt = let (linktxt,rtxt) = break (==']') txt in
  if null rtxt || null (tail rtxt) || (rtxt!!1 /= '(')
  then outsideMarkdownElem "[" txt
  else let (url,mtxt) = break (==')') (drop 2 rtxt)
        in if null mtxt
           then outsideMarkdownElem "[" txt
           else SMDHRef linktxt url : outsideMarkdownElem "" (tail mtxt)

markdownHRef txt = let (url,rtxt) = break (=='>') txt in
  if null rtxt
  then outsideMarkdownElem "" ('<':txt)
  else SMDHRef url url : outsideMarkdownElem "" (dropFirst rtxt)

insideMarkdownElem marker etext s =
  if marker `isPrefixOf` s
  then text2MDElem marker (reverse etext)
        : outsideMarkdownElem "" (drop (length marker) s)
  else case s of
        []     -> [SMDText (marker ++ reverse etext)] -- end marker missing
        ('\\':c:cs) -> if c `elem` markdownEscapeChars
                       then insideMarkdownElem marker (c:'\\':etext) cs
                       else insideMarkdownElem marker ('\\':etext) (c:cs)
        (c:cs)      -> insideMarkdownElem marker (c:etext) cs

text2MDElem marker txt = case marker of
  "**" -> SMDStrong txt
  "__" -> SMDStrong txt
  "*"  -> SMDEmph txt
  "_"  -> SMDEmph txt
  "`"  -> SMDCode txt
  _    -> error $ "Markdown.text2MDElem: unknown marker \""++marker++"\""


-----------------------------------------------------------------------
-- Translate markdown document to HTML.

mdDoc2html :: MarkdownDoc -> [HtmlExp]
mdDoc2html = map mdElem2html

-- translate markdown special characters in text to HTML
mdtxt2html s = HtmlText (removeEscapes s)

mdElem2html :: MarkdownElem -> HtmlExp
mdElem2html (Text s) = mdtxt2html s
mdElem2html (Emph s) = emphasize [mdtxt2html s]
mdElem2html (Strong s) = HtmlStruct "strong" [] [mdtxt2html s]
mdElem2html (HRef s url) = if s==url
                             then href url [code [mdtxt2html s]]
                             else href url [mdtxt2html s]
mdElem2html (Code s) = code [HtmlText s]
mdElem2html (CodeBlock s) = verbatim s
mdElem2html (Quote md) = HtmlStruct "blockquote" [] (mdDoc2html md)
mdElem2html (Par md) = par (mdDoc2html md)
mdElem2html (UList mds) = ulist (map mdDoc2htmlWithoutPar mds)
mdElem2html (OList mds) = olist (map mdDoc2htmlWithoutPar mds)
mdElem2html HRule = hrule
mdElem2html (Header l s) = HtmlStruct ('h':show l) [] [mdtxt2html s]

mdDoc2htmlWithoutPar :: MarkdownDoc -> [HtmlExp]
mdDoc2htmlWithoutPar mdoc = case mdoc of
  [] -> []
  [Par md] -> mdDoc2html md
  [md] -> [mdElem2html md]
  (Par md1:md2:mds) -> mdDoc2html md1 ++ breakline : 
                         mdDoc2htmlWithoutPar (md2:mds)
  (md1:md2:mds) -> mdElem2html md1 : mdDoc2htmlWithoutPar (md2:mds)

--- Translate a markdown text into a (partial) HTML document.
markdownText2HTML :: String -> [HtmlExp]
markdownText2HTML = mdDoc2html . fromMarkdownText

--- Translate a markdown text into a complete HTML text
--- that can be viewed as a standalone document by a browser.
markdownText2CompleteHTML :: String -> String
markdownText2CompleteHTML s =
   showHtmlPage (page "Markdown Syntax" (markdownText2HTML s))

-----------------------------------------------------------------------
--- Translate markdown document to a LaTeX string where the first
--- argument is a function to translate the basic text occurring
--- in markdown elements to a LaTeX string.
--- Note that the basic text (execept for code blocks)
--- contains escaped markdown characters
--- that needs also to be removed by the translation function.
mdDoc2latex :: (String->String) -> MarkdownDoc -> String
mdDoc2latex txt2latex = concatMap (mdElem2latex txt2latex)

mdElem2latex :: (String->String) -> MarkdownElem -> String
mdElem2latex txt2latex (Text s) = txt2latex s
mdElem2latex txt2latex (Emph s) = "\\emph{"++txt2latex s++"}"
mdElem2latex txt2latex (Strong s) = "\\textbf{"++txt2latex s++"}"
mdElem2latex txt2latex (HRef s url) =
  if s==url then "\\url{"++url++"}"
            else "\\href{"++url++"}{"++txt2latex s++"}"
mdElem2latex txt2latex (Code s) = "\\texttt{"++txt2latex s++"}"
mdElem2latex _ (CodeBlock s) =
  "\\begin{verbatim}\n"++s++"\n\\end{verbatim}\n"
mdElem2latex txt2latex (Quote md) =
  "\\begin{quote}\n"++mdDoc2latex txt2latex md++"\\end{quote}\n"
mdElem2latex txt2latex (Par md) = mdDoc2latex txt2latex md++"\n\n"
mdElem2latex txt2latex (UList s) =
  "\\begin{itemize}"++
    concatMap (\i -> "\n\\item\n"++mdDoc2latex txt2latex i) s ++
  "\\end{itemize}\n"
mdElem2latex txt2latex (OList s) =
  "\\begin{enumerate}"++
    concatMap (\i -> "\n\\item\n"++mdDoc2latex txt2latex i) s ++
  "\\end{enumerate}\n"
mdElem2latex _ HRule = "\\begin{center}\\rule{3in}{0.4pt}\\end{center}\n\n"
mdElem2latex txt2latex (Header l s) = case l of
  1 -> "\\section{"++txt2latex s++"}\n\n"
  2 -> "\\subsection{"++txt2latex s++"}\n\n"
  3 -> "\\subsubsection{"++txt2latex s++"}\n\n"
  4 -> "\\paragraph{"++txt2latex s++"}\n\n"
  5 -> "\\textbf{"++txt2latex s++"}\n\n"
  _ -> "\\emph{"++txt2latex s++"}\n\n"


--- Translator for basic text to LaTeX.
--- markdown escapes are removed and possible HTML markups
--- are translated to LaTeX.
html2latex = showLatexExps . parseHtmlString . removeEscapes

--- Translate a markdown text into a (partial) LaTeX document.
--- All characters with a special meaning in LaTeX, like dollar
--- or ampersand signs, are quoted.
markdownText2LaTeX :: String -> String
markdownText2LaTeX = mdDoc2latex html2latex . fromMarkdownText

--- Translate a markdown text into a (partial) LaTeX document
--- where the first argument is a function to translate the basic text
--- occurring in markdown elements to a LaTeX string.
--- For instance, one can use a translation operation
--- that supports passing mathematical formulas in LaTeX style
--- instead of quoting all special characters.
markdownText2LaTeXWithFormat :: (String->String) -> String -> String
markdownText2LaTeXWithFormat txt2latex = mdDoc2latex txt2latex . fromMarkdownText

--- Translate a markdown text into a complete LaTeX document
--- that can be formatted as a standalone document.
markdownText2CompleteLaTeX :: String -> String
markdownText2CompleteLaTeX mds =
  latexHeader ++ mdDoc2latex html2latex (fromMarkdownText mds) ++
  "\\end{document}\n"

latexHeader =
 "\\documentclass{article}\n"++
 "\\usepackage[utf8x]{inputenc}\n"++
 "\\usepackage{url}\n"++
 "\\usepackage[breaklinks=true,unicode=true]{hyperref}\n"++
 "\\setlength{\\parindent}{0pt}\n"++
 "\\setlength{\\parskip}{6pt plus 2pt minus 1pt}\n"++
 "\\setcounter{secnumdepth}{0}\n"++
 "\\begin{document}\n"


--- Format the standard input (containing markdown text) as PDF.
formatMarkdownInputAsPDF :: IO ()
formatMarkdownInputAsPDF = getContents >>= formatMarkdownAsPDF

--- Format a file containing markdown text as PDF.
formatMarkdownFileAsPDF :: String -> IO ()
formatMarkdownFileAsPDF fname = readFile fname >>= formatMarkdownAsPDF

--- Format a file containing markdown text as PDF.
formatMarkdownAsPDF :: String -> IO ()
formatMarkdownAsPDF mdstr = do
  pid <- getPID
  let tmp = "tmp_"++show pid
  writeFile (tmp++".tex") (markdownText2CompleteLaTeX mdstr)
  pdflatexFile tmp

-- Format a file tmp.tex with pdflatex and show the result
pdflatexFile :: String -> IO ()
pdflatexFile tmp = do
  system $ "pdflatex \'\\nonstopmode\\input{"++tmp++".tex}\'"
  system $ "/bin/rm -f "++tmp++".tex "++tmp++".aux "++tmp++".log "++tmp++".out"
  system $ "evince "++tmp++".pdf"
  system $ "/bin/rm -f "++tmp++".pdf"
  done

-----------------------------------------------------------------------

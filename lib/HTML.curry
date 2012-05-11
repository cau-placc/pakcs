------------------------------------------------------------------------------
--- Library for HTML and CGI programming.
--- [This paper](http://www.informatik.uni-kiel.de/~mh/papers/PADL01.html)
--- contains a description of the basic ideas behind this library.
---
--- The installation of a cgi script written with this library
--- can be done by the command
---
---     makecurrycgi -m initialForm -o /home/joe/public_html/prog.cgi prog
---
--- where `prog` is the name of the Curry program with
--- the cgi script, `/home/joe/public_html/prog.cgi` is
--- the desired location of the
--- compiled cgi script, and `initialForm` is the Curry expression
--- (of type IO HtmlForm) computing the HTML form (where makecurrycgi
--- is a shell script stored in *pakcshome*/bin).
---
--- @author Michael Hanus (with extensions by Bernd Brassel and Marco Comini)
--- @version January 2012
------------------------------------------------------------------------------

module HTML(HtmlExp(..),HtmlPage(..),PageParam(..), 
            HtmlForm(..),FormParam(..),CookieParam(..),
            CgiRef,idOfCgiRef,CgiEnv,HtmlHandler,
            HtmlElem,Form, -- for backward compatibility
            defaultEncoding, defaultBackground,
            form,standardForm,answerText,answerEncText,
            cookieForm,getCookies,
            page,standardPage,pageEnc,pageCSS,addPageParam,
            formEnc,formCSS,addFormParam,
            htxt,htxts,hempty,nbsp,h1,h2,h3,h4,h5,
            par,emphasize,bold,italic,code,
            center,blink,teletype,pre,verbatim,address,href,anchor,
            ulist,olist,litem,dlist,table,headedTable,addHeadings,
            hrule,breakline,image,
            styleSheet,style,textstyle,blockstyle,inline,block,
            redirect,expires,
            button,resetbutton,imageButton,coordinates,
            textfield,password,textarea,checkbox,checkedbox,
            radio_main,radio_main_off,radio_other,
            selection,selectionInitial,multipleSelection,
            hiddenfield,htmlQuote,htmlIsoUmlauts,addAttr,addAttrs,
            showHtmlExps,showHtmlExp,showHtmlPage,
            showHtmlDoc,showHtmlDocCSS,
            runFormServerWithKey,runFormServerWithKeyAndFormParams,
            intForm,intFormMain,
            getUrlParameter,urlencoded2string,string2urlencoded,
            showLatexExps,showLatexExp,showLatexDoc,showLatexDocs,
            showLatexDocsWithPackages,showLatexDocWithPackages,
            germanLatexDoc,htmlSpecialChars2tex,
            addSound,addCookies) where

import System
import Char
import List
import Time
import HtmlCgi
import NamedSocket
import ReadNumeric(readNat,readHex)
import ReadShowTerm(showQTerm,readsQTerm)
import Unsafe(showAnyQExpression) -- to show status of cgi server
import Distribution(installDir)
import IO
import Profile

infixl 0 `addAttr`
infixl 0 `addAttrs`
infixl 0 `addPageParam`
infixl 0 `addFormParam`

------------------------------------------------------------------------------
--- The default encoding used in generated web pages.
defaultEncoding = "iso-8859-1"

--- The default background for generated web pages.
defaultBackground = ("bgcolor","#ffffff")

------------------------------------------------------------------------------
--- The (abstract) data type for representing references to input elements
--- in HTML forms.
data CgiRef = CgiRef String

--- Internal identifier of a CgiRef (intended only for internal use in other
--- libraries!).
idOfCgiRef :: CgiRef -> String
idOfCgiRef (CgiRef i) = i

--- The type for representing cgi environments
--- (i.e., mappings from cgi references to the corresponding values of
--- the input elements).
type CgiEnv = CgiRef -> String

--- The type of event handlers in HTML forms.
type HtmlHandler = CgiEnv -> IO HtmlForm

--- The data type for representing HTML expressions.
--- @cons HtmlText s - a text string without any further structure
--- @cons HtmlStruct t as hs - a structure with a tag, attributes, and
---                            HTML expressions inside the structure
--- @cons HtmlCRef h ref - an input element (described by the first argument)
---                        with a cgi reference
--- @cons HtmlEvent h hdlr - an input element (first arg) with an associated
---                          event handler (tpyically, a submit button)
data HtmlExp =
   HtmlText String
 | HtmlStruct String [(String,String)] [HtmlExp]
 | HtmlCRef   HtmlExp CgiRef
 | HtmlEvent  HtmlExp HtmlHandler

--- A single HTML element with a tag, attributes, but no contents
--- (deprecated, included only for backward compatibility).
HtmlElem :: String -> [(String,String)] -> HtmlExp
HtmlElem tag attrs = HtmlStruct tag attrs []

--- Extracts the textual contents of a list of HTML expressions.
---
--- For instance,
--- <code>textOf [HtmlText "xy", HtmlStruct "a" [] [HtmlText "bc"]] == "xy bc"</code>
textOf :: [HtmlExp] -> String
textOf = unwords . filter (not . null) . map textOfHtmlExp
 where
   textOfHtmlExp (HtmlText s) = s
   textOfHtmlExp (HtmlStruct _ _ hs) = textOf hs
   textOfHtmlExp (HtmlCRef   hexp _) = textOf [hexp]
   textOfHtmlExp (HtmlEvent  hexp _) = textOf [hexp]


------------------------------------------------------------------------------
--- The data type for representing HTML forms (active web pages)
--- and return values of HTML forms.
--- @cons HtmlForm t ps hs - an HTML form with title t, optional parameters
---         (e.g., cookies) ps, and contents hs
--- @cons HtmlAnswer t c - an answer in an arbitrary format where t
---         is the content type (e.g., "text/plain") and c is the contents
data HtmlForm =
        HtmlForm String [FormParam] [HtmlExp]
      | HtmlAnswer String String -- content type (e.g., "text/plain") / content

--- The possible parameters of an HTML form.
--- The parameters of a cookie (FormCookie) are its name and value and
--- optional parameters (expiration date, domain, path (e.g., the path "/"
--- makes the cookie valid for all documents on the server), security) which
--- are collected in a list.
--- @cons FormCookie name value params - a cookie to be sent to the
---                                      client's browser
--- @cons FormCSS s - a URL for a CSS file for this form
--- @cons FormJScript s - a URL for a Javascript file for this form
--- @cons FormOnSubmit s - a JavaScript statement to be executed when the form
---                        is submitted (i.e., &lt;form ... onsubmit="s"&gt;)
--- @cons FormTarget s - a name of a target frame where the output of the
---                      script should be represented (should only be used
---                      for scripts running in a frame)
--- @cons FormEnc - the encoding scheme of this form
--- @cons HeadInclude he - HTML expression to be included in form header
--- @cons MultipleHandlers - indicates that the event handlers of the form
---   can be multiply used (i.e., are not deleted if the form is submitted
---   so that they are still available when going back in the browser;
---   but then there is a higher risk that the web server process
---   might overflow with unused events); the default is a single use
---   of event handlers, i.e., one cannot use the back button in the
---   browser and submit the same form again (which is usually
---   a reasonable behavior to avoid double submissions of data).
--- @cons BodyAttr  ps - optional attribute for the body element (more than
---                      one occurrence is allowed)
data FormParam = FormCookie   String String [CookieParam]
               | FormCSS      String
               | FormJScript  String
               | FormOnSubmit String
               | FormTarget   String
               | FormEnc      String
               | HeadInclude  HtmlExp
               | MultipleHandlers
               | BodyAttr     (String,String)

--- An encoding scheme for a HTML form.
formEnc :: String -> FormParam
formEnc enc = FormEnc enc

--- A URL for a CSS file for a HTML form.
formCSS :: String -> FormParam
formCSS css = FormCSS css

--- A cookie to be sent to the client's browser when a HTML form is
--- requested.
formCookie :: (String,String) -> FormParam
formCookie (n,v) = FormCookie n v []

--- The possible parameters of a cookie.
data CookieParam = CookieExpire ClockTime
                 | CookieDomain String
                 | CookiePath   String
                 | CookieSecure

--- A basic HTML form for active web pages with the default encoding
--- and a default background.
--- @param title - the title of the form
--- @param hexps - the form's body (list of HTML expressions)
--- @return an HTML form
form :: String -> [HtmlExp] -> HtmlForm
form title hexps = HtmlForm title [BodyAttr defaultBackground] hexps

--- A basic HTML form for active web pages
--- (deprecated, included only for backward compatibility).
--- @param title - the title of the form
--- @param hexps - the form's body (list of HTML expressions)
--- @return an HTML form
Form :: String -> [HtmlExp] -> HtmlForm
Form = form

--- A standard HTML form for active web pages where the title is included
--- in the body as the first header.
--- @param title - the title of the form
--- @param hexps - the form's body (list of HTML expressions)
--- @return an HTML form with the title as the first header
standardForm :: String -> [HtmlExp] -> HtmlForm
standardForm title hexps = form title (h1 [htxt title] : hexps)

--- An HTML form with simple cookies.
--- The cookies are sent to the client's browser together with this form.
--- @param title - the title of the form
--- @param cookies - the cookies as a list of name/value pairs
--- @param hexps - the form's body (list of HTML expressions)
--- @return an HTML form
cookieForm :: String -> [(String,String)] -> [HtmlExp] -> HtmlForm
cookieForm t cs he = HtmlForm t (map (\(n,v)->FormCookie n v []) cs) he

--- Add simple cookie to HTML form.
--- The cookies are sent to the client's browser together with this form.
--- @param cs - the cookies as a list of name/value pairs
--- @param form - the form to add cookies to
--- @return a new HTML form
addCookies :: [(String,String)] -> HtmlForm -> HtmlForm
addCookies cs (HtmlForm t as hs) =
  HtmlForm t (map (\ (n,v) -> FormCookie n v []) cs++as) hs
addCookies _ (HtmlAnswer _ _) =
  error "addCookies: cannot add cookie to Html answer"

-- Shows the cookie in standard syntax:
formatCookie (name,value,params) =
  "Set-Cookie: " ++ name ++ "=" ++ string2urlencoded value ++
  concatMap (\p->"; "++formatCookieParam p) params

-- Formats a cookie parameter:
formatCookieParam :: CookieParam -> String
formatCookieParam (CookieExpire e) = "expires=" ++ toCookieDateString e
formatCookieParam (CookieDomain d) = "domain="  ++ d
formatCookieParam (CookiePath   p) = "path="    ++ p
formatCookieParam CookieSecure     = "secure"

-- Formats a clock time into a date string for cookies:
toCookieDateString :: ClockTime -> String
toCookieDateString time =
 let (CalendarTime y mo d h mi s tz) = toUTCTime time
  in (show d ++ "-" ++ shortMonths!!(mo-1) ++ "-" ++ show y ++ " " ++
         toTimeString (CalendarTime y mo d h mi s tz) ++ " UTC")
  where shortMonths = ["Jan","Feb","Mar","Apr","May","Jun",
                       "Jul","Aug","Sep","Oct","Nov","Dec"]


--- A textual result instead of an HTML form as a result for active web pages.
--- @param txt - the contents of the result page
--- @return an HTML answer form
answerText :: String -> HtmlForm
answerText = HtmlAnswer "text/plain"

--- A textual result instead of an HTML form as a result for active web pages
--- where the encoding is given as the first parameter.
--- @param enc - the encoding of the text(e.g., "utf-8" or "iso-8859-1")
--- @param txt - the contents of the result page
--- @return an HTML answer form
answerEncText :: String -> String -> HtmlForm
answerEncText enc = HtmlAnswer ("text/plain; charset="++enc)

--- Adds a parameter to an HTML form.
--- @param form - a form
--- @param param - a form's parameter
--- @return an HTML form
addFormParam :: HtmlForm -> FormParam -> HtmlForm
addFormParam (HtmlForm title params hexps) param =
              HtmlForm title (param:params) hexps
addFormParam hexp@(HtmlAnswer _ _) _ = hexp

addFormParams :: HtmlForm -> [FormParam] -> HtmlForm
addFormParams hform [] = hform
addFormParams hform (fp:fps) = addFormParams (hform `addFormParam` fp) fps

--- Adds redirection to given HTML form.
--- @param secs - Number of seconds to wait before executing autromatic redirection
--- @param url - The URL whereto redirect to
--- @param form - The form to add the header information to
redirect :: Int -> String -> HtmlForm -> HtmlForm
redirect secs url hform =
  hform `addFormParam`
      HeadInclude (HtmlStruct "meta" [("http-equiv","refresh"),
                                      ("content",show secs++"; URL="++url)] [])

--- Adds expire time to given HTML form.
--- @param secs - Number of seconds before document expires
--- @param form - The form to add the header information to
expires :: Int -> HtmlForm -> HtmlForm
expires secs hform =
  hform `addFormParam`
      HeadInclude (HtmlStruct "meta" [("http-equiv","expires"),
                                      ("content",show secs)] [])

--- Adds sound to given HTML form. The functions adds two different declarations
--- for sound, one invented by Microsoft for the internet explorer, one introduced
--- for netscape. As neither is an official part of HTML, addsound might not work
--- on all systems and browsers. The greatest chance is by using sound files
--- in MID-format.
--- @param soundfile - Name of file containing the sound to be played
--- @param loop - Should sound go on infinitely? Use with care.
--- @param form - The form to add sound to
addSound :: String -> Bool -> HtmlForm -> HtmlForm
addSound soundfile loop (HtmlForm title params conts) =
  HtmlForm title
           (HeadInclude
             (HtmlStruct "bgsound"
                         [("src",soundfile),
                         ("loop",if loop then "infinite" else "1")] []):params)
           (HtmlStruct "embed"
             ((if loop then [("loop","true")] else []) ++
              [("src",soundfile),("autostart","true"), ("hidden","true"),
               ("height","0"), ("width","0")]) []:
              conts)
addSound _ _ (HtmlAnswer _ _)
  = error "HTML.addSound: unable to add sound to general HTML Answer"


------------------------------------------------------------------------------
--- The data type for representing HTML pages.
--- The constructor arguments are the title, the parameters, and
--- the contents (body) of the web page.
data HtmlPage = HtmlPage String [PageParam] [HtmlExp]

--- The possible parameters of an HTML page.
--- @cons PageEnc - the encoding scheme of this page
--- @cons PageCSS s - a URL for a CSS file for this page
--- @cons PageJScript s - a URL for a Javascript file for this page
data PageParam = PageEnc     String
               | PageCSS     String
               | PageJScript String

--- An encoding scheme for a HTML page.
pageEnc :: String -> PageParam
pageEnc enc = PageEnc enc

--- A URL for a CSS file for a HTML page.
pageCSS :: String -> PageParam
pageCSS css = PageCSS css

--- A basic HTML web page with the default encoding.
--- @param title - the title of the page
--- @param hexps - the page's body (list of HTML expressions)
--- @return an HTML page
page :: String -> [HtmlExp] -> HtmlPage
page title hexps = HtmlPage title [PageEnc defaultEncoding] hexps

--- A standard HTML web page where the title is included
--- in the body as the first header.
--- @param title - the title of the page
--- @param hexps - the page's body (list of HTML expressions)
--- @return an HTML page with the title as the first header
standardPage :: String -> [HtmlExp] -> HtmlPage
standardPage title hexps = page title (h1 [htxt title] : hexps)

--- Adds a parameter to an HTML page.
--- @param form - a page
--- @param param - a page's parameter
--- @return an HTML page
addPageParam :: HtmlPage -> PageParam -> HtmlPage
addPageParam (HtmlPage title params hexps) param =
  HtmlPage title (param:params) hexps


------------------------------------------------------------------------------
-- some useful abbreviations:

--- Basic text as HTML expression.
--- The text may contain special HTML chars (like &lt;,&gt;,&amp;,&quot;)
--- which will be quoted so that they appear as in the parameter string.
htxt   :: String -> HtmlExp
htxt s = HtmlText (htmlQuote s)

--- A list of strings represented as a list of HTML expressions.
--- The strings may contain special HTML chars that will be quoted.
htxts :: [String] -> [HtmlExp]
htxts = map htxt

--- An empty HTML expression.
hempty :: HtmlExp
hempty = HtmlText ""

--- Non breaking Space
nbsp   :: HtmlExp
nbsp = HtmlText "&nbsp;"

--- Header 1
h1      :: [HtmlExp] -> HtmlExp
h1 hexps = HtmlStruct "h1" [] hexps

--- Header 2
h2      :: [HtmlExp] -> HtmlExp
h2 hexps = HtmlStruct "h2" [] hexps

--- Header 3
h3      :: [HtmlExp] -> HtmlExp
h3 hexps = HtmlStruct "h3" [] hexps

--- Header 4
h4      :: [HtmlExp] -> HtmlExp
h4 hexps = HtmlStruct "h4" [] hexps

--- Header 5
h5      :: [HtmlExp] -> HtmlExp
h5 hexps = HtmlStruct "h5" [] hexps

--- Paragraph
par      :: [HtmlExp] -> HtmlExp
par hexps = HtmlStruct "p" [] hexps

--- Emphasize
emphasize      :: [HtmlExp] -> HtmlExp
emphasize hexps = HtmlStruct "em" [] hexps

--- Boldface
bold      :: [HtmlExp] -> HtmlExp
bold hexps = HtmlStruct "b" [] hexps

--- Italic
italic      :: [HtmlExp] -> HtmlExp
italic hexps = HtmlStruct "i" [] hexps

--- Program code
code      :: [HtmlExp] -> HtmlExp
code hexps = HtmlStruct "code" [] hexps

--- Centered text
center      :: [HtmlExp] -> HtmlExp
center hexps = HtmlStruct "center" [] hexps

--- Blinking text
blink      :: [HtmlExp] -> HtmlExp
blink hexps = HtmlStruct "blink" [] hexps

--- Teletype font
teletype      :: [HtmlExp] -> HtmlExp
teletype hexps = HtmlStruct "tt" [] hexps

--- Unformatted input, i.e., keep spaces and line breaks and
--- don't quote special characters.
pre      :: [HtmlExp] -> HtmlExp
pre hexps = HtmlStruct "pre" [] hexps

--- Verbatim (unformatted), special characters (&lt;,&gt;,&amp;,&quot;)
--- are quoted.
verbatim  :: String -> HtmlExp
verbatim s = HtmlStruct "pre" [] [HtmlText (htmlQuote s)]

--- Address
address       :: [HtmlExp] -> HtmlExp
address hexps = HtmlStruct "address" [] hexps

--- Hypertext reference
href           :: String -> [HtmlExp] -> HtmlExp
href ref hexps = HtmlStruct "a" [("href",ref)] hexps

--- An anchor for hypertext reference inside a document
anchor           :: String -> [HtmlExp] -> HtmlExp
anchor anc hexps = HtmlStruct "a" [("name",anc)] hexps

--- Unordered list
--- @param items - the list items where each item is a list of HTML expressions
ulist       :: [[HtmlExp]] -> HtmlExp
ulist items = HtmlStruct "ul" [] (map litem items)

--- Ordered list
--- @param items - the list items where each item is a list of HTML expressions
olist :: [[HtmlExp]] -> HtmlExp
olist items = HtmlStruct "ol" [] (map litem items)

--- A single list item (usually not explicitly used)
litem hexps = HtmlStruct "li" [] hexps

--- Description list
--- @param items - a list of (title/description) pairs (of HTML expressions)
dlist       :: [([HtmlExp],[HtmlExp])] -> HtmlExp
dlist items = HtmlStruct "dl" [] (concatMap ditem items)
 where
   ditem (hexps1,hexps2) = [HtmlStruct "dt" [] hexps1,
                            HtmlStruct "dd" [] hexps2]

--- Table with a matrix of items where each item is a list of HTML expressions.
table :: [[[HtmlExp]]] -> HtmlExp
table items = HtmlStruct "table" []
 (map (\row->HtmlStruct "tr" []
                 (map (\item -> HtmlStruct "td" [] item) row)) items)

--- Similar to <code>table</code> but introduces header tags for the first row.
headedTable :: [[[HtmlExp]]] -> HtmlExp
headedTable = withinTable . table
 where
  withinTable (HtmlStruct "table" attrs (HtmlStruct "tr" rowAttrs row:rows)) =
      HtmlStruct "table" attrs
        (HtmlStruct "tr" rowAttrs (map addTh row):rows)
  addTh x = case x of
             (HtmlStruct "td" attrs conts) -> HtmlStruct "th" attrs conts
             other -> other

--- Add a row of items (where each item is a list of HTML expressions)
--- as headings to a table. If the first argument is not a table,
--- the headings are ignored.
addHeadings :: HtmlExp -> [[HtmlExp]] -> HtmlExp
addHeadings htable headings = case htable of
   HtmlStruct "table" attrs rows ->
      HtmlStruct "table" attrs
         (HtmlStruct "tr" [] (map (\item->HtmlStruct "th" [] item) headings):rows)
   _ -> htable


--- Horizontal rule
hrule :: HtmlExp
hrule = HtmlStruct "hr" [] []

--- Break a line
breakline :: HtmlExp
breakline = HtmlStruct "br" [] []

--- Image
--- @param src - the URL of the image
--- @param alt - the alternative text shown instead of the image
image :: String -> String -> HtmlExp
image src alt = HtmlStruct "img" [("src",src),("alt",htmlQuote alt)] []


-------------- styles and document structuring:
--- Defines a style sheet to be used in this HTML document.
--- @param css - a string in CSS format
styleSheet :: String -> HtmlExp
styleSheet css = HtmlStruct "style" [("type","text/css")] [HtmlText css]

--- Provides a style for HTML elements.
--- The style argument is the name of a style class defined in a
--- style definition (see <code>styleSheet</code>) or in an
--- external style sheet (see form and page parameters <code>FormCSS</code>
--- and <code>PageCSS</code>).
--- @param st - name of a style class
--- @param hexps - list of HTML expressions
style :: String -> [HtmlExp] -> HtmlExp
style st hexps = HtmlStruct "span" [("class",st)] hexps

--- Provides a style for a basic text.
--- The style argument is the name of a style class defined in an
--- external style sheet.
--- @param st - name of a style class
--- @param txt - a string (special characters will be quoted)
textstyle :: String -> String -> HtmlExp
textstyle st txt = HtmlStruct "span" [("class",st)] [htxt txt]

--- Provides a style for a block of HTML elements.
--- The style argument is the name of a style class defined in an
--- external style sheet. This element is used (in contrast to "style")
--- for larger blocks of HTML elements since a line break is placed
--- before and after these elements.
--- @param st - name of a style class
--- @param hexps - list of HTML expressions
blockstyle :: String -> [HtmlExp] -> HtmlExp
blockstyle st hexps = HtmlStruct "div" [("class",st)] hexps

--- Joins a list of HTML elements into a single HTML element.
--- Although this construction has no rendering, it is sometimes useful
--- for programming when several HTML elements must be put together.
--- @param hexps - list of HTML expressions
inline :: [HtmlExp] -> HtmlExp
inline hexps = HtmlStruct "span" [] hexps

--- Joins a list of HTML elements into a block.
--- A line break is placed before and after these elements.
--- @param hexps - list of HTML expressions
block :: [HtmlExp] -> HtmlExp
block hexps = HtmlStruct "div" [] hexps


-------------- forms and input fields:
--- Submit button with a label string and an event handler
button :: String -> HtmlHandler -> HtmlExp
button label handler =
    HtmlEvent
       (HtmlStruct "input" [("type","submit"),("name","EVENT"),
                            ("value",htmlQuote label)] [])
       handler

--- Reset button with a label string
resetbutton :: String -> HtmlExp
resetbutton label =
    HtmlStruct "input" [("type","reset"),("value",htmlQuote label)] []

--- Submit button in form of an imag.
--- @param src - url of the image
--- @param handler - event handler
imageButton :: String -> HtmlHandler -> HtmlExp
imageButton src handler
  = HtmlEvent
       (HtmlStruct "input" [("type","image"),("name","EVENT"),("src",src)] [])
       handler

--- Input text field with a reference and an initial contents
textfield :: CgiRef -> String -> HtmlExp
textfield cref contents
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","text"),("name",ref),
                            ("value",htmlQuote contents)] [])
       cref
 where ref free

--- Input text field (where the entered text is obscured) with a reference
password :: CgiRef -> HtmlExp
password cref
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","password"),("name",ref)] [])
       cref
 where
   ref free

--- Input text area with a reference, height/width, and initial contents
textarea :: CgiRef -> (Int,Int) -> String -> HtmlExp
textarea cref (height,width) contents
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "textarea" [("name",ref),
                                ("rows",show height),("cols",show width)]
                               [htxt contents])
       cref
 where
   ref free

--- A checkbox with a reference and a value.
--- The value is returned if checkbox is on, otherwise "" is returned.
checkbox :: CgiRef -> String -> HtmlExp
checkbox cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","checkbox"),("name",ref),
                            ("value",htmlQuote value)] [])
       cref
 where
   ref free

--- A checkbox that is initially checked with a reference and a value.
--- The value is returned if checkbox is on, otherwise "" is returned.
checkedbox :: CgiRef -> String -> HtmlExp
checkedbox cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","checkbox"),("name",ref),
                            ("value",htmlQuote value),("checked","checked")] [])
       cref
 where
   ref free

--- A main button of a radio (initially "on") with a reference and a value.
--- The value is returned of this button is on.
--- A complete radio button suite always consists of a main button
--- (radio_main) and some further buttons (radio_others) with the
--- same reference. Initially, the main button is selected
--- (or nothing is selected if one uses radio_main_off instead of radio_main).
--- The user can select another button but always at most one button
--- of the radio can be selected. The value corresponding to the
--- selected button is returned in the environment for this radio reference.
radio_main :: CgiRef -> String -> HtmlExp
radio_main cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","radio"),("name",ref),
                            ("value",htmlQuote value),("checked","yes")] [])
       cref
 where
   ref free

--- A main button of a radio (initially "off") with a reference and a value.
--- The value is returned of this button is on.
radio_main_off :: CgiRef -> String -> HtmlExp
radio_main_off cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","radio"),("name",ref),
                            ("value",htmlQuote value)] [])
       cref
 where
   ref free

--- A further button of a radio (initially "off") with a reference (identical
--- to the main button of this radio) and a value.
--- The value is returned of this button is on.
radio_other :: CgiRef -> String -> HtmlExp
radio_other cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlStruct "input"
               [("type","radio"),("name",ref),("value",htmlQuote value)] []
 where
   ref free

--- A selection button with a reference and a list of name/value pairs.
--- The names are shown in the selection and the value is returned
--- for the selected name.
selection :: CgiRef -> [(String,String)] -> HtmlExp
selection cref menue
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "select" [("name",ref)]
         ((concat . map (\(n,v)->[HtmlStruct "option" [("value",v)] [htxt n]]))
          menue))
       cref
 where
   ref free

--- A selection button with a reference, a list of name/value pairs,
--- and a preselected item in this list.
--- The names are shown in the selection and the value is returned
--- for the selected name.
--- @param ref - a CGI reference
--- @param nvs - list of name/value pairs
--- @param sel - the index of the initially selected item in the list nvs
--- @return an HTML expression representing the selection button
selectionInitial :: CgiRef -> [(String,String)] -> Int -> HtmlExp
selectionInitial cref sellist sel
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef (HtmlStruct "select" [("name",ref)] (selOption sellist sel))
             cref
 where
   ref free

   selOption [] _ = []
   selOption ((n,v):nvs) i =
      HtmlStruct "option"
                 ([("value",v)] ++ if i==0 then [("selected","yes")] else [])
                 [htxt n] : selOption nvs (i-1)

--- A selection button with a reference and a list of name/value/flag pairs.
--- The names are shown in the selection and the value is returned
--- if the corresponding name is selected. If flag is True, the
--- corresonding name is initially selected. If more than one name
--- has been selected, all values are returned in one string
--- where the values are separated by newline (`'\n'`) characters.
multipleSelection :: CgiRef -> [(String,String,Bool)] -> HtmlExp
multipleSelection cref sellist
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef (HtmlStruct "select" [("name",ref),("multiple","yes")]
                                  (map selOption sellist))
            cref
 where
   ref free

   selOption (n,v,flag) =
      HtmlStruct "option"
                 ([("value",v)] ++ if flag then [("selected","yes")] else [])
                 [htxt n]

--- A hidden field to pass a value referenced by a fixed name.
--- This function should be used with care since it may cause
--- conflicts with the CGI-based implementation of this library.
hiddenfield :: String -> String -> HtmlExp
hiddenfield name value =
    HtmlStruct "input" [("type","hidden"),("name",name),("value",value)] []


------------------------------------------------------------------------------
--- Quotes special characters (`<`,`>`,`&`,`"`, umlauts) in a string
--- as HTML special characters.
htmlQuote :: String -> String
htmlQuote [] = []
htmlQuote (c:cs) | c=='<' = "&lt;"   ++ htmlQuote cs
                 | c=='>' = "&gt;"   ++ htmlQuote cs
                 | c=='&' = "&amp;"  ++ htmlQuote cs
                 | c=='"' = "&quot;" ++ htmlQuote cs
                 | otherwise = htmlIsoUmlauts [c] ++ htmlQuote cs

--- Translates umlauts in iso-8859-1 encoding into HTML special characters.
htmlIsoUmlauts :: String -> String
htmlIsoUmlauts [] = []
htmlIsoUmlauts (c:cs) | oc==228 = "&auml;"  ++ htmlIsoUmlauts cs
                      | oc==246 = "&ouml;"  ++ htmlIsoUmlauts cs
                      | oc==252 = "&uuml;"  ++ htmlIsoUmlauts cs
                      | oc==196 = "&Auml;"  ++ htmlIsoUmlauts cs
                      | oc==214 = "&Ouml;"  ++ htmlIsoUmlauts cs
                      | oc==220 = "&Uuml;"  ++ htmlIsoUmlauts cs
                      | oc==223 = "&szlig;" ++ htmlIsoUmlauts cs
                      | oc==197 = "&Aring;" ++ htmlIsoUmlauts cs
                      | oc==250 = "&uacute;"++ htmlIsoUmlauts cs
                      | oc==237 = "&iacute;"++ htmlIsoUmlauts cs
                      | oc==225 = "&aacute;"++ htmlIsoUmlauts cs
                      | otherwise = c : htmlIsoUmlauts cs
  where oc = ord c

------------------------------------------------------------------------------
--- Adds an attribute (name/value pair) to an HTML element.
addAttr :: HtmlExp -> (String,String) -> HtmlExp
addAttr hexp attr = addAttrs hexp [attr]

--- Adds a list of attributes (name/value pair) to an HTML element.
addAttrs :: HtmlExp -> [(String,String)] -> HtmlExp
addAttrs (HtmlText s) _ = HtmlText s  -- strings have no attributes
addAttrs (HtmlStruct tag attrs hexps) newattrs =
    HtmlStruct tag (attrs++newattrs) hexps
addAttrs (HtmlEvent hexp handler) attrs =
    HtmlEvent (addAttrs hexp attrs) handler
addAttrs (HtmlCRef  hexp cref) attrs =
    HtmlCRef (addAttrs hexp attrs) cref


------------------------------------------------------------------------------
-- Auxiliaries for faster show (could be later put into a standard library)

type ShowS = String -> String

showString s = (s++)
showChar   c = (c:)
nl    = showChar '\n'

concatS [] = id
concatS xs@(_:_) = foldr1 (\ f g -> f . g) xs

------------------------------------------------------------------------------
--- Transforms a list of HTML expressions into string representation.
showHtmlExps :: [HtmlExp] -> String
showHtmlExps hexps = showsHtmlExps hexps ""

showsHtmlExps :: [HtmlExp] -> ShowS
showsHtmlExps [] = id
showsHtmlExps (he:hes) = showsWithLnPrefix he . showsHtmlExps hes
 where
   showsWithLnPrefix hexp = let s = getText hexp
                            in if s/="" && isSpace (head s)
                               then nl . showString (tail s)
                               else showsHtmlExp hexp

-- get the string contents of an HTML expression:
getText :: HtmlExp -> String
getText (HtmlText s)       = s
getText (HtmlStruct _ _ _) = ""
getText (HtmlEvent  he _)  = getText he
getText (HtmlCRef   he _)  = getText he

-- get the (last) tag of an HTML expression:
getTag :: HtmlExp -> String
getTag (HtmlText _)         = ""
getTag (HtmlStruct tag _ _) = tag
getTag (HtmlEvent  he _)    = getTag he
getTag (HtmlCRef   he _)    = getTag he

-- is this a tag where a line break can be safely added?
tagWithLn t = t/="" &&
              t `elem` ["br","p","li","ul","ol","dl","dt","dd","hr",
                        "h1","h2","h3","h4","h5","h6",
                        "html","title","head","body","form","table","tr","td"]


--- Transforms a single HTML expression into string representation.
showHtmlExp :: HtmlExp -> String
showHtmlExp hexp = showsHtmlExp hexp ""

showsHtmlExp :: HtmlExp -> ShowS
showsHtmlExp (HtmlText s) = showString s
showsHtmlExp (HtmlStruct tag attrs hexps) =
  let maybeLn = if tagWithLn tag then nl else id
   in maybeLn .
      (if null hexps && tag/="script" -- due to problems with older browsers
       then showsHtmlOpenTag tag attrs "/>"
       else showsHtmlOpenTag tag attrs ">" . maybeLn . showExps hexps .
            maybeLn . showString "</" . showString tag . showChar '>'
      ) . maybeLn
 where
  showExps = if tag=="pre" then concatS . map showsHtmlExp else showsHtmlExps
showsHtmlExp (HtmlEvent hexp _) = showsHtmlExp hexp
showsHtmlExp (HtmlCRef  hexp _) = showsHtmlExp hexp

showsHtmlOpenTag :: String -> [(String,String)] -> String -> ShowS
showsHtmlOpenTag tag attrs close =
  showChar '<' . showString tag .
  concatS (map attr2string attrs) . showString close
 where
    attr2string (attr,value) = showChar ' ' . showString attr .
         showString "=\"" . encodeQuotes value . showChar '"'

    -- encode double quotes as "&quot;":
    encodeQuotes [] = id
    encodeQuotes (c:cs) | c=='"'    = showString "&quot;" . encodeQuotes cs
                        | otherwise = showChar c . encodeQuotes cs


------------------------------------------------------------------------------
--- Transforms HTML expressions into string representation of complete
--- HTML document with title
--- (deprecated, included only for backward compatibility).
--- @param title - the title of the HTML document
--- @param hexps - the body (list of HTML expressions) of the document
--- @return string representation of the HTML document
showHtmlDoc :: String -> [HtmlExp] -> String
showHtmlDoc title html = showHtmlPage (page title html)

--- Transforms HTML expressions into string representation of complete
--- HTML document with title and a URL for a style sheet file
--- (deprecated, included only for backward compatibility).
--- @param title - the title of the HTML document
--- @param css - the URL for a CSS file for this document
--- @param hexps - the body (list of HTML expressions) of the document
--- @return string representation of the HTML document
showHtmlDocCSS :: String -> String -> [HtmlExp] -> String
showHtmlDocCSS title css html =
  showHtmlPage (page title html `addPageParam` pageCSS css)

--- Transforms HTML page into string representation.
--- @param page - the HTML page
--- @return string representation of the HTML document
showHtmlPage :: HtmlPage -> String
showHtmlPage (HtmlPage title params html) =
  htmlPrelude ++
  showHtmlExp (HtmlStruct "html" htmlTagAttrs
                  [HtmlStruct "head" []
                       ([HtmlStruct "title" [] [HtmlText (htmlQuote title)]] ++
                       concatMap param2html params),
                   HtmlStruct "body" [defaultBackground] html])
 where
  param2html (PageEnc enc) =
     [HtmlStruct "meta" [("http-equiv","Content-Type"),
                         ("content","text/html; charset="++enc)] []]
  param2html (PageCSS css) =
     [HtmlStruct "link" [("rel","stylesheet"),("type","text/css"),("href",css)]
                 []]
  param2html (PageJScript js) =
     [HtmlStruct "script" [("type","text/javascript"),("src",js)] []]


--- Standard header for generated HTML pages.
htmlPrelude =
 "<?xml version=\"1.0\" encoding=\""++defaultEncoding++"\"?>\n"++
 "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n"++
 "  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"

--- Standard attributes for element "html".
htmlTagAttrs = [("xmlns","http://www.w3.org/1999/xhtml"),
                ("xml:lang","en"),("lang","en")]

------------------------------------------------------------------------------
--- Gets the parameter attached to the URL of the script.
--- For instance, if the script is called with URL
--- "http://.../script.cgi?parameter", then "parameter" is
--- returned by this I/O action.
--- Note that an URL parameter should be "URL encoded" to avoid
--- the appearance of characters with a special meaning.
--- Use the functions "urlencoded2string" and "string2urlencoded"
--- to decode and encode such parameters, respectively.

getUrlParameter :: IO String
getUrlParameter = getEnviron "QUERY_STRING"

--- Translates urlencoded string into equivalent ASCII string.
urlencoded2string :: String -> String
urlencoded2string [] = []
urlencoded2string (c:cs)
  | c == '+'  = ' ' : urlencoded2string cs
  | c == '%'  = chr (maybe 0 fst (readHex (take 2 cs)))
                 : urlencoded2string (drop 2 cs)
  | otherwise = c : urlencoded2string cs

--- Translates arbitrary strings into equivalent urlencoded string.
string2urlencoded :: String -> String
string2urlencoded [] = []
string2urlencoded (c:cs)
  | isAlphaNum c = c : string2urlencoded cs
  | c == ' '     = '+' : string2urlencoded cs
  | otherwise = let oc = ord c
    in '%' : int2hex(oc `div` 16) : int2hex(oc `mod` 16) : string2urlencoded cs
 where
   int2hex i = if i<10 then chr (ord '0' + i)
                       else chr (ord 'A' + i - 10)


------------------------------------------------------------------------------
--- Gets the cookies sent from the browser for the current CGI script.
--- The cookies are represented in the form of name/value pairs since
--- no other components are important here.
getCookies :: IO [(String,String)]
getCookies =
 do cookiestring <- getEnviron "HTTP_COOKIE"
    return $ parseCookies cookiestring

-- translate a string of cookies (of the form "NAME1=VAL1; NAME2=VAL")
-- into a list of name/value pairs:
parseCookies :: String -> [(String,String)]
parseCookies str = if str=="" then [] else
  let (c1,cs) = break (==';') str
   in parseCookie c1 :
      parseCookies (dropWhile (==' ') (if cs=="" then "" else tail cs))
 where
  parseCookie s = let (name,evalue) = break (=='=') s in
      (name,if evalue=="" then "" else urlencoded2string (tail evalue))

--- For image buttons: retrieve the coordinates where the user clicked
--- within the image.
coordinates :: CgiEnv -> Maybe (Int,Int)
coordinates env = let x = env (CgiRef "x")
                      y = env (CgiRef "y")
                   in if x/="" && y/=""
                        then Just (tryReadNat 0 x, tryReadNat 0 y)
                        else Nothing


------------------------------------------------------------------------------
--- The server implementing an HTML form (possibly containing input fields).
--- It receives a message containing the environment of the client's
--- web browser, translates the HTML form w.r.t. this environment
--- into a string representation of the complete HTML document
--- and sends the string representation back to the client's browser
--- by binding the corresponding message argument.
--- @param url - the URL of this executable.
--- @param cgikey - a unique key to identify this CGI script (used for safe
---                 storing of event handlers in this server)
--- @param hformact - an IO action returning an HTML form
runFormServerWithKey :: String -> String -> IO HtmlForm -> IO ()
runFormServerWithKey url cgikey hformact =
  runFormServerWithKeyAndFormParams url cgikey [] hformact

--- The server implementing an HTML form (possibly containing input fields).
--- It receives a message containing the environment of the client's
--- web browser, translates the HTML form w.r.t. this environment
--- into a string representation of the complete HTML document
--- and sends the string representation back to the client's browser
--- by binding the corresponding message argument.
--- @param url - the URL of this executable.
--- @param cgikey - a unique key to identify this CGI script (used for safe
---                 storing of event handlers on the web server)
--- @param formparams - form parameters added to the initial and all
---                     subsequent forms
--- @param hformact - an IO action returning an HTML form
runFormServerWithKeyAndFormParams :: String -> String -> [FormParam]
                                  -> IO HtmlForm -> IO ()
runFormServerWithKeyAndFormParams url cgikey formparams hformact = do
  args <- getArgs
  let (timeout,rargs) = stripTimeoutArg args
  case rargs of
    ["-port",port,"-scriptkey",skey] -> startCgiServer timeout port skey
    _ -> putErrLn $ "ERROR: cgi server called with illegal arguments"
 where
  stripTimeoutArg args = case args of
    ("-servertimeout":tos:rargs) ->
         (tryReadNat defaultCgiServerTimeout tos, rargs)
    _ -> (defaultCgiServerTimeout,args)

  startCgiServer timeout port scriptkey = do
    time <- getClockTime
    (state,htmlstring) <- computeFormInStateAndEnv True url cgikey formparams
                             (initialServerState time) scriptkey hformact []
    putStr htmlstring
    hClose stdout
    if isServerStateWithoutHandlers state
     then done
     else -- start server process:
      do let portname = port++scriptkey
         socket <- listenOn portname
         ltime <- toCalendarTime time
         putErrLn $ calendarTimeToString ltime ++
                    ": server started on port " ++ portname
         registerCgiServer url portname
         serveCgiMessagesForForm timeout url cgikey portname formparams
                                 hformact socket state

-- The default timeout period for the cgi server in milliseconds:
defaultCgiServerTimeout = 7200000 -- two hours


-- The main server loop:
serveCgiMessagesForForm :: Int -> String -> String -> String -> [FormParam]
                        -> IO HtmlForm -> Socket -> ServerState -> IO ()
serveCgiMessagesForForm servertimeout url cgikey portname
                        fparams initform socket = serveCgiMessages
 where
  serveCgiMessages state =
    if isServerStateWithoutHandlers state
    then do -- terminate server due to inactivity
            ltime <- getLocalTime
            putErrLn $ calendarTimeToString ltime ++
                       ": terminated due to empty handler list"
            unregisterCgiServer portname
            sClose socket
    else waitForSocketAccept socket servertimeout >>=
         maybe (do -- terminate server due to inactivity
                   ltime <- getLocalTime
                   putErrLn $ calendarTimeToString ltime ++
                              ": terminated due to timeout"
                   unregisterCgiServer portname
                   sClose socket )
               (\ (rhost,hdl) -> do
                  hostname <- getHostname
                  if rhost `elem` ["localhost","localhost.localdomain",hostname]
                     || take 8 rhost == "127.0.0."
                   then readCgiServerMsg hdl >>=
                        maybe (hClose hdl >> serveCgiMessages state)
                              (serveCgiMessage state hdl)
                   else putErrLn ("Ignored message from: "++rhost) >>
                        hClose hdl >> serveCgiMessages state )

  -- Process the received CgiServerMsg:
  serveCgiMessage _ hdl StopCgiServer = do
    hClose hdl
    ltime <- getLocalTime
    putErrLn $ calendarTimeToString ltime ++
              ": server terminated by stop message"
    unregisterCgiServer portname
    sClose socket

  serveCgiMessage state hdl CleanServer = do
    hClose hdl
    nstate <- cleanOldEventHandlers state
    serveCgiMessages nstate

  serveCgiMessage oldstate hdl GetLoad = do
    state <- cleanOldEventHandlers oldstate
    serverload <- getServerLoad state
    hPutStrLn hdl serverload
    hClose hdl
    serveCgiMessages state

  serveCgiMessage oldstate hdl SketchStatus = do
    state <- cleanOldEventHandlers oldstate
    serverstatus <- getServerStatus state
    hPutStrLn hdl serverstatus
    hClose hdl
    serveCgiMessages state

  serveCgiMessage state hdl SketchHandlers =
    reportStatus state hdl sketchEventHandler
   where
    sketchEventHandler (key,time,_,_,gkey) = do
      ltime <- toCalendarTime time
      return $ "No. " ++ show key ++ " (" ++ showGroupKey gkey ++
               "), expires at: " ++
               calendarTimeToString ltime ++ "\n"

  serveCgiMessage state hdl ShowStatus =
    reportStatus state hdl showEventHandler
   where
    showEventHandler (key,time,_,(_,handler),gkey) = do
      ltime <- toCalendarTime time
      return $ "No. " ++ show key ++ " (" ++ showGroupKey gkey ++
               "), expires at " ++
               calendarTimeToString ltime ++ ": " ++
               showAnyQExpression handler ++ "\n"

  serveCgiMessage state hdl (CgiSubmit scriptenv formenv) = do
      let scriptkey = maybe "" id (lookup "SCRIPTKEY" scriptenv)
      mapIO_ (\(var,val) -> if var=="SCRIPTKEY" then done
                                                else setEnviron var val)
             scriptenv
      if null formenv -- initial form?
       then serveFormInEnv state scriptkey initform []
       else do
         (rstate,mfe) <- getNextFormAndCgiEnv state cgikey formenv
         maybe (do urlparam <- getUrlParameter
                   hPutStrLn hdl (noHandlerPage url urlparam)
                   hClose hdl
                   serveCgiMessages rstate)
               (\ (ioform,env) -> serveFormInEnv rstate scriptkey ioform env )
               mfe
   where
    serveFormInEnv rstate scriptkey hformact cenv = do
      (nstate,htmlstring) <- computeFormInStateAndEnv False url cgikey fparams
                                                rstate scriptkey hformact cenv
      hPutStrLn hdl htmlstring
      hClose hdl
      serveCgiMessages nstate

  reportStatus state@(stime,maxkey,ctime,ehs) hdl eh2string = do
    lstime <- toCalendarTime stime
    lctime <- toCalendarTime ctime
    ehsstrings <- mapIO eh2string ehs
    hPutStrLn hdl $ "Started at: " ++ calendarTimeToString lstime ++ "\n" ++
                    "Next cleanup: " ++ calendarTimeToString lctime ++
                    " (maxkey: " ++ show maxkey ++")\n"++
                    "Current event handlers:\n" ++ concat ehsstrings
    hClose hdl
    serveCgiMessages state

-- computes a HTML form w.r.t. a state and a cgi environment:
computeFormInStateAndEnv isinitform url cgikey fparams state scriptkey
                         hformact cenv =
  catch tryComputeForm
        (\e -> do uparam <- getUrlParameter
                  return (state,errorAsHtml e uparam))
 where
  errorAsHtml e urlparam = addHtmlContentType isinitform $ showHtmlPage $
   page "Server Error"
    [h1 [htxt "Error: Failure during computation"],
     par [htxt "Your request cannot be processed due to a run-time error:"],
     pre [htxt (showError e)],
     par [htxt "You can try to ",
          href (url ++ if null urlparam then "" else '?':urlparam)
               [htxt "click here"],
          htxt " to try again loading the web page or inform the web ",
          htxt "administrator about this problem."]]

  tryComputeForm = do
    cform <- hformact
    let (cookiestring,hform) = extractCookies cform
    (htmlstring,evhs) <- showAnswerFormInEnv isinitform url scriptkey
                                             (addFormParams hform fparams)
                                             (getMaxFieldNr cenv + 1)
    nstate <- storeEnvHandlers state
                (formWithMultipleHandlers hform)
                (encodeKey cgikey)
                (filter (\ (t,_) -> t/="DEFAULT" && take 6 t /= "EVENT_") cenv)
                evhs
    seq (isList htmlstring) done -- to ensure to catch all failures here
    return (nstate, cookiestring++htmlstring)

  isList [] = success
  isList (_:xs) = isList xs

formWithMultipleHandlers :: HtmlForm -> Bool
formWithMultipleHandlers (HtmlAnswer _ _) = False
formWithMultipleHandlers (HtmlForm _ params _) = any (==MultipleHandlers) params

-- Encode an arbitrary string to make it less readable.
-- Used for encoding CGI keys before storing them on the web server.
encodeKey :: String -> String
encodeKey = map mapchr . reverse . filter (not . isSpace)
 where
  mapchr c | oc<33 || oc>126 = c
           | oc<114          = chr (oc+13)
           | otherwise       = chr (oc-81)
   where oc = ord c

-- Puts a line to stderr:
putErrLn s = hPutStrLn stderr s >> hFlush stderr


--------------------------------------------------------------------------
-- Auxiliaries to implement the cgi script server:

-- get the next form and environment from a current environment (specifying a
-- user-selected event handler) and a server state holding all event handlers:
getNextFormAndCgiEnv :: ServerState -> String -> [(String,String)]
                     -> IO (ServerState, Maybe (IO HtmlForm,[(String,String)]))
getNextFormAndCgiEnv state cgikey newcenv = do
  (nstate,mbh) <- retrieveEnvHandlers state (encodeKey cgikey)
                             (urlencoded2string (getFormEvent "" newcenv))
  return $ maybe (nstate,Nothing)
                 (\ (oldcenv,handler) -> let cenv = newcenv++oldcenv in
                         (nstate, Just (handler (cgiGetValue cenv), cenv)))
                 mbh


-- put the HTML string corresponding to an HtmlForm with HTTP header on stdout:
showAnswerFormInEnv :: Bool -> String -> String -> HtmlForm -> Int
                      -> IO (String,[(HtmlHandler,String)])
showAnswerFormInEnv withlength url key hform@(HtmlForm _ _ _) crefnr = do
  (htmlstring,evhs) <- showHtmlFormInEnv url key hform crefnr
  return (addHtmlContentType withlength htmlstring, evhs)
showAnswerFormInEnv _ _ _ (HtmlAnswer ctype cont) _ = do
  return ("Content-Type: "++ctype++"\n\n"++cont, [])


-- Adds the initial content lines to an HTML string.
-- If the first argument is True, the content length is computed and also added.
addHtmlContentType withlength htmlstring =
    (if withlength
     then "Connection: close\nContent-Length: " ++
          show (length htmlstring) ++ "\n"
     else "") ++
    "Content-Type: text/html\n\n" ++ htmlstring

-- return the HTML string corresponding to an HtmlForm:
showHtmlFormInEnv :: String -> String -> HtmlForm -> Int
                     -> IO (String,[(HtmlHandler,String)])
showHtmlFormInEnv url key (HtmlForm ftitle fparams fhexp) crefnr = do
  qstr <- getEnviron "QUERY_STRING"
  --putStrLn (showHtmlExps [pre [par (env2html cenv),hrule]]) --debug
  (title,params,hexps,firsthandler,evhs) <-
    htmlForm2html (HtmlForm ftitle fparams fhexp) crefnr
  return (showForm [("SCRIPTKEY",key),("DEFAULT","EVENT_"++firsthandler)]
                   (if qstr=="" then url else url++"?"++qstr)
                   (HtmlForm title params hexps),
          evhs)


-- extract the cookies contained in a form and return the "set cookie" string
-- and the form without the cookies:
extractCookies :: HtmlForm -> (String,HtmlForm)
extractCookies (HtmlAnswer ctype cont) = ("",HtmlAnswer ctype cont)
extractCookies (HtmlForm title params hexp) =
  let cookiestring = if cookies==[]
                     then ""
                     else "Cache-control: no-cache=\"set-cookie\"\n" ++
                          concatMap ((++"\n") . formatCookie) cookies
   in (cookiestring, HtmlForm title otherparams hexp)
 where
   (cookies,otherparams) = splitFormParams params

   splitFormParams [] = ([],[])
   splitFormParams (fparam:fps) =
     let (cs,ops) = splitFormParams fps
      in case fparam of
           FormCookie n v ps -> ((n,v,ps):cs,ops)
           _                 -> (cs,fparam:ops)

-- get the EVENT_ definition of the cgi environment
-- (or "DEFAULT" value if it is not there):
getFormEvent :: String -> [(String,String)] -> String
getFormEvent deflt [] = deflt
getFormEvent deflt ((tag,val):tvs) =
   if tag == "DEFAULT" then getFormEvent (drop 6 val) tvs else
   if take 6 tag == "EVENT_" then urlencoded2string (drop 6 tag)
                             else getFormEvent deflt tvs

-- compute the maximal field number of all "FIELD_nr" in a CGI environment:
getMaxFieldNr :: [(String,String)] -> Int
getMaxFieldNr [] = 0
getMaxFieldNr ((name,_):env) =
  if take 6 name == "FIELD_"
  then max (tryReadNat 0 (drop 6 name)) (getMaxFieldNr env)
  else getMaxFieldNr env

max x y = if x>y then x else y

-- try to read a natural number in a string or return first argument:
tryReadNat :: Int -> String -> Int
tryReadNat d s = maybe d (\(i,rs)->if null rs then i else d) (readNat s)

-- get the value assigned to a name in a given cgi environment
cgiGetValue :: [(String,String)] -> CgiRef -> String
cgiGetValue cenv (CgiRef ref) =
    concat (intersperse "\n" (map snd (filter ((ref==) . fst) cenv)))

-- transform HTML form into HTML document (by instantiating CgiRefs
-- (starting with the second argument) and modifying event handlers):
-- (Result: title/HTML document/form params/encoded first handler)
htmlForm2html :: HtmlForm -> Int
             -> IO (String,[FormParam],[HtmlExp],String,[(HtmlHandler,String)])
htmlForm2html (HtmlForm title params html) crefnr = do
  let (htmlwithoutcrefs,newrefnr) = numberCgiRefs html crefnr
  -- enforce instantiation before handlers are stored:
  seq newrefnr done
  -- seq (normalForm htmlwithoutcrefs) done
  let (transhtml, evhs, fh) = translateHandlers htmlwithoutcrefs
  --storeEventHandlers cgikey oldcenv evhs
  return (title, params, transhtml, fh, evhs)


-- instantiate all CgiRefs with a unique tag in HTML expressions:
numberCgiRefs :: [HtmlExp] -> Int -> ([HtmlExp],Int)
-- arguments: HTMLExps, number for cgi-refs
-- result: translated HTMLExps, new number for cgi-refs
numberCgiRefs [] i = ([],i)
numberCgiRefs (HtmlText s : hexps) i =
   let (nhexps,j) = numberCgiRefs hexps i
   in (HtmlText s : nhexps, j)
numberCgiRefs (HtmlStruct tag attrs hexps1 : hexps2) i =
   let (nhexps1,j) = numberCgiRefs hexps1 i
       (nhexps2,k) = numberCgiRefs hexps2 j
   in (HtmlStruct tag attrs nhexps1 : nhexps2, k)
numberCgiRefs (HtmlEvent (HtmlStruct tag attrs hes) handler : hexps) i =
   let (nhexps,j) = numberCgiRefs hexps i
   in (HtmlEvent (HtmlStruct tag attrs hes) handler : nhexps, j)
numberCgiRefs (HtmlCRef hexp (CgiRef ref) : hexps) i
  | ref =:= ("FIELD_"++show i)
  = let ([nhexp],j) = numberCgiRefs [hexp] (i+1)
        (nhexps,k) = numberCgiRefs hexps j
    in (nhexp : nhexps, k)


-- translate all event handlers into their internal form:
-- (assumption: all CgiRefs have already been instantiated and eliminated)
-- the result is the translated HTML expression list (without HtmlEvents),
-- the list of event handlers and their corresponding logical variables
-- denoting the key that is inserted for the event handler in the translated
-- HTML expression, and the string encoding of the first event handler
-- (for the default handler)
translateHandlers :: [HtmlExp] -> ([HtmlExp],[(HtmlHandler,String)],String)
translateHandlers [] = ([],[],"")
translateHandlers (HtmlText s : hexps) =
  let (nhexps,evhs,fh) = translateHandlers hexps
   in (HtmlText s : nhexps, evhs, fh)
translateHandlers (HtmlStruct tag attrs hexps1 : hexps2) =
  let (nhexps1,evhs1,fh1) = translateHandlers hexps1
      (nhexps2,evhs2,fh2) = translateHandlers hexps2
   in (HtmlStruct tag attrs nhexps1 : nhexps2, evhs1++evhs2,
       if fh1=="" then fh2 else fh1)
translateHandlers (HtmlEvent (HtmlStruct tag attrs hes) handler : hexps) =
  let (nhexps,evhs,_) = translateHandlers hexps
      fh = string2urlencoded key
   in (HtmlStruct tag (changeAssoc attrs "name" ("EVENT_" ++ fh)) hes : nhexps,
       (handler,key):evhs, fh)
 where key free

-- show a HTML form in String representation:
showForm cenv url (HtmlForm title params html) =
  htmlPrelude ++
  showHtmlExp
   (HtmlStruct "html" htmlTagAttrs
     [HtmlStruct "head" []
                 ([HtmlStruct "title" [] [HtmlText (htmlQuote title)]] ++
                  concatMap param2html params),
      HtmlStruct "body" bodyattrs
       ((if null url then id
         else \he->[HtmlStruct "form"
                               ([("method","post"),("action",url)]
                                ++ onsubmitattr ++ targetattr)
                               he])
          ( --[par (env2html cenv),hrule] ++ -- debug
           cenv2hidden cenv ++
           html))])
 where
  param2html (FormEnc enc) =
     [HtmlStruct "meta" [("http-equiv","Content-Type"),
                         ("content","text/html; charset="++enc)] []]
  param2html (FormCSS css) =
     [HtmlStruct "link" [("rel","stylesheet"),("type","text/css"),("href",css)]
                 []]
  param2html (FormJScript js) =
     [HtmlStruct "script" [("type","text/javascript"),("src",js)] []]
  param2html (FormOnSubmit _) = []
  param2html (FormTarget _) = []
  -- no rule for FormCookie since they have been already processed
  param2html (HeadInclude hexp) = [hexp]
  param2html MultipleHandlers = []
  param2html (BodyAttr _) = []
  -- no rule for BodyAttr since it is considered later

  bodyattrs = [ps | (BodyAttr ps) <- params]

  onsubmit = [s | (FormOnSubmit s) <- params]

  onsubmitattr = if null onsubmit then [] else [("onsubmit",head onsubmit)]

  target = [s | (FormTarget s) <- params]

  targetattr = if null target then [] else [("target",head target)]


-- translate cgi environment into HTML (for debugging purposes):
env2html :: [(String,String)] -> [HtmlExp]
env2html env = concat (map (\(n,v)->[htxt (n++": "++v),breakline]) env)


-- translate environment into hidden fields (without EVENT field!):
-- (note: the field values are urlencoded to avoid problems
--  with passing special characters; moreover, the names of fields
--  containing urlencoded values are prefixed by "U")
cenv2hidden env = concat (map pair2hidden env)
 where
   pair2hidden (n,v)
     | take 6 n == "EVENT_" = []
     | take 6 n == "FIELD_" = [hiddenfield ('U':n) (string2urlencoded v)]
     | otherwise            = [hiddenfield n v]

------------------------------------------------------------------------------
-- association lists (list of tag/value pairs):

-- change an associated value (or add association, if not there):
changeAssoc :: [(tt,tv)] -> tt -> tv -> [(tt,tv)]
changeAssoc [] tag val = [(tag,val)]
changeAssoc ((tag1,val1):tvs) tag val =
   if tag1 == tag then (tag,val) : tvs
                  else (tag1,val1) : changeAssoc tvs tag val


------------------------------------------------------------------------------
--- Transforms HTML expressions into LaTeX string representation.

showLatexExps :: [HtmlExp] -> String
showLatexExps hexps = concat (map showLatexExp hexps)

--- Transforms an HTML expression into LaTeX string representation.
showLatexExp :: HtmlExp -> String
showLatexExp (HtmlText s) = "{" ++ specialchars2tex s ++ "}"
showLatexExp (HtmlStruct tag attrs htmlexp)
 | tag=="html" = showLatexExps htmlexp
 | tag=="head" = ""                    -- ignore header
 | tag=="body" = showLatexExps htmlexp
 | tag=="form" = showLatexExps htmlexp
 | tag=="h1"   = "\\section*{" ++ showLatexExps htmlexp ++ "}\n"
 | tag=="h2"   = "\\subsection*{" ++ showLatexExps htmlexp ++ "}\n"
 | tag=="h3"   = "\\subsubsection*{" ++ showLatexExps htmlexp ++ "}\n"
 | tag=="h4"   = "\\paragraph*{" ++ showLatexExps htmlexp ++ "}\n"
 | tag=="h5"   = "\\subparagraph*{" ++ showLatexExps htmlexp ++ "}\n"
 | tag=="p"    = showLatexExps htmlexp ++ "\\par\n"
 | tag=="b"    = "{\\bf " ++ showLatexExps htmlexp ++ "}"
 | tag=="em"   = "\\emph{" ++ showLatexExps htmlexp ++ "}"
 | tag=="i"    = "{\\it " ++ showLatexExps htmlexp ++ "}"
 | tag=="tt"   = "{\\tt " ++ showLatexExps htmlexp ++ "}"
 | tag=="code" = "{\\tt " ++ showLatexExps htmlexp ++ "}"
 | tag=="center" = latexEnvironment "center" (showLatexExps htmlexp)
 | tag=="pre"  = latexEnvironment "verbatim" (textOf htmlexp)
 | tag=="font" = showLatexExps htmlexp  -- ignore font changes
 | tag=="address" = showLatexExps htmlexp
 | tag=="blink"   = showLatexExps htmlexp
 | tag=="a"    = showLatexExps htmlexp ++
                 -- add href attribute as footnote, if present:
                 maybe ""
                       (\url->"\\footnote{\\tt "++specialchars2tex url++"}\n")
                       (findHtmlAttr "href" attrs)
 | tag=="ul"   = latexEnvironment "itemize"  (showLatexExps htmlexp)
 | tag=="ol"   = latexEnvironment "enumerate" (showLatexExps htmlexp)
 | tag=="li"   = "\\item\n" ++ showLatexExps htmlexp ++ "\n"
 | tag=="dl"   = latexEnvironment "description" (showLatexExps htmlexp)
 | tag=="dt"  = "\\item[" ++ showLatexExps htmlexp ++ "]~\\\\\n"
 | tag=="dd"  = showLatexExps htmlexp
 -- tables will be set using the longtable environment,
 -- (The package longtable is added by default to every latex document)
 | tag=="table" = attrLatexEnv "longtable" (latexTabFormat htmlexp)
                                           (showLatexTableContents htmlexp)
 | tag=="tr"   = let cells = map showLatexExp htmlexp
                  in concat (intersperse " & " cells) ++ "\\\\\n"
 | tag=="td"   = showLatexExps htmlexp
 | tag=="br"   = "\\par\n"
 | tag=="hr"   = "\\vspace{2ex}\\hrule\n"
 | tag=="img" = "{" ++  maybe "{\\tt<IMAGE>}" specialchars2tex
                             (findHtmlAttr "alt" attrs)
                    ++ "}"
 | tag=="input" && maybe "" id (findHtmlAttr "type" attrs) == "hidden" = ""
 | otherwise   = "{\\tt<"++tag++">}" ++ showLatexExps htmlexp ++
                "{\\tt</"++tag++">}"

-- create latex environment of name "env" with content "content"
latexEnvironment :: String -> String -> String
latexEnvironment env content = attrLatexEnv env "" content

-- create latex environment of name "env" with content "content"
-- adding the parameters "attr"
attrLatexEnv :: String -> String -> String -> String
attrLatexEnv env attr content
 = "\\begin{"++env++"}"++attr++"\n"
 ++content
 ++"\n\\end{"++env++"}\n"

-- yield the format of a table, e.g. {lll} from list of html rows.
-- for longtables we set the chunksize big enough
-- to avoid having to rerun latex for inaccurat tables.
latexTabFormat :: [HtmlExp] -> String
latexTabFormat rows = "{" ++ replicate breadth 'l' ++ "}"
  ++ "\\setcounter{LTchunksize}{"++show (length rows+5)++"}%"
  where
    breadth = foldl max 0 (map getBreadth rows)

-- retrieve the breadth of an Html row
getBreadth :: HtmlExp -> Int
getBreadth row = case row of
                     HtmlStruct "tr" _ tds -> length tds
                     _ -> error "getBreadth: no row given"

-- tranlate expressions inside tables
showLatexTableContents :: [HtmlExp] -> String
showLatexTableContents hexps = concatMap showLatexTableContent hexps

-- tranlate expressions inside tables
showLatexTableContent :: HtmlExp -> String
showLatexTableContent (HtmlText s) = "{" ++ specialchars2tex s ++ "}"
showLatexTableContent (HtmlStruct tag attrs htmlexp)
 | tag=="html" = showLatexTableContents htmlexp
 | tag=="head" = ""                    -- ignore header
 | tag=="body" = showLatexTableContents htmlexp
 | tag=="form" = showLatexTableContents htmlexp
 | tag=="p"    = showLatexTableContents htmlexp ++ "\\par\n"
 | tag=="b"    = "{\\bf " ++ showLatexTableContents htmlexp ++ "}"
 | tag=="em"   = "\\emph{" ++ showLatexTableContents htmlexp ++ "}"
 | tag=="i"    = "{\\it " ++ showLatexTableContents htmlexp ++ "}"
 | tag=="tt"   = "{\\tt " ++ showLatexTableContents htmlexp ++ "}"
 | tag=="font" = showLatexTableContents htmlexp  -- ignore font changes
 | tag=="address" = showLatexTableContents htmlexp
 | tag=="blink"   = showLatexTableContents htmlexp
 | tag=="a"    = showLatexTableContents htmlexp ++
                 -- add href attribute as footnote, if present:
                 maybe ""
                       (\url->"\\footnote{\\tt "++specialchars2tex url++"}\n")
                       (findHtmlAttr "href" attrs)
 | tag=="tr"   = let cells = map showLatexTableContent htmlexp
                  in concat (intersperse " & " cells) ++ "\\\\\n"
 | tag=="td"   = showLatexTableContents htmlexp
 | tag=="br"   = "\\par\n"
 | tag=="hr"   = "\\vspace{2ex}\\hrule\n"
 | tag=="img"  = "{" ++  maybe "{\\tt<IMAGE>}" specialchars2tex
                               (findHtmlAttr "alt" attrs)
                    ++ "}"
 | tag=="input" && maybe "" id (findHtmlAttr "type" attrs) == "hidden" = ""
 | otherwise   = "{\\tt<"++tag++">}" ++ showLatexTableContents htmlexp ++
                 "{\\tt</"++tag++">}"

-- find a specific tag field in a list of HTML attributes:
findHtmlAttr :: String -> [(String,String)] -> Maybe String
findHtmlAttr _    [] = Nothing
findHtmlAttr atag ((t,f):attrs) =
  if atag==t then Just f
             else findHtmlAttr atag attrs


--- Convert special characters into TeX representation, if necessary.
specialchars2tex :: String -> String
specialchars2tex = htmlSpecialChars2tex . escapeLaTeXSpecials

escapeLaTeXSpecials :: String -> String
escapeLaTeXSpecials [] = []
escapeLaTeXSpecials (c:cs)
  | c=='^'      = "{\\tt\\char94}" ++ escapeLaTeXSpecials cs
  | c=='~'      = "{\\tt\\char126}" ++ escapeLaTeXSpecials cs
  | c=='\\'     = "{\\tt\\char92}" ++ escapeLaTeXSpecials cs
  | c=='<'      = "{\\tt\\char60}" ++ escapeLaTeXSpecials cs
  | c=='>'      = "{\\tt\\char62}" ++ escapeLaTeXSpecials cs
  | c=='_'      = "\\_" ++ escapeLaTeXSpecials cs
  | c=='#'      = "\\#" ++ escapeLaTeXSpecials cs
  | c=='$'      = "\\$" ++ escapeLaTeXSpecials cs
  | c=='%'      = "\\%" ++ escapeLaTeXSpecials cs
  | c=='{'      = "\\{" ++ escapeLaTeXSpecials cs
  | c=='}'      = "\\}" ++ escapeLaTeXSpecials cs
  | otherwise   = c : escapeLaTeXSpecials cs

--- Convert special HTML characters into their LaTeX representation,
--- if necessary.
htmlSpecialChars2tex :: String -> String
htmlSpecialChars2tex [] = []
htmlSpecialChars2tex (c:cs)
  | c==chr 228  = "\\\"a"  ++ htmlSpecialChars2tex cs
  | c==chr 246  = "\\\"o"  ++ htmlSpecialChars2tex cs
  | c==chr 252  = "\\\"u"  ++ htmlSpecialChars2tex cs
  | c==chr 196  = "\\\"A"  ++ htmlSpecialChars2tex cs
  | c==chr 214  = "\\\"O"  ++ htmlSpecialChars2tex cs
  | c==chr 220  = "\\\"U"  ++ htmlSpecialChars2tex cs
  | c==chr 223  = "\\ss{}" ++ htmlSpecialChars2tex cs
  | c=='&'      = let (special,rest) = break (==';') cs
                  in  if null rest
                      then "\\&" ++ htmlSpecialChars2tex special -- wrong format
                      else htmlspecial2tex special ++
                           htmlSpecialChars2tex (tail rest)
  | otherwise   = c : htmlSpecialChars2tex cs

htmlspecial2tex special
  | special=="Auml"   =  "{\\\"A}"
  | special=="Euml"   =  "{\\\"E}"
  | special=="Iuml"   =  "{\\\"I}"
  | special=="Ouml"   =  "{\\\"O}"
  | special=="Uuml"   =  "{\\\"U}"
  | special=="auml"   =  "{\\\"a}"
  | special=="euml"   =  "{\\\"e}"
  | special=="iuml"   =  "{\\\"\\i}"
  | special=="ouml"   =  "{\\\"o}"
  | special=="uuml"   =  "{\\\"u}"
  | special=="szlig"  =  "{\\ss}"
  | special=="Aacute" =  "{\\\'A}"
  | special=="Eacute" =  "{\\\'E}"
  | special=="Iacute" =  "{\\\'I}"
  | special=="Oacute" =  "{\\\'O}"
  | special=="Uacute" =  "{\\\'U}"
  | special=="aacute" =  "{\\\'a}"
  | special=="eacute" =  "{\\\'e}"
  | special=="iacute" =  "{\\\'\\i}"
  | special=="oacute" =  "{\\\'o}"
  | special=="uacute" =  "{\\\'u}"
  | special=="Agrave" =  "{\\`A}"
  | special=="Egrave" =  "{\\`E}"
  | special=="Igrave" =  "{\\`I}"
  | special=="Ograve" =  "{\\`O}"
  | special=="Ugrave" =  "{\\`U}"
  | special=="agrave" =  "{\\`a}"
  | special=="egrave" =  "{\\`e}"
  | special=="igrave" =  "{\\`\\i}"
  | special=="ograve" =  "{\\`o}"
  | special=="ugrave" =  "{\\`u}"
  | special=="Acirc"  =  "{\\^A}"
  | special=="Ecirc"  =  "{\\^E}"
  | special=="Icirc"  =  "{\\^I}"
  | special=="Ocirc"  =  "{\\^O}"
  | special=="Ucirc"  =  "{\\^U}"
  | special=="acirc"  =  "{\\^a}"
  | special=="ecirc"  =  "{\\^e}"
  | special=="icirc"  =  "{\\^\\i}"
  | special=="ocirc"  =  "{\\^o}"
  | special=="ucirc"  =  "{\\^u}"
  | special=="Oslash" =  "{\\O}"
  | special=="oslash" =  "{\\o}"
  | special=="amp"    =  "{\\&}"
  | special=="ntilde" =  "{\\~n}"
  | special=="otilde" =  "{\\~o}"
  | special=="ccedil" =  "{\\c{c}}"
  | special=="nbsp"   =  "~"
  | special=="quot"   =  "\""
  | special=="lt"     =  "{$<$}"
  | special=="gt"     =  "{$>$}"
  | otherwise = "\\&"++special++";"

------------------------------------------------------------------------------
--- Transforms HTML expressions into a string representation of a complete
--- LaTeX document.

showLatexDoc :: [HtmlExp] -> String
showLatexDoc htmlexps = showLatexDocs [htmlexps]

--- Transforms HTML expressions into a string representation of a complete
--- LaTeX document.
--- The variable "packages" holds the packages to add to the latex document
--- e.g. "ngerman"

showLatexDocWithPackages :: [HtmlExp] -> [String] -> String
showLatexDocWithPackages hexps packages
  = showLatexDocsWithPackages [hexps] packages

--- Transforms a list of HTML expressions into a string representation
--- of a complete LaTeX document where each list entry appears
--- on a separate page.

showLatexDocs :: [[HtmlExp]] -> String
showLatexDocs htmlexps_list = showLatexDocsWithPackages htmlexps_list []


--- Transforms a list of HTML expressions into a string representation
--- of a complete LaTeX document where each list entry appears
--- on a separate page.
--- The variable "packages" holds the packages to add to the latex document
--- (e.g., "ngerman").

showLatexDocsWithPackages :: [[HtmlExp]] -> [String] -> String
showLatexDocsWithPackages htmlexps_list packages =
 "\\documentclass[12pt]{article}\n"++
 concatMap (\p->"\\usepackage{"++p++"}\n") packages++
 -- Package longtable is added by default.
 "\\usepackage{longtable}"++
 "\\nonstopmode\n"++
 "\\setlength{\\topmargin}{ -1.5cm}\n"++
 "\\setlength{\\oddsidemargin}{0.0cm}\n"++
 "\\setlength{\\evensidemargin}{0.0cm}\n"++
 "\\setlength{\\marginparwidth}{0.0cm}\n"++
 "\\setlength{\\marginparsep}{0.0cm}\n"++
 "\\setlength{\\textwidth}{16.5cm}\n"++
 "\\setlength{\\textheight}{24.0cm}\n"++
 "\\pagestyle{empty}\n"++
 "\\begin{document}\n\\sloppy\n"++
 "\\addtolength{\\baselineskip}{0.0ex}\n"++
 "\\setlength{\\parindent}{0.0ex}\n"++
 "\\addtolength{\\parskip}{0.5ex}\n"++
 concat (intersperse "\\newpage\n" (map showLatexExps htmlexps_list))++
 "\\end{document}\n"

--- show german latex document
germanLatexDoc :: [HtmlExp] -> String
germanLatexDoc hexps = showLatexDocWithPackages hexps ["ngerman"]


------------------------------------------------------------------------------
--- Execute an HTML form in "interactive" mode.
intForm :: IO HtmlForm -> IO ()
intForm = intFormMain "" "" "" "" False ""
--intcgi = intFormMain "http://localhost/~mh/" "/home/mh/public_html/" "" "fwdcgienv.cgi" False ""

--- Execute an HTML form in "interactive" mode with various parameters.
--- @param baseurl  - the base URL where this script is accessible for clients
--- @param basecgi  - the base directory in the local file system where
---                   this script should stored for execution
--- @param reldir   - the relative path added to baseurl and basecgi
--- @param cginame  - the name of the executable cgi script
--- @param forever  - True if the interactive execution should not be terminated
---                   when the final web page (without a handler) is shown
--- @param urlparam - the URL parameter for the initial call to the cgi script
--- @param hformact - IO action returning the HTML form
intFormMain :: String -> String -> String -> String ->
              Bool -> String -> IO HtmlForm -> IO ()
intFormMain baseurl basecgi reldir cginame forever urlparam hformact = do
  pid      <- getPID
  user     <- getEnviron "USER"
  home     <- getEnviron "HOME"
  let portname = "intcgi_" ++ show pid
  socket <- listenOn portname
  let cgiprogname = if null cginame then "cgitest_"++show pid++".cgi"
                                    else cginame
      url = (if null baseurl then "http://localhost/~"++user else baseurl)
            ++ "/" ++ reldir ++ "/" ++ cgiprogname
      cgifile = (if null basecgi then home++"/public_html/" else basecgi++"/")++
                (if null reldir  then "" else reldir ++"/") ++ cgiprogname
      cgikey = url++" 42"
  installShowCgiEnvScript portname cgifile
  setEnviron "QUERY_STRING" urlparam
  time <- getClockTime
  intFormInEnv url cgikey hformact hformact [] (initialServerState time)
               forever socket
  system ("rm "++cgifile) >> done

intFormInEnv :: String -> String -> IO HtmlForm -> IO HtmlForm
          -> [(String,String)] -> ServerState -> Bool -> Socket -> IO ()
intFormInEnv url cgikey initform hformact cenv state forever socket = do
  if null cenv then putStrLn ">>> Start initial web form..." else done
  cform <- hformact
  let (cookiestring,hform) = extractCookies cform
  (htmlstring,evhs) <- showHtmlFormInEnv url "" (extendForm hform)
                                         (getMaxFieldNr cenv + 1)
  nstate <- storeEnvHandlers state
                (formWithMultipleHandlers hform)
                (encodeKey cgikey)
                (filter (\ (t,_) -> t/="DEFAULT" && take 6 t /= "EVENT_") cenv)
                evhs
  showHtmlStringInBrowser (cookiestring++htmlstring)
  if forever || formWithHandlers hform
   then do putStrLn ">>> Waiting for web page submission..."
           (_,hdl) <- socketAccept socket
           mbmsg <- readCgiServerMsg hdl
           maybe (intFormInEnv url cgikey initform hformact
                               cenv state forever socket)
                 (intFormProceed nstate hdl)
                 mbmsg
   else putStrLn ">>> Final web page reached"
 where
   intFormProceed nstate hdl (CgiSubmit scriptenv newcenv) = do
    hPutStrLn hdl answerTxt
    hClose hdl
    mapIO_ (\ (var,val) -> setEnviron var val) scriptenv
    if null newcenv -- call to initial script?
     then intFormInEnv url cgikey initform initform [] nstate forever socket
     else do
       (rstate,mfe) <- getNextFormAndCgiEnv nstate cgikey newcenv
       maybe (putStrLn "ERROR: no submission handler")
             (\ (ioform,env) -> intFormInEnv url cgikey initform ioform
                                             env rstate forever socket)
             mfe

   answerTxt = "Content-Type: text/html\n\n" ++
                showHtmlExp (italic [htxt "Waiting for next web form..."])

   extendForm orgform =
     orgform `addFormParam` HeadInclude (HtmlStruct "base" [("href",url)] [])

-- has an HTML form event handlers?
formWithHandlers (HtmlForm _ _ hexps) = hasHandlers hexps
 where
  hasHandlers :: [HtmlExp] -> Bool
  hasHandlers [] = False
  hasHandlers (HtmlText _ : hes) = hasHandlers hes
  hasHandlers (HtmlStruct _ _ hes1 : hes2) =
    hasHandlers hes1 || hasHandlers hes2
  hasHandlers (HtmlCRef he _ : hes) = hasHandlers [he] || hasHandlers hes
  hasHandlers (HtmlEvent _ _ : _) = True

--- Shows a string in HTML format in a browser.
showHtmlStringInBrowser htmlstring = do
  pid <- getPID
  let htmlfilename = "tmpcgiform_" ++ show pid ++ ".html"
  writeFile htmlfilename htmlstring
  system ("remote-netscape file:`pwd`/"++htmlfilename)
  done

-- install web script that forward user inputs:
installShowCgiEnvScript :: String -> String -> IO ()
installShowCgiEnvScript portname cgifile = do
  putStrLn ">>> Installing web script..."
  putStrLn $ "for port name: "++portname
  writeFile cgifile $ "#!/bin/sh\n"++
                      installDir++"/www/submitform \""++portname++"\"\n"
  system ("chmod 755 "++cgifile)
  done


------------------------------------------------------------------------------
-- The server for each dynamic web page manages the event handlers used in
-- dynamic web pages on the server side.
-- Each event handler is stored on the server side with a unique key.
-- Only this key is sent in the actual web page to the client.
-- Event handlers are only valid for a particular time period
-- specified by <code>eventHandlerExpiration</code>, i.e., after that time
-- event handlers will be deleted.

-- The structure of the internal state of the server:
-- Argument 1: Time when the server has been started.
-- Argument 2: Current index for numbering new events
-- Argument 3: Next date when cleanup is necessary
-- Argument 4: The current event handlers
--               (index,expiration date,cgikey,env,handler,groupindex)
--             where groupindex is Nothing for handlers with multiple use
--             and (Just gk) if the handlers should be deleted together
--             with all other handlers having the same groupindex
--             (usually, belonging to the same page)
type ServerState =
 (ClockTime, Int, ClockTime,
  [(Int,ClockTime,String,([(String,String)],HtmlHandler),Maybe Int)])

--- Creates a new state for a server started at some time.
initialServerState :: ClockTime -> ServerState
initialServerState ctime = (ctime, 0, nextCleanup ctime, [])

--- Is the list of event handlers of a server state empty?
isServerStateWithoutHandlers :: ServerState -> Bool
isServerStateWithoutHandlers (_,_,_,evhandlers) = null evhandlers

--- Gets a string describing the load of the server process.
--- If the server is "busy" it cannot accept further requests
--- for initial web pages.
getServerLoad :: ServerState -> IO String
getServerLoad (stime,maxkey,_,evs) = do
  ctime  <- getClockTime
  let busy = maxkey>500
             || (compareClockTime ctime (addMinutes 30 stime) == GT)
             || null evs -- since a server without handlers will be terminated
  return (if busy then "busy" else "ready")

--- Gets a string describing the status of the server process.
getServerStatus :: ServerState -> IO String
getServerStatus state@(stime,maxkey,_,evs) = do
  busy   <- getServerLoad state
  lstime <- toCalendarTime stime
  pinfos <- getProcessInfos
  return $ "Status: " ++ busy ++ ", Maxkey: "++show maxkey ++ ", #Handlers: " ++
           show (length evs) ++ ", Start time: " ++
           calendarTimeToString lstime ++ "\n" ++
           showMemInfo pinfos

--- Shows the group key of a handler as a string.
showGroupKey :: Maybe Int -> String
showGroupKey Nothing = "multiple use"
showGroupKey (Just gk) = "group " ++ show gk

--- Stores a list of new event handlers for a given cgi program and
--- the corresponding arguments with a new key.
--- The second argument is True if the event handlers should only be used once.
storeEnvHandlers :: ServerState -> Bool -> String -> [(String,String)]
                 -> [(HtmlHandler,String)] -> IO ServerState
storeEnvHandlers ostate multipleuse cgikey env handlerkeys = do
  time <- getClockTime
  cstate <- cleanOldEventHandlers ostate
  let nstate = generateEventServerMessages
                 (if multipleuse then Nothing else Just (keyOfState cstate))
                 (eventHandlerExpiration time)
                 cstate
                 handlerkeys
  seq nstate done -- to ensure that handler keys are instantiated
  return nstate
 where
   generateEventServerMessages _ _ state [] = state
   generateEventServerMessages groupkey expiredate state ((handler,hkey):evhs)
     | show (keyOfState state) ++ ' ':showQTerm (toUTCTime expiredate) =:= hkey
     = generateEventServerMessages
            groupkey
            expiredate
            (storeNewEnvEventWithCgiKey groupkey expiredate state env handler)
            evhs

   keyOfState (_,key,_,_) = key

   storeNewEnvEventWithCgiKey groupkey date (stime,maxkey,cleandate,ehs)
                              cenv info =
     (stime,
      if maxkey>30000 then 0 else maxkey+1, -- to avoid integer overflows
      cleandate,
      (maxkey,date,cgikey,(cenv,info),groupkey):ehs)

-- clean event handlers that are too old:
cleanOldEventHandlers :: ServerState -> IO ServerState
cleanOldEventHandlers state@(_,_,_,[]) = return state
cleanOldEventHandlers state@(stime,maxkey,cleandate,ehs@(_:_)) = do
  ctime <- getClockTime
  if compareClockTime ctime cleandate == LT
   then return state
   else do
     let currentehs = filter (isNotExpired ctime) ehs
         noehs = length ehs
         nocurrentehs = length currentehs
     if nocurrentehs < noehs
      then do -- report cleanup numbers:
        ltime <- toCalendarTime ctime
        putErrLn $ calendarTimeToString ltime ++ ": cleanup " ++
                   "(number of handlers: old = "++ show noehs ++ " / " ++
                   "current = "++ show nocurrentehs ++ ")"
      else done
     return (stime,maxkey, nextCleanup ctime, currentehs)
 where
  isNotExpired time (_,etime,_,_,_) = compareClockTime time etime == LT

-- Retrieves a previously stored event handler for a cgi program.
-- Returns Nothing if the handler is no longer available, i.e., expired.
retrieveEnvHandlers :: ServerState -> String -> String
                    -> IO (ServerState,Maybe ([(String,String)],HtmlHandler))
retrieveEnvHandlers state cgikey skey =
  let (numstring,datestring) = break (==' ') skey
      dateps = readsQTerm datestring
      num    = tryReadNat (-1) numstring
   in if null datestring || null dateps || num < 0
      then return (state,Nothing)
      else let (newstate,info) =
                   getEnvEventWithCgiKey state num (fst (head dateps))
            in seq newstate (return (newstate, info))
      -- the "seq"s are put here and below to enfore the evaluation of the
      -- new state in order to avoid space leaks with old, unused handlers
 where
  getEnvEventWithCgiKey oldstate@(stime,maxkey,cleandate,ehs) key date =
    maybe (oldstate,Nothing)
          (\ (evhdlr,groupkey) ->
            maybe (oldstate, Just evhdlr)
                  (\gk -> let newehs = deleteEv gk ehs
                           in seq newehs ((stime,maxkey,cleandate,newehs),
                                          Just evhdlr))
                  groupkey )
          (searchEv ehs)
   where
    -- search event handler
    searchEv [] = Nothing
    searchEv ((n,t,c,i,gk):es) =
      if key==n && date == toUTCTime t
      then if c==cgikey then Just (i,gk) else Nothing
      else searchEv es

    -- delete event handlers of the same group
    deleteEv _ [] = []
    deleteEv groupkey (ev@(_,_,_,_,Nothing):es) =
      let des = deleteEv groupkey es in seq des (ev : des)
    deleteEv groupkey (ev@(_,_,_,_,Just gk):es) =
      if groupkey==gk
      then deleteEvInGroup groupkey es
      else let des = deleteEv groupkey es in seq des (ev : des)

    deleteEvInGroup _ [] = []
    deleteEvInGroup _        (ev@(_,_,_,_,Nothing):es) = ev : es
    deleteEvInGroup groupkey (ev@(_,_,_,_,Just gk):es) =
      if groupkey==gk
      then deleteEvInGroup groupkey es
      else ev : es -- a new group has started so we stop the deletion


-- Define for a given date a new date when the event handler expires.
eventHandlerExpiration :: ClockTime -> ClockTime
eventHandlerExpiration = addHours 1
--eventHandlerExpiration = addMinutes 1

-- Define for a given date a new date when the next cleanup of event handlers
-- should be done.
nextCleanup :: ClockTime -> ClockTime
nextCleanup = addMinutes 5

---------------------------------------------------------------------------

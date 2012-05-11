---------------------------------------------------------------------------------
-- Definition of the datatypes for the various analyses contained in the browser.
---------------------------------------------------------------------------------

module AnalysisTypes(FunctionAnalysis(..),AnalysisResult(..),
                     ModuleAnalysis(..),ModuleAnalysisResult(..),
                     ContentsKind(..)) where

import FlatCurry

-----------------------------------------------------------------------------------
-- Types for analyzing functions:
-- Interface of various kinds of function analyses:
data FunctionAnalysis a =
   LocalAnalysis      (FuncDecl -> a)
 | LocalDataAnalysis  ([TypeDecl] -> FuncDecl -> a)
 | GlobalAnalysis     ([FuncDecl] -> [(QName,a)])
 | GlobalDataAnalysis ([TypeDecl] -> [FuncDecl] -> [(QName,a)])

-- The possible results of a function analysis:
data AnalysisResult =
   MsgResult String       -- a message to be shown in the browser
 | ActionResult (IO ())   -- an I/O action to show the result externally


-----------------------------------------------------------------------------------
-- Types for analyzing complete modules:
data ModuleAnalysis a =
   InterfaceAnalysis  (Prog -> a)   -- analysis based on the module interface
 | FlatCurryAnalysis  (Prog -> a)   -- analysis based on the FlatCurry representation
 | SourceCodeAnalysis (String -> IO a) -- analysis based on the module's source file
                                       -- (argument is the name of the source file)

-- The possible results of a module analysis:
data ModuleAnalysisResult =
   ContentsResult ContentsKind String  -- a program to be shown in main contents window
 | ModuleAction (IO ())                -- an I/O action to show the result externally


-- Kind of contents produced as the result of a module analysis
-- and shown in the main content window:
data ContentsKind =
    CurryProg      -- Curry source code
  | LCurryProg     -- Literate Curry source code
  | FlatCurryExp   -- FlatCurry expression
  | OtherText      -- some other text


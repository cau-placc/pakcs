-- Auxuiliaries to handle the parameters of the CurryDoc tool.

module CurryDocParams where

import System(system)

--------------------------------------------------------------------------
-- The kind of documentations which can be generated.
data DocType = HtmlDoc | TexDoc

-- The parameters for the documentation generator. Parameters:
-- doctype - the target format of the documentation
-- withindex - True if the index pages should also be generated
-- withmarkdown - True if the comments should be processed as markdown code
data DocParams = DocParams DocType Bool Bool

docType :: DocParams -> DocType
docType (DocParams dt _ _) = dt

setDocType :: DocType -> DocParams -> DocParams
setDocType dt (DocParams _ _ wmd) = DocParams dt (dt==HtmlDoc) wmd

withIndex :: DocParams -> Bool
withIndex (DocParams _ windex _) = windex

setIndex :: Bool -> DocParams -> DocParams
setIndex windex (DocParams dt _ wmd) = DocParams dt windex wmd

withMarkdown :: DocParams -> Bool
withMarkdown (DocParams _ _ wmd) = wmd

setMarkDown :: Bool -> DocParams -> DocParams
setMarkDown wmd (DocParams dt windex _) = DocParams dt windex wmd

-- Default parameters
defaultCurryDocParams = DocParams HtmlDoc True True

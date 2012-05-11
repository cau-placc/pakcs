------------------------------------------------------------------------------
--- Note: This library has been renamed into FlatCurryShow.
--- Look there for further documentation.
---
--- This module is only included for backward compatibility
--- and might be deleted in future releases.
---
--- Note that the function "writeFLC" contained in previous releases
--- is no longer supported. Use Flat2Fcy.writeFCY instead
--- and change file suffix into ".fcy"!
---
--- @author Michael Hanus
--- @version December 2005
------------------------------------------------------------------------------

module FlatCurryTools(showFlatProg,showFlatType,showFlatFunc,
                      showCurryType,showCurryExpr,showCurryId,showCurryVar)
   where

import FlatCurryShow

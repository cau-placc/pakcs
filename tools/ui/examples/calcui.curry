----------------------------------------------------------------------------
-- A simple desk calculator GUI where the local state is stored in an IORef.
----------------------------------------------------------------------------

--import UI      -- to compile without an executable implementation
import UI2GUI  -- to compile with a GUI implementation
--import UI2HTML -- to compile with a web implementation

import Char
import IOExts -- use IORefs for the GUI state 

-- the GUI needs a reference to the calculator state
calcUI :: IORef (Int,Int->Int) -> UIWidget
calcUI stateref =
  col [entryS [Class [Bg Yellow]] display "0",
       row (map cbutton ['1','2','3','+']),
       row (map cbutton ['4','5','6','-']),
       row (map cbutton ['7','8','9','*']),
       row (map cbutton ['C','0','=','/'])]
  where
    display free

    cbutton c = button (buttonPressed c) [c]

    buttonPressed c env = do
       state <- readIORef stateref
       let (d,f) = processButton c state
       writeIORef stateref (d,f)
       setValue display (show d) env

-- compute new state when a button is pressed:
processButton :: Char -> (Int,Int->Int) -> (Int,Int->Int)
processButton b (d,f)
 | isDigit b = (10*d + ord b - ord '0', f)
 | b=='+' = (0,((f d) + ))
 | b=='-' = (0,((f d) - ))
 | b=='*' = (0,((f d) * ))
 | b=='/' = (0,((f d) `div` ))
 | b=='=' = (f d, id)
 | b=='C' = (0, id)

main = do
  stateref <- newIORef (0,id)  
  runUI "Calculator" (calcUI stateref)

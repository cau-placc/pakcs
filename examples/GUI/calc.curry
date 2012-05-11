-- A simple desk calculator GUI where the local state is stored in an IORef.

import GUI
import Char
import IOExts -- use IORefs for the GUI state 

-- the GUI needs a reference to the calculator state
calcGUI :: IORef (Int,Int->Int) -> Widget
calcGUI stateref =
  Col [] [
    Entry [WRef display, Text "0", Background "yellow"],
    Row [] (map cbutton ['1','2','3','+']),
    Row [] (map cbutton ['4','5','6','-']),
    Row [] (map cbutton ['7','8','9','*']),
    Row [] (map cbutton ['C','0','=','/'])]
  where
    display free

    cbutton c = Button (buttonPressed c) [Text [c]]

    buttonPressed c gp = do
       state <- readIORef stateref
       let (d,f) = processButton c state
       writeIORef stateref (d,f)
       setValue display (show d) gp

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
  runGUI "Calculator" (calcGUI stateref)



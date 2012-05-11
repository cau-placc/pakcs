-- Drawing Hilbert curves in a canvas GUI

import GUI
import IOExts

---------------------------------------------------------------------
-- An implementation of a plotter.
-- A call (plotter pos gport wref color (tx,ty)) moves the plotter by (tx,ty)
-- and draws a line in canvas widget wref on GUI port gport (pos keeps
-- the internal plotter state, i.e., current coordinates).

plotter :: IORef (Int,Int) -> GuiPort -> WidgetRef -> String -> (Int,Int)
                                                             -> IO ()
plotter pos gport wref color (tx,ty) = do
  (x,y) <- readIORef pos
  addCanvas wref [CLine [(x,y),(x+tx,y+ty)] ("-fill "++color)] gport
  writeIORef pos (x+tx,y+ty)

---------------------------------------------------------------------

-- drawing Hilbert curves:
h=5

left  p = p (-h,0)
right p = p (h,0)
up    p = p (0,-h)
down  p = p (0,h)

data FigureType stroketype = Figure (FigureType stroketype) stroketype
                                    (FigureType stroketype) stroketype
                                    (FigureType stroketype) stroketype
                                    (FigureType stroketype)


fa = Figure fd left  fa down  fa right fb
fb = Figure fc up    fb right fb down  fa
fc = Figure fb right fc up    fc left  fd
fd = Figure fa down  fd left  fd up    fc


drawHilbert p order (Figure f1 s1 f2 s2 f3 s3 f4) =
  if order==0
  then done
  else drawHilbert p (order-1) f1 >> s1 p >>
       drawHilbert p (order-1) f2 >> s2 p >>
       drawHilbert p (order-1) f3 >> s3 p >>
       drawHilbert p (order-1) f4


hilbertWidget =
  Col [] [
    Label [Text "Drawing a Hilbert curve", WRef lt, Background "red"],
    Canvas [WRef cref, Height 330, Width 330],
    Row [] (map (\o -> Button (drawCurve o) [Text (show o)]) [1..6]),
    Button exitGUI [Text "Stop"]]
 where
   cref,lt free

   drawCurve o gport = do
     setValue lt ("Hilbert curve of order "++show o) gport
     pos <- newIORef (320,10)
     drawHilbert (plotter pos gport cref "red") o fa

main = runGUI "Hilbert Demo" hilbertWidget


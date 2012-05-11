-- Drawing Sierpinski curves in a canvas GUI

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
-- drawing Sierpinski curves:
h=3

left2  p = p (-(2*h),0)
right2 p = p (2*h,0)
up2    p = p (0,-(2*h))
down2  p = p (0,2*h)

leftdown  p = p (-h,h)
rightdown p = p (h,h)
leftup    p = p (-h,-h)
rightup   p = p (h,-h)


data FigureType stroketype = Figure (FigureType stroketype) stroketype
                                    (FigureType stroketype) stroketype
                                    (FigureType stroketype) stroketype
                                    (FigureType stroketype)


drawSierpinski p order (Figure f1 s1 f2 s2 f3 s3 f4) =
  if order==0
  then done
  else drawSierpinski p (order -1) f1 >> s1 p >>
       drawSierpinski p (order -1) f2 >> s2 p >>
       drawSierpinski p (order -1) f3 >> s3 p >>
       drawSierpinski p (order -1) f4


fa = Figure fa rightdown fb right2 fd rightup   fa
fb = Figure fb leftdown  fc down2  fa rightdown fb
fc = Figure fc leftup    fd left2  fb leftdown  fc
fd = Figure fd rightup   fa up2    fc leftup    fd

fs = Figure fa rightdown fb leftdown fc leftup fd -- rightup

sierpinskiWidget =
  Col [] [
    Label [Text "Drawing a Sierpinski curve", WRef lt, Background "red"],
    Canvas [WRef cref, Height 400, Width 400],
    Row [] (map (\o -> Button (drawCurve o) [Text (show o)]) [1..6]),
    Button exitGUI [Text "Stop"]]
 where
   cref,lt free

   drawCurve o gport = do
     setValue lt ("Sierpinski curve of order "++show o) gport
     pos <- newIORef (10,10)
     let p = plotter pos gport cref "red"
     drawSierpinski p o fs
     rightup p

main = runGUI "Sierpinski Demo" sierpinskiWidget


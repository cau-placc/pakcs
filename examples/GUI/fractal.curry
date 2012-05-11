-- The beauty of fractals:

-- with a "plotter" object which directly writes to a GUI canvas:


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
-- drawing fractal curves:

left  p h = p (-h,0)
right p h = p (h,0)
up    p h = p (0,-h)
down  p h = p (0,h)

data FigureType stroketype = Figure stroketype (FigureType stroketype)
                                               (FigureType stroketype)
                                               (FigureType stroketype)



fr = Figure right fr fu fd
fu = Figure up    fu fl fr
fd = Figure down  fd fr fl
fl = Figure left  fl fd fu


draw p order diff (Figure s f1 f2 f3) =
  if order==0 then s p diff
              else do draw p (order-1) h     f1
                      draw p (order-1) (h-1) f2
                      draw p (order-1) h     f1
                      draw p (order-1) (h-1) f3
                      draw p (order-1) h     f1
                      s p (diff-3*h)  -- to avoid rounding problems
  where h = diff `div` 3

draw_all p order diff = do draw p order diff fr
                           draw p order diff fd
                           draw p order diff fl
                           draw p order diff fu

fractalWidget =
  Col [] [
    Label [Text "Drawing a simple fractal curve:"],
    Row []
      ([Label [Text "Select the order of the fractal:"]] ++
       map (\o -> Button (drawFractal o) [Text (show o)]) [2..5]),
    Canvas [WRef cref, Background "white", Height 600, Width 600],
    Button exitGUI [Text "Stop"]]

 where
  cref free

  drawFractal order gport = do
    pos <- newIORef (150,150)
    draw_all (plotter pos gport cref (color order)) order 300
 
  color order = if order==2 then "green" else
                if order==3 then "blue"  else "red"


main = runGUI "Fractal Demo" fractalWidget

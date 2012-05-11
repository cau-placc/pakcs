------------------------------------------------------------------------------
-- A simple counter demo demonstrating the handling of different
-- events (e.g., left and right mouse buttons) inside one widget.
------------------------------------------------------------------------------

import GUI
import Read

counterGUI =
 Col [] [
   Label [Text "A simple counter:"],
   Row []
    [Entry [WRef val, Text "5", Background "yellow"],
            PlainButton
               [-- the standard event handler for the left button:
                Cmd (updateValue decrText val),
                -- an event handler for the right button:
                Handler MouseButton3 (\gp->updateValue incrText val gp >> return []),
                Text "Decrement/Increment"]],
   Row []
    [Label [Text "Text of stop button:"],
     Entry [WRef txt, Text "Stop", Background "yellow",
            -- The standard event handler for Entry widget is invoked only on <Return>,
            -- therefore we define a event handler for every <KeyPress>:
            Handler KeyPress (\gp->getValue txt gp >>= \v->
                                   setConfig stopbutton (Text v) gp >> return [])]],
   Button exitGUI [WRef stopbutton, Text "Stop"]]
 where
   val,txt,stopbutton free

   decrText s = show (readInt s - 1)

   incrText s = show (readInt s + 1)

main = runGUI "Event Demo" counterGUI


-- a simple counter GUI to demonstrate the use of menu buttons

import GUI
import Read

counterGUI =
 Col [] [
   Label [Text "A simple counter:"],
   Row []
    [Entry [WRef val, Text "0", Background "yellow"],
     MenuButton [Text "Menu",
                   Menu [MButton increment "Increment",
                         MButton (setTo 0) "Reset",
                         MMenuButton "Menu again"
                             [MButton increment "Increment",
                              MButton (setTo 0) "Reset"],
                         MSeparator,
                           MMenuButton "Menu reverse"
                             [MButton (setTo 0) "Reset",
                              MButton increment "Increment"],
                         MSeparator,
                         MMenuButton "Menu 42"
                           [MButton (setTo 42) "Set",
                            MButton increment "Increment"]]]],
   Row [] [ConfigButton increment [Text "Increment"],
           ConfigButton (setTo 0) [Text "Reset"],
           Button exitGUI [Text "Stop"]]]
 where
   val free

   increment gport = updateValue incrText val gport >> return []

   setTo n gport = setValue val (show n) gport >> return []

incrText s = show (readInt s + 1)

main = runGUI "Counter Demo with Menu" counterGUI


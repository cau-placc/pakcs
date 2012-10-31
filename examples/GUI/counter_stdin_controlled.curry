-- A simple counter demo for the GUI library.
--
-- This counter GUI can be controlled not only by GUI buttons
-- but also by inputs from the terminal.
--
-- IMPORTANT NOTE: Due to a bug in older versions of Sicstus-Prolog,
-- you need version 3.8.5 (or newer) to execute this program
-- (without "segmentation violation")

import IO
import GUI
import Read

-- The definition of the counter GUI together with a handler
-- "ext_handler" that is responsible to handle the external messages:
counterGUI =
 (Col [] [
   Label [Text "A simple counter:"],
   Entry [WRef val, Text "0", Background "yellow"],
   Row [] [Button (updateValue incrText val) [Text "Increment"],
           Button (setValue val "0")         [Text "Reset"],
           Button exitGUI                    [Text "Stop"]]],
  [ext_handler])

 where
   val free

   incrText s = show (readInt s + 1)

   ext_handler h gp = do
     l <- hGetLine h
     if l=="stop" then exitGUI gp
                  else setValue val l gp
     return []

-- start the counter GUI:
main = do
  putStrLn "Input numbers (one per line) to be shown in the Counter GUI:"
  putStrLn "(to terminate the application, input \"stop\")"
  runHandlesControlledGUI "Counter Demo" counterGUI [stdin]


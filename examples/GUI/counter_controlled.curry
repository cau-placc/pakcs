-- A simple counter demo for the GUI library:
--
-- This counter GUI can be controlled not only by the user
-- but also by external processes that send messages to the GUI
--
-- IMPORTANT NOTE: Due to a bug in older versions of Sicstus-Prolog,
-- you need version 3.8.5 (or newer) to execute this program
-- (without "segmentation violation")

import Ports
import GUI
import Read

-- The messages that can be sent to the counter GUI:
data Msg = Set Int

-- The definition of the counter GUI together with a handler
-- "ext_handler" that is responsible to handler the external messages:
counter_gui =
 (Col [] [
   Label [Text "A simple counter:"],
   Entry [WRef val, Text "0", Background "yellow"],
   Row [] [Button (updateValue incrText val) [Text "Increment"],
           Button (setValue val "0")         [Text "Reset"],
           Button exitGUI                    [Text "Stop"]]], ext_handler)

     where val free

           incrText s = show (readInt s + 1)

           ext_handler (Set v) gp = setValue val (show v) gp

-- start the counter GUI: messages can be sent to this GUI
-- via the external port "test@<this_machine>":
main =
 do msgs <- openNamedPort "test"
    runControlledGUI "Counter Demo" counter_gui msgs

-- the value of the counter can be externally set to i by this function:
-- (change the value "localhost" to the name of your machine)
client i =
 do p <- connectPort "test@localhost"
    doSend (Set i) p


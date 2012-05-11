----------------------------------------------------------------------------
-- A UI consisting of four independent simple counters.
----------------------------------------------------------------------------

--import UI      -- to compile without an executable implementation
import UI2GUI  -- to compile with a GUI implementation
--import UI2HTML -- to compile with a web implementation

import Read

counterUI :: UIWidget
counterUI = col [label "A simple counter:",
                 entry val "0" `addStyle` Class [Bg White],
                 row [button inc    "Increment",
                      button reset  "Reset",
                      button exitUI "Stop" ]]
 where
   val free

   reset env = setValue val "0" env  

   inc env = do v <- getValue val env
                setValue val (show (readInt v + 1)) env


counter4 = col [row [counterUI,counterUI],
                row [counterUI,counterUI]]

main = runUI "4 counters" counter4

----------------------------------------------------------------------------
-- A simple counter example for declarative UI programming.
----------------------------------------------------------------------------

--import UI      -- to compile without an executable implementation
import UI2GUI  -- to compile with a GUI implementation
--import UI2HTML -- to compile with a web implementation

import Read

counterUI :: UIWidget
counterUI = col [label "A simple counter:",
                 entry val "0",
                 row [button incr   "Increment",
                      button reset  "Reset",
                      button exitUI "Stop"]]    
  where
    val :: UIRef  
    val free

    reset :: UIEnv -> IO ()  
    reset env = setValue val "0" env        

    incr env = do v <- getValue val env
                  setValue val (show (readInt v + 1)) env

main = runUI "Counter Demo" counterUI

----------------------------------------------------------------------------
-- A type-safe counter UI.
----------------------------------------------------------------------------

--import TypedUI2GUI
import TypedUI2HTML


counters :: [Int] -> UIWidget
counters xs =
   col [label "A list of counters:",
        widget,
        row [button (updval (map (+1))) "Increment all",
             button (setval (repeat 0)) "Reset all",
             button compute "Compute sum:", entry sval ""]]
  where
    sval free

    (widget,getval,setval,updval) = typedui2ui (wList wInt) xs
  
    compute env = do cs <- getval env
                     setValue sval (maybe "" (show . sum) cs) env
  

main = runUI "Counters" (counters [1..4])

sum = foldr (+) 0

-- Generate with
-- export CURRYPATH=.. && makecurrycgi -o ~/public_html/tcounter.cgi tcounter

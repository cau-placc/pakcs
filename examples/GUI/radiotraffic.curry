-- an implementation of radiobuttons with checkbuttons:

import GUI

-- column of radiobuttons:
-- (radioButtonCol <WRef to radio> <labels> <command>)
radioButtonCol rbs names cmd
 | rbs =:= gen_vars n  = Col [LeftAlign] (gen_rb 0)
 where n = length names
       gen_rb i =
         if i==n
         then []
         else CheckButton [Text (names!!i), WRef (rbs!!i),
                           Cmd (rbcmd (rbs!!i) (remove_ith i rbs) cmd)]
              : gen_rb (i+1)

-- radiobutton command:
-- if button selected -> deselect others and check constraint
rbcmd vsel vothers cmd wp =
  do sel <- getValue vsel wp
     if sel=="1" then mapIO_ (\vo->setValue vo "0" wp) vothers
                 else done
     cmd wp

-- generate the n different unbound variables:
gen_vars n = if n==0 then [] else unknown : gen_vars (n-1)

-- remove i-th element in a list:
remove_ith _ [] = []
remove_ith i (x:xs) = if i==0 then xs else x : remove_ith (i-1) xs

getRadioValue [] _ = return (-1)
getRadioValue (r:rs) wp = do
  rval <- getValue r wp
  if rval=="1" then return 0
               else do rspos <- getRadioValue rs wp
                       return (if rspos>=0 then rspos+1 else -1)

setRadioValue [] _ _ = done
setRadioValue (r:rs) i wp =
  do setValue r (if i==0 then "1" else "0") wp
     setRadioValue rs (i-1) wp



-- a simple example: a traffic light controller:
traffic =
 Row [] [radioButtonCol tr1 ["Red","Yellow","Green"] (excl tr1 tr2),
         radioButtonCol tr2 ["Red","Yellow","Green"] (excl tr2 tr1)]
  where tr1,tr2 free

excl tr1 tr2 wp =
 do sel <- getRadioValue tr1 wp
    if sel>=0 then setRadioValue tr2 (2-sel) wp else done

main = runGUI "Traffic Light" traffic


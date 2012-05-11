-- A simple GUI demonstrating the use of "appendStyledValue"
-- in TextEdit widgets:

import GUI

addlineGUI =
 Col [] [
   Label [Text "A simple GUI for appending styled lines:"],
   Row [] [Label [Text "Line to be appended in window below:"],
           Entry [WRef rline, Width 30, Background "white", Cmd addline]],
   Row [] [Label [Text "Select text style:"],
           CheckButton [Text "underline", WRef cundl],
           CheckButton [Text "yellow background", WRef cback],
           CheckButton [Text "red face", WRef cred]],
   TextEditScroll [WRef rtxt, Background "white"],
   Button exitGUI [Text "Exit"]]
 where
   rline,rtxt,cundl,cback,cred free

   addline gp = do
    vundl <- getValue cundl gp
    vback <- getValue cback gp
    vred  <- getValue cred  gp
    line <- getValue rline gp
    let style = (if vundl=="1" then [Underline] else []) ++
                (if vback=="1" then [Bg Yellow] else []) ++
                (if vred =="1" then [Fg Red]    else [])
    appendStyledValue rtxt (line++"\n") style gp

main = runGUI "appendStyledValue Demo" addlineGUI



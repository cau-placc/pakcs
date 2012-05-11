-- A simple demo for dynamic reconfiguration of widget options:

import GUI

widget =
 Col [] [
   Label [Text "Text of buttons below:"],
   EntryScroll [WRef tentry, Text "Stop", Background "white",
                Command changetext],
   Label [Text "Background color of exit button below:"],
   Row []
     [Entry [WRef centry, Text "red", Background "white",
             Command changecolor],
      PlainButton [Command changecolorp, Text "Select from palette"]],
   CheckButton [WRef actbutton, Text "Deactivate buttons below",
                Command deactivate],
   Button (\_->putStrLn "XXX") [WRef prtbutton, Text "Print 'XXX'"],
   Button exitGUI [WRef stopbutton, Text "Stop", Background "red"]]

  where
    tentry,centry,actbutton,prtbutton,stopbutton free

    changetext wp = do
      text <- getValue tentry wp
      focusInput centry wp
      return
       [WidgetConf stopbutton (Text text), -- change button text
        WidgetConf prtbutton (Text ("Print '"++text++"'")), --change button text
        WidgetConf prtbutton (Cmd (\_->putStrLn text))] -- change button command

    changecolor wp = do
       color <- getValue centry wp
       return [WidgetConf stopbutton (Background color)]

    changecolorp _ = do
       color <- chooseColor
       return [WidgetConf stopbutton (Background color)]

    deactivate wp = do
       act <- getValue actbutton wp
       return [WidgetConf stopbutton (Active (act=="0")),
               WidgetConf prtbutton (Text ""),
               WidgetConf prtbutton (Cmd (\_->done))]


main = runGUI "Config Demo" widget


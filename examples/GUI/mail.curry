-- a simple mail widget to write and send an email

import GUI
import System

mailWidget =
 Col [] [
   Label [Text "A simple mail composer:"],
   Matrix [LeftAlign] [[Label [Text "To:"],
                        Entry [WRef rto, FillX]],
                       [Label [Text "Subject:"],
                        Entry [WRef rsubject, FillX]]],
   TextEdit [WRef rtxt, Background "yellow", Fill],
   Row [] [Button (sendmail rto rsubject rtxt) [Text "Send"],
           Button exitGUI [Text "Cancel"]]
   ]
 where
   rto,rsubject,rtxt free

   sendmail to sub txt gport =
    do to_cont <- getValue to gport
       sub_cont <- getValue sub gport
       body <- getValue txt gport
       writeFile "tmp.mailtext" body
       system ("mailx -s \""++sub_cont++"\" "++to_cont++" < tmp.mailtext")
       system "rm tmp.mailtext"
       exitGUI gport


main = runGUI "Mail Demo" mailWidget



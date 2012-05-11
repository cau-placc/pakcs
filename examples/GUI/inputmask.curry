-- This program is a demo of the Matrix construct
-- A Matrix is useful, when widgets should oriented in a table-like style.

import GUI
import Read
import ReadShowTerm
import IOExts

data Person = Person String String String String String String String String 

widget (Person name1 fname1 uni1 city1 street1 nr1 zip1 country1) noe =
 Col [LeftAlign] [
   Row []
     [Label [Text "Entry #"], Label [WRef nrlab,Text "1"],
      Label [Text "/"], Label [WRef nrOfEntries,Text (show noe)]],
   Matrix [LeftAlign] 
     [
      [Label [Text "Surname:"], 
       Entry [WRef name, Width 30,FillX, Text name1],
       Label [Text "Forename:"], 
       Entry [WRef fname, Width 30,FillX, Text fname1]],
      [Label [Text "University:"], 
       Entry [WRef uni, Width 30,FillX, Text uni1],
       Label [Text "City:"], 
       Entry [WRef city, Width 30,FillX, Text city1]],
      [Label [Text "Street:"], 
       Row [LeftAlign] 
           [Entry [WRef street, Width 30,FillX, Text street1],
            Label [Text "Nr:"], 
            Entry [WRef nr, Width 5,FillX, Text nr1]],
       Label [Text "Zip Code:"], 
       Entry [WRef zip, Width 30,FillX, Text zip1]],
      [Label [Text "Country:"], 
       Entry [WRef country, Width 30,FillX, Text country1]]
     ],Label [FillX],
   Row []
     [ConfigButton new  [Text "New"], Label [FillX],
      ConfigButton prev [WRef prevbutton, Text "<<", Active False],
      Label [FillX],
      Button save [Text "Save"], Label [FillX],
      ConfigButton next [WRef nextbutton, Text ">>", Active (noe>1)],
      Label [FillX],
      Button exitGUI [Text "Exit"]
     ]
   ]
 where street,zip,name,fname,uni,city,nr,country,nrlab,
              nextbutton,prevbutton,nrOfEntries free

       next wp =    getPerson (+1) wp         >>= \person 
                 -> setPerson (+1) person wp  >>= \number 
                 -> getValue nrOfEntries wp >>= \noe_cont 
                 -> return $ (if number == readNat noe_cont
                              then [WidgetConf nextbutton (Active False)]
                              else []) ++
                             [WidgetConf prevbutton (Active True)]

       prev wp =    getPerson (+(-1)) wp         >>= \person 
                 -> setPerson (+(-1)) person wp  >>= \number 
                 -> return $ (if number == 1
                              then [WidgetConf prevbutton (Active False)]
                              else []) ++
                             [WidgetConf nextbutton (Active True)]

       getPerson f wp = readFile pFile >>= \fileconts ->
                        let persons = map readQTerm (lines fileconts) in
                        getValue nrlab wp >>= \nrlab_cont ->
                        return $ persons !! (f (readNat nrlab_cont) -1)
               
       setPerson f (Person pname pfname puni pcity pstreet pnr pzip pcountry) wp =
          setValue name    pname    wp >>
          setValue fname   pfname   wp >> 
          setValue uni     puni     wp >>
          setValue city    pcity    wp >>
          setValue street  pstreet  wp >>
          setValue nr      pnr      wp >>
          setValue zip     pzip     wp >>
          setValue country pcountry wp >>
          getValue nrlab wp >>= \nrlab_cont ->
          let number = f $ readNat nrlab_cont in
          setValue nrlab (show number) wp >>
          return number

       save wp = getValue name wp    >>= \name_cont    ->
                 getValue fname wp   >>= \fname_cont   ->    
                 getValue uni wp     >>= \uni_cont    ->
                 getValue city wp    >>= \city_cont    ->     
                 getValue street wp  >>= \street_cont  ->       
                 getValue nr wp      >>= \nr_cont      ->   
                 getValue zip wp     >>= \zip_cont     ->    
                 getValue country wp >>= \country_cont ->
                 getValue nrlab wp   >>= \nrlab_cont   ->
                 getValue nrOfEntries wp >>= \noe_cont ->
                 let number = readNat nrlab_cont 
                     person = Person name_cont fname_cont uni_cont 
                                     city_cont street_cont nr_cont 
                                     zip_cont country_cont 
                 in
                 if number > readNat noe_cont
                   then appendFile pFile (showQTerm person ++ "\n")
                   else readCompleteFile pFile >>= \conts ->
                        let ls = lines conts in 
                        writeFile pFile 
                          (unlines (take (number - 1) ls ++ 
                                   [showQTerm person] ++ 
                                    drop number ls))

       new wp = getValue nrOfEntries wp >>= \noe_cont ->
                let max = readNat noe_cont in
                setValue nrOfEntries (show $ max+1) wp >>
                setPerson (\_->max+1) empty wp >>
                appendFile pFile (showQTerm empty ++ "\n") >>
                return [WidgetConf prevbutton (Active True),
                        WidgetConf nextbutton (Active False)]
             
main = appendFile pFile "" >>
       readFile pFile >>= \conts ->
       let exists = conts/="" in
       (if not exists 
         then writeFile pFile (unlines (map showQTerm someCurryHeroes)) >>
              return someCurryHeroes
         else return (map readQTerm (lines conts))) >>= \persons ->
       let person = head persons 
           nrOfEntries = length persons 
       in
       runGUI "A simple Entry Form in table Style" $ 
              widget person nrOfEntries


pFile = "tmp.matrixdemodata" 

empty = Person "" "" "" "" "" "" "" ""

someCurryHeroes = 
  [Person "Hanus" "Michael" "Christian-Albrechts-University" "Kiel" "Ohlshausener Straﬂe" "40" "D-24098" "Germany",
   Person "Antoy" "Sergio" "Portland State University" "Portland, Oregon" "SW 4th Avenue" "1900" "OR 97207-0751" "USA",
   Person "Vidal Oriola" "Germ·n F. " "Universidad PolitÈcnica" "Valencia" "Camino de Vera" "s/n" "E-46022" "Spain",
   Person "Huch" "Frank" "Christian-Albrechts-University" "Kiel" "Ohlshausener Straﬂe" "40" "D-24098" "Germany",
   Person "Braﬂel" "Bernd" "Christian-Albrechts-University" "Kiel" "Ohlshausener Straﬂe" "40" "D-24098" "Germany"]


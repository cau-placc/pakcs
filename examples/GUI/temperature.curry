-- temperature converter

import GUI
import Read

-- only a scale for Celsius:
temp_widget =
  Col [] [
    Label [Text "Temperature in Celsius:"],
    Scale 0 100 [WRef cels, Cmd convert],
    Row [] [Label [Text "Fahrenheit: "],
            Message [WRef fahr, Background "white"],
            Label [Text "Kelvin: "],
            Message [WRef kelv, Background "white"]],
    Button exitGUI [Text "Stop"]]

 where
   cels,fahr,kelv free

   convert wp = do cs <- getValue cels wp
                   let c = readInt cs
                   setValue fahr (show (c * 9 `div` 5 + 32)) wp
                   setValue kelv (show (c + 273)) wp

main = runGUI "Temperature Conversion" temp_widget



-- a scale for Celsius and a scale for Fahrenheit:
temp_widget2 =
  Col [] [
    Label [Text "Temperature in Celsius:"],
    Scale 0 100 [WRef cels, Cmd convertC],
    Row [] [Label [Text "Fahrenheit: "],
            Message [WRef fahr, Background "white"],
            Label [Text "Kelvin: "],
            Message [WRef kelv, Background "white"]],
    Label [Text "Temperature in Fahrenheit:"],
    Scale 0 212 [WRef fscl, Cmd convertF],
    Button exitGUI [Text "Stop"]]

 where
   cels,fahr,kelv,fscl free

   convertC wp = do cs <- getValue cels wp
                    let c = readInt cs
                    setValue fahr (show (c * 9 `div` 5 + 32)) wp
                    setValue kelv (show (c + 273)) wp
                    setValue fscl (show (c * 9 `div` 5 + 32)) wp

   convertF wp = do fs <- getValue fscl wp
                    let c = ((readInt fs)-32) * 5 `div` 9
                    setValue cels (show c) wp
                    setValue fahr (show (c * 9 `div` 5 + 32)) wp
                    setValue kelv (show (c + 273)) wp

main2 = runGUI "Temperature Conversion" temp_widget2


import Text.Show.Functions

data Pirata = Pirata {
	nombrePirata :: String,
	botinPirata :: [String],
	valorBotin ::	[Int],
import Text.Show.Functions
import Data.List

type ValorBotin = Int


data Pirata = Pirata {
 nombrePirata :: String,
 botinPirata :: [(String, Int)],
 nombreBarcoEmbarcado :: String
} deriving (Eq, Show)

jack = Pirata {
 nombrePirata = "Jack Sparrow",
 botinPirata = [("Sombrero", 100),("botella de Ron", 150)],
 nombreBarcoEmbarcado =  "Perla Negra"
}

david = Pirata {
 nombrePirata = "David Jones",
 botinPirata = [("Doblones de Oro", 50000),("botella de Arena", 1)],
 nombreBarcoEmbarcado =  "Perla Negra"
}

anne = Pirata {
 nombrePirata = "Anne Bonny",
 botinPirata = [("Caja Musical", 60),("botella de Ron", 200)],
 nombreBarcoEmbarcado =  "Holandes Errante"
}

will = Pirata {
 nombrePirata = "Will Turner",
 botinPirata = [],
 nombreBarcoEmbarcado =  ""
}

elizabeth = Pirata {
 nombrePirata = "Elizabeth Swann",
 botinPirata = [("Sombrero", 200),("Doblones de plata", 30000)],
 nombreBarcoEmbarcado =  "Holandes Errante"
}

cantidadDeTesorosDePirata pirata = length (botinPirata pirata)

valoresTesorosPirata pirata = map snd (botinPirata pirata ) 

sumaTesorosPirata pirata = sum (valoresTesorosPirata pirata )

esRico pirata = sumaTesorosPirata pirata > 9999

valorTesoroMasValioso pirata = maximum (valoresTesorosPirata pirata)

agregarTesoro pirata nombreTesoro valorBotin = (nombreTesoro,valorBotin) : botinPirata pirata


abordarBarco pirata nombreBarcoEmbarcadoNuevo = let nombreBarcoEmbarcado pirata = nombreBarcoEmbarcadoNuevo

desabordarBarco pirata = " "  = nombreBarcoEmbarcado pirata


import Text.Show.Functions
import Data.List

type ValorBotin = Int


data Pirata = Pirata {
 nombrePirata :: String,
 botinPirata :: [(String, Int)],
 nombreBarcoEmbarcado :: String
} deriving (Eq, Show)

data Barco = Barco {
 nombreBarco :: String
 --piratasEmbarcados :: [Pirata]
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

holandes = Barco {
 nombreBarco = "Holandes Errante"
 --piratasEmbarcados = []
} 

perlaNegra = Barco {
 nombreBarco = "Perla Negra"
 --piratasEmbarcados = []
} 

primero (x,_) = x
segundo (_,x) = x

nombreTesoro pirata = map primero (botinPirata pirata )

cantidadDeTesorosDePirata pirata = length (botinPirata pirata)

valoresTesorosPirata pirata = map snd (botinPirata pirata ) 

sumaTesorosPirata pirata = sum (valoresTesorosPirata pirata )

esRico pirata = sumaTesorosPirata pirata > 9999

valorTesoroMasValioso pirata = maximum (valoresTesorosPirata pirata)

agregarTesoro pirata nombreTesoro valorBotin = (nombreTesoro,valorBotin) : botinPirata pirata

--tienenMismoTesoro :: (Pirata->Pirata) -> bool
tienenTesorosDeIgualNombre pirata pirata2 = any(==True)(zipWith (==)(nombreTesoro pirata)(nombreTesoro pirata2))

tienenMismoTesoroDeIgualValor pirata pirata2 =  any(==True) (zipWith (/=)(valoresTesorosPirata pirata) (valoresTesorosPirata pirata2)) 

tienenTesorosIgualesYDiferenteValor pirata pirata2 =  (tienenMismoTesoroDeIgualValor pirata pirata2 &&(tienenTesorosDeIgualNombre pirata pirata2) )

sacarConValorMayor100 pirata = filter  ((<100).segundo)   (botinPirata pirata) 


sacarSiSeLLama pirata nombreTesoroPerder = filter  ((/=nombreTesoroPerder).primero)   (botinPirata pirata) 
--perderTesoroValioso pirata = map filter (tieneValorMayor100 pirata)



















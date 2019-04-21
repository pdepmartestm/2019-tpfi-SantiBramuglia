import Text.Show.Functions
import Data.List

type ValorBotin = Int


data Pirata = Pirata {
 nombrePirata :: String,
 botinPirata :: [(String, Int)],
 formaSaqueo :: FormaSaquear
}deriving ( Show)

data Barco = Barco {
 nombreBarco :: String,
 piratasEmbarcados :: [Pirata]
} deriving ( Show)


jack = Pirata {
 nombrePirata = "Jack Sparrow",
 botinPirata = [("Sombrero", 100),("botella de Ron", 150)],
 formaSaqueo = mayoresDe100
}

david = Pirata {
 nombrePirata = "David Jones",
 botinPirata = [("Doblones de Oro", 50000),("botella de Arena", 1)],
 formaSaqueo = saqueoComplejo
 
}

anne = Pirata {
 nombrePirata = "Anne Bonny",
 botinPirata = [("Caja Musical", 60),("botella de Ron", 200)],
 formaSaqueo = palabraClave
 }

will = Pirata {
 nombrePirata = "Will Turner",
 botinPirata = [],
 formaSaqueo = noSaquear
}

elizabeth = Pirata {
 nombrePirata = "Elizabeth Swann",
 botinPirata = [("Sombrero", 200),("Doblones de plata", 30000)],
 formaSaqueo = mayoresDe100
}

holandes = Barco {
 nombreBarco = "Holandes Errante",
 piratasEmbarcados = [jack, david]		
} 

perlaNegra = Barco {
 nombreBarco = "Perla Negra",
 piratasEmbarcados = [elizabeth,will]
} 

primero (x,_) = x
segundo (_,x) = x

--Agragar Devolver Pirata completo 
devolverPirataModificado pirata = pirata



nombreTesoro pirata = map primero (botinPirata pirata )

cantidadDeTesorosDePirata pirata = length (botinPirata pirata)

valoresTesorosPirata pirata = map snd (botinPirata pirata ) 

sumaTesorosPirata pirata = sum (valoresTesorosPirata pirata )

esRico pirata = sumaTesorosPirata pirata > 9999

valorTesoroMasValioso pirata = maximum (valoresTesorosPirata pirata)

agregarTesoro pirata (nombreTesoro, valorBotin) = ((nombreTesoro,valorBotin) : botinPirata pirata)  -- le dan TUPLA devuelve pirata 

tienenTesorosDeIgualNombre pirata pirata2 = any (==True)(zipWith (==)(nombreTesoro pirata)(nombreTesoro pirata2)) --MODIFICAR

tienenMismoTesoroDeIgualValor pirata pirata2 = any (==True) (zipWith (/=)(valoresTesorosPirata pirata) (valoresTesorosPirata pirata2)) 

tienenTesorosIgualesYDiferenteValor pirata pirata2 =  (tienenMismoTesoroDeIgualValor pirata pirata2 &&(tienenTesorosDeIgualNombre pirata pirata2) )

sacarConValorMayor100 pirata = filter  ((<100).segundo)   (botinPirata pirata) 

sacarTesoroSiSeLLama pirata nombreTesoroPerder = filter  ((/=nombreTesoroPerder).primero)   (botinPirata pirata) 


--TEMPORADA DE TESOROS REVISAR 

type FormaSaquear = Pirata -> [(String,Int)]


--saqueo pirata = saquearSegun pirata 


saqueo :: FormaSaquear
saqueo formaSaqueo pirata (nombreTesoro, valorBotin) | formaSaqueo pirata (nombreTesoro, valorBotin) = agregarTesoro pirata (nombreTesoro,  valorBotin)
									                   | otherwise = botinPirata pirata

mayoresDe100 :: FormaSaquear	
mayoresDe100 pirata (nombreTesoro,valorBotin) | (>100) valorBotin = agregarTesoro pirata (nombreTesoro, valorBotin)
														 | otherwise = botinPirata pirata 
														 
														 
														 
--soloTomaConNombre pirata nombreRoboPirata = pasar la palabra clave y que de ahi se cree la condicion 



--tomarConNombre pirata (nombreTesoro, valorBotin)  | (==nombreTesoroTomar) soloRoba) = (agregarTesoro pirata (nombreTesoro, valorBotin))
--										| otherwise = botinPirata pirata
	
noSaquear :: FormaSaquear
noSaquear pirata  =  botinPirata pirata

--saqueoComplejo pirata

-- saquear jack (saquearPorNombre "oro")













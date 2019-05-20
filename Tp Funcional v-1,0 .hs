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
 formaSaqueo = noSaquear
}

david = Pirata {
 nombrePirata = "David Jones",
 botinPirata = [("Doblones de Oro", 50000),("botella de Arena", 1)],
 formaSaqueo = noSaquear
 
}

anne = Pirata {
 nombrePirata = "Anne Bonny",
 botinPirata = [("Caja Musical", 60),("botella de Ron", 200)],
 formaSaqueo = noSaquear
 }

will = Pirata {
 nombrePirata = "Will Turner",
 botinPirata = [],
 formaSaqueo = noSaquear
}

elizabeth = Pirata {
 nombrePirata = "Elizabeth Swann",
 botinPirata = [("Sombrero", 200),("Doblones de plata", 30000)],
 formaSaqueo = noSaquear
}

holandes = Barco {
 nombreBarco = "Holandes Errante",
 piratasEmbarcados = [david]		
} 

perlaNegra = Barco {
 nombreBarco = "Perla Negra",
 piratasEmbarcados =[will]
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

agregarTesoro pirata (nombreTesoro, valorBotin) = pirata { botinPirata = (nombreTesoro,valorBotin) : (botinPirata pirata) }  

tienenTesorosDeIgualNombre pirata pirata2 = any (==True)(zipWith (==)(nombreTesoro pirata)(nombreTesoro pirata2)) --MODIFICAR

tienenMismoTesoroDeIgualValor pirata pirata2 = any (==True) (zipWith (/=)(valoresTesorosPirata pirata) (valoresTesorosPirata pirata2)) 

tienenTesorosIgualesYDiferenteValor pirata pirata2 =  (tienenMismoTesoroDeIgualValor pirata pirata2 &&(tienenTesorosDeIgualNombre pirata pirata2) )

sacarTesoro :: Pirata -> (Pirata -> t -> [(String, Int)]) -> t -> Pirata
sacarTesoro pirata sacarElemento nombreTesoros = pirata {botinPirata = sacarElemento pirata nombreTesoros }

sacarConValorMayor100 pirata nombreTesoros = filter  ((<100).segundo)   (botinPirata pirata) 

sacarTesoroSiSeLLama pirata nombreTesoroPerder = filter  ((/=nombreTesoroPerder).primero)   (botinPirata pirata) 


--TEMPORADA DE TESOROS REVISAR ---

type FormaSaquear = Pirata  -> Pirata


--saqueo :: FormaSaquear
saqueo pirata (nombreTesoro, valorBotin) formaSaqueo = (formaSaqueo pirata) (pirata (nombreTesoro, valorBotin))
									                   

--mayoresDe100 :: FormaSaquear	
mayoresDe100 pirata (nombreTesoro,valorBotin) | ((>100) valoresTesorosPirata pirata) = agregarTesoro pirata (nombreTesoro, valorBotin)
											 |otherwise =  pirata

--soloTomaConNombre pirata nombreRoboPirata = pasar la palabra clave y que de ahi se cree la condicion 

--tomarConNombre pirata (nombreTesoro, valorBotin)  | (==nombreTesoroTomar) soloRoba) = (agregarTesoro pirata (nombreTesoro, valorBotin))
--										| otherwise =  pirata
	

--noSaquear :: FormaSaquear
noSaquear pirata  =  pirata 

--saqueoComplejo pirata

-- saquear jack (saquearPorNombre "oro")

--- TRIPULACIONES ----
---type Embarcar = Barco -> Pirata -> Barco
--abordarBarco :: Embarcar
--abordarBarco pirata barco = (piratasEmbarcados barco):(pirata)

--desabordarBarco :: Embarcar
--desabordarBarco pirata barco = filter (pirata) (piratasEmbarcados barco) 








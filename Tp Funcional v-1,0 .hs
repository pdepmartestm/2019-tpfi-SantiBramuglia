import Text.Show.Functions

data Pirata = Pirata {
	nombrePirata :: String,
	botinPirata :: [String],
	valorBotin ::	[Int],
	nombreBarcoEmbarcado :: String
} deriving (Show)

data Botin = Botin {
	nombreBotin :: String,
	valor ::  Int
} deriving (Show)

data Barco = Barco {
	nombreBarco :: String,
	tripulantes :: [String]
} deriving (Show)

sombrero = Botin {
	nombreBotin = "Sombrero",
	valor = 10
} 

jack = Pirata {
	nombrePirata = "Jack Parrow"
}

will = Pirata {
	nombrePirata = "Will Turner"
}
elizabeth = Pirata {
	nombrePirata = "Elizabeth Swann"
}

david = Pirata {
	nombrePirata = "David Jones"
}

perla = Barco {
	nombreBarco = "Perla Negra",
	tripulantes = ["Elizabeth Swann"]
}

holandes = Barco {
	nombreBarco = "Holandes Errante",
	tripulantes = ["David Jones"]
}

noHayDivisores minimo maximo n 
    | esDivisor minimo n = False
    | minimo == maximo   = True
    | otherwise          = noHayDivisores (minimo + 1) maximo n

esDivisor unNumero otroNumero = mod otroNumero unNumero == 0

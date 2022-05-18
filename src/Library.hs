module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Animal= Raton {nombre :: String, edad :: Number, peso :: Number,
 enfermedades :: [String]} deriving Show
-- Ejemplo de raton
cerebro = Raton "Cerebro" 9.0 0.2 ["brucelosis", "sarampión", "tuberculosis"]
-- Estos son las enfermedades infecciosas
enfermedadesInfecciosas = [ "brucelosis", "tuberculosis"]


modificarNombre :: ( String -> String )  ->   Animal    -> Animal
modificarNombre f animal = animal { nombre = (f.nombre) animal}

modificarEdad :: (Number -> Number)  ->   Animal  -> Animal
modificarEdad f raton = raton { edad = (f.edad)raton }

modificarPeso :: (Number -> Number)  ->  Animal -> Animal
modificarPeso f raton = raton { peso =  f.peso $ raton}

modificarEnfermedades ::  ([String] -> [String])   -> Animal         -> Animal
modificarEnfermedades f raton = raton {enfermedades = f.enfermedades $ raton}

hierbaBuena :: Animal -> Animal
hierbaBuena unAnimal = modificarEdad sqrt unAnimal

hierbaVerde :: String -> Animal -> Animal
hierbaVerde unaEnfermedad raton = modificarEnfermedades (filter (/= unaEnfermedad)) raton 

alcachofa :: Animal -> Animal
alcachofa raton = modificarPeso perderPeso  raton

perderPeso peso | peso > 2 = peso * 0.9
                | otherwise = peso * 0.95


hierbaMagica :: Animal -> Animal
hierbaMagica raton = ( modificarEdad  (0*)  .  modificarEnfermedades (const []) ) raton

medicamento :: [(Animal -> Animal)] -> Animal -> Animal
medicamento hierbas  raton = foldl  (\unRaton  unaHierba ->  unaHierba unRaton )  raton   hierbas

medicamento' hierbas raton = foldl (flip ($)) raton hierbas

antiAge :: Animal  -> Animal
antiAge raton = medicamento (replicate 3 hierbaBuena  ++ [alcachofa])   raton

reduceFatFast :: Number -> Animal -> Animal
reduceFatFast potencia raton = medicamento ([hierbaVerde "obesidad"] ++ (replicate potencia alcachofa)) raton


hierbaMilagrosa :: Animal -> Animal
hierbaMilagrosa raton = foldr hierbaVerde raton enfermedadesInfecciosas


hierbaMilagrosa' raton = medicamento (map hierbaVerde enfermedadesInfecciosas) raton

cantidadIdeal criterio =  head. filter criterio $ [1..] 


estanMejoresQueNunca :: [Animal] -> (Animal-> Animal) -> Bool
estanMejoresQueNunca ratones  unMedicamento =  all ((<1).peso.unMedicamento)  ratones 

{- 
*Spec Library Spec> estanMejoresQueNunca [cerebro] hierbaMagica
True
-}

nuevoExperimento ratones = cantidadIdeal (estanMejoresQueNunca ratones.reduceFatFast)

data Postulante = UnPostulante {nombrePostulante :: String, edadPostulante :: Number, remuneracion :: Number, conocimientos :: [String]} deriving Show 
pepe = UnPostulante "Jose Perez" 35 15000.0 ["Haskell", "Prolog", "Wollok", "C"]
tito = UnPostulante "Roberto Gonzalez" 20 12000.0 ["Haskell", "Php"]

type Nombre = String
data Puesto = UnPuesto {puesto:: String, conocimientoRequeridos :: [String]} deriving Show

jefe = UnPuesto "gerente de sistemas" ["Haskell", "Prolog", "Wollok"]
chePibe = UnPuesto "cadete" ["ir al banco"]
 
apellidoDueno:: Nombre
apellidoDueno = "Gonzalez"
type Requisito = Postulante -> Bool

tieneConocimientos :: Puesto -> Requisito
tieneConocimientos puesto postulante = (all (\requerido -> elem requerido (conocimientos postulante)) .conocimientoRequeridos) puesto

{-*Spec Library Spec> tieneConocimientos  jefe pepe
True -}

edadAceptable :: Number -> Number -> Requisito
edadAceptable edadMin edadMax postulante = edadPostulante postulante >= edadMin && edadPostulante postulante <= edadMax

sinArreglo :: Requisito
sinArreglo unPostulante =  (apellidoDueno /=).last.words.nombrePostulante  $ unPostulante


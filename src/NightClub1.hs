module NightClub1 where

import Text.Show.Functions
import Data.List

type Trago = (Cliente -> Cliente)

data Cliente = UnCliente {
  nombre :: String,
  resistencia :: Int,
  amigos :: [Cliente],
  tragos :: [Trago]
} 

instance Eq Cliente where
  (==) cliente1 cliente2 = nombre cliente1 == nombre cliente2

instance Show Cliente where
  show cliente = "{ nombre: " ++ show (nombre cliente)
              ++ ", resistencia: " ++ show (resistencia cliente)
              ++ ", amigos: " ++ show (map nombre (amigos cliente))
              ++ ", tragos: [Tragos x"++show (length . tragos $ cliente)++"]"
          --    ++ ", tragos: " ++ show (tragos cliente)++" }"

modificarNombre :: String -> Cliente -> Cliente
modificarNombre nuevoNombre cliente = cliente { nombre = nuevoNombre }

modificarResistencia :: (Int -> Int -> Int) -> Int -> Cliente -> Cliente
modificarResistencia operation valor cliente = cliente { resistencia = (resistencia cliente) `operation` valor }

modificarAmigos :: (Cliente -> Cliente) -> Cliente -> Cliente
modificarAmigos function cliente = cliente { amigos = (map function . amigos) cliente }

modificarTragos :: ([Trago] -> [Trago]) -> Cliente -> Cliente
modificarTragos function cliente = cliente { tragos = function $ tragos cliente }

---

rodri = UnCliente { 
  nombre = "Rodri", 
  resistencia = 55, 
  amigos = [],
  tragos = [tomarTintico]
}
marcos = UnCliente { 
  nombre = "Marcos", 
  resistencia = 40, 
  amigos = [rodri],
  tragos = [(tomarKlusener "Guinda")]
}
cristian = UnCliente { 
  nombre = "Cristian", 
  resistencia = 2, 
  amigos = [],
  tragos = [tomarGrogXD, tomarJarraLoca]
}
ana = UnCliente { 
  nombre = "Ana", 
  resistencia = 120, 
  amigos = [marcos,rodri],
  tragos = []
}
robertoCarlos = UnCliente {
  nombre = "Roberto Carlos",
  resistencia = 165,
  amigos = [],
  tragos = []
}
chuckNorris = UnCliente {
  nombre = "Chuck Norris",
  resistencia = 1000,
  amigos = [ana],
  tragos = [ tomarSoda x | x <- [1..] ]
}

{-
Justificar: ¿Puede chuckNorris pedir otro trago con la función dameOtro?
La funcion `dameOtro` no sera valida con chuck, ya que el compilador intentara reducir una lista infinita para encontrar el ultimo elementos
Justificar: ¿puedo hacer que chuckNorris realice el itinerario básico y conocer su resistencia resultante?
Si, gracias al `lazy evaluation` de Haskell, al no necesitar la lista de tragos (infinita) de chuck, no habra nignu tipo de problema para aplicarle el itinerario.
Justificar: ¿puedo preguntar si chuckNorris tiene más resistencia que ana?
Si, exactamente por lo mismo del punto anterior
-}

---

comoEsta :: Cliente -> String
comoEsta cliente
  |  resistencia cliente > 50 = "fresco"
  |  (length . amigos) cliente > 1 = "piola"
  |  otherwise = "duro"

---

esAmigo :: Cliente -> Cliente -> Bool
esAmigo amigo = elem amigo . amigos

reconocerAmigo :: Cliente -> Cliente -> Cliente
reconocerAmigo amigo cliente
  |  amigo == cliente || amigo `esAmigo` cliente = cliente
  |  otherwise = cliente { amigos =  amigo : amigos cliente }

---

agregarTrago :: Trago -> Cliente -> Cliente
agregarTrago trago = modificarTragos ((:) trago)

tomarGrogXD :: Trago
tomarGrogXD = agregarTrago tomarGrogXD . modificarResistencia (*) 0

tomarJarraLoca :: Trago
tomarJarraLoca = agregarTrago tomarJarraLoca . modificarAmigos efectoJarra . efectoJarra 
  where efectoJarra = modificarResistencia (-) 10

tomarKlusener :: String -> Trago
tomarKlusener gusto = agregarTrago (tomarKlusener gusto) . modificarResistencia (-) (length gusto) 

tomarTintico :: Trago
tomarTintico cliente = agregarTrago tomarTintico $ modificarResistencia (+) dif cliente
  where dif = 5 * (length . amigos) cliente

tomarSoda :: Int -> Trago
tomarSoda fuerza cliente = agregarTrago (tomarSoda fuerza) $ modificarNombre nuevoNombre cliente
  where nuevoNombre = "e" ++ replicate fuerza 'r' ++ "p" ++ nombre cliente

tomarJarraPopular :: Int -> Trago
tomarJarraPopular 0 cliente = cliente
tomarJarraPopular espirituosidad cliente = tomarJarraPopular (espirituosidad - 1) (hacerseAmigo cliente)

---

rescatarse :: Int -> Cliente -> Cliente
rescatarse horas cliente
  | (>3) horas = modificarResistencia (+) 200 cliente
  | (>0) horas = modificarResistencia (+) 100 cliente
  | otherwise  = error "Not valid hour input"

----

tomarTragos :: [Trago] -> Cliente -> Cliente
tomarTragos [] = id
tomarTragos tragos  =  foldl1 (.) tragos

dameOtro :: Cliente -> Cliente
dameOtro cliente 
  | not (null $ tragos cliente) = ultimoTrago cliente
  | otherwise = error "Cliente no tomo nada"
  where ultimoTrago = last $ tragos cliente

---

cualesPuedeTomar :: [Trago] -> Cliente -> [Trago]
cualesPuedeTomar listaTragos cliente = filter resistenciaMayorCero listaTragos
--cualesPuedeTomar listaTragos cliente = [ trago | trago <- listaTragos, resistenciaMayorCero trago]
  where resistenciaMayorCero trago = (resistencia . trago) cliente > 0

cuantasPuedeTomar :: [Trago] -> Cliente -> Int
cuantasPuedeTomar listaTragos = length . cualesPuedeTomar listaTragos

---

data Itinerario = UnItinerario {
  descripcion :: String,
  duracion :: Float,
  acciones :: [Cliente -> Cliente]
} 
instance Eq Itinerario where
  (==) itinerario1 itinerario2 = intensidadItinerario itinerario1 == intensidadItinerario itinerario2
instance Ord Itinerario where
  compare itinerario1 itinerario2 = intensidadItinerario itinerario1 `compare` intensidadItinerario itinerario2

mezclaExplosiva = UnItinerario { 
  descripcion = "Mezcla Explosiva", 
  duracion = 2.5, 
  acciones = [tomarGrogXD, tomarGrogXD, tomarKlusener "Huevo", tomarKlusener "Frutilla"]
}
itinerarioBasico = UnItinerario { 
  descripcion = "Basico", 
  duracion = 5, 
  acciones = [tomarJarraLoca, tomarKlusener "Chocolate", rescatarse 2, tomarKlusener "Huevo"]
}
salidaDeAmigos = UnItinerario { 
  descripcion = "Salida de amigos", 
  duracion = 1, 
  acciones = [tomarSoda 1, tomarTintico, reconocerAmigo robertoCarlos, tomarJarraLoca]
}

realizarItinerario :: Itinerario -> Cliente -> Cliente
realizarItinerario itinerario = foldl1 (.) (acciones itinerario)
--Acá podriamos usar `tomarTragos` pero un itinerario podría incluir acciones que no son un trago, como `rescatarse`.

----

intensidadItinerario :: Itinerario -> Float
intensidadItinerario itinerario = genericLength (acciones itinerario) / duracion itinerario

----

itinerarioMasIntenso :: [Itinerario] -> Itinerario
itinerarioMasIntenso = maximum

----

hacerseAmigo :: Cliente -> Cliente
hacerseAmigo cliente = foldr reconocerAmigo cliente amigosDeAmigos
  where amigosDeAmigos = (concat . map amigos . amigos) cliente

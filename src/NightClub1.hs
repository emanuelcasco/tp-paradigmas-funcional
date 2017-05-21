module NightClub1 where

import Text.Show.Functions

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
              ++ ", tragos: " ++ show (tragos cliente)++" }"

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


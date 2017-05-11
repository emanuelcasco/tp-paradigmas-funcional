module NightClub where

data Cliente = UnCliente {
  nombre :: String,
  resistencia :: Int,
  amigos :: [Cliente]
} 

instance Eq Cliente where
  (==) cliente1 cliente2 = nombre cliente1 == nombre cliente2

instance Show Cliente where
  show cliente = "{ nombre: "++show (nombre cliente)++", resistencia: "++show (resistencia cliente)++", amigos: " ++ show (map nombre (amigos cliente))++" }"

---

rodri = UnCliente { nombre = "Rodri", resistencia = 55, amigos = [] }
marcos = UnCliente { nombre = "Marcos", resistencia = 40, amigos = [rodri] }
cristian = UnCliente { nombre = "Cristian", resistencia = 2, amigos = [] }
ana = UnCliente { nombre = "Ana", resistencia = 120, amigos = [marcos,rodri] }

itinerarioAna = [tomarJarraLoca, (tomarKlusener "Chocolate"), (rescatarse 2), (tomarKlusener "Huevo")]

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

modificarNombre :: String -> Cliente -> Cliente
modificarNombre nuevoNombre cliente = cliente { nombre = nuevoNombre }

modificarResistencia :: (Int -> Int -> Int) -> Int -> Cliente -> Cliente
modificarResistencia operation valor cliente = cliente { resistencia = (resistencia cliente) `operation` valor }

modificarAmigos :: (Cliente -> Cliente) -> Cliente -> Cliente
modificarAmigos function cliente = cliente { amigos = (map function . amigos) cliente }

---

tomarGrogXD :: Cliente -> Cliente
tomarGrogXD = modificarResistencia (*) 0

tomarJarraLoca :: Cliente -> Cliente
tomarJarraLoca = modificarAmigos efectoJarra . efectoJarra 
  where efectoJarra = modificarResistencia (-) 10

tomarKlusener :: String -> Cliente -> Cliente
tomarKlusener gusto = modificarResistencia (-) (length gusto) 

tomarTintico :: Cliente -> Cliente
tomarTintico cliente = modificarResistencia (+) dif cliente
  where dif = 5 * (length . amigos) cliente

tomarSoda :: Int -> Cliente -> Cliente
tomarSoda fuerza cliente = modificarNombre nuevoNombre cliente
  where nuevoNombre = "e" ++ replicate fuerza 'r' ++ "p" ++ nombre cliente

---

rescatarse :: Int -> Cliente -> Cliente
rescatarse horas cliente
  | (>3) horas = modificarResistencia (+) 200 cliente
  | (>0) horas = modificarResistencia (+) 100 cliente
  | otherwise  = error "Not valid hour input"

---

aplicarItinerario :: [(Cliente -> Cliente)] -> Cliente -> Cliente
aplicarItinerario itinerario cliente = foldr ($) cliente (reverse itinerario) 

-- 
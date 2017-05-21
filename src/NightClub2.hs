module NightClub2 where


import NightClub1
import Text.Show.Functions

type Itinerario = (Cliente, [Trago])

----

tragosClientes :: [Itinerario]
tragosClientes = [ 
        (rodri, [tomarTintico]),
        (marcos, [(tomarKlusener "Guinda")]),
        (cristian, [tomarGrogXD, tomarJarraLoca]),
        (ana, []) 
    ]
clienteItinerario :: Itinerario -> Cliente
clienteItinerario = fst
tragosItinerario :: Itinerario -> [Trago]
tragosItinerario = snd

----

tomarTragos :: [Trago] -> Cliente -> Cliente
tomarTragos [] = id
tomarTragos tragos  =  . foldl1 (.) tragos

itinerarioCliente :: Cliente -> [Trago]
itinerarioCliente cliente = foldl seleccionarTragos [] tragosClientes
    where seleccionarTragos = (\ys y -> if (clienteItinerario y == cliente) then tragosItinerario y ++ ys else [] ++ ys)
--itinerarioCliente cliente = tragosItinerario (head $ filter ((==cliente) . clienteItinerario) tragosClientes)

----

dameOtro cliente = (last . itinerarioCliente) 

ultimoTrago = (last . itinerarioCliente)
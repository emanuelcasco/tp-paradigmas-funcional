module NightClubSpec where

import Test.Hspec
import Control.Exception (evaluate)

import NightClub1
import Text.Show.Functions
import Data.List

---

testAll :: IO ()
testAll = hspec $ do

----------------------------------------------------------
---          TESTS PRIMERA PARTE TP FUNCIONAL          ---
----------------------------------------------------------

  describe "[TP Nº 1] Verificar puntos 1 y 2] " $ do
    it "Nombre de Rodri debe ser 'Rodri'" $ do
      (nombre rodri) `shouldBe` "Rodri"
    it "Resistencia de Ana debe ser 120" $ do
      (resistencia ana) `shouldBe` 120
    it "Amigos de Ana deben ser [rodri]" $ do
      (amigos marcos) `shouldMatchList` [rodri]

  describe "[TP Nº 1] Verificar punto 3] " $ do
    it "Cristian debe estar 'duro'" $ do
      (comoEsta cristian) `shouldBe` "duro"
    it "Rodri debe estar 'fresco'" $ do
      (comoEsta rodri) `shouldBe` "fresco"
    it "Marcos debe estar 'duro'" $ do
      (comoEsta marcos) `shouldBe` "duro"
    it "Si Marcos se hace amigo de Ana y Rodri, está 'piola'" $ do
      (comoEsta . reconocerAmigo rodri . reconocerAmigo ana) marcos `shouldBe` "piola"

  describe "[TP Nº 1] Verificar punto 4] " $ do
    it "Cristian reconoce a Marcos como amigo" $ do
      (amigos . reconocerAmigo marcos) cristian `shouldMatchList` [marcos]
    it "Cristian no puede reconocerse a si mismo como amigo" $ do
      reconocerAmigo cristian cristian `shouldBe` cristian
    it "Cristian no puede reconocerse a Marcos dos veces como amigo" $ do
      (reconocerAmigo marcos . reconocerAmigo marcos) cristian `shouldBe` cristian

  describe "[TP Nº 1] Verificar punto 5] " $ do
    it "Ana toma grogXD. Queda con resistencia 0" $ do
      (resistencia . tomarGrogXD) ana `shouldBe` 0
    it "Si Ana toma la jarraLoca. Marcos queda con resistencia 30 (-10)" $ do
      (resistencia . head . amigos . tomarJarraLoca) ana `shouldBe` 30
    it "Marcos toma la jarraLoca. Queda con resistencia 30" $ do
      (resistencia . tomarJarraLoca) marcos `shouldBe` 30
    it "Rodri toma la jarraLoca. Queda con resistencia 110" $ do
      (resistencia . tomarJarraLoca) rodri `shouldBe` 45

    it "Si Ana toma klusener de Huevo disminuye se resistencia a 50 (-5)" $ do
      (resistencia . tomarKlusener "Huevo") ana `shouldBe` 115
    it "Si Ana toma klusener de Chocolate disminuye se resistencia a 50 (-8)" $ do
      (resistencia . tomarKlusener "Chocolate") ana `shouldBe` 111

    it "Si Cristian toma un tintico, queda con 2 de resistencia por no tener" $ do
      (resistencia . tomarTintico) cristian `shouldBe` 2
    it "Ana toma un Tintico, pasa a 130 de resistencia (tiene 2 amigos)" $ do
      (resistencia . tomarTintico) ana `shouldBe` 130

    it "Rodri toma una Soda de fuerza 2, queda con nombre 'errpRodri'" $ do
      (nombre . tomarSoda 2) rodri `shouldBe` "errpRodri"
    it "Ana toma una Soda de fuerza 10, queda con nombre 'errrrrrrrrrpAna'" $ do
      (nombre . tomarSoda 10) ana `shouldBe` "errrrrrrrrrpAna"
    it "Ana toma una Soda de fuerza 0, queda con nombre 'epAna'" $ do
      (nombre . tomarSoda 0) ana `shouldBe` "epAna"

  describe "[TP Nº 1] Verificar punto 6] " $ do
    it "Si Rodri se rescata por 5 horas debería tener 255 de resistencia" $ do
      (resistencia . rescatarse 5) rodri `shouldBe` 255
    it "Si Marcos se rescata por 3 horas debería tener 140 de resistencia" $ do
      (resistencia . rescatarse 3) marcos `shouldBe` 140
    it "Si Cristian se rescata por 1 horas debería tener 155 de resistencia" $ do
      (resistencia . rescatarse 1) rodri `shouldBe` 155
    it "Si Ana se rescata por 0 horas debería aparecer un error" $ do
      evaluate ( (resistencia . rescatarse 0) ana ) `shouldThrow` anyException
    it "Si Ana se rescata por menos de 0 horas debería aparecer un error" $ do
      evaluate ( (resistencia . rescatarse (-1)) ana ) `shouldThrow` anyException

----------------------------------------------------------
---          TESTS SEGUNDA PARTE TP FUNCIONAL          ---
----------------------------------------------------------

  describe "[TP Nº 2] Verificar punto 1.a." $ do
     it "Rodri tomó un tintico" $ do
        tomarTragos (tragos rodri) rodri `shouldBe` tomarTintico rodri
  
  describe "[TP Nº 2] Verificar punto 1.b." $ do
     it "Rodri tomó un tintico" $ do
        (length . tragos) (tomarGrogXD ana) `shouldBe` 1 

  let anaElDiaDespues = tomarTragos [tomarJarraLoca, tomarKlusener "Chocolate", rescatarse 2, tomarKlusener "Huevo"] ana
  describe "[TP Nº 2] Verificar punto 1.c: Ana toma una jarra loca, un klusener de chocolate, se rescata 2 horas y luego toma un klusener de huevo." $ do
     it "Su resistencia queda en 196" $ do
        resistencia anaElDiaDespues `shouldBe` 196
     it "Quedan como amigos: Marcos (30 de resistencia) y Rodri (45 de resistencia)" $ do
        (map resistencia . amigos) anaElDiaDespues `shouldBe` [30, 45]
     it "En su lista de tragos ahora hay 3 elementos" $ do
        (length . tragos) anaElDiaDespues `shouldBe` 3

  describe "[TP Nº 2] Verificar punto 1.d: Con `dameOtro` marcos vuelve a tomar la ultima bebida de su lista" $ do
     it "La resistencia de Marcos pasa a ser 34" $ do
        (resistencia . dameOtro) marcos `shouldBe` 34
     it "Este trago se suma a su lista, pasando este a tener 2 elementos" $ do
        (length . tragos . dameOtro) marcos `shouldBe` 2
     it "Ana pide `dameOtro`, debe dar error" $ do
        evaluate (dameOtro ana) `shouldThrow` anyException
     it "Rodri toma una soda de fuerza 1, `dameOtro` hace que tenga 3 elementos en su lista de tragos" $ do
         (length . tragos . dameOtro . tomarSoda 1) rodri `shouldBe` 3

  describe "[TP Nº 2] Verificar punto 2: `cuantasPuedeTomar`" $ do
     it "Rodri puede tomar dos tragos, entre un grogXD, un tintico y un klusener de frutilla" $ do
         cuantasPuedeTomar [tomarGrogXD, tomarTintico, tomarKlusener "Frutilla"] rodri `shouldBe` 2
     it "Cristian no puede tomar un trago, entre un tintico y un klusener de huevo" $ do
         cuantasPuedeTomar [tomarGrogXD, tomarTintico, tomarKlusener "Huevo"] cristian `shouldBe` 1

  describe "[TP Nº 2] Verificar punto 3" $ do
     it "Ana realiza una salida de amigos, debe quedar con 1 amigo" $ do
         (length . amigos . realizarItinerario salidaDeAmigos) ana `shouldBe` 3
     it "Ana realiza una salida de amigos, ahora se llama 'erpAna'" $ do
         (nombre . realizarItinerario salidaDeAmigos) ana `shouldBe` "erpAna"
     it "Ana realiza una salida de amigos, debe quedar con 125 de Resistencia" $ do
         (resistencia . realizarItinerario salidaDeAmigos) ana `shouldBe` 125
     it "Ana realiza una salida de amigos, ahora es amiga de Roberto Carlos" $ do
         (amigos . realizarItinerario salidaDeAmigos) ana `shouldSatisfy` elem robertoCarlos

  describe "[TP Nº 2] Verificar punto 4" $ do
     it "Rodri puede tomar dos tragos, entre un grogXD, un tintico y un klusener de frutilla" $ do
         intensidadItinerario itinerarioBasico `shouldBe` 0.8
     it "Rodri puede tomar dos tragos, entre un grogXD, un tintico y un klusener de frutilla" $ do
         intensidadItinerario mezclaExplosiva `shouldBe` 1.6
     it "Rodri puede tomar dos tragos, entre un grogXD, un tintico y un klusener de frutilla" $ do
         intensidadItinerario salidaDeAmigos `shouldBe` 4.0
     it "El itinerario más intenso, de los conocidos, es la salida de amigos" $ do
         (descripcion . itinerarioMasIntenso) [salidaDeAmigos, mezclaExplosiva, itinerarioBasico] `shouldBe` "Salida de amigos"

  describe "[TP Nº 2] Verificar punto 5: Chuck 'The God' Norris" $ do
     it "La resistencia de Chuck es mayor a la de Ana" $ do
         resistencia chuckNorris `shouldSatisfy` (> resistencia ana)
     it "Chuck realiza un itinerario basico, al finalizar su resistencia es 1076" $ do
         (resistencia . realizarItinerario itinerarioBasico) chuckNorris `shouldBe` 1076

  describe "[TP Nº 2] Verificar punto 6: Jarra Popular" $ do
      it "Roberto Carlos se hace amigo de Ana, toma una jarra popular de espirituosidad 0, sigue teniendo una sola amiga (Ana)" $ do
         (length . amigos . tomarJarraPopular 0 . reconocerAmigo ana) robertoCarlos `shouldBe` 1
      it "Roberto Carlos se hace amigo de Ana, toma una jarra popular de espirituosidad 1, ganó dos amigos" $ do
         (length . amigos . tomarJarraPopular 1 . reconocerAmigo ana) robertoCarlos `shouldBe` 3
      it "Roberto Carlos se hace amigo de Ana, toma una jarra popular de espirituosidad 1, sus nuevos amigos son rodri y marcos" $ do
         (map nombre . amigos . tomarJarraPopular 1 . reconocerAmigo ana) robertoCarlos `shouldBe` ["Marcos", "Rodri", "Ana"]
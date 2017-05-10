module NightClubSpec where

import Test.Hspec
import Control.Exception (evaluate)

import NightClub

---

testAll :: IO ()
testAll = hspec $ do
  describe "[Verificar puntos 1 y 2] " $ do
    it "Nombre de Rodri debe ser 'Rodri'" $ do
      (nombre rodri) `shouldBe` "Rodri"
    it "Resistencia de Ana debe ser 120" $ do
      (resistencia ana) `shouldBe` 120
    it "Amigos de Ana deben ser [rodri]" $ do
      (amigos marcos) `shouldMatchList` [rodri]

  describe "[Verificar punto 3] " $ do
    it "Cristian debe estar 'duro'" $ do
      (comoEsta cristian) `shouldBe` "duro"
    it "Rodri debe estar 'fresco'" $ do
      (comoEsta rodri) `shouldBe` "fresco"
    it "Marcos debe estar 'duro'" $ do
      (comoEsta marcos) `shouldBe` "duro"
    it "Si Marcos se hace amigo de Ana y Rodri, está 'piola'" $ do
      (comoEsta . reconocerAmigo rodri . reconocerAmigo ana) marcos `shouldBe` "piola"

  describe "[Verificar punto 4] " $ do
    it "Cristian reconoce a Marcos como amigo" $ do
      (amigos . reconocerAmigo marcos) cristian `shouldMatchList` [marcos]
    it "Cristian no puede reconocerse a si mismo como amigo" $ do
      reconocerAmigo cristian cristian `shouldBe` cristian
    it "Cristian no puede reconocerse a Marcos dos veces como amigo" $ do
      (reconocerAmigo marcos . reconocerAmigo marcos) cristian `shouldBe` cristian

  describe "[Verificar punto 5] " $ do
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

  describe "[Verificar punto 6] " $ do
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

  let anaElDiaDespues = aplicarItinerario [tomarJarraLoca, (tomarKlusener "Chocolate"), (rescatarse 2), (tomarKlusener "Huevo")] ana
  describe "[Verificar punto 7] Ana toma una jarra loca, un klusener de chocolate, se rescata 2 horas y luego toma un klusener de huevo." $ do
    it "Su resistencia queda en 196" $ do
      resistencia anaElDiaDespues `shouldBe` 196
    it "Quedan como amigos: Marcos (30 de resistencia) y Rodri (45 de resistencia)" $ do
      (map resistencia . amigos) anaElDiaDespues `shouldBe` [30, 45]
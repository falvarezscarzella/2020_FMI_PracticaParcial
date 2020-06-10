module Lib where
import Text.Show.Functions

laVerdad = True

--Punto 1
--a
type Recurso = String
data Pais =  UnPais{
    ingresoPerCapita:: Float,
    poblacionActivaPublico:: Int,
    poblacionActivaPrivado:: Int,
    recursosNaturales:: [Recurso],
    deudaEnMillones:: Float
} deriving (Show, Eq) 

--b
namibia = UnPais 4140 400000 650000 ["Mineria","Ecoturismo","Petroleo"] 50

--Punto 2

type Estrategia = Pais -> Pais

lePrestaMillones :: Float -> Estrategia
lePrestaMillones prestamo pais = pais{deudaEnMillones = (deudaEnMillones pais)+(prestamo*1.5)}

reducirPuestosPublicos :: Int -> Estrategia
reducirPuestosPublicos numeroPuestos pais = 
    pais{poblacionActivaPublico = poblacionActivaPublico pais - numeroPuestos, 
    ingresoPerCapita = disminucionPorcentajeIngreso numeroPuestos (ingresoPerCapita pais)}

disminucionPorcentajeIngreso :: Int -> Float -> Float
disminucionPorcentajeIngreso puestos ingreso | puestos > 100 = ingreso - (ingreso * 0.2)
                                             | otherwise = ingreso - (ingreso * 0.15)

darleExplotacion :: Recurso -> Estrategia
darleExplotacion recurso = disminuirDeuda 2 . quitarRecurso recurso

disminuirDeuda :: Float -> Pais -> Pais
disminuirDeuda valorDisminucion pais = pais{deudaEnMillones = deudaEnMillones pais - valorDisminucion}

quitarRecurso :: Recurso -> Pais -> Pais
quitarRecurso recurso pais = pais{recursosNaturales = filter  (recurso /=) (recursosNaturales pais)}  

establecerBlindaje :: Estrategia
establecerBlindaje pais = lePrestaMillones (((/2).productoBrutoInterno) pais) . reducirPuestosPublicos 500 $ pais

productoBrutoInterno :: Pais -> Float
productoBrutoInterno pais = fromIntegral(poblacionActivaPrivado pais + poblacionActivaPublico pais) * ingresoPerCapita pais

--Punto 3
--a
type Receta = [Estrategia]

recetaGarca :: Receta
recetaGarca = [lePrestaMillones 200, darleExplotacion "Mineria"]

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta receta pais = foldl (\pais estrategia -> estrategia $ pais) pais receta

--b
--Lib Lib> aplicarReceta recetaGarca namibia 
--UnPais {ingresoPerCapita = 4140.0, 
--  poblacionActivaPublico = 400000, 
--  poblacionActivaPrivado = 650000, 
--       recursosNaturales = ["Ecoturismo"], 
--         deudaEnMillones = 348.0}

--Punto 4

conocerInfoDePaises :: ([Pais] -> a) -> [Pais] -> a
conocerInfoDePaises infoDepais paises = infoDepais $ paises

paisZafa :: [Pais] -> [Pais]
paisZafa = filter (tienePetroleo) 

tienePetroleo :: Pais -> Bool
tienePetroleo pais = elem "Petroleo" (recursosNaturales pais)

deudaAFavor :: [Pais] -> Float
deudaAFavor = sum.map deudaEnMillones

--Punto 5

estaOrdenada :: Pais -> [Receta] -> Bool
estaOrdenada pais [receta] = True
estaOrdenada pais (receta1 : receta2 : recetas) = productoBrutoInterno (aplicarReceta receta1 pais) <= productoBrutoInterno (aplicarReceta receta2 pais) 
    && estaOrdenada pais (receta2:recetas)

--Punto 6

recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

--No funciona, es infinito
--Si funciona, tiene que ver con la evaluacion diferida o lazy evaluation




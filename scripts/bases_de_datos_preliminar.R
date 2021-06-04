library(tidyverse)
library(readxl)

setwd("~/Documents/GitHub/semestre6_git/macroeconomia_financiera/Articulo_final/bases_de_datos/acciones_tentativo/")

# 1. Contrucción precios de acciones mercado accionario colombiano ----

# 1.1 Importación bases de datos acciones

# Nota: en total se van a trabajar con 20 de las acciones más líquidas del mercado accionario colombiano 

isa = read_csv("ISA Historical Data.csv")
ecopetrol = read_csv("ECO Historical Data.csv")
pf_bancolombia = read_csv("BIC_p1 Historical Data.csv")
pf_davivienda = read_csv("DVI_p Historical Data.csv")
sura = read_csv("Datos históricos SIS.csv")
cem_argos = read_csv("Datos históricos CCB.csv")
pf_aval = read_csv("Datos históricos GAA_p.csv")
celsia = read_csv("Datos históricos CEL.csv")
geb = read_csv("Datos históricos GEB.csv")
nutresa = read_csv("Datos históricos NCH.csv")
promigas = read_csv("Datos históricos PMG.csv")
bogota = read_csv("Datos históricos BBO.csv")
bvc = read_csv("Datos históricos BVC.csv")
bolivar = read_csv("Datos históricos SCA.csv")
cemex = read_csv("Datos históricos CLH.csv")
etb = read_csv("Datos históricos ETB.csv")
terpel = read_csv("Datos históricos TPL.csv")
exito = read_csv("Datos históricos IMI.csv")
canacol = read_csv("Datos históricos CNE.csv")
mineros = read_csv("Datos históricos MAS.csv")

lista = list(isa$Date, ecopetrol$Date, pf_bancolombia$Date, pf_davivienda$Date, sura$Fecha, cem_argos$Fecha, 
         pf_aval$Fecha, celsia$Fecha, geb$Fecha, nutresa$Fecha, promigas$Fecha, bogota$Fecha, bvc$Fecha, 
         bolivar$Fecha, cemex$Fecha, etb$Fecha, terpel$Fecha, exito$Fecha, canacol$Fecha, mineros$Fecha)

nombres = c("isa", "ecopetrol", "bancolombia", "davivienda", "sura", "cementos argos", "grupo aval", "celsia", 
            "geb", "nutresa", "promigas", "bogota", "bvc", "bolivar", "cemex", "etb", "terpel", "exito", "canacol", 
            "mineros")

# Función para sacar la última fecha de cada 
ultimas_fechas = sapply(vect, tail, c(n=1))
names(ultimas_fechas) = nombres

# Voy a eliminar las acciones de cemex, de terpel y de aval
ultimas_fechas = ultimas_fechas[setdiff(names(ultimas_fechas), c("terpel", "grupo aval", "cemex"))]











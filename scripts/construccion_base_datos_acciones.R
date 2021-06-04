#### Construcción bases de datos de los precios de las acciones ####

library(tidyverse)
library(readxl)
library(writexl)
library(tidyquant)

setwd("~/Documents/GitHub/semestre6_git/macroeconomia_financiera/Articulo_final/bases_de_datos/acciones_final/")

# 1. Contrucción precios de acciones mercado accionario colombiano ----

# 1.1 Importación bases de datos acciones

# Nota: en total se van a trabajar con 17 de las acciones más líquidas del mercado accionario colombiano 

isa = read_csv("Datos históricos ISA.csv")
ecopetrol = read_csv("Datos históricos ECO.csv")
pf_bancolombia = read_csv("Datos históricos BIC_p1.csv")
pf_davivienda = read_csv("Datos históricos DVI_p.csv")
sura = read_csv("Datos históricos SIS.csv")
cem_argos = read_csv("Datos históricos CCB.csv")
celsia = read_csv("Datos históricos CEL.csv")
geb = read_csv("Datos históricos GEB.csv")
nutresa = read_csv("Datos históricos NCH.csv")
promigas = read_csv("Datos históricos PMG.csv")
bogota = read_csv("Datos históricos BBO.csv")
bvc = read_csv("Datos históricos BVC.csv")
bolivar = read_csv("Datos históricos SCA.csv")
etb = read_csv("Datos históricos ETB.csv")
exito = read_csv("Datos históricos IMI.csv")
canacol = read_csv("Datos históricos CNE.csv")
mineros = read_csv("Datos históricos MAS.csv")

# Nota: Para encontrar las fechas en formato <date>
DAX_monthly = tq_get("^GDAXI",get="stock.prices",from="2011-01-01",to='2019-12-31',periodicity='month')
fechas = DAX_monthly$date

# Data frame con los precios de las acciones del mercado accionario colombiano (se tomaron en total 15 acciones)
# Se descartaron las acciones de: bolivar, terpel, cemex, aval y promigas
acciones = tibble(isa = isa$Último, ecopetrol = ecopetrol$Último, 
                  pf_bancolombia = pf_bancolombia$Último, pf_davivienda = pf_davivienda$Último, sura = sura$Último,
                  cem_argos = cem_argos$Último, celsia = celsia$Último, geb = geb$Último, nutresa = nutresa$Último,
                  bogota = bogota$Último, bvc = bvc$Último, etb = etb$Último, exito = exito$Último, cancol = canacol$Último,
                  mineros = mineros$Último)

acciones = acciones %>% 
  map_df(rev)

acciones$fecha = fechas

acciones = acciones %>% 
  select(fecha, isa:mineros)

write_xlsx(acciones, path = "~/Documents/GitHub/semestre6_git/macroeconomia_financiera/Articulo_final/bases_de_datos/acciones_final/acciones.xlsx")

# nombres = c("isa", "ecopetrol", "bancolombia", "davivienda", "sura", "cementos argos", "celsia", 
#             "geb", "nutresa", "promigas", "bogota", "bvc", "bolivar", "etb", "exito", "canacol", 
#             "mineros")
# 
# lista_df = list(isa, ecopetrol, pf_bancolombia, pf_davivienda, sura, cem_argos, celsia, geb, 
#                 nutresa, promigas, bogota, bvc, bolivar, etb, exito, canacol, mineros)
# 
# lista = list(isa$Fecha, ecopetrol$Fecha, pf_bancolombia$Fecha, pf_davivienda$Fecha, sura$Fecha, cem_argos$Fecha, 
#              celsia$Fecha, geb$Fecha, nutresa$Fecha, promigas$Fecha, bogota$Fecha, bvc$Fecha, 
#              bolivar$Fecha, etb$Fecha, exito$Fecha, canacol$Fecha, mineros$Fecha)
# 
# names(lista_df) = nombres
# 
# # Función para sacar la última fecha de cada 
# ultimas_fechas = sapply(lista, tail, c(n=1))
# names(ultimas_fechas) = nombres






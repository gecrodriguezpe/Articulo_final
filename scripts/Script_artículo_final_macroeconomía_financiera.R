##### Script artículo final macroeconomía financiera #####

library(tidyverse)
library(readxl)
library(dynlm)
library(xts)

###
# Modelo macro factorial para analizar el mercado accionario colombiano 
###

# 1. Importación de bases de datos ----

# 1.1 Importar base de datos de las acciones del mercado accionario colombiano ----

setwd("~/Documents/GitHub/semestre6_git/macroeconomia_financiera/Articulo_final/bases_de_datos/acciones_final")
acciones = read_xlsx("acciones.xlsx")

# 1.2 Importar base de datos de las principales variables macrofinancieras usadas en el modelo ----

setwd("~/Documents/GitHub/semestre6_git/macroeconomia_financiera/Articulo_final/bases_de_datos/bases_pesonales")
variables = read_xlsx("base_variables_macro_financieras.xlsx")

# 2. Construcción de variables de interés ----

# Transformo los precios de las acciones en un objeto xts
acciones = xts(acciones[,-1], order.by = acciones$fecha)

# Retornos mensuales continuos 
acciones_retornos = diff(log(acciones))

# Transformo las variables macro y financieras en un objeto xts
variables = xts(variables[,-1], order.by = variables$fecha)  

# 2.1 Creación de variables para la regresión del paso 1 para estimar los factor loadings ----

# Tasa de crecimiento producción industrial: MP
variables$MP =  diff(log(variables$IPI))
# Inflación inesperada: UI
variables$UI = variables$inflacion_observada - variables$Inflacion_pronosticada
# Cambios en la inflación esperada: DEI
variables$DEI = diff(variables$Inflacion_pronosticada)
# Term structure: UTS
variables$UTS = variables$tasa_10_años - variables$tasa_1_año

  
# 3. Regreiones ----

# 3.1 Estimación de factor loadings por medio de una regresión de series de tiempo ----

# Creo un data frame para la estimación 
completa = as.zoo(merge(acciones_retornos, variables))

# Función para calcular los factor loadings de cada acción 

### Se estimaran 4 especificaciones distintas: 
###### 1. Sin petroleo ni market
###### 2. Con market sin petroleo
###### 3. Con petroleo sin market 
###### 4. Con market con petroleo
factor_loadings = function(completa, tipo){
  if (tipo == "none"){
    # 1. Sin petroleo ni market
    fact_load_original = tibble(MP_fl = double(), UI_fl = double(), DEI_fl = double(), UTS_fl = double()) 
    for (equity in 1:15){
      fact_load_original[equity,] = t(dynlm(completa[, equity] ~ MP + UI + DEI + UTS, data = completa)$coefficients[-1])
    }
    return(fact_load_original)
  }else if (tipo == "market"){
    # 2. Con market sin petroleo
    fact_load_market = tibble(MP_fl = double(), UI_fl = double(), DEI_fl = double(), UTS_fl = double(), colcap = double())   
    for (equity in 1:15){
      fact_load_market[equity,] = t(dynlm(completa[, equity] ~ MP + UI + DEI + UTS + colcap, data = completa)$coefficients[-1])
    }
    return(fact_load_market)
  }else if (tipo == "petroleum"){
    # 3. Con petroleo sin market 
    fact_load_petroleo = tibble(MP_fl = double(), UI_fl = double(), DEI_fl = double(), UTS_fl = double(), brent = double())   
    for (equity in 1:15){
      fact_load_petroleo[equity,] = t(dynlm(completa[, equity] ~ MP + UI + DEI + UTS + brent_dolares, data = completa)$coefficients[-1])
    }
    return(fact_load_petroleo)
  }else if (tipo == "market_petroleum"){
    # 4. Con market con petroleo
    fact_load_petroleo_market = tibble(MP_fl = double(), UI_fl = double(), DEI_fl = double(), UTS_fl = double(), colcap = double(), brent = double())   
    for (equity in 1:15){
      fact_load_petroleo_market[equity,] = t(dynlm(completa[, equity] ~ MP + UI + DEI + UTS + colcap + brent_dolares, data = completa)$coefficients[-1])
    }
    return(fact_load_petroleo_market)
  }
}

# Estimaciones de la etapa 1. para calcular las diferentes factor loadings para cada especificación

# 1. Sin petroleo ni market
none = factor_loadings(completa, "none")
# 2. Con market sin petroleo
market = factor_loadings(completa, "market")
# 3. Con petroleo sin market 
petroleum = factor_loadings(completa, "petroleum")
# 4. Con market con petroleo
market_petroleum = factor_loadings(completa, "market_petroleum")


# 3.2 Estimación de los factor risk premiums para cada uno de los macro factor ----

factor_risk_premium = function(factor_loadings, completa, tipo){
  # Se va a tomar como la tasa libre de riesgo el promedio de la tasa de los TES a 1 año durante el periodo estudiado 
  riskless_return = log(mean(completa$tasa_1_año)/100)
  mean_return = c()
  for (equity in 1:15){
    mean_i = mean(completa[,equity], na.rm = TRUE)
    mean_return = append(mean_return, mean_i)
  }
  factor_loadings$risk_premia = mean_return - riskless_return
  if (tipo == "none"){
    reg = lm(risk_premia ~ MP_fl + UI_fl + DEI_fl + UTS_fl, factor_loadings)
    return(reg)
  }else if (tipo == "market"){
    reg = lm(risk_premia ~ MP_fl + UI_fl + DEI_fl + UTS_fl + colcap, factor_loadings)
    return(reg)
  }else if (tipo == "petroleum"){
    reg = lm(risk_premia ~ MP_fl + UI_fl + DEI_fl + UTS_fl + brent, factor_loadings)
    return(reg)
  }else if (tipo == "market_petroleum"){
    reg = lm(risk_premia ~ MP_fl + UI_fl + DEI_fl + UTS_fl + colcap + brent, factor_loadings)
    return(reg)
  }
}

# Estimaciones de la etapa 2. para calcular las diferentes factor risk premium para cada especificación

# 1. Sin petroleo ni market
none_risk_p = factor_risk_premium(none, completa, "none")
# 2. Con market sin petroleo
market_risk_p = factor_risk_premium(market, completa, "market")
# 3. Con petroleo sin market 
petroleum_risk_p = factor_risk_premium(petroleum, completa, "petroleum")
# 4. Con market con petroleo
market_petroleum_risk_p = factor_risk_premium(market_petroleum, completa, "market_petroleum")


##### Script artículo final macroeconomía financiera #####

library(tidyverse)
library(readxl)
library(dynlm)
library(xts)
library(stargazer)
library(corrplot)

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
variables$UTS = variables$tasa_1_año_numero - variables$tasa_10_años_numero

# Matriz de correlaciones entre los diferentes factores macroeconómicos que se usaran en las estimaciones
variables_simple = as_tibble(variables) %>% 
  select(MP, UI, DEI, UTS, colcap, brent_dolares)

source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(variables_simple)

# 3. Regresiones ----

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
  completa2 = as_tibble(completa)
  prov = factor_loadings
  if (tipo == "none"){
    coef = tibble(constant = double(), MP_fl = double(), UI_fl = double(), DEI_fl = double(), UTS_fl = double())
    t_values = tibble(constant = double(), MP_fl = double(), UI_fl = double(), DEI_fl = double(), UTS_fl = double())
    for (month in 2:nrow(completa2)){
      vector_returns = c()
      for (equity in 1:15){
        vector_returns = append(vector_returns, completa2[month,equity])  
      }
      prov$month_var = as.double(vector_returns)
      reg = lm(month_var ~ MP_fl + UI_fl + DEI_fl + UTS_fl, prov) 
      other_month = month - 1
      coef[other_month,] = t(summary(reg)$coefficients[,1])
      t_values[other_month,] = t(summary(reg)$coefficients[,3])
    }
    final = data.frame(constant = double(), MP_fl = double(), UI_fl = double(), DEI_fl = double(), UTS_fl = double())
    mean_factor = c()
    mean_t = c()
    for (factor in 1:ncol(coef)){
      mean_factor = append(mean_factor, mean(coef[[factor]], na.rm = TRUE))
      mean_t = append(mean_t, mean(t_values[[factor]], na.rm = TRUE))
    }
    final[1, ] = t(as.double(mean_factor))
    final[2, ] = t(as.double(mean_t))
    row.names(final) = c("value", "t")
    return(final)
  }else if (tipo == "market"){
    coef = tibble(constant = double(), MP_fl = double(), UI_fl = double(), DEI_fl = double(), UTS_fl = double(), colcap = double())
    t_values = tibble(constant = double(), MP_fl = double(), UI_fl = double(), DEI_fl = double(), UTS_fl = double(), colcap = double())
    for (month in 2:nrow(completa2)){
      vector_returns = c()
      for (equity in 1:15){
        vector_returns = append(vector_returns, completa2[month,equity])  
      }
      prov$month_var = as.double(vector_returns)
      reg = lm(month_var ~ MP_fl + UI_fl + DEI_fl + UTS_fl + colcap, prov) 
      other_month = month - 1
      coef[other_month,] = t(summary(reg)$coefficients[,1])
      t_values[other_month,] = t(summary(reg)$coefficients[,3])
    }
    final = data.frame(constant = double(), MP_fl = double(), UI_fl = double(), DEI_fl = double(), UTS_fl = double(), colcap = double())
    mean_factor = c()
    mean_t = c()
    for (factor in 1:ncol(coef)){
      mean_factor = append(mean_factor, mean(coef[[factor]], na.rm = TRUE))
      mean_t = append(mean_t, mean(t_values[[factor]], na.rm = TRUE))
    }
    final[1, ] = t(as.double(mean_factor))
    final[2, ] = t(as.double(mean_t))
    row.names(final) = c("value", "t")
    return(final)
  }else if (tipo == "petroleum"){
    coef = tibble(constant = double(), MP_fl = double(), UI_fl = double(), DEI_fl = double(), UTS_fl = double(), brent = double())
    t_values = tibble(constant = double(), MP_fl = double(), UI_fl = double(), DEI_fl = double(), UTS_fl = double(), brent = double())
    for (month in 2:nrow(completa2)){
      vector_returns = c()
      for (equity in 1:15){
        vector_returns = append(vector_returns, completa2[month,equity])  
      }
      prov$month_var = as.double(vector_returns)
      reg = lm(month_var ~ MP_fl + UI_fl + DEI_fl + UTS_fl + brent, prov) 
      other_month = month - 1
      coef[other_month,] = t(summary(reg)$coefficients[,1])
      t_values[other_month,] = t(summary(reg)$coefficients[,3])
    }
    final = data.frame(constant = double(), MP_fl = double(), UI_fl = double(), DEI_fl = double(), UTS_fl = double(), brent = double())
    mean_factor = c()
    mean_t = c()
    for (factor in 1:ncol(coef)){
      mean_factor = append(mean_factor, mean(coef[[factor]], na.rm = TRUE))
      mean_t = append(mean_t, mean(t_values[[factor]], na.rm = TRUE))
    }
    final[1, ] = t(as.double(mean_factor))
    final[2, ] = t(as.double(mean_t))
    row.names(final) = c("value", "t")
    return(final)
  }else if (tipo == "market_petroleum"){
    coef = tibble(constant = double(), MP_fl = double(), UI_fl = double(), DEI_fl = double(), UTS_fl = double(), colcap = double(), brent = double())
    t_values = tibble(constant = double(), MP_fl = double(), UI_fl = double(), DEI_fl = double(), UTS_fl = double(), colcap = double(), brent = double())
    for (month in 2:nrow(completa2)){
      vector_returns = c()
      for (equity in 1:15){
        vector_returns = append(vector_returns, completa2[month,equity])  
      }
      prov$month_var = as.double(vector_returns)
      reg = lm(month_var ~ MP_fl + UI_fl + DEI_fl + UTS_fl + colcap + brent, prov) 
      other_month = month - 1
      coef[other_month,] = t(summary(reg)$coefficients[,1])
      t_values[other_month,] = t(summary(reg)$coefficients[,3])
    }
    final = data.frame(constant = double(), MP_fl = double(), UI_fl = double(), DEI_fl = double(), UTS_fl = double(), colcap = double(), brent = double())
    mean_factor = c()
    mean_t = c()
    for (factor in 1:ncol(coef)){
      mean_factor = append(mean_factor, mean(coef[[factor]], na.rm = TRUE))
      mean_t = append(mean_t, mean(t_values[[factor]], na.rm = TRUE))
    }
    final[1, ] = t(as.double(mean_factor))
    final[2, ] = t(as.double(mean_t))
    row.names(final) = c("value", "t")
    return(final)
  }
}

# Estimaciones de la etapa 2. para calcular las diferentes factor risk premium para cada especificación

# 1. Sin petroleo ni market
none_risk_p = factor_risk_premium(none, completa, "none")
# # 2. Con market sin petroleo
market_risk_p = factor_risk_premium(market, completa, "market")
# # 3. Con petroleo sin market 
petroleum_risk_p = factor_risk_premium(petroleum, completa, "petroleum")
# # 4. Con market con petroleo
market_petroleum_risk_p = factor_risk_premium(market_petroleum, completa, "market_petroleum")
# 
# # 4. Presentación de resultados ----
# 
# stargazer(none_risk_p, market_risk_p, petroleum_risk_p, market_petroleum_risk_p, type = "latex",
#           title = "Primas de riesgo para los factores macroeconómicos seleccionados", keep.stat = c("n", "rsq"),
#           style = "AER")
# 
# 

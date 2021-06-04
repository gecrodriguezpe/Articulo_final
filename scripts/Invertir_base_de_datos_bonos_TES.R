#### Invertir base de datos bonos TES ####

library(readxl)

# Tasas de los bonos TES 

setwd("~/Documents/GitHub/semestre6_git/macroeconomia_financiera/Articulo_final/bases_de_datos/bases_pesonales")

tasas_tes = read_xlsx("tasas_bonos_sin_invertir.xlsx")

tasas_tes$fecha = as.Date(tasas_tes$fecha)
tasas_tes = tasas_tes %>%  
  map_df(rev)

write_xlsx(tasas_tes, "~/Documents/GitHub/semestre6_git/macroeconomia_financiera/Articulo_final/bases_de_datos/bases_pesonales/tasas_tes_ordern_correcto.xlsx")

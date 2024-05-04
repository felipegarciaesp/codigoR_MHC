rm(list=ls())
graphics.off()

# =====================================================================
# Lectura y extraccion de info climatica desde archivos NetCDF / Felipe Garcia
# =====================================================================

# Carga de paquetes
library(ncdf4)

# Definicion directorio de trabajo
setwd("C:/Users/Usuario/Codigos_R/datos_NETCDF")
getwd()

# Importar archivos NetCDF (los GCM).
name <- "pr_Amon_ACCESS-CM2_historical_r1i1p1f1_gn_185001-201412.nc"
name2 <- "pr_Amon_ACCESS-CM2_ssp585_r1i1p1f1_gn_201501-210012.nc"
#No olvidar cambiar los nombres name y name2.

# Comando para abrir arhivos tipo NetCDF:
pr <- nc_open(name)

# Comando para visualizar el archivo NetCDF que se ha importado:
View(pr)


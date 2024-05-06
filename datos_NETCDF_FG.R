rm(list=ls())
graphics.off()

# =====================================================================
# Lectura y extraccion de info climatica desde archivos NetCDF / Felipe Garcia
# =====================================================================

# Carga de paquetes
library(ncdf4)

# Definicion directorio de trabajo
setwd("C:/Users/Usuario/Codigos_R/leer_datos_NETCDF")
getwd()

# Importar archivos NetCDF (los GCM).
name <- "pr_Amon_ACCESS-CM2_historical_r1i1p1f1_gn_185001-201412.nc"
name2 <- "pr_Amon_ACCESS-CM2_ssp585_r1i1p1f1_gn_201501-210012.nc"
#No olvidar cambiar los nombres name y name2.

# Comando para abrir arhivos tipo NetCDF:
pr <- nc_open(name)

# Comando para visualizar el archivo NetCDF que se ha importado:
View(pr)

# Comando que muestra en Consola los atributos dentro de la variable "pr"
# de tipo NetCDF.
print(pr)

# Es importante explorar los archivos NetCDF, asi como tambien es importante
# cerrar el archivo NetCDF.

# Con "ncvar_get" se extraen valores de la variable de interes:
data <- ncvar_get(pr)

# Con "$" se puede extraer la informacion de la variable en cuestion:
print(pr$dim$lat)

# Con el comando anterior tenemos que los valores van desde -89.375
# hasta 89.375. Esto es, desde el Polo Sur hasta el Polo Norte.
# -89.375 quiere decir 89.375°S.

# Con rm() se borran las variables que ya no se van a utilizar. En nuestro
# caso, los valores de pr ya fueron pasados a la variable data.
rm(pr)

# Importamos los valores del GCM futuro:
pr2 <- nc_open(name2)

# Pasamos los valores que nos interesa utilizar a la variable data2:
data2 <- ncvar_get(pr2)

# Se guadan los valores de latitud y longitud del punto que queremos
# estudiar (Rio Cauquenes En Desembocadura, en este caso).

QNlat <- (-35.90)

# Queremos encontrar las coordenadas del NNetCDF que tienen esa latitud y
# longitud.

# Nos quedamos con la mayor coordenada dentro del archivo NetCDF que sea
# menos a la del punto de estudio.

corLat <- max(which(pr2$dim$lat$vals<QNlat))

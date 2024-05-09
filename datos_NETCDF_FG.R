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

latitud <- -35.90
longitud <- -72.05

QNlat <- latitud

# Queremos encontrar las coordenadas del NNetCDF que tienen esa latitud y
# longitud.

# =====================================================================
# Se puede hacer una funcion de lo que viene ahora (respecto a corLat y corLon)
# Revisa bien si el if que hace para determinar el verdadero indice esta correcto,
# independiene si QNlat es positivo o negativo.

# Nos quedamos con la mayor coordenada dentro del archivo NetCDF que sea
# menos a la del punto de estudio.

corLat <- max(which(pr2$dim$lat$vals<QNlat))

# corLat es por ende la latitud justo menor a las coordenadas de la estación
# Debe notarse que corLat es un resultado que indica un indice dentro de pr2$dim$lat$vals.

# En el siguiente if se compara la distancia entre QNLat, con el valor de latitudes del GCM y se verifica
# si la latitud justo menor a la más cercana o la más cercana es la justo mayor. De ser la mayor le
# suma 1 a la coordenada de corLat
if ((QNlat-pr2$dim$lat$vals[corLat])>(pr2$dim$lat$vals[corLat+1]-QNlat)){
  corLat=corLat+1}

# El objetivo de este if es que verifica si el punto de la grilla más cercano es el justo menor a la
# coordenada que estamos buscando, la cual tenemos guardada en corLat, de no ser asi, asume que la
# coordenada es la justo mayor

# Repetimos el proceso para la longitud.
# cuidado, con las coordenadas de la longitud, los GCM consideran longitud Este, por lo que para
# introducir la longitud en el caso que sea negativo (-70°), hay que restarle dicho valor a los
# 360° del planeta.
QNlon<-(360+longitud)

corLon<-max(which(pr2$dim$lon$vals<QNlon))
if ((QNlon-pr2$dim$lon$vals[corLon])>(pr2$dim$lon$vals[corLon+1]-QNlon)){
  corLon=corLon+1}

# =====================================================================

# Una vez que se encuentran las coordenadas más cercanas a la estación en el NetCDF,
# puedo seleccionar dichas coordenadas, trabajar con ellas y olvidarme del resto del
# archivo. OJO CON TRANSFORMAR UNIDADES!

# =====================================================================

# Se puede hacer una funcion para automatizar esto de dejar la data em aux y aux2.
# Y que ademas no asuma que todos los dias tienen 30 dias.

# Dentro de los archivos "data" y "data2", tenemos en la primera coordenada la longitud, en la segunda
# la latitud y en la tercera la serie de tiempo de precipitaciones, por lo que le entregamos
# la primera coordenada y la segunda, la tercera se deja vacía, dado que queremos obtener la
# serie de tiempo completa

aux<-data[corLon,corLat, ]* (1000*3600*24*30/1000)
aux2<-data2[corLon,corLat, ]* (1000*3600*24*30/1000)

# Finalmente, tenemos que en aux y aux2 quedan almacenados una serie de tiempo mensual de
# precipitaciones simuladas por el GCM para el período histórico y para el futuro.

# =====================================================================

# Ya leímos los archivos, ahora pasemos a hacer un par de gráficos. Graficar los valores de
# precipitaciones de los NetCDF. Primero uno a nivel mensual, luego uno a nivel anual.

# Generamos una serie de tiempo con los años para graficar
year<-1850:2100

# Concatenamos la serie de tiempo del período histórico, junto a la serie de tiempo del período
# futuro "aux" y "aux2", para formar una única serie de tiempo "AUX"
AUX<-c(aux,aux2)
# Graficamos esta única serie de tiempo
plot(AUX, type="l")

# Reorganizamos la serie AUX en un arreglo con 2 dimensiones, en la cual la primera dimensión tiene
# 12 valores (uno por cada mes) y el segundo tiene el número de años, el cual se calcula como el
# cuociente entre el número total de meses (length(AUX)) y 12 (12 meses por año).
Norder<-array(AUX,c(12,length(AUX)/12))

# Ahora pasamos a sumar los valores, manteniendo la segunda dimensión (es decir suma en la dimensión
# de los meses) para llegar a los valores anuales
AUXAn<-apply(Norder,2,sum)

# Por último pasamos a graficar una línea que tiene los valores anuales que acabamos de obtener
plot(year,AUXAn, type="l", ylab = "Precipitación [mm]",xlab = "Años",
     col=rgb(.22,.35,.85) , ylim = range(0,max(AUXAn)*1.8))



rm(list=ls())
graphics.off()

# =====================================================================
# Lectura y extraccion de info climatica desde archivos NetCDF / Felipe Garcia
# =====================================================================

# Carga de paquetes
library(ncdf4)
library(xlsx)

# Definicion directorio de trabajo
# Es importante que el directorio de trabajo no sea muy largo, podría ocurrir que
# te lea dos veces el mismo archivo netcdf.
setwd("C:/Users/Usuario/Codigos_R/leer_datos_NETCDF")
getwd()

# Definicion de funciones:
coordenadas <- function(ID, netcdf, lat, lon) {
  QNlat <- lat
  corLat <- max(which(netcdf$dim$lat$vals<QNlat))
  if ((QNlat-netcdf$dim$lat$vals[corLat])>(netcdf$dim$lat$vals[corLat+1]-QNlat)){
    corLat=corLat+1
  }
  
  if (lon < 0 && ID == "GCM") {
    QNlon <- (360 + lon)
  } else {
    QNlon <- (lon)
  }
  
  corLon<-max(which(netcdf$dim$lon$vals<QNlon))
  if ((QNlon-netcdf$dim$lon$vals[corLon])>(netcdf$dim$lon$vals[corLon+1]-QNlon)){
    corLon=corLon+1
  }
  resultados <- list(corLat = corLat, corLon = corLon)
  return(resultados)
}

#OJO QUE ESTA FUNCION ESTA HECHA SOLO PARA pr_Amon e historical.
nombre_GCM <- function(filename) {
  nombre <- gsub("pr_Amon_", "", filename) #Se borra el prefijo.
  nombre <- gsub("historical_", "", nombre) #Se borra el historical.
  nombre <- sub("(_gn|_gr).*", "", nombre) #Se borra todo a la derecha de _gn o _gr (inclusive)
  return(nombre)
}

# Funcion que arroja True or False si lo que se ingresa como "texto" está o no en
# la cadena original.
contiene <- function(texto, cadena_original) {
  boolean <- grepl(texto, cadena_original)
  return(boolean)
}

# Función para abrir los netcdf, extraer la variable que nos interesa y rellenar el
# dataframe vacío.
ExtraerRellenar <- function(name_hist, name_fut_1, name_fut_2, df, col, ID, lat, lon) {
  # name_hist: nombre del archivo netcdf historico
  # name_fut1: nombre del archivo netcdf futuro del escenario 1 (ssp245).
  # name_fut2: nombre del archivo netcdf futuro del escenario 2 (ssp585).
  # col: es el numero de columna del df que se va a rellenar.
  # A los tres archivos se les extraerá la data, ya sea pr, tas, hurs, rsds, o lo que
  # sea que se haya descargado.
  
  #Pasos para extraer data de netcdf historico:
  var_hist <- nc_open(name_hist) #abrir netcdf historico.
  data_hist <- ncvar_get(var_hist) #extraer data de interes.

  #Se repiten los pasos para extraer data de los dos netcdf futuro:
  var_fut_1 <- nc_open(name_fut_1)
  data_fut_1 <- ncvar_get(var_fut_1)
  
  var_fut_2 <- nc_open(name_fut_2)
  data_fut_2 <- nc_open(var_fut_2)
  
  #Vamos a ocupar el var_hist para identificar el nodo del archivo netcdf que
  #nos va a servir para el area de estudio:
  
  coords_GCM <- coordenadas(ID, var_hist, lat, lon)
  
  corLat <- coords_GCM$corLat
  corLon <- coords_GCM$corLon
  
  # Dentro de los archivos "data_hist", "data_fut_1" y "data_fut_2", tenemos en 
  # la primera coordenada la longitud, en la segunda la latitud y en la tercera 
  # la serie de tiempo de precipitaciones, por lo que le entregamos la primera 
  # coordenada y la segunda, la tercera se deja vacía, dado que queremos obtener 
  # la serie de tiempo completa. Se mutiplica por un factor para tener una serie 
  # de precipitaciones mensuales en mm. Se asume que los días del mes tienen 30 días.
  
  aux_hist  <- data_hist[corLon,corLat, ] * (1000*3600*24*30/1000)
  aux_fut_1 <- data_fut_1[corLon,corLat, ] * (1000*3600*24*30/1000)
  aux_fut_2 <- data_fut_2[corLon,corLat, ] * (1000*3600*24*30/1000)
  
  # Finalmente, tenemos que en aux_hist, aux_fut_1 y aux_fut_2 quedan almacenados 
  # una serie de tiempo mensual de precipitaciones simuladas por el GCM para el 
  # período histórico y para el periodo futuro.
  
  # Concatenamos dos series de tiempo: una que tenga la serie historica seguida
  # de la serie del escenario 1 (ssp245) y otra que tenga la serie historica seguida
  # de la serie del escenario 2 (ssp585).
  # De esta forma obtenemos:
  # AUX_1: hist + ssp245.
  # AUX_2: hist + ssp585.
  
  AUX_1<-c(aux_hist, aux_fut_1)
  AUX_2<-c(aux_hist, aux_fut_2)
  
  # Generamos un vector de fechas a nivel mensual desde 1850-01 hasta 2100-12.
  # Es de notar que estamos considerando que los GCM históricos comienzan en 01/1850 y
  # que los GCM futuros terminan en 12/2100.
  fechas <- seq(from = as.Date("1850-01-01"), to = as.Date("2100-12-31"), by = "month")
  
  # Se rellenan los datos historicos y futuros (ambos escenarios) del GCM en cuestión.
  df[[1]][, col] <- AUX_1
  df[[2]][, col] <- AUX_2
  
  #Cerramos los archivos netcdf:
  nc_close(var_hist)
  nc_close(var_fut_1)
  nc_close(var_fut_2)
  
  return(df)
}


# Lectura de archivos netcdf en carpeta y posterior exportacion a Excel.

# 1) Directorio que contiene archivos netcdf:
netcdf_files_CMIP6 <- paste0(getwd(),"/netcdf_files/CMIP6")

# 1.1) Seteamos este directorio como el nuevo directorio de trabajo.
setwd(netcdf_files_CMIP6)

# 2) Lista de archivos NetCDF en el directorio:
# Este codigo va a arrojar una lista con la ruta completa, desde C:/User/Usuario ...
archivos <- list.files(netcdf_files_CMIP6, pattern ="\\.nc$", full.names = TRUE)

# 3) Obtener solo los nombres de los archivos:
NombresArchivos<- basename(archivos)
nombres_netcdf <- NombresArchivos

# 4) Obtenemos 3 listas, cada uno con los nombres de los escenarios a evaluar 
# (historico, ssp245, ssp585):
nombres_netcdf_hist <- nombres_netcdf[contiene("historical",nombres_netcdf)=='TRUE']
nombres_netcdf_sc1 <- nombres_netcdf[contiene("ssp245",nombres_netcdf)=='TRUE']
nombres_netcdf_sc2 <- nombres_netcdf[contiene("ssp585",nombres_netcdf)=='TRUE']

# 5) Nos quedamos con una lista que contiene solamente los nombres de los GCM:
nombres_netcdf <- nombre_GCM(nombres_netcdf_hist) #Podemos ocupar el historico o cualquier otro.

# 6) Generamos un vector de fechas a nivel mensual desde 1850-01 hasta 2100-12:
fechas <- seq(from = as.Date("1850-01-01"), to = as.Date("2100-12-31"), by = "month")
num_fechas <- length(fechas)

# 7) Creamos un dataframe que contengan los nombres de estos archivos:
nombres_gcm <- data.frame(
  'hist' = nombres_netcdf_hist,
  'ssp245' = nombres_netcdf_sc1,
  'ssp585' = nombres_netcdf_sc2
)

# Transpone el dataframe para que en las columnas queden los nombres de los
# modelos:
nombres_gcm <- t(nombres_gcm)

# Cambiamos los nombres de las columnas para que tengan el nombre de cada modelo
# evaluado
colnames(nombres_gcm) <- nombres_netcdf

# 8) Creamos un dataframe vacío con los GCM como nombres de las columnas.
# Es importante que el dataframe vacio sea inicializado con la cantidad de filas
# que tendrá una vez rellenado.

df_empty <- data.frame(matrix(ncol = length(nombres_netcdf), nrow = num_fechas))

df <- list(
  df_empty,
  df_empty
)

# Asignar nombres de columnas y filas a cada dataframe en la lista:
for (i in 1:2) {
  colnames(df[[i]]) <- nombres_netcdf
  rownames(df[[i]]) <- fechas
}


# 9) Se indican las coordenadas del punto a estudiar. Deben estar en grados decimales.

latitud <- -24.38851
longitud <- -69.16828

# 8) Obtenemos los nombres de los archivos netcdf de 2 en 2.
# Este paso es importante, ya que vamos a asumir lo siguiente para este codigo:
# a) La carpeta con los archivos netcdf siempre van a tener el archivo con data
#    histórica + el archivo con el escenario a evaluar (ssp245, ssp585, etc).
# b) La carpeta va a ordenar los archivos de manera alfabetica, por lo que para
#    cada GCM tendremos el archivo historico seguido del escenario futuro a evaluar.

#col = 1
#for (i in seq(1, length(NombresArchivos), by = 2)) {
#  name_hist <- NombresArchivos[i]
#  name_fut <- NombresArchivos[i+1]
#  # Se llama a la funcion ExtraerRellenar para extraer la data de los GCM y rellenar
#  # el dataframe df.
#  # Recordar que el ID es para diferenciar un GCM del producto CR2MET.
#  df <- ExtraerRellenar(name_hist, name_fut, df, col, "GCM",latitud, longitud)
#  col <- col + 1
#}

# 10) Se procede a leer cada archivo netcdf, se extraen los resultados de interes
# y se pasa la información a un archivo Excel.

# 9) Se indican datos de la estacion y de la variable climatica estudiada:
N_Estacion <- "1"
Nombre_Estacion <- "LagunaSeca"
var <- "pr"
Escenario <- "ssp585"
Nombre_Archivo <- paste0(N_Estacion, "_", Nombre_Estacion, "_", var, "_", Escenario, "_raw_analysis.xlsx")  

# 10) Exportacion a Excel:
# Primero se filtra el dataframe para las fechas a evaluar (no nos va a interesar
# toda la data desde 1850):

# Se definen las fechas de inicio y final que interesa para el estudio:
fecha_inicio <- as.Date("1950-01-01")
fecha_fin <- as.Date("2100-12-01")

# Se filtran las filas del df donde las fechas están dentro del rango especificado:
df_filtrado <- df[rownames(df) >= fecha_inicio & rownames(df) <= fecha_fin, ]

# Se cambia el directorio y se exporta el dataframe a Excel:
# El directorio se cambia nuevamente para que el Excel quede guardado en la 
# misma ruta del codigo y no donde están los archivos netcdf.
setwd("C:/Users/Usuario/Codigos_R/leer_datos_NETCDF")
write.xlsx(df_filtrado, Nombre_Archivo)

#####
# EL CODIGO DE ARRIBA PERMITE EXTRAER LA DATA HISTORICA Y FUTURA DE CADA GCM Y
# ALMACENARLA EN UN ARCHIVO EXCEL. PROXIMOS DESAFIOS:
# 1. DESCARGAR/CONSEGUIRSE MAS GCM QUE HAYAS OCUPADO EN LAGUNA SECA, PARA SEGUIR
#    VALIDANDO ESTE CODIGO.
# 2. SOLO SI VALE LA PENA, HAZ UNA FUNCION QUE TOME LA CANTIDAD DE DIAS DEL MES.
#    ESTO PARA NO MULTIPLICAR POR 30 AL MOMENTO DE LA COVNERSIÓN PARA PASAR LA
#    PP A MM/MES.
# 3. OCUPANDO LOS ARGUMENTOS DE ncvar_get PUEDES SACAR DE UNA EL RANGO DE FECHAS
#    QUE TE INTERESAN. NO SERIA NECESARIO SACAR TODA LA DATA Y DESPUES HACER UN
#    FILTRADO. PRUEBA ESTO A VER QUE RESULTA.
#####



# Importar archivos NetCDF (los GCM).
name <- "pr_Amon_ACCESS-CM2_historical_r1i1p1f1_gn_185001-201412.nc"
name2 <- "pr_Amon_ACCESS-CM2_ssp585_r1i1p1f1_gn_201501-210012.nc"
#No olvidar cambiar los nombres name y name2.

# Comando para abrir arhivos tipo NetCDF:
pr <- nc_open(name)

# Extraemos el nombre del GCM:
nombre <- pr$filename
nombre <- nombre_GCM(nombre)


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

latitud <- -24.38851 #-35.90
longitud <- -69.16828 #-72.05

# Datos de la estacion y de la variable climatica estudiada:
N_Estacion <- "1"
Nombre_Estacion <- "LagunaSeca"
var <- "pr"
Escenario <- "ssp585"
Nombre_Archivo <- paste0(N_Estacion, "_", Nombre_Estacion, "_", var, "_", Escenario, "_raw_analysis.xlsx")  

# Se encuentran los indices de los archivos NetCDF que están más cercanas a esa
# latitud y longitud:
coords_GCM <- coordenadas(ID = "GCM", netcdf =pr2, lat = latitud, lon = longitud)

corLat <- coords_GCM$corLat
corLon <- coords_GCM$corLon

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
#year<-1850:2100

# Concatenamos la serie de tiempo del período histórico, junto a la serie de tiempo del período
# futuro "aux" y "aux2", para formar una única serie de tiempo "AUX"
AUX<-c(aux,aux2)

# Generamos un vector de fechas a nivel mensual desde 1850-01 hasta 2100-12:
fechas <- seq(from = as.Date("1850-01-01"), to = as.Date("2100-12-31"), by = "month")

# Generamos un DataFrame con los datos concatenados
df_pr <- data.frame(
  Fecha = fechas, pr = AUX
)

df_pr_filtrado <- subset(df_pr, Fecha >= as.Date("1950-01-01") &
                           Fecha <= as.Date("2100-12-01"))

# Cambiamos el nombre de la columna que tiene las variables a estudiar por el nombre del GCM:
colnames(df_pr_filtrado)[colnames(df_pr_filtrado)=='pr'] <- nombre

# Se exporta el dataframe a Excel
write.xlsx(df_pr_filtrado, Nombre_Archivo)


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

# Ahora queda leer los archivos de los datos del CR2MET V2.0 de precipitaciones, para
# trabajar con estos datos y poder agregarlos al gráfico.

# Para poder graficar contra los datos observados, tenemos que importar los
# datos observados. para esto importamos el archivo CR2MET de precipitaciones
# mensuales
name3<-"CR2MET_pr_v2_0_mon_1979_2019_005deg.nc"

# Abrimos el archivo tipo NetCDF
pr3<-nc_open(name3)

coords_CR2MET <- coordenadas(ID = "CR2MET", netcdf =pr3, lat = latitud, lon = longitud)

corLat_2 <- coords_CR2MET$corLat
corLon_2 <- coords_CR2MET$corLon

# Si se quiere ser más eficiente en el uso de la memoria RAM del computador, o
# en caso de que el archivo que se esté abriendo sea tan grande que el computador
# no sea capaz de leerlo. Se recomienda en vez de extraer todos los datos del
# NetCDF, extraer con "ncvar_get" solo los datos que nos interesan.

# Primero estimar el número de valores en el tiempo
nt<-dim(pr3$dim$time$vals)

# Luego dentro del comando "ncvar_get", además de darle la varaible tipo NetCDF
# hay que especificar la varaible que se va a extraer, en este caso la
# precipitación, que tiene nombre "pr". Luego, el punto de partida de los datos
# que serán extraídos con "start", este tiene las coordenadas y un "1" dado que
# comenzaremos con el primer mes. Por último "count" nos dice cuántas variables
# vamos a extraer por dimensión, en este caso es 1 longitud, 1 latitud y todo el
# tiempo. Esto se guarda en "a".
a<-ncvar_get(pr3,varid="pr",start = c(corLon_2,corLat_2,1), count=c(1,1,nt))

x<-1979:2019

# Se cambia las dimensiones del arreglo de vector columna a un arreglo con 2 dimensiones,
# en la primera (filas) están los meses y columnas los años.
Norder1<-array(a,c(12,nt/12))

# Al llegar a este punto, en vez de guardar el dato de un solo mes, suma los datos de
# los 12 meses, para obtener los valores de precipitación anual. El comando "apply",
# permite sumar (además de tener otras funciones). Sumas al usar "sum" y el 2, indica
# en qué dirección tiene que sumar
y<-apply(Norder1,2,sum)

# Este comando permite graficar en el mismo gráfico los datos medidos en Cuenca Cauquenes
matplot(x,y,type="l" , add = TRUE, col=rgb(.8,.15,.25))
legend(2035, 2800, legend=c("GCM", "Obs"), col=c(rgb(.22,.35,.85),rgb(.8,.15,.25)), lty=1, cex=0.8)

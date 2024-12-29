# codigoR_MHC
Codigo en R realizado para el Diplomado en Modelacion Hidrologica de Cuencas. El objetivo del código es cargar datos de tipo NetCDF.

Consideraciones del código:
1. Los GCM históricos comienzan en Enero/1850 y los GCM futuro terminan en Diciembre/2100. Ojo que no todos los GCM evaluados tendrán este periodo completo.
2. La carpeta con los archivos netcdf siempre va a tener el archivo con data histórica + los dos archivos con los escenarios a evaluar (ssp245 y ssp585 en el mommento de hacer este codigo).
3. La carpeta va a ordenar los archivos de manera alfabética, por lo que para cada GCM tendremos el archivo histórico seguido de los escenarios futuros a evaluar.
4. Tener cuidado con dejar algún archivo netcdf sin alguno de los escenarios (historico, ssp245 o ssp585). El codigo actual asume que cada archivo tiene 3 escenarios en la carpeta: historico, ssp245 y ssp585.

Tareas a realizar para el codigo:
1. Cambia el nombre del vector 'nombres_netcdf' que le da el nombre a las columnas del df 'nombres_gcm' para que no se confunda con el de la linea 131

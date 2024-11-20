# codigoR_MHC
Codigo en R realizado para el Diplomado en Modelacion Hidrologica de Cuencas. El objetivo del código es cargar datos de tipo NetCDF.

Consideraciones del código:
1. Los GCM históricos comienzan en Enero/1850 y los GCM futuro terminan en Diciembre/2100.
2. La carpeta con los archivos netcdf siempre va a tener el archivo con data histórica + el archivo con el escenario a evaluar (ssp245, ssp585, etc.).
3. La carpeta va a ordenar los archivos de manera alfabética, por lo que para cada GCM tendremos el archivo histórico seguido del escenario futuro a evaluar.

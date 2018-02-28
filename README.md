# VegCommunity

## Español
Este proyecto consta de una simple función que pretende ayudar a los biólogos que no saben programar en R para calcular medidas básicas a nivel de comunidad como área basal, cobertura de copa, altura media, riqueza de especies por parcela. 

### Preparación de datos
El único detalle es que hay que preparar la base de datos para poder hacer estos cálculos. Los nombres de la base de datos deben de contener los siguientes campos:
  * Sitio.
  * DAP1, DAP2 ... hasta DAPn.
  * Cobertura.
  * Altura.
  * Especie.
  
Consultar el archivo ejemplo.csv para un ejemplo.

https://github.com/JonathanVSV/VegCommunity/blob/master/Ejemplo.csv

Además se puede consultar un ejemplo de como cargar la función y correr la rutina: 

https://github.com/JonathanVSV/VegCommunity/blob/master/Example.R

## English
This project is a simple function that is aimed at helping non-R-coder biologists to calculate basic plant community attributes such as Basal area, crown cover, mean height, species richness per sampling plot. Th only detail is that the data has to be inserted in a specific format so the function can work properly.

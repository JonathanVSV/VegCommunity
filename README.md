# VegCommunity

## Español
Este proyecto consta de una simple función que pretende ayudar a los biólogos que no saben programar en R para calcular medidas básicas a nivel de comunidad como área basal, cobertura de copa, altura media, riqueza de especies por parcela. 

## Uso de función

### Instalar R
Ir a https://cran.r-project.org/
Instalar la versión de R más reciente

### Dentro de R
En la pantalla que aparece al abrir R (consola) escribir:

```
install.packages("vegan")
```
Escoger cualquier ubicación (de preferencia cerca) para instalar el paquete.

Una vez instalado:
Irse a Archivo -> Nuevo documento.
Toda la siguiente sección se escribe en el nuevo documento.

```
library("vegan")
```
Esto carga el paquete de vegan en el espacio de trabajo

A continuación hay que definir el espacio de trabajo, es decir, la carpeta donde se van a leer y escribir los archivos por default.
Revisar que la ruta esté bien escrita y en lugar de utilizar \, sustituirlos por /

```
setwd("directorio local") #p.ej. "C:/users/documents/folder"
```

### Preparación de datos
El único detalle es que hay que preparar la base de datos para poder hacer estos cálculos. Los nombres de la base de datos deben de contener los siguientes campos como nombres de las columnas:
  * Sitio.
  * DAP1, DAP2 ... hasta DAPn.
  * Cobertura.
  * Altura.
  * Especie.
  
Consultar el archivo ejemplo.csv para un ejemplo.
[ejemplo.csv](Ejemplo.csv)

Además se puede consultar un ejemplo de como cargar la función y correr la rutina: 
[ejemplo.R](Example.R)

Después hay que leer el archivo con los datos (previamente guardado desde excel como .csv o delimitado por comas), recordar escribirlo con la extensión, .csv

```
datos<<-read.csv("BasedeDatos.csv",stringsAsFactors = F,header=T)
```

### Uso de la función veg_community

Recordar que todas las medidas están o deberían estar en metros cuadrados, a excepción del DAP, que debe estar en cm.

Los parámetros de la función son los siguientes: 
1. radio se refiere al radio del círculo si es parcela circular, o a un lado del cuadrado si la parcela es cuadrada. En caso de no tratarste de parcelas de alguno de estos dos tipos, escoger una de las dos figuras que den el área muestrada.
2. parcela puede ser circular o cuadarada solamente. Se debe escribir la palabra entre comillas.
3. DAP_ini se refiere al número de columna en la base de datos en el cual empiezan los DAP.
4. DAP_fin se refiere al número de columna en el cual acaban los DAP.

A continuación podemos utilizar la función de la siguiente manera:

Guardar el archivo de la función en el directorio de trabajo.
[veg_community.R](veg_community.R)

Escribir en el nuevo documento: 

```
source("veg_community.R")
```

Esto carga la función a nuestro espacio de trabajo

Una vez, realizado lo anterior se puede utilizar la función, de la siguiente manera:

```
veg_community(radio=10,parcela="cuadrada",DAP_ini=4,DAP_fin=6)
```

Ya que haya corrido la función ir al directorio de trabajo a buscar los archivos generados.

¡Listo!



## English
This project is a simple function that is aimed at helping non-R-coder biologists to calculate basic plant community attributes such as Basal area, crown cover, mean height, species richness per sampling plot. Th only detail is that the data has to be inserted in a specific format so the function can work properly.

(under construction)

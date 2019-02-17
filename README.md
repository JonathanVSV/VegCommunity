# VegCommunity

## Español
Este proyecto consta de una simple función que pretende ayudar a los biólogos que no saben programar en R para calcular medidas básicas a nivel de comunidad como área basal, cobertura de copa, altura media, riqueza y diversidad de especies por parcela. 

## Uso de función

### Instalar R
Ir a [CRAN](https://cran.r-project.org/)
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

A continuación hay que definir el espacio de trabajo, es decir, la carpeta donde se van a leer y escribir los archivos por default.
Revisar que la ruta esté bien escrita y en lugar de utilizar \ (símbolo utilizado por default en windows), sustituirlos por /

```
setwd("directorio local") #p.ej. "C:/users/documents/folder"
```

### Importación de datos
Para ejemplificar, se añadió un archivo de ejemplo de una base de datos.
[ejemplo.csv](Ejemplo.csv)
Primero, hay que leer el archivo con los datos (previamente guardado desde excel como .csv o delimitado por comas) de la siguiente manera:

```
datos<-read.csv("Ejemplo.csv",header=T)
```

Esto importa nuestros datos al objeto "datos" en el entorno de R

### Preparación de datos
Para preparar los datos el paquete cuenta con la función: veg_preparar

Se ejemplifica cómo se llama esa función con la base de Ejemplo.csv
Hay que utilizar la función veg_preparar() de la siguiente forma:
nombre del objeto donde se importaron los datos: datos.
número de columna con el número o nombre de sitios: 1.
número de columna con el nombre de especies: 2.
número de columna con la altura: 3.
número de columna donde comienza la información de DAP_ini: 4.
número de columna donde comienza la información de DAP_ini: 6.
número de columna con la cobertura de copas: 7.

```
datos<-veg_preparar(datos,sitio=1,especie=2,altura=3,DAP_ini=4,DAP_fin=6,cobertura=7)
```

En caso de que alguna de las variables antes mencionadas no sea haya medido se puede poner una F (de FALSE) para indicar que no se midió esa variable. Por ejemplo, si no se midió cobertura quedaría así: 

```
datos_prep<-veg_preparar(datos,sitio=1,especie=2,altura=3,DAP_ini=4,DAP_fin=6,cobertura=F)
```

### Uso de la función veg_community
Recordar que todas las medidas están o deberían estar en metros, a excepción del DAP, que debe estar en cm. Y recordar que antes de usar esta función se debe utilizar veg_preparar()

Los parámetros de la función son los siguientes: 
1. radio se refiere al radio del círculo si es parcela circular, o a un lado del cuadrado si la parcela es cuadrada. En caso de no tratarste de parcelas de alguno de estos dos tipos, escoger una de las dos figuras que den el área muestrada. P. ej., si se muestrearon rectángulos de 5 x 2 m = 10 m2, entonces se puede elegir un cuadrado con lado = 3.162277660168379; de tal manera que el área muestreada sean 10 m2. 
2. circular puede ser T (TRUE) para unidades de muestreo circulares o F (FALSE) para unidades de muestreo cuadaradas solamente. Se debe escribir T si es circular o F si es cuadrada.

A continuación podemos utilizar la función de la siguiente manera:

```
veg_community(datos_prep,radio=10,circular=T)
```

Ya que haya corrido la función ir al directorio de trabajo a buscar los archivos generados.

¡Listo!

Los archivos finales deberían quedar así con los datos del Ejemplo:

[Atributos por especie.csv](Atributos_Especie.csv)
[Atributos por especie y sitio.csv](Atributos_Especie_Sitio.csv)
[Atributos por sitio.csv](Atributos_Sitio.csv)

## English
This project is a simple function that is aimed at helping non-R-coder biologists to calculate basic plant community attributes such as Basal area, crown cover, mean height, species richness per sampling plot. Th only detail is that the data has to be inserted in a specific format so the function can work properly.

(under construction)

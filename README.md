# VegCommunity

English version see bottom section

## Español
Este proyecto consta de dos simples funciones que pretenden ayudar a los biólogos que no saben programar en R para calcular medidas básicas a nivel de comunidad como área basal, cobertura de copa, altura media, riqueza y diversidad de especies por parcela. 

## Uso de las funciones

### Instalar R
Ir a [CRAN](https://cran.r-project.org/)
Instalar la versión de R más reciente

### Dentro de R
En la pantalla que aparece al abrir R (consola) escribir:

```
install.packages("vegan")
```
Escoger cualquier ubicación (de preferencia cerca) para instalar el paquete.

Luego instalar el paquete devtools

```
install.packages("devtools")
```

Cargar el paquete devtools e instalar el paquete

```
library("devtools")
install_github("JonathanVSV/VegCommunity")
```

Una vez descargado el paquete, cargarlo en el espacio de trabajo

```
library(VegCommunity)
```

Una vez instalado:
Irse a Archivo -> Nuevo documento.
Toda la siguiente sección se escribe en el nuevo documento.

A continuación hay que definir el directorio de trabajo, es decir, la carpeta donde se van a leer y escribir los archivos por default.
Revisar que la ruta esté bien escrita y en lugar de utilizar \ (símbolo utilizado por default en windows), sustituirlos por /
Cambiar el directorio de trabajo mediante el siguiente comando: 

```
setwd("directorio local") #p.ej. "C:/users/documents/folder"
```

### Importación de datos
Para ejemplificar, se añadió un archivo de ejemplo de una base de datos.

Primero, hay que cargar el archivo con los datos de ejemplo de la siguiente manera:

```
data(dummy.df)
```
Esto va a cargar el objeto dummy.df en nuestro espacio de trabajo. Se puede escribir el siguiente comando para ver el contenido del objeto:

```
dummy.df
```

Para utilizar datos propios hay que guardar los datos desde excel como .csv o delimitado por comas y después importarlo a R de la siguiente manera:

```
datos<-read.csv("Ejemplo.csv",header=T)
```

Esto importa nuestros datos al objeto "datos" en el entorno de R

Para utilizar los datos de dummy.df, los guardamos en el objeto datos
```
datos<-dummy.df
```

### Preparación de datos
Para preparar los datos el paquete cuenta con la función: veg_preparar

Se ejemplifica cómo se llama esa función con la base de dummy.df
Hay que utilizar la función veg_preparar() de la siguiente forma:
nombre del objeto donde se importaron los datos: datos.
número de columna con el número o nombre de sitios: 1.
número de columna con el nombre de especies: 2.
número de columna con la altura: 3.
número de columna donde comienza la información de DAP_ini: 4.
número de columna donde comienza la información de DAP_ini: 6.
número de columna con la cobertura de copas: 7.

```
datos_prep<-veg_preparar(datos,sitio=1,especie=2,altura=3,DAP_ini=4,DAP_fin=6,cobertura=7)
```

En caso de que alguna de las variables antes mencionadas no sea haya medido se puede poner una F (de FALSE) para indicar que no se midió esa variable. Por ejemplo, si no se midió cobertura quedaría así: 

```
datos_prep<-veg_preparar(datos,sitio=1,especie=2,altura=3,DAP_ini=4,DAP_fin=6,cobertura=F)
```

### Uso de la función veg_community
Recordar que todas las medidas están o deberían estar en metros, a excepción del DAP, que debe estar en cm. Y recordar que antes de usar esta función se debe utilizar veg_preparar(). En este ejemplo, los datos procesados por la función veg_preparar se guardaron en el objeto datos_prep.

Los parámetros de la función son los siguientes:
1. datos preparados que deben provenir de la función veg_preparar().
2. radio se refiere al radio del círculo si es parcela circular, o a un lado del cuadrado si la parcela es cuadrada. En caso de no tratarste de parcelas de alguno de estos dos tipos, escoger una de las dos figuras que den el área muestrada. P. ej., si se muestrearon rectángulos de 5 x 2 m = 10 m2, entonces se puede elegir un cuadrado con lado = 3.162277660168379; de tal manera que el área muestreada sean 10 m2. 
3. circular puede ser T (TRUE) para unidades de muestreo circulares o F (FALSE) para unidades de muestreo cuadaradas solamente. Se debe escribir T si es circular o F si es cuadrada.

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
This project is a simple two-function package that is aimed at helping non-R-coder biologists to calculate basic plant community attributes such as basal area, crown cover, mean height, species richness and diversity indices per sampling plot. 

## Using the functions

### Installing R
Go to [CRAN](https://cran.r-project.org/)
and intall the newest version of R

### In R
In the first screen that appears (console) type:

```
install.packages("vegan")
```
Choose any location (it is better to choose close locations) and install the package.

Install devtools package

```
install.packages("devtools")
```

Load de package devtools and install VegCommunity package

```
library("devtools")
install_github("JonathanVSV/VegCommunity")
```

Once the package is downloadad load it in the workspace 

```
library(VegCommunity)
```

Once it is installed:
Go to File -> New document.
All the following commands will be written in this new document.

Next, set the working directory (the folder where the files will be read and written by default).
Check that the directory is well written. If you are using windows, substitute every \ (default symbol used by windows directories ) by /
Set the working directory by typing the following command:

```
setwd("directorio local") #p.ej. "C:/users/documents/folder"
```

### Importing data
As an example, a file with a dummy data frame was added in the package.
First, lets load the data frame by typing the following: 

```
data(dummy.df)
```

This will load the dumm.df object in our worskpace. By tying the following command you can see the contents of the object: 

```
dummy.df
```

To use your own data, you should save your data frame in excel or any similar software as .csv (comma separated values) and then import it in R with the command shown:

```
datos<-read.csv("Ejemplo.csv",header=T)
```

This command imports the data to the object "datos" in the R environment.

To use the data provided by dummy.df, this data should be saved inside the object datos
```
datos<-dummy.df
```

### Preparing the data
To prepare the data for analysis, the package counts with the function: veg_preparar

For example, for the data.frame provided by dumm.df (now saved in the datos object), the function 
veg_preparar() should be used following these arguments: 

name of the object where the data is saved: datos.
number of the column with the number or name of the sampling plots: 1.
number of the column where the species information is stored: 2.
number of the column with the height information: 3.
number of the column where DBH information starts: 4.
number of the column where DBH information ends: 6.
number of column with the crown cover information is stored: 7.

```
datos_prep<-veg_preparar(datos,sitio=1,especie=2,altura=3,DAP_ini=4,DAP_fin=6,cobertura=7)
```

If one of these variables was not measured, an F (for FALSE) can be set in order to indicate that variable was nor measured. For example, if crown cover was not measured, veg_preparar should be called like: 

```
datos_prep<-veg_preparar(datos,sitio=1,especie=2,altura=3,DAP_ini=4,DAP_fin=6,cobertura=F)
```

### Using veg_community
Before using veg_community, check that every measure should be in m or square meters (e.g., crown cover), except DBH which should be in cm. Remember that before using veg_community, data should be processed with veg_preparar. In this example, the output of veg_preparar was saved in the datos_prep object.

The arguments of the function are: 
1. data output by veg_preparar() function.
2. radio refers to the ratius of the circular plot (if the plot is circular) or to the length of a squared plot if the plot is not circular (sqaured). In the case that the plot is neither square nor circular, an equivalent sampling area can be defined. For exaple, if rectangular plots of 5 x 2 m = 10 m2 were used, then one can choose a square plot with a side = 3.162277660168379; so the sampling area is 10 m2. 
3. circular is boolean that can be T (TRUE) for circular sampling plots or F (FALSE) for square sampling plots. This argument can only take this two values, T if it is a circular plot or F if it is square.

The function can be implemented as following for the dummy.df: 

```
veg_community(datos_prep,radio=10,circular=T)
```

This means that circular plot where used and these, had a ratius of 10 m.
Once the function has finished, go to the working directory and look for the generated files. 

Done!

Final files should be equal to the following:

[Attributes per species.csv](Atributos_Especie.csv)

[Attributes per species and site.csv](Atributos_Especie_Sitio.csv)

[Attributes per site.csv](Atributos_Sitio.csv)

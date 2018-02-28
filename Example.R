#Como correr la rutina

#Cargar el paquete vegan
library("vegan")

#si no está instalado, escribir
install.packages("vegan")
#Escoger cualquier servidor

#Luego volver a correr
library("vegan")

#Revisar que la ruta esté bien escrita y en lugar de utilizar \, sustituirlos por /
setwd("directorio local") #p.ej. "C:/users/documents/folder"

#Leer el archivo con los datos, recordar escribirlo con la extensión, .csv
#Recordar haber guardado el archivo como .csv
datos<<-read.csv("BasedeDatos.csv",stringsAsFactors = F,header=T)

#Todas las medidas están en metros o metros cuadrados
#Ingresar datos de DAP en cm

#radio se refiere al radio del círculo si es parcela circular, o 
#a un lado del cuadrado si la parcela es cuadrada. En caso de no tratarste
#de parcelas de alguno de estos dos tipos, escoger una de las dos figuras
#que den el área muestrada.

#Parcela puede ser circular o cuadarada solamente

#DAP_ini se refiere al número de columna en el cual empiezan los DAP
#DAP_fin se refiere al número de columna en el cual acaban los DAP
veg_community(radio=10,parcela="cuadrada",DAP_ini=4,DAP_fin=6)

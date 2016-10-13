#' Análisis de vegetación por estratos
#'
#' Esta función saca descripciones de los sitios que se encuentran en Sitios unique.csv
#' @param BD=Base de datos del análisis en csv
#' @keywords descripción sitios
#' @export descripcion
#' @examples descripcion("db.csv",5,"Arboreo")
#' descripcion()

descripcion<-function(BD)
{
	#Checar sitios
	satasitt<-unique(BD$Id.Sitio)[order(unique(BD$Id.Sitio))]
	write.csv(satasitt,"Sitios unique.csv")
	length(unique(BD$Id.Sitio)[order(unique(BD$Id.Sitio))])
	print("Listo el listado de sitios únicos")
	
	#Checar especies
	unique(BD$Especie)[order(unique(BD$Especie))]
	espe<-table(BD$Especie)
	write.csv(espe[order(espe,decreasing=T)],"Especies_conteo.csv")
	print("Listo conteo de especies")
	
	#Número de familias
	length(unique(BD$Familia)[order(unique(BD$Familia))])
	famili<-table(BD$Familia)
	write.csv(famili[order(famili,decreasing=T)],"Familias_conteo.csv")
	print("Listo conteo de familias")
}


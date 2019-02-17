#' @title ENG Prepares data frame to be passed on to veg_structural
#'        SPA Prepara los datos para ser pasados a veg_structural
#'
#' @description ENG This function prepares data to be passed on to veg_community
#'        SPA Esta función prepara los datos para ser pasados a veg_community
#'
#' @param datos=datos data.frame. ENG Object where the data is stored must come from read.csv(...)
#'        SPA objeto donde se tienen guardados los datos, debe venir de un objeto creado mediante:
#'        read.csv(...)
#'
#' @param sitio=number numeric. ENG Number of column where the plot code or site name is stored
#' SPA Número de la columna donde se encuentra la información de nombre de cada unidad de muestreo o sitio
#'
#' @param especie=number numeric. ENG number of column species information, put F if no species data was acquired
#' SPA Número de columna con la información de las especies, poner F si no se registró esa información
#'
#' @param altura=number numeric. ENG number of column with height information, F if this information was not acquired
#' SPA número de columna con la información de altura, F si o se registró esta información
#'
#' @param DAP_ini=number numeric. ENG number of column that starts with DBH information, F if this information was not acquired
#' SPA número de columna en la que empieza la información de DAP, F si o se registró esta información
#'
#' @param DAP_fin=number numeric. ENG number of column that ends with DBH information, F if this information was not acquired
#' Remember to put DBH info in cm!
#' SPA número de columna en la que termina la información de DAP, F si o se registró esta información
#' Recuerda de poner los valores de DAP en cm
#' @param cobertura=number numeric. ENG number of column with crown cover information, F if this information was not acquired
#' SPA número de columna en la que empieza la información de cobertura, F si o se registró esta información
#'
#' @return ENG when a data.frame is imported, veg_preparar will return the same data.frame, but with its headers with different
#' names. SPA cuando se importa un data.frame veg_preparar regresa el mismo data.frame, pero con otros nombres de columna.
#'
#' @keywords ENG analysis, vegetation; SPA análisis, vegetación
#'
#' @examples veg_preparar("db.csv",1,2,4,5,9,3)
#'
#' @export veg_preparar

veg_preparar<-function(datos,sitio,especie,altura,DAP_ini,DAP_fin,cobertura)
{
  #Checar si tiene datos el archivo
  if(nrow(datos)==0)
   {
    print("El archivo de datos no contiene registros")
  }else{
    colnames(datos)[sitio]<-"Sitio"
    if(is.numeric(especie) == F)
    {
      datos[,ncol(datos)+1]<-rep(NA,nrow(datos))
      colnames(datos)[ncol(datos)+1]<-"Especie"
    }else{
      colnames(datos)[especie]<-"Especie"
    }
    if(is.numeric(altura) == F)
    {
      datos[,ncol(datos)+1]<-rep(NA,nrow(datos))
      colnames(datos)[ncol(datos)+1]<-"Altura"
    }else{
      colnames(datos)[altura]<-"Altura"
    }
    if(is.numeric(DAP_ini) == F)
    {
      datos[,ncol(datos)+1]<-rep(NA,nrow(datos))
      colnames(datos)[ncol(datos)+1]<-"DAPini"
    }else{
      colnames(datos)[DAP_ini]<-"DAPini"
    }
    if(is.numeric(DAP_fin) == F)
    {
      datos[,ncol(datos)+1]<-rep(NA,nrow(datos))
      colnames(datos)[ncol(datos)+1]<-"DAPfin"
    }else{
      colnames(datos)[DAP_fin]<-"DAPfin"
    }
    if(is.numeric(cobertura) == F)
    {
      datos[,ncol(datos)+1]<-rep(NA,nrow(datos))
      colnames(datos)[ncol(datos)+1]<-"Cobertura"
    }else{
      colnames(datos)[cobertura]<-"Cobertura"
    }
    colnames(datos)[cobertura]<-"Cobertura"

    return (datos)
  }
}

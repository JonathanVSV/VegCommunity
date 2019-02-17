#' @title ENG Calculates structural and diversity attributes of the plant community of each sampling plot
#'        SPA Calcula atributos estructurales y de diversidad de la comunidad vegetal de cada unidad de muestreo
#'
#' @description ENG This function calculates several attributes of a vegetation community, it
#' calculates structural characteristics per plot, per species and per species and plot. Import
#' data with encoding utf-8 and stringsAsFactors=F.
#'        SPA Esta función calcula distintas características de la vegetación, calcula las
#' características estructurales por sitio, por especie y por especie y sitio.importar datos
#' con encoding utf-8, stringsAsFactors=F
#'
#' @param datos=datos data.frame. ENG Object where the PREPARED data is stored must come from veg_preparar(...)
#'        SPA objeto donde se tienen guardados los datos PREPARADOS, debe venir de un objeto creado mediante:
#'        veg_preparar(...)
#'
#' @param radio=radio numeric. ENG ratius in meters of the circle or equivalent side of a square plot, e.g. 0.5,3,5
#' Equivalent side stands for example, for rectangular plots with 5 x 2 m = 10 m2 can be expressed as a square
#' plot with a square plot with a side = 3.162277660168379 m.
#' SPA radio en m del círculo o lado equivalente del cuadrado de muestreo, p.ej. 0.5,3,5.
#' El lado equivalente se refiere a, por ejemplo, se tienen rectangulos de  muestreo de 5 x 2 m = 10m2, puede
#' expresarse como un cuadrado de muestreo de lado =  3.162277660168379 m.
#'
#' @param circular=T Boolean. ENG If the plot is circular or not. If true, then data is taken a the ratius of a circular
#' plot, else, i.e., False, it is taken as a side of a squared plot.
#' SPA si se trata de una unidad de muestreo circular o cuadrada, circular, TRUE, el radio se da tal cual,
#' en cambio en cuadrada, FALSE, se da la raíz del área total de muestreo.
#'
#' @keywords ENG analysis, vegetation; SPA análisis, vegetación
#'
#' @examples veg_community("datos_preparados.csv",5,T)
#'
#' @export veg_community

veg_community<-function(datos,radio,circular=T)
  {
    if(nrow(datos)==0)
	  {
	    print("No tiene entradas la base de datos, revisar que se esté leyendo de manera adecuada el archivo")

	  }
	  #Forma de parcelas
	  #Total de superficie de muestreo
	  if(circular==T)
	  {
	    #Opción extrapolando por área de círculo
	    area.circ.sitio=(pi*radio^2)
	    #Total de superficie de muestreo
	    area.circ=(pi*radio^2)*length(unique(datos$Sitio))
	  }
	  if(circular==F)
	  {
	    #Opción extrapolando por área de círculo
	    area.circ.sitio=(radio^2)
	    #Total de superficie de muestreo
	    area.circ=(radio^2)*length(unique(datos$Sitio))
	  }

    #Quitar como factores Sitio y Especie
    datos<-as.data.frame(datos)
    datos$Sitio<-as.character(datos$Sitio)
    datos$Especie<-as.character(datos$Especie)
	  #Número de sitios
	  sitios<-unique(datos$Sitio)
	  #Para ver sitios ordenados y a ver si se repiten
	  num.sitios<-length(sitios)

	  #Lista final con resultados por Sitio y Especie
	  final.resul.arbol_sit<-vector("list",num.sitios)
	  #Los nombres que tienen guión los mete con ""
	  names(final.resul.arbol_sit)<-sitios

	  #DAP a AB
	  DAP_ini<-which(colnames(datos)=="DAPini")
	  DAP_fin<-which(colnames(datos)=="DAPfin")

	  DAP_temp<-datos[,seq(DAP_ini,DAP_fin,1)]
	  DAP_temp<-pi*((DAP_temp/200)^2)
	  ABtotal<-apply(DAP_temp,1,function (x) sum(x,na.rm=T))

	  datos[,ncol(datos)+1]<-ABtotal
	  colnames(datos)[ncol(datos)]<-"ABsuma"
	  #Base final de por sitios
	  resul.arbol<-as.data.frame(matrix(nrow=length(unique(datos$Especie)),ncol=17,
	                                    dimnames=list(c(),c("Especie","Ind","Dha-1","Dr",
	                                                        "Hm","Hmax","ABt","ABha-1","ABm",
	                                                        "ABr","Cobt","Cobha-1","Cobm",
	                                                        "Cobr","Fs","Fr","VIR/3"))))
	  resul.sit<-as.data.frame(matrix(nrow=length(sitios),ncol=9,
	                                  dimnames=list(c(),c("Sitio","Riqueza","Simpson","Shannon","InvSimpson",
	                                                      "Abun","Hm","Cobha-1","ABha-1"))))

	  #Este checarlo bien, de aquí mismo se calcula densidad
	  inds.arbol<-c(1:length(unique(datos$Especie)))
	  inds.sit<-c(1:length(sitios))
	  esp.sit<-by(datos$Especie,datos$Sitio,function(x) length(unique(x)))
	  abun.sit<-by(datos$Especie,datos$Sitio,length)
	  alt.arbol<-by(datos$Altura,datos$Especie,mean)
	  altmax.arbol<-by(datos$Altura,datos$Especie,max)
	  alt.sit<-by(datos$Altura,datos$Sitio,mean)
	  dapmean.arbol<-by(datos$ABsuma,datos$Especie,mean)
	  dapsum.arbol<-by(datos$ABsuma,datos$Especie,sum)
	  dapsum.sit<-by(datos$ABsuma,datos$Sitio,sum)
	  cobmean.arbol<-by(datos$Cobertura,datos$Especie,mean)
	  cobsum.arbol<-by(datos$Cobertura,datos$Especie,sum)
	  cobsum.sit<-by(datos$Cobertura,datos$Sitio,sum)
	  resul.arbol[,1]<-names(dapsum.arbol)

  #-------------------Cálculo por sitio--------------------------------------
	  resul.sit[,1]<-names(dapsum.sit)

	  for(k in 1:num.sitios)
	  {
	    #Diversidad
	    temp<-which(datos$Sitio==resul.sit[k,1])
	    subDatos<-datos[temp,]
	    #Abundancia
	    abundan<-as.data.frame(matrix(nrow=length(unique(subDatos$Sitio)),
	                                  ncol=length(unique(subDatos$Especie)),
	                                  dimnames=list(c(as.character(unique(subDatos$Sitio))),c(as.character(unique(subDatos$Especie))))))
	    abundan[is.na(abundan)] <- 0
	    for(h in 1:nrow(subDatos))
	    {
	      entrx<-which(row.names(abundan)==subDatos[h,1])
	      entry<-which(colnames(abundan)==subDatos[h,2])
	      abundan[entrx,entry]<-sum(1,abundan[entrx,entry])
	    }

	    #Diversidad por sitio
	    resul.sit[k,2]<-vegan::specnumber(abundan)
	    resul.sit[k,3]<-vegan::diversity(abundan, index="simpson")
	    resul.sit[k,4]<-vegan::diversity(abundan, index="shannon")
	    resul.sit[k,5]<-vegan::diversity(abundan, index="invsimpson")

	    #Abundancia
	    resul.sit[k,6]<-abun.sit[k]
	    #Altura promedio
	    resul.sit[k,7]<-alt.sit[k]
	    #Cob / ha
	    resul.sit[k,8]<-cobsum.sit[k]*10000/area.circ.sitio
	    #AB / ha
	    resul.sit[k,9]<-dapsum.sit[k]*10000/area.circ.sitio
	 }

	  export_arch<-paste0("Atributos_Sitio",".csv")
	  write.csv(resul.sit,export_arch,row.names=F)
	  print(paste0("Listo por Sitios "))

	  #------------------------Por especie y por sitio------------------------
	  for(j in 1:num.sitios)
	  {
	    #Datos por sitio
	    datos.sitios<-subset(datos,datos$Sitio==names(final.resul.arbol_sit)[j])
	    #Definir resul.arbol_sit en función de # spp. por sitio
	    resul.arbol_sit<-as.data.frame(matrix(nrow=length(unique(datos.sitios$Especie)),
	                                          ncol=17,
	                                          dimnames=list(c(),c("Especie","Ind","Dha-1","Dr",
	                                                              "Hm","Hmax","ABt","ABha-1","ABm",
	                                                              "ABr","Cobt","Cobha-1","Cobm",
	                                                              "Cobr","Fs","Fr","VIR/2"))))

	    #Este checarlo bien, de aquí mismo se calcula densidad
	    inds.arbol_sit<-c(1:(length(unique((datos.sitios$Especie)))*length(unique((datos.sitios$Especie)))))

	    alt.arbol_sit<-by(datos.sitios$Altura,datos.sitios$Especie,mean)
	    altmax.arbol_sit<-by(datos.sitios$Altura,datos.sitios$Especie,max)
	    dapmean.arbol_sit<-by(datos.sitios$ABsuma,datos.sitios$Especie,mean)
	    dapsum.arbol_sit<-by(datos.sitios$ABsuma,datos.sitios$Especie,sum)
	    cobmean.arbol_sit<-by(datos.sitios$Cobertura,datos.sitios$Especie,mean)
	    cobsum.arbol_sit<-by(datos.sitios$Cobertura,datos.sitios$Especie,sum)
	    resul.arbol_sit[,1]<-names(dapsum.arbol_sit)

	    #Llenar resul hasta cobertura / ha porque las demás necesitan totales (relativos)
	    for(i in 1:length(unique(datos.sitios$Especie)))
	    {
	      #Calcular abundancia y densidad escalada a 1 ha
	      inds.arbol_sit[i]<-sum(datos.sitios$Especie==resul.arbol_sit[i,1])

	      #Inds
	      resul.arbol_sit[i,2]<-inds.arbol_sit[i]
	      #Dha-1
	      resul.arbol_sit[i,3]<-inds.arbol_sit[i]*10000/area.circ.sitio

	      #Calcular Hm
	      resul.arbol_sit[i,5]<-alt.arbol_sit[i]
	      #Hmax
	      resul.arbol_sit[i,6]<-altmax.arbol_sit[i]

	        #Cob / ha
	        resul.arbol_sit[i,11]<-cobsum.arbol_sit[i]
	        #Cob / ha
	        resul.arbol_sit[i,12]<-cobsum.arbol_sit[i]*10000/area.circ.sitio
	        #Cob promedio
	        resul.arbol_sit[i,13]<-cobmean.arbol_sit[i]

	         #AB
	          resul.arbol_sit[i,7]<-dapsum.arbol_sit[i]
	          #AB / ha
	          resul.arbol_sit[i,8]<-dapsum.arbol_sit[i]*10000/area.circ.sitio
	          #AB promedio
	          resul.arbol_sit[i,9]<-dapmean.arbol_sit[i]


	      }

	    #Llenar los relativos y VIR
	    for(i in 1:length(unique(datos.sitios$Especie)))
	    {
	      #Densidad relativa
	      resul.arbol_sit[i,4]<-100*resul.arbol_sit[i,2]/sum(resul.arbol_sit[,2])
	      #Cob relativa
	      resul.arbol_sit[i,14]<-100*(resul.arbol_sit[i,11])/(sum(resul.arbol_sit[,11]))
	      #resul.arbol_sit[i,12]<-100*(resul.arbol_sit[i,8]*area.circ.sitio/10000)/(sum(resul.arbol_sit[,8])*area.circ.sitio/10000)
	      #AB relativa
	      resul.arbol_sit[i,10]<-100*(resul.arbol_sit[i,7])/(sum(resul.arbol_sit[,7]))

	      #Frecuencia absoluta
	      #a<-which(resul.arbol_sit$Especie==resul.arbol_sit[i,1])
	      #Le puse un 1 porque namas es una especie
	      resul.arbol_sit[i,15]<-(1/nrow(resul.arbol_sit))*100
	    }

	    for(i in 1:length(unique(datos.sitios$Especie)))
	    {
	      #Frecuencia relativa
	      resul.arbol_sit[i,16]<-100*(resul.arbol_sit[i,15])/sum(resul.arbol_sit[,15])
	    }

	    for(i in 1:length(unique(datos.sitios$Especie)))
	    {
	      #VIR, se le quita frecuencia relativa de la ecuación
	      rela<-2
	      resul.arbol_sit[i,17]<-sum(resul.arbol_sit[i,4],resul.arbol_sit[i,10])/rela

	    }
	    final.resul.arbol_sit[[j]]<-resul.arbol_sit
	  }
	  ultimate<-do.call("rbind",final.resul.arbol_sit)
	  nombres<-row.names(ultimate)
	  recorte<-strsplit(nombres,"[.]")
	  Sitio<-vector(length=length(recorte))

	  for(i in 1:length(recorte))
	  {
	    Sitio[i]<-strsplit(nombres,"[.]")[[i]][1]
	  }

	  FIN<-cbind(Sitio,ultimate)

	  export_arch2<-paste0("Atributos_Especie_Sitio",".csv")

	  write.csv(FIN,export_arch2,row.names=F)
	  print(paste0("Listo por Especie y Sitios "))

	  #--------------------------------Por Especie-----------------------------
	  for(i in 1:length(unique(datos$Especie)))
	  {

	      #Calcular abundancia y densidad escalada a 1 ha
	      inds.arbol[i]<-sum(datos$Especie==resul.arbol[i,1])

	    #Abundancia
	    resul.arbol[i,2]<-inds.arbol[i]
	    #Densidad ha-1
	    resul.arbol[i,3]<-inds.arbol[i]*10000/area.circ

	    #Altura m
	    resul.arbol[i,5]<-alt.arbol[i]
	    #Hmax
	    resul.arbol[i,6]<-altmax.arbol[i]


	      #Cob / ha
	      resul.arbol[i,11]<-cobsum.arbol[i]
	      #Cob / ha
	      resul.arbol[i,12]<-cobsum.arbol[i]*10000/area.circ
	      #Cob promedio
	      resul.arbol[i,13]<-cobmean.arbol[i]


	        #ABt
	        resul.arbol[i,7]<-dapsum.arbol[i]
	        #AB / ha
	        resul.arbol[i,8]<-dapsum.arbol[i]*10000/area.circ
	        #AB promedio
	        resul.arbol[i,9]<-dapmean.arbol[i]


	  }
	  #Hacer matriz de presencia ausencia
	  presencia<-as.data.frame(matrix(nrow=length(unique(datos$Especie)),
	                                  ncol=num.sitios,
	                                  dimnames=list(c(unique(datos$Especie)),
	                                                c(as.character(sitios)))))
	  for(h in 1:nrow(datos))
	  {
	    entrx<-which(row.names(presencia)==datos$Especie[h])
	    entry<-which(colnames(presencia)==datos$Sitio[h])
	    presencia[entrx,entry]<-1
	  }
	  presencia[is.na(presencia)] <- 0
	  #Llenar los relativos y VIR
	  for(i in 1:length(unique(datos$Especie)))
	  {
	    #Densidad relativa
	    resul.arbol[i,4]<-100*resul.arbol[i,2]/sum(resul.arbol[,2])

	    #Cobertura relativa
	    resul.arbol[i,14]<-100*(resul.arbol[i,11])/sum(resul.arbol[,11])

	    #AB relativa
	    resul.arbol[i,10]<-100*(resul.arbol[i,7])/sum(resul.arbol[,7])

	    #Frecuencia absoluta
	    a<-which(row.names(presencia)==resul.arbol[i,1])
	    resul.arbol[i,15]<-(sum(presencia[a,])/num.sitios)*100

	  }
	  for(i in 1:length(unique(datos$Especie)))
	  {
	    #Frecuencia relativa
	    resul.arbol[i,16]<-100*(resul.arbol[i,15])/sum(resul.arbol[,15])
	  }
	  for(i in 1:length(unique(datos$Especie)))
	  {
	    #VIR
	    rela<-3

	      resul.arbol[i,17]<-(sum(resul.arbol[i,4],resul.arbol[i,10],resul.arbol[i,16]))/rela

	  }
	  resul.arbol<-resul.arbol[order(resul.arbol[,17],decreasing=T),]


	 export_arch3<-paste0("Atributos_Especie",".csv")


	  write.csv(resul.arbol,export_arch3,row.names=F)
	  print(paste0("Listo por Especie "))

  }

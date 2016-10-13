#' Análisis de vegetación por estratos
#'
#' Esta función calcula distintas características de la vegetación analizada calcula las características estructurales por sitio, por especie y por especie y sitio.importar datos con encoding utf-8, stringasFactor=F
#' @param BD=Base de datos del análisis en csv
#' @param radio=radio del círculo de muestreo, p.ej. 0.5,3,5
#' @param estrato=Arboreo, Arbustivo, Bajo (debe de ir entre comillas)
#' @keywords análisis, vegetación, estratos
#' @export veg_an
#' @examples veg_an("db.csv",5,"Arboreo")
#' veg_an()

veg_an<-function(BD,radio,parcela,estrato)
{
	datos<-subset(BD,BD$Estrato==estrato)
	#Forma de parcelas	
		#Total de superficie de muestreo  
		if(parcela=="circular")
		{
			#Opción extrapolando por área de círculo
			area.circ.sitio=(pi*radio^2)
			#Total de superficie de muestreo, en función de total de sitios de base de datos
			area.circ=(pi*radio^2)*length(unique(BD$Id.Sitio))
		}
		if(parcela=="cuadrada")
		{
			#Opción extrapolando por área de círculo
			area.circ.sitio=(radio^2)
			#Total de superficie de muestreo, en función de total de sitios de base de datos
			area.circ=(radio^2)*length(unique(BD$Id.Sitio))
		}
	#Decidir si se pone la extrapolación a 1 ha por número total de parcelas (levels(datos$Id.Sitio))
	#incluyendo control o número total de parcelas con árboles (sitios))
	#num.sitios<-nlevels(datos$Id.Sitio)
	sitios<-unique(datos$Id.Sitio)
	#Para ver sitios ordenados y a ver si se repiten
	sort(sitios)
	num.sitios<-length(sitios)
	#Lista final con resultados por Sitio y Especie
	final.resul.arbol_sit<-vector("list",num.sitios)
	#Los nombres que tienen guión los mete con ""
	names(final.resul.arbol_sit)<-sitios
		
		#Base final de por sitios
		if(estrato=="Bajo")
		{	
			resul.arbol<-as.data.frame(matrix(nrow=length(unique(datos$Especie)),ncol=13,
																				dimnames=list(c(),c("Especie","Abundancia","Densidad inds_ha","Altura promedio","Area Basal promedio","SumaAB_ha","Cobertura promedio %","Suma Coberturas %","Densidad relativa","Frecuencia absoluta","Frecuencia relativa","Cobertura % relativa","VIR_3"))))
			resul.sit<-as.data.frame(matrix(nrow=length(sitios),ncol=5,
																			dimnames=list(c(),c("Sitio","Riqueza","Altura promedio","Suma Coberturas %","SumaAB_ha"))))
		}else{
			resul.arbol<-as.data.frame(matrix(nrow=length(unique(datos$Especie)),ncol=13,dimnames=list(c(),c("Especie","Abundancia","Densidad inds_ha","Altura promedio","Area Basal promedio","SumaAB_ha","Cobertura promedio","Suma Coberturas_ha","Densidad relativa","Frecuencia absoluta","Frecuencia relativa","AB relativa","VIR_3"))))
			resul.sit<-as.data.frame(matrix(nrow=length(sitios),
																			ncol=5,
																			dimnames=list(c(),c("Sitio","Riqueza","Altura promedio","Suma Coberturas_ha","SumaAB_ha"))))
		}
		#Este checarlo bien, de aquí mismo se calcula densidad
		inds.arbol<-c(1:length(unique(datos$Especie)))
		inds.sit<-c(1:length(sitios))
		esp.sit<-by(datos$Especie,datos$Id.Sitio,function(x) length(unique(x)))
		alt.arbol<-by(datos$Altura.directa,datos$Especie,mean)
		alt.sit<-by(datos$Altura.directa,datos$Id.Sitio,mean)
		dapmean.arbol<-by(datos$ABtotal,datos$Especie,mean)
		dapsum.arbol<-by(datos$ABtotal,datos$Especie,sum)
		dapsum.sit<-by(datos$ABtotal,datos$Id.Sitio,sum)
		cobmean.arbol<-by(datos$Cobertura,datos$Especie,mean)
		cobsum.arbol<-by(datos$Cobertura,datos$Especie,sum)
		cobsum.sit<-by(datos$Cobertura,datos$Id.Sitio,sum)
		resul.arbol[,1]<-names(dapsum.arbol)
		#--------------------------Por sitio--------------------------------------
		resul.sit[,1]<-names(dapsum.sit)
		for(k in 1:num.sitios)
			{
				#Riqeuza
				resul.sit[k,2]<-esp.sit[k]
				#Altura promedio
				resul.sit[k,3]<-alt.sit[k]
				if(estrato=="Bajo")
				{
					#Cob / ha
					resul.sit[k,4]<-cobsum.sit[k]
					#AB / ha
					resul.sit[k,5]<-NA
				}else{
				#Cob / ha
				resul.sit[k,4]<-cobsum.sit[k]*10000/area.circ.sitio
				#AB / ha
				resul.sit[k,5]<-dapsum.sit[k]*10000/area.circ.sitio
				}
			}
		write.csv(resul.sit,paste("Resumen por Sitio de Estrato",estrato,".csv"),row.names=F)
		print("Listo por Sitios")
		#Llenar resul hasta cobertura / ha porque las demás necesitan totales (relativos)
			#------------------------Por especie y por sitio------------------------
				for(j in 1:num.sitios)
				{
					#Datos por sitio
					datos.sitios<-subset(datos,datos$Id.Sitio==names(final.resul.arbol_sit)[j])
					#Definir resul.arbol_sit en función de # spp. por sitio
					if(estrato=="Bajo"){
						resul.arbol_sit<-as.data.frame(matrix(nrow=length(unique(datos.sitios$Especie)),
																									ncol=13,
																									dimnames=list(c(),c("Especie","Abundancia","Densidad inds_ha","Altura promedio","Area Basal promedio","SumaAB_ha","Cobertura promedio %","Suma_Coberturas_%","Densidad relativa","Frecuencia absoluta","Frecuencia relativa","Cobertura % relativa","VIR_2"))))
					}else{
						resul.arbol_sit<-as.data.frame(matrix(nrow=length(unique(datos.sitios$Especie)),
																									ncol=13,
																									dimnames=list(c(),c("Especie","Abundancia","Densidad inds_ha","Altura promedio","Area Basal promedio","SumaAB_ha","Cobertura promedio","Suma Coberturas_ha","Densidad relativa","Frecuencia absoluta","Frecuencia relativa","AB relativa","VIR_2"))))
					}	
					#Este checarlo bien, de aquí mismo se calcula densidad
					if(estrato=="Bajo"){
						inds.arbol_sit<-vector()
					}else{
					inds.arbol_sit<-c(1:(length(unique((datos.sitios$Especie)))*length(unique((datos.sitios$Especie)))))
					}	
					alt.arbol_sit<-by(datos.sitios$Altura.directa,datos.sitios$Especie,mean)
					dapmean.arbol_sit<-by(datos.sitios$ABtotal,datos.sitios$Especie,mean)
					dapsum.arbol_sit<-by(datos.sitios$ABtotal,datos.sitios$Especie,sum)
					cobmean.arbol_sit<-by(datos.sitios$Cobertura,datos.sitios$Especie,mean)
					cobsum.arbol_sit<-by(datos.sitios$Cobertura,datos.sitios$Especie,sum)
					resul.arbol_sit[,1]<-names(dapsum.arbol_sit)
					#Llenar resul hasta cobertura / ha porque las demás necesitan totales (relativos)
					for(i in 1:length(unique(datos.sitios$Especie)))
					{
						#Calcular abundancia y densidad escalada a 1 ha
						if(estrato=="Bajo"){
							reng<-which(datos.sitios$Especie==resul.arbol_sit[i,1])
							inds.arbol_sit[i]<-sum(datos.sitios$Abundancia[reng])
						}else{
							inds.arbol_sit[i]<-sum(datos.sitios$Especie==resul.arbol_sit[i,1])
						}	
						resul.arbol_sit[i,2]<-inds.arbol_sit[i]
						resul.arbol_sit[i,3]<-inds.arbol_sit[i]*10000/area.circ.sitio
						
						#Calcular Altura
						resul.arbol_sit[i,4]<-alt.arbol_sit[i]
						if(estrato=="Bajo"){
							#AB promedio
							resul.arbol_sit[i,5]<-NA
							#AB / ha
							resul.arbol_sit[i,6]<-NA
							#Cob promedio
							resul.arbol_sit[i,7]<-cobmean.arbol_sit[i]
							#Cob / ha
							resul.arbol_sit[i,8]<-cobsum.arbol_sit[i]
						}else{
						#AB promedio
						resul.arbol_sit[i,5]<-dapmean.arbol_sit[i]
						#AB / ha
						resul.arbol_sit[i,6]<-dapsum.arbol_sit[i]*10000/area.circ.sitio
						#Cob promedio
						resul.arbol_sit[i,7]<-cobmean.arbol_sit[i]
						#Cob / ha
						resul.arbol_sit[i,8]<-cobsum.arbol_sit[i]*10000/area.circ.sitio
						}	
					}
					#Llenar los relativos y VIR
					for(i in 1:length(unique(datos.sitios$Especie)))
					{
						#Densidad relativa
						resul.arbol_sit[i,9]<-100*resul.arbol_sit[i,2]/sum(resul.arbol_sit$Abundancia)
						if(estrato=="Bajo")
							{
							resul.arbol_sit[i,12]<-100*(resul.arbol_sit[i,8])/(sum(resul.arbol_sit[,8]))
							#resul.arbol_sit[i,12]<-100*(resul.arbol_sit[i,8]*area.circ.sitio/10000)/(sum(resul.arbol_sit[,8])*area.circ.sitio/10000)
							}else{
								#AB relativa
								resul.arbol_sit[i,12]<-100*(resul.arbol_sit[i,6]*area.circ.sitio/10000)/(sum(resul.arbol_sit$SumaAB_ha)*area.circ.sitio/10000)
							}
						#Frecuencia absoluta
						#a<-which(resul.arbol_sit$Especie==resul.arbol_sit[i,1])
						#Le puse un 1 porque namas es una especie
						resul.arbol_sit[i,10]<-(1/nrow(resul.arbol_sit))*100
					}
					
					for(i in 1:length(unique(datos.sitios$Especie)))
					{
						#Frecuencia relativa
						resul.arbol_sit[i,11]<-100*(resul.arbol_sit[i,10])/sum(resul.arbol_sit[,10])
					}	
					
					for(i in 1:length(unique(datos.sitios$Especie)))
					{
						#VIR, se le quita frecuencia relativa de la ecuación
						rela<-2
						resul.arbol_sit[i,13]<-sum(resul.arbol_sit[i,9],resul.arbol_sit[i,12])/rela
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
				write.csv(FIN,paste("Resumen por Especie y Sitio de Estrato",estrato,".csv"),row.names=F)
				print("Listo por Especie y Sitios")
	#--------------------------------Por Especie-----------------------------
		for(i in 1:length(unique(datos$Especie)))
		{
			if(estrato=="Bajo"){
				#Calcular abundancia y densidad escalada a 1 ha
				reng<-which(datos$Especie==resul.arbol[i,1])
				inds.arbol[i]<-sum(datos$Abundancia[reng])
			}else{
				#Calcular abundancia y densidad escalada a 1 ha
				inds.arbol[i]<-sum(datos$Especie==resul.arbol[i,1])
			}
			resul.arbol[i,2]<-inds.arbol[i]
			resul.arbol[i,3]<-inds.arbol[i]*10000/area.circ
			#Calcular Altura
			resul.arbol[i,4]<-alt.arbol[i]
			#AB promedio
			resul.arbol[i,5]<-dapmean.arbol[i]
			#AB / ha
			resul.arbol[i,6]<-dapsum.arbol[i]*10000/area.circ
			#Cob promedio
			resul.arbol[i,7]<-cobmean.arbol[i]
			#Cob / ha
			resul.arbol[i,8]<-cobsum.arbol[i]*10000/area.circ
			if(estrato=="Bajo"){
				#AB promedio
				resul.arbol[i,5]<-NA
				#AB / ha
				resul.arbol[i,6]<-NA
				#Cob promedio
				resul.arbol[i,7]<-cobmean.arbol[i]
				#Cob / ha
				resul.arbol[i,8]<-cobsum.arbol[i]
			}
		}
		#Hacer matriz de presencia ausencia
		presencia<-as.data.frame(matrix(nrow=length(unique(datos$Especie)),
																		ncol=num.sitios,
																		dimnames=list(c(unique(datos$Especie)),c(as.character(sitios)))))
		for(h in 1:nrow(datos))
		{
			entrx<-which(row.names(presencia)==datos$Especie[h])
			entry<-which(colnames(presencia)==datos$Id.Sitio[h])
			presencia[entrx,entry]<-1
		}
		presencia[is.na(presencia)] <- 0
		#Llenar los relativos y VIR
		for(i in 1:length(unique(datos$Especie)))
		{
			#Densidad relativa
			resul.arbol[i,9]<-100*resul.arbol[i,2]/sum(resul.arbol$Abundancia)
			if(estrato=="Bajo"){
				#Cobertura relativa
				resul.arbol[i,12]<-100*(resul.arbol[i,8])/(sum(resul.arbol[,8]))
				#resul.arbol[i,12]<-100*(resul.arbol[i,8]*area.circ/10000)/(sum(resul.arbol[,8])*area.circ/10000)
			}else{
				#AB relativa
				resul.arbol[i,12]<-100*(resul.arbol[i,6]*area.circ/10000)/(sum(resul.arbol$SumaAB_ha)*area.circ/10000)
			}	
			#Frecuencia absoluta
			a<-which(row.names(presencia)==resul.arbol[i,1])
			resul.arbol[i,10]<-(sum(presencia[a,])/num.sitios)*100
			#resul.arbol[i,10]<-(sum(presencia[a,])/length(unique(BD$Id.Sitio)))*100
		}
		for(i in 1:length(unique(datos$Especie)))
		{
			#Frecuencia relativa
			resul.arbol[i,11]<-100*(resul.arbol[i,10])/sum(resul.arbol[,10])
		}	
		for(i in 1:length(unique(datos$Especie)))
		{
			#VIR
			rela<-3
			resul.arbol[i,13]<-(sum(resul.arbol[i,9],resul.arbol[i,12],resul.arbol[i,11]))/rela
		}
		resul.arbol<-resul.arbol[order(resul.arbol[,13],decreasing=T),]
		write.csv(resul.arbol,paste("Resumen por Especie de Estrato",estrato,".csv"),row.names=F)
		print("Listo por Especie")
}

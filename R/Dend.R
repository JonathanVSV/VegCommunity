#' Dendrograma de asociaciones y características por grupo
#'
#' Esta función hace dendrograma de asociaciones de la vegetación y saca algunas características por grupo.importar datos con encoding utf-8, stringasFactor=F. Se basa en los archivos creados por veg_an
#' @param BD=Base de datos del análisis en csv; num.gpos=núm. de grupos deseados para la clasificación; nombres=1: quitar nombres de descriptores del de las especies, 0:no hacerlo
#' @keywords dendrograma, clasificación de vegetación
#' @export Dend
#' @examples Dend("db.csv",6,1)
#' Dend()

#tienen que estar los archivos de veg_an en la carpeta
Dend<-function(BD,num.gpos,Lista,nombres,metod.ligamiento,metod.dist,cex.1,cex.2)
{
	#Preparar BD
	arb<-subset(BD,BD$Estrato=="Arboreo"|BD$Estrato=="Arbustivo")
	#Archivos VIR cargar
	datosVIR.arbol<-read.csv("Resumen por Especie y Sitio de Estrato Arboreo .csv",
													 header=T,encoding="Utf-8",stringsAsFactors=FALSE)
	datosVIR.arbustivo<-read.csv("Resumen por Especie y Sitio de Estrato Arbustivo .csv",
															 header=T,encoding="Utf-8",stringsAsFactors=FALSE)
	sarbu.total<-matrix(nrow=length(unique(arb$Id.Sitio)),ncol=length(unique(arb$Especie)),
											dimnames=list(c(unique(arb$Id.Sitio)),c(unique(arb$Especie))))
	#Para asignar número de filas más grande de arbustivo o arboreo	
	if(nrow(datosVIR.arbol)>=nrow(datosVIR.arbustivo))
	{
		renglM<-nrow(datosVIR.arbol)
	}else{
		renglM<-nrow(datosVIR.arbustivo)
	}
	temp<-0
	sarbu.total[is.na(sarbu.total)]<-0
	#llenar sarbu
	h<-1
	for(h in 1:renglM)
	{
		entrx<-which(row.names(sarbu.total)==datosVIR.arbol$Sitio[h])
		entry<-which(colnames(sarbu.total)==datosVIR.arbol$Especie[h])
		if(is.na(datosVIR.arbol[h,14])==TRUE)
		{
			temp<-0
		}else{
			temp<-datosVIR.arbol[h,14]
		}
		sarbu.total[entrx,entry]<-sum(temp,sarbu.total[entrx,entry])
		entrxa<-which(row.names(sarbu.total)==datosVIR.arbustivo$Sitio[h])
		entrya<-which(colnames(sarbu.total)==datosVIR.arbustivo$Especie[h])
		if(is.na(datosVIR.arbustivo[h,14])==TRUE)
		{
			temp<-0
		}else{
			temp<-datosVIR.arbustivo[h,14]
		}
		sarbu.total[entrxa,entrya]<-sum(temp,sarbu.total[entrxa,entrya])
	}
	for(h in 1:nrow(sarbu.total))
	{
		temp<-sum(sarbu.total[h,])
		for(j in 1:ncol(sarbu.total))
		{
			sarbu.total[h,j]<-(as.numeric(sarbu.total[h,j])/temp)*100
		}
	}
	sarbu.total<-as.data.frame(sarbu.total)
	sarbu.total[is.na(sarbu.total)]<-0
	sarbu<-sarbu.total
	#write.csv(sarbu.total,paste("Sarbu matriz.csv"),row.names=T)
	
	#--------------------------Dendrograma---------------------------------------
	#Da matriz de disimilitud por medio del m?todo de Bray-Curtis
	ssa<-vegdist(sarbu,method=metod.dist,binary=FALSE,diag=FALSE)
	Tsa<-agnes(ssa,diss=F,metric="euclidean",method=metod.ligamiento)
	#plot(Tsa)
	ksks<-plot(Tsa,main="Disimilitud Floristica",ylab="Disimilitud",which.plots=2,cex=0.7)
	
	#---------------------------------Vir por sitios y grupo--------------------------------
	quesqu<-rbind(datosVIR.arbustivo,datosVIR.arbol)
	tuchi<-matrix(nrow=nrow(quesqu),ncol=3,dimnames=list(c(),c("Sitio","Especie","VIR")))
	sarbu.ma<-as.matrix(sarbu)
	
	k<-1
	for(i in 1:nrow(sarbu.ma))
	{
		for(j in 1:ncol(sarbu.ma))
		{
			proba<-sarbu.ma[i,j]!=0
			if(proba==T){
				tuchi[k,1]<-dimnames(sarbu.ma)[[1]][i]
				tuchi[k,2]<-dimnames(sarbu.ma)[[2]][j]
				tuchi[k,3]<-sarbu.ma[i,j]
				k<-k+1
			}
		}
	}
	ficale<-na.omit(tuchi)
	#Corte de dendrograma
	corte<-cutree(Tsa,k=num.gpos)
	sitios<-row.names(sarbu)
	sit.gpos<-as.data.frame(cbind(corte,sitios))
	
	#Hacer matriz de grupos de asociaciones por especie
	gpos<-data.frame(Sitio=ficale[,1],Grupo=vector(length=nrow(ficale)),
									 Especie=ficale[,2],VIR=ficale[,3])
	for(h in 1:nrow(gpos))
	{
		entreng<-which(as.character(sit.gpos$sitios)==as.character(gpos$Sitio[h]))
		gpos$Grupo[h]<-sit.gpos$corte[entreng]
	}
	
	#---------------------------Cachito extra de la lista de asociaciones
	if(Lista==1)
	{
		eq.asoci<-read.csv("Asociaciones.eq.csv")
		for(i in 1:num.gpos)
		{
			aux.reng<-which(gpos$Grupo==eq.asoci$Grupo[i])
			gpos$Grupo[aux.reng]<-as.character(eq.asoci$Asociacion[i])
		}
	}
	
	write.csv(gpos,"VIR por sitio arbustos y arboles.csv",row.names=F)
	#-----------------------------------Pa hacer tabla de VIR final---------------------------
	base.vir<-read.csv("VIR por sitio arbustos y arboles.csv")
	#Pa hacer la tabla		
	base.vir$Especie<-as.character(base.vir$Especie)
	base.vir$VIR<-as.numeric(base.vir$VIR)
	sitios.vir<-unique(as.character(base.vir$Sitio))
	grupos<-unique(base.vir$Grupo)
	virere<-data.frame(Sp=vector(),VIR=vector(),Grupo=vector())
	#Para quitar los nombres de los que describieron la especie
	for(i in 1:nrow(base.vir))
	{
		base.vir$Especie[i]<-paste(strsplit(as.character(base.vir$Especie[i]),split=" ")[[1]][1],strsplit(as.character(base.vir$Especie[i]),split=" ")[[1]][2])
	}
	#Tabla rellenado
	h<-1
	i<-1
	for(i in 1:length(sitios.vir))
	{
		sitx<-which(base.vir$Sitio==sitios.vir[i])
		#Esto todavía no funciona
		temp<-order(as.numeric(base.vir$VIR[sitx]),decreasing=T)+(sitx[1]-1)
		base.vir[sitx,]<-base.vir[temp,]
		a<-length(sitx)+1
		for(j in 1:a)
		{
			if(j==1)
			{
				virere[h,1]<-as.character(sitios.vir[i])
				virere[h,2]<-"% VIR"
				virere[h,3]<-unique(base.vir$Grupo[sitx])
				h<-h+1
			}else{
				virere[h,1]<-as.character(base.vir$Especie[sitx[(j-1)]])
				virere[h,2]<-round(as.numeric(base.vir$VIR[sitx[(j-1)]]),digits=2)
				virere[h,3]<-round(as.numeric(base.vir$Grupo[sitx[(j-1)]]),digits=2)
				h<-h+1
			}
		}
	}
	#Escribe las tablas por grupo
	for(i in 1:num.gpos)
	{
		subgrop<-subset(virere,virere$Grupo==i)
		write.csv(subgrop,paste("VIR grupo",i,".csv"),row.names=F)
	}
	print("Listo VIR por grupo")
	#--------------------------AB y AP por grupos-------------------------------------
	datosVIR.arbol[,ncol(datosVIR.arbol)+1]<-rep("Arboreo",nrow(datosVIR.arbol))
	datosVIR.arbustivo[,ncol(datosVIR.arbustivo)+1]<-rep("Arbustivo",nrow(datosVIR.arbustivo))
	
	gpos.arbol<-data.frame(Sitio=datosVIR.arbol$Sitio,Grupo=vector(length=nrow(datosVIR.arbol)),
												 Especie=datosVIR.arbol$Especie,AB.prom=datosVIR.arbol$Area.Basal.promedio,AP=datosVIR.arbol$Altura.promedio)
	for(h in 1:nrow(gpos.arbol))
	{
		entreng<-which(as.character(sit.gpos$sitios)==as.character(gpos.arbol$Sitio[h]))
		gpos.arbol$Grupo[h]<-sit.gpos$corte[entreng]
	}
	AB.arb<-by(gpos.arbol$AB.prom,gpos.arbol$Grupo,mean)
	AP.arb<-by(gpos.arbol$AP,gpos.arbol$Grupo,mean)
	base.asoc<-cbind(unlist(AB.arb),unlist(AP.arb))
	dimnames(base.asoc)<-list(c(as.character(seq(1,length(unique(gpos.arbol$Grupo)),1))),
														c("AB prom","AP"))
	
	write.csv(base.asoc,"Grupos Arboreo AP y AB.csv",row.names=T)
	
	gpos.arbustivo<-data.frame(Sitio=datosVIR.arbustivo$Sitio,
														 Grupo=vector(length=nrow(datosVIR.arbustivo)),
														 Especie=datosVIR.arbustivo$Especie,
														 AB.prom=datosVIR.arbustivo$Area.Basal.promedio,
														 AP=datosVIR.arbustivo$Altura.promedio)
	for(h in 1:nrow(gpos.arbustivo))
	{
		entreng<-which(as.character(sit.gpos$sitios)==as.character(gpos.arbustivo$Sitio[h]))
		gpos.arbustivo$Grupo[h]<-sit.gpos$corte[entreng]
	}
	AB.arb<-by(gpos.arbustivo$AB.prom,gpos.arbustivo$Grupo,mean)
	AP.arb<-by(gpos.arbustivo$AP,gpos.arbustivo$Grupo,mean)
	base.asoc<-cbind(unlist(AB.arb),unlist(AP.arb))
	dimnames(base.asoc)<-list(c(as.character(seq(1,length(unique(gpos.arbustivo$Grupo)),1))),
														c("AB prom","AP"))
	write.csv(base.asoc,"Grupos arbustivo AP y AB.csv",row.names=T)
	print("Listo AB y AP por grupo")
	
	#---------------Plot de dendrograma con grupos marcados en rojo-------------------
	jpeg(file="dendrograma agrupado.jpeg",width=50,height=35,units="cm",res=300)
	plot(Tsa,main="Disimilitud Floristica",ylab="Disimilitud",which.plots=2, cex=cex.1,cex.lab=cex.2)
	rect.hclust(Tsa, k=num.gpos, border="red")
	dev.off()
	print("Listo Dendrograma plot")
}


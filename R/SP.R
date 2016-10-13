#' Curva de acumulación de especies 
#'
#' Esta función calcula curva de acumulación de especies.importar datos con encoding utf-8, stringasFactor=F. Se basa en los archivos creados por veg_an
#' @param Lee los archivos creados por vegan
#' @keywords dendrograma, clasificación de vegetación
#' @export SP
#' @examples SP()
#' SP()

SP<-function(numsp.est,num.sit.est,break.x,break.y,int.lowess)
{
	espechiarbo<-read.csv("Resumen por Especie y Sitio de Estrato Arboreo .csv")
	espechiarbu<-read.csv("Resumen por Especie y Sitio de Estrato Arbustivo .csv")
	#espechihier<-read.csv("Resumen por Especie y Sitio de Estrato Bajo .csv")
	colnames(espechihier)<-colnames(espechiarbo)
	espechi<-rbind(espechiarbo,espechiarbu,espechihier)
	
	#Abundancia
	pres<-as.data.frame(matrix(nrow=nlevels(espechi$Sitio),
														 ncol=length(unique(espechi$Especi)),
														 dimnames=list(c(as.character(unique(espechi$Sitio))),c(as.character(unique(espechi$Especie))))))
	#Presencia Ausencia
	pres.2<-as.data.frame(matrix(nrow=nlevels(espechi$Sitio),
															 ncol=length(unique(espechi$Especi)),
															 dimnames=list(c(as.character(unique(espechi$Sitio))),c(as.character(unique(espechi$Especie))))))
	
	pres[is.na(pres)] <- 0
	pres.2[is.na(pres.2)] <- 0
	for(h in 1:nrow(espechi))
	{
		entrx<-which(row.names(pres)==espechi[h,1])
		entry<-which(colnames(pres)==espechi[h,2])
		pres[entrx,entry]<-sum(espechi[h,3],pres[entrx,entry])
		pres.2[entrx,entry]<-1
	}
	write.csv(pres.2,"Presencia por sitios.csv")
	
	#Diversidad por sitio
		presencia<-read.csv("Presencia por sitios.csv",stringsAsFactors = F,row.name=1)
		diver<-data.frame(matrix(ncol=3,nrow=nrow(presencia)))
		row.names(diver)<-row.names(presencia)
		colnames(diver)<-c("Riqueza","Simpson","Shannon")
		for(i in 1:nrow(presencia))
		{
			extra<-presencia[i,which(presencia[i,]>=1)]
			diver[i,]<-cbind(specnumber(extra),diversity(extra, index="simpson"),
											 diversity(extra, index="shannon"))
			diver[i,]<-round(diver[i,],2)
		}
		write.csv(diver,"Diversidad por sitio.csv")
	
	sp1<-specaccum(pres,method="exact",gamma="boot")
	specpol<-specpool(pres)
	spec.tot<-specpol$jack1
	sp.estim<-spp.est(t(pres))
	sp.plot<-t(cbind(sp.estim[,1],sp.estim[,11],sp.estim[,12]-sp.estim[,11]))
	
	#Pa ver la gráfica que se guarda en el jpg
	plot(sp1,ci.type="poly",col="blue",lwd=2,ci.lty=0,ci.col="lightblue",
			 xlab="Sitios",ylab="S",xlim=c(0,num.sit.est),ylim=c(0,numsp.est),axes=FALSE)
	axis(1, at = seq(0, num.sit.est, by = break.x), las=2, pos=0)
	axis(2, at = seq(0, numsp.est, by = break.y), las=2, pos=0)
	lines(lowess(sp.estim[,1],sp.estim[,11],f=int.lowess),col="red",lwd=2)
	lines(lowess(sp.estim[,1],sp.estim[,12],f=int.lowess),col="red",lwd=1,lty=2)
	lines(lowess(sp.estim[,1],sp.estim[,13],f=int.lowess),col="red",lwd=1,lty=2)
	
	print(spec.tot)
	jpeg(file="Curva acumulación Todos.jpeg",width=10,height=10,units="cm",res=300)
	#Namas le cambie el ci.col a white
	plot(sp1,ci.type="poly",col="blue",lwd=2,ci.lty=0,ci.col="lightblue",
			 xlab="Sitios",ylab="S",xlim=c(0,num.sit.est),ylim=c(0,numsp.est),axes=FALSE)
	axis(1, at = seq(0, num.sit.est, by = break.x), las=2, pos=0)
	axis(2, at = seq(0, numsp.est, by = break.y), las=2, pos=0)
	lines(lowess(sp.estim[,1],sp.estim[,11],f=int.lowess),col="red",lwd=2)
	lines(lowess(sp.estim[,1],sp.estim[,12],f=int.lowess),col="red",lwd=1,lty=2)
	lines(lowess(sp.estim[,1],sp.estim[,13],f=int.lowess),col="red",lwd=1,lty=2)
	dev.off()	
	print("Listo Grafica acumulacion sp")

	diver<-data.frame(Riqueza=vector(length=length(unique(base.vir$Grupo))),
										Simpson=vector(length=length(unique(base.vir$Grupo))),
										Shannon=vector(length=length(unique(base.vir$Grupo))))
	base.vir<-read.csv("VIR por sitio arbustos y arboles.csv",encoding="utf-8")
	i<-1
	for(i in 1:length(unique(base.vir$Grupo)))
	{
		temp<-as.character(unique(base.vir[which(base.vir$Grupo==i),1]))
		subpres<-vector()
		for(j in 1:length(temp))
		{
			subpres<-c(subpres,which(row.names(pres)==temp[j]))
		}
		extra<-colSums(pres[subpres,])
		diver[i,]<-cbind(specnumber(extra),diversity(extra, index="simpson"),
										 diversity(extra, index="shannon"))
		diver[i,]<-round(diver[i,],2)
	}	
	write.csv(diver,file="Indices Diversidad por Gpo.csv")
	print("Listo Indices Diversidad por Gpo")
	print("Riqueza total por sitio")
	sp.fin<-c(sp1$richness,spec.tot)
	return(sp.fin)
}


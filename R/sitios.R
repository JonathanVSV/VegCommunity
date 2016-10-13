#' Análisis de vegetación por estratos
#'
#' Esta función calcula orientaciones y intervalo de altitudes para los sitios de estudio
#' @param Sites.grp=Base de datos del análisis en csv
#' @param radio=radio del círculo de muestreo, p.ej. 0.5,3,5
#' @param estrato=Arboreo, Arbustivo, Bajo (debe de ir entre comillas)
#' @keywords análisis, vegetación, estratos
#' @export sitios
#' @examples sitios("Sitios.csv",5,"Arboreo")
#' sitios()

sitios<-function(Sites.grp,Lista)
{
	base.vir<-read.csv("VIR por sitio arbustos y arboles.csv",stringsAsFactors=FALSE)
	
	#Checar si hay correspondencia entre la base de sitios y los que aparecen en VIR por sitio arbustos y arboles
	print("Checar si el listado de sitios corresponde entre Sitios y VIR sitios arbustos y arboles; 
				T es bien")
	
	if(length(Sites.grp$Id.completo)<length(unique(base.vir$Sitio)))
	{
		print("Menos sitios en base de sitios que en base de datos de plantas")
		print("Este sitio difiere")
		unique(base.vir$Sitio)[order(unique(base.vir$Sitio),decreasing=T)][which(unique(base.vir$Sitio)[order(unique(base.vir$Sitio),decreasing=T)]%in%Sites.grp$Id.completo[order(Sites.grp$Id.completo,decreasing=T)]==F)]
	}else{
		print("Más sitios en base de sitios que en base de datos de plantas")
		print("Este sitio difiere")
		Sites.grp$Id.completo[order(Sites.grp$Id.completo,decreasing=T)][which(Sites.grp$Id.completo[order(Sites.grp$Id.completo,decreasing=T)]%in%unique(base.vir$Sitio)[order(unique(base.vir$Sitio),decreasing=T)]==F)]
	}
	
	chequeo<-Sites.grp$Id.completo[order(Sites.grp$Id.completo,decreasing=T)]==unique(base.vir$Sitio)[order(unique(base.vir$Sitio),decreasing=T)]		
	print(chequeo)
	
	#Columnas correspondientes de cada sitio con el grupo del dendrograma
	caca<-data.frame(Sitio=vector(length=length(unique(base.vir$Sitio))),
									 Grupo=vector(length=length(unique(base.vir$Sitio))))
	
	for(i in 1:length(unique(base.vir$Sitio)))
	{
		caca[i,1]<-as.character(unique(base.vir$Sitio)[i])
		temp<-which(as.character(base.vir$Sitio)==as.character(unique(base.vir$Sitio)[i]))
		caca[i,2]<-base.vir$Grupo[temp[1]]
	}
	
	cols.Sites.grp<-(ncol(Sites.grp))+1
	for(h in 1:nrow(caca))
	{
		entrx<-which(Sites.grp$Id.completo==caca$Sitio[h])
		Sites.grp[entrx,cols.Sites.grp]<-caca$Grupo[h]
	}
	colnames(Sites.grp)[cols.Sites.grp]<-"Grupo"
	SIts<-Sites.grp
	
	#Clasificacion de Orientacion
	ori.res<-data.frame(Orientacion=c("Este","Sur","Oeste","Norte","Sin Orientación"),Conteo=vector(length=5))
	
	Este<-which(SIts$Rumbo>=45&SIts$Rumbo<135)
	ori.res[1,2]<-length(Este)
	Sur<-which(SIts$Rumbo>=135&SIts$Rumbo<225)
	ori.res[2,2]<-length(Sur)
	Oeste<-which(SIts$Rumbo>=225&SIts$Rumbo<315)
	ori.res[3,2]<-length(Oeste)
	Norte<-which(SIts$Rumbo>=315||SIts$Rumbo<45)
	ori.res[4,2]<-length(Norte)
	Sin_or<-which(SIts$Rumbo==0)
	ori.res[5,2]<-length(Sin_or)
	
	write.csv(ori.res,"Conteo_por_orientacion y asoc.csv",row.names=F)
	print("Listo conteos Orientacion")
	
	#Clasificación altitud de max-min en 3 
	intervalo.altitud<-(max(SIts$Altitud)-min(SIts$Altitud))/3
	alt.res<-data.frame(Altitud=c(as.character(ceiling(min(SIts$Altitud))),
																as.character(ceiling(min(SIts$Altitud)+intervalo.altitud)),
																as.character(ceiling(max(SIts$Altitud)-intervalo.altitud))),
											Conteo=vector(length=3))
	
	alt.1<-which(SIts$Altitud>=ceiling(min(SIts$Altitud))&SIts$Altitud<(ceiling(min(SIts$Altitud)+intervalo.altitud)))
	alt.res[1,2]<-length(alt.1)
	alt.2<-which(SIts$Altitud>=(ceiling(min(SIts$Altitud))+intervalo.altitud)&SIts$Altitud<ceiling((max(SIts$Altitud)-intervalo.altitud)))
	alt.res[2,2]<-length(alt.2)
	alt.3<-which(SIts$Altitud>=(ceiling(max(SIts$Altitud))-intervalo.altitud))
	alt.res[3,2]<-length(alt.3)
	
	write.csv(alt.res,"Conteo_por_altitud.csv",row.names=F)
	print("Listo conteos altitud")
	
	SIts$X.Sitio<-seq(1,nrow(SIts),1)
	
	SITES<-data.frame(Num=SIts$X.Sitio,Sitio=SIts$Id.completo,
										Grupo=SIts$Grupo,Altitud=SIts$Altitud,
										Orientacion=vector(length=length(SIts$Rumbo)),
										Rango.Elevacion=vector(length=length(SIts$Rumbo)))
	
	SITES$Orientacion[Este]<-"Este"
	SITES$Orientacion[Oeste]<-"Oeste"
	SITES$Orientacion[Norte]<-"Norte"
	SITES$Orientacion[Sur]<-"Sur"
	SITES$Orientacion[Sin_or]<-"Plano"
	
	SITES$Rango.Elevacion[alt.1]<-paste("> ",as.character(ceiling(min(SIts$Altitud))))
	SITES$Rango.Elevacion[alt.2]<-paste("> ",as.character(ceiling(min(SIts$Altitud)+intervalo.altitud)))
	SITES$Rango.Elevacion[alt.3]<-paste("> ",as.character(ceiling(max(SIts$Altitud)-intervalo.altitud)))
	
	write.csv(SITES,"Tabla.sitios.elev.csv")
	print("Listo Tabla.sitios.elev")
	
	#------------------------------Gráficas-------------------------
	sotos<-read.csv("Tabla.sitios.elev.csv",stringsAsFactors=F)
	
	#Meter tabla de equivalencias de asociaciones en nombres por números de grupo!!!

	if(Lista==1)
	{
		eq.asoci<-read.csv("Asociaciones.eq.csv")
		for(i in 1:nrow(sotos))
		{
			aux.reng<-which(sotos$Grupo==eq.asoci$Grupo[i])
			sotos$Grupo[aux.reng]<-as.character(eq.asoci$Asociacion[i])
		}
	}
	
	counts<-table(sotos$Grupo,sotos$Rango.Elevacion)
	conteo<-as.data.frame(counts)
	colnames(conteo)<-c("Asociacion","Altitud","Número.de.sitios")
	
	jpeg("Sitios Elevacion.jpeg",height=20,width=25,res=250,units="cm")
	p1=ggplot(conteo,aes(x=Altitud,y=Número.de.sitios,fill=Asociacion))
	p1=p1+geom_bar(stat="identity",position="dodge")
	p1=p1+theme_bw()+theme(strip.background=element_rect(fill="white"),
												 plot.title=element_text(size=20),text=element_text(size=18),
												 axis.line.x=element_line(colour="black"),
												 axis.line.y=element_line(colour="black"),
												 panel.grid.major=element_blank(),
												 panel.grid.minor=element_blank(),
												 panel.border=element_blank(),
												 panel.background=element_rect(fill="white"),
												 legend.key = element_blank())
	p1=p1+scale_y_continuous(name="Frecuencia de sitios",expand=c(0,0),
													 breaks=seq(0,max(conteo$Número.de.sitios),1))
	p1=p1+scale_fill_brewer(palette="Dark2")
	print(p1)
	dev.off()
	
	print("Listo gráfica elevación")
	
	#----------------------------------Gráficas por sitios y Asociacion------------------------------
	counts<-table(sotos$Grupo,sotos$Orientacion)
	conteo<-as.data.frame(counts)
	colnames(conteo)<-c("Asociacion","Orientacion","Número.de.sitios")
	
	jpeg("Sitios Orientacion y Asoc.jpeg",height=20,width=25,res=250,units="cm")
	p1=ggplot(conteo,aes(x=Orientacion,y=Número.de.sitios,fill=Asociacion))
	p1=p1+geom_bar(stat="identity",position="dodge")
	p1=p1+theme_bw()+theme(strip.background=element_rect(fill="white"),
												 plot.title=element_text(size=20),
												 text=element_text(size=18),
												 axis.line.x=element_line(colour="black"),
												 axis.line.y=element_line(colour="black"),
												 panel.grid.major=element_blank(),
												 panel.grid.minor=element_blank(),
												 panel.border=element_blank(),
												 panel.background=element_rect(fill="white"),
												 legend.key = element_blank())
	p1=p1+scale_y_continuous(name="Frecuencia de sitios",expand=c(0,0),
													 breaks=seq(0,max(conteo$Número.de.sitios),1))
	p1=p1+scale_fill_brewer(palette="Dark2")
	print(p1)
	dev.off()
	
	print("Listo gráfica Orientacion por Asociacion")
	
	#-----------------------------------Gráficas por sitios-------------------------------------
	counts<-(table(sotos$Orientacion))
	conteo<-data.frame(Orientacion=c("Este","Sur","Oeste","Norte","Plano"),Número.de.sitios=c(0,0,0,0,0))
	for (i in 1:5)
	{
		conteo[which(names(counts[i])==conteo$Orientacion),2]<-counts[i]
	}
	
	jpeg("Sitios Orientacion.jpeg",height=20,width=25,res=250,units="cm")
	p1=ggplot(conteo,aes(x=Orientacion,y=Número.de.sitios,fill=Orientacion))
	p1=p1+geom_bar(stat="identity")
	p1=p1+theme_bw()+theme(strip.background=element_rect(fill="white"),
												 plot.title=element_text(size=20),
												 text=element_text(size=18),
												 axis.line.x=element_line(colour="black"),
												 axis.line.y=element_line(colour="black"),
												 panel.grid.major=element_blank(),
												 panel.grid.minor=element_blank(),
												 panel.border=element_blank(),
												 panel.background=element_rect(fill="white"),
												 legend.key = element_blank())
	p1=p1+scale_y_continuous(name="Frecuencia de sitios",expand=c(0,0),
													 breaks=seq(0,max(conteo$Número.de.sitios),1))
	p1=p1+scale_fill_brewer(palette="Set1")
	print(p1)
	dev.off()
	
	print("Listo gráfica Orientacion")
}


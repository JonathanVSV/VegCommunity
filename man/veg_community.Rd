% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/veg_community.R
\name{veg_community}
\alias{veg_community}
\title{ENG Calculates structural and diversity attributes of the plant community of each sampling plot
       SPA Calcula atributos estructurales y de diversidad de la comunidad vegetal de cada unidad de muestreo}
\usage{
veg_community(datos, radio, circular = T)
}
\arguments{
\item{datos=datos}{data.frame. ENG Object where the PREPARED data is stored must come from veg_preparar(...)
SPA objeto donde se tienen guardados los datos PREPARADOS, debe venir de un objeto creado mediante:
veg_preparar(...)}

\item{radio=radio}{numeric. ENG ratius in meters of the circle or equivalent side of a square plot, e.g. 0.5,3,5
Equivalent side stands for example, for rectangular plots with 5 x 2 m = 10 m2 can be expressed as a square
plot with a square plot with a side = 3.162277660168379 m.
SPA radio en m del círculo o lado equivalente del cuadrado de muestreo, p.ej. 0.5,3,5.
El lado equivalente se refiere a, por ejemplo, se tienen rectangulos de  muestreo de 5 x 2 m = 10m2, puede
expresarse como un cuadrado de muestreo de lado =  3.162277660168379 m.}

\item{circular=T}{Boolean. ENG If the plot is circular or not. If true, then data is taken a the ratius of a circular
plot, else, i.e., False, it is taken as a side of a squared plot.
SPA si se trata de una unidad de muestreo circular o cuadrada, circular, TRUE, el radio se da tal cual,
en cambio en cuadrada, FALSE, se da la raíz del área total de muestreo.}
}
\description{
ENG This function calculates several attributes of a vegetation community, it
calculates structural characteristics per plot, per species and per species and plot. Import
data with encoding utf-8 and stringsAsFactors=F.
       SPA Esta función calcula distintas características de la vegetación, calcula las
características estructurales por sitio, por especie y por especie y sitio.importar datos
con encoding utf-8, stringsAsFactors=F
}
\examples{
veg_community("datos_preparados.csv",5,T)

}
\keyword{ENG}
\keyword{SPA}
\keyword{analysis,}
\keyword{análisis,}
\keyword{vegetación}
\keyword{vegetation;}

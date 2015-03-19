\name{preview, preview.RkmlFolder}
\alias{preview}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Preview a kml Object
}
\description{
Allow you to quickly view your kml object. You must install Google Earth to your computer. Make sure .kml ae associated with google earth by defult
}
\usage{
yourkml$preview()
}
%- maybe also 'usage' for other objects documented here.


\references{
\href{http:\/\/www.google.com/earth/download/ge/agree.html}{Google Earth Download}
}
\author{
Brent Cameron\cr
Department of Fisheries and Oceans Canada\cr
Population Ecology Division
}



\examples{

mykml = RKmlObject()

mykml$addFolder(fid = "EMPTY", name = "EMPTY")
x = data.frame(cbind(c(45.9178, 46.807), c(-59.967, -60.321)))
names(x) = c("lat", "lon")
mykml$addPoint(x)
mykml$preview()

}

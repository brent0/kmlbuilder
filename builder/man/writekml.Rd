\name{writekml, writekml.RkmlFolder}
\alias{writekml}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write kml Object to file
}
\description{
Write the kml object to the specified file
}
\usage{
yourkml$writekml(path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
Mandatory. The full path to you kml file. Remember that the filename must end with .kml }

}


\author{
Brent Cameron\cr
Department of Fisheries and Oceans Canada\cr
Population Ecology Division

}


\examples{

mykml = RKmlObject()


mykml$addLabelStyle(styleid = "labelstyle1", color = "red", transparency = .2, scale = 3)
mykml$addLabelStyle(styleid = "labelstyle2", color = "green", transparency = 1, scale = 1)
 
x = data.frame(cbind(c(45.9178, 46.807), c(-59.967, -60.321), c("labelstyle1", "labelstyle2"), c("Example1", "Example2"), c("Louisbourg", "Neil's Harbour" )))
names(x) = c("lat", "lon", "styleUrl", "name", "description")
  
mykml$addPoint(x)
mykml$writekml(path = file.path(getwd(), "kml_outputs", "example_kml.kml"))

}

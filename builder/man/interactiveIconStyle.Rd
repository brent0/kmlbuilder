\name{interactiveIconStyle, interactiveIconStyle.RKmlFolder}
\alias{interactiveIconStyle}
\title{
Interactively Create IconStyle
}
\description{
This function is usually automatically entered by calling yourkmlobj$styleBuilder(). However it can be called on its own, but you must supply an id.
}
\usage{
interactiveIconStyle(id)
}
\arguments{
  \item{id}{
Mandatory. Define the id for this style. This id can be the same as other non-icon style ids previously created.
  }
}
\author{
Brent Cameron\cr
Department of Fisheries and Oceans Canada\cr
Population Ecology Division
}
\note{
\strong{id}\cr
 Do not attempt to define more than one IconStyle for the same id.
 
}

\examples{

mykml = RKmlObject()

##ANSWER THE QUESTIONS. For this example the answers are supplied below. Typically the questions
##will be answered on the fly.
\dontrun{
mykml$interactiveIconStyle(id = "mystyle")
upload.wikimedia.org/wikipedia/commons/c/c1/Rlogo.png
2
180
Y
red
N
fraction
.5
fraction
.5
}
x = data.frame(cbind(c(45.9178, 46.807), c(-59.967, -60.321), c("Example1", "Example2"), c("Louisbourg", "Neil's Harbour" )))
names(x) = c("lat", "lon", "name", "description")
  
mykml$addPoint(x, styleUrl = "mystyle")
mykml$preview()

}
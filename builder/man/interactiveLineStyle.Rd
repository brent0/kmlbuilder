\name{interactiveLineStyle, interactiveLineStyle.RKmlFolder}
\alias{interactiveLineStyle}
\title{
Interactively Create LineStyle
}
\description{
This function is usually automatically entered by calling yourkmlobj$styleBuilder(). However it can be called on its own, but you must supply an id.
}
\usage{
interactiveLineStyle(id)
}
\arguments{
  \item{id}{
Mandatory. Define the id for this style. This id can be the same as other non-line style ids previously created.
  }
}
\author{
Brent Cameron\cr
Department of Fisheries and Oceans Canada\cr
Population Ecology Division
}
\note{
\strong{id}\cr
 Do not attempt to define more than one LineStyle for the same id.
 
}

\examples{

mykml = RKmlObject()

##ANSWER THE QUESTIONS. For this example the answers are supplied below. Typically the questions
##will be answered on the fly.
\dontrun{
mykml$interactiveLineStyle(id = "mystyle")
M
yellow
1
grey
.8
100
.8
N
Y
}
x = data.frame(cbind(c(1, 1, 1), c(45.9178, 46.3624, 46.807), c(-59.967,-60.114, -60.321)))
names(x) = c("pid", "lat", "lon")
  
mykml$addLineString(x, name= "fake road", description = "Louisbourg to Neil's Harbour", styleUrl = "mystyle", tessellate = 1 )
mykml$preview()
#Notice the line disapears on close zoom, this is an error in Google Earth. This can be mitigated by adding more #points along the line.  
}
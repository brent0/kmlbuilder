\name{interactiveLabelStyle}
\alias{interactiveLabelStyle}
\title{
Interactively Create LabelStyle
}
\description{
This function is usually automatically entered by calling yourkmlobj$styleBuilder(). However it can be called on its own, but you must supply an id.
}
\usage{
interactiveLabelStyle(id)
}
\arguments{
  \item{id}{
Mandatory. Define the id for this style. This id can be the same as other non-label style ids previously created.
  }
}
\author{
Brent Cameron\cr
Department of Fisheries and Oceans Canada\cr
Population Ecology Division
}
\note{
\strong{id}\cr
 Do not attempt to define more than one LabelStyle for the same id.
 
}

\examples{

mykml = RKmlObject()

##ANSWER THE QUESTIONS. For this example the answers are supplied below. Typically the questions
##will be answered on the fly.
\dontrun{
mykml$interactiveLabelStyle(id = "mystyle")
orange
.3
3
N
}
x = data.frame(cbind(c(45.9178, 46.807), c(-59.967, -60.321), c("Example1", "Example2"), c("Louisbourg", "Neil's Harbour" )))
names(x) = c("lat", "lon", "name", "description")
  
mykml$addPoint(x, styleUrl = "mystyle")
#mykml$preview()

}
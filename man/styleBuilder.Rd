\name{styleBuilder}
\alias{styleBuilder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interactive Style Builder
}
\description{
Build a complete style with this function. The builder has the option to include mouseover effects in the style. Remember the style id that you provide as you will need it to link to features that you wish to have that style
}
\usage{
yourkmlobj$styleBuilder()
}

\author{
Brent Cameron\cr
Department of Fisheries and Oceans Canada\cr
Population Ecology Division
}


\examples{
mykml = RKmlObject()

##ANSWER THE QUESTIONS. For this example the answers are supplied below. Typically the questions
##will be answered on the fly.
\dontrun{
mykml$styleBuilder()
ms
Y
nms
mms
Y
Hello World
green
red
Y
N
Y
purple
9
1.1
N
N
N
Y
Hello Google
red
green
Y
N
N
N
N

}
lat = c(44, 44.1, 44, 44.2, 44.2, 44, 44.3, 44.3, 44)
lon = c(-60, -59.9, -59.8, -60, -59.8, -60.4, -60, -59.8, -59.4)
extrude = c(0, 0, 1, 1, 1, 0, 0, 1, 0)
altitude = c(100, 80, 100, 100, 120, 500, 500, 1000, 1000)
name = c("p1","p2","p3","p4","p5","p6","p7","p8","p9")
x = cbind(lat, lon, extrude, altitude, name)
  
mykml$addPoint(x, styleUrl = "ms", description = "This is an example description applied to all points")
#mykml$preview()

}

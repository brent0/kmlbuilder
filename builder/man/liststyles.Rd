\name{liststyles, liststyles.RkmlFolder}
\alias{liststyles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print kml Styles
}
\description{
Allows you to quickly view your kml styles. 
}
\usage{
yourkml$liststyles()
}
\author{
Brent Cameron\cr
Department of Fisheries and Oceans Canada\cr
Population Ecology Division
}

\examples{

mykml = RKmlObject()
mykml$addLineStyle(styleid = "mystyle", color = "red", transparency = .5, width = 100)
mykml$addLineStyle(styleid = "yourstyle", color = "#0000ff", transparency = 1)

mykml$addPolyStyle(styleid = "mystyle", color = "red", transparency = 1, fill = 0)
mykml$addPolyStyle(styleid = "yourstyle", color = "yellow", transparency = 1, outline = 0)

mykml$liststyles()

}

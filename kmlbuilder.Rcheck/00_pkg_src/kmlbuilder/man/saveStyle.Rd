\name{saveStyle, loadStyle}
\alias{saveStyle, loadStyle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write/load kml style Object to/from file
}
\description{
Write a style to file for future use. The file can be read back into an existing kml object with loadStyle. 
}
\usage{
yourkml$saveStyle(path)
yourkml$loadStyle(path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{Mandatory. The full path to where the style will be saved/loaded. }

}


\author{
Brent Cameron\cr
Department of Fisheries and Oceans Canada\cr
Population Ecology Division

}


\examples{

xmaskml = RKmlObject()
path = file.path(getwd(), "xmas_style")

xmaskml$addLabelStyle(styleid = "redlabel", color = "red", transparency = .2, scale = 3)
xmaskml$addLabelStyle(styleid = "greenlabel", color = "green", transparency = 1, scale = 1)
#xmaskml$saveStyle(path)


port_starboard_kml = RKmlObject()
port_starboard_kml$addLabelStyle(styleid = "yellowlabel", color = "yellow", transparency = .2, scale = 3)
#port_starboard_kml$loadStyle(path) 
port_starboard_kml$liststyles()

}

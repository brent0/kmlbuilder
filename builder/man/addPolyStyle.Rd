\name{addPolyStyle, addPolyStyle.RkmlFolder}
\alias{addPolyStyle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add PolyStyle to KML Object
}
\description{
After creating a kml object, this method defines a style that can be used by Polygon features within the object. When a feature uses a polygon, be sure that the feature's styleUrl matches the desired styleid argument. See references for documentation on how to effectively use the text argument.
}
\usage{

  
yourKMLobj$addLabelStyle(styleid, color, transparency, colorMode, fill, outline)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{styleid}{
Mandatory. Define the id for this style. kml features will need to reference this id to use the defined style. You may choose an id that has already been defined in other styles types such as LineStyle or PolyStyle.
  }

   
  \item{color}{Define the color of the Polygon. This can be one of the predefined colors(), color as hex, or color as rgb object. (Default: "red")}
     \item{transparency}{numeric. Set the transparency for the color. 0.0 - 1.0, fully opaque:0, solid:1. (Default: 1)}
      \item{colorMode}{character. One of 'normal' or 'random'. Random will sudo-randomly apply a color based on the color argument, use color = white for true random. (Default: "normal")}
   \item{fill}{boolean (0-no fill or 1-fill). Choose weather to fill the polygon with the color. (Default: 1)}
\item{outline}{boolean (0-no outline or 1-outline). Choose weather to draw an outline arround the polygon. (Default: 1)}
 
}

\references{
\href{https:\/\/developers.google.com/kml/documentation/kmlreference\#polystyle}{KML polyStyle Reference}
}
\author{
Brent Cameron\cr
Department of Fisheries and Oceans Canada\cr
Population Ecology Division
}
\note{
\strong{colorMode}\cr
see \href{https:\/\/developers.google.com/kml/documentation/kmlreference\#colormode}{KML colorMode Reference}
\cr

\strong{styleid}\cr
 Do not attempt to define more than one polystyle for the same styleid.
 
}


\examples{

mykml = RKmlObject()

#Create styles
mykml$addPolyStyle(styleid = "mystyle1", color = "red", transparency = .5)
mykml$addPolyStyle(styleid = "mystyle2", color = "yellow", transparency = 1, fill = 0)
mykml$addPolyStyle(styleid = "mystyle3", color = "yellow", transparency = 1, outline = 0)

pid = c(1, 1, 1, 2, 2, 2, 6, 6, 6, 6)
lat = c(44, 44.1, 44, 44.2, 44.2, 44.1, 44, 44.1, 44.1, 44)
lon = c(-60, -59.9, -59.8, -60.2, -60.1, -60, -60.5, -60.5, -60.2, -60.2)

x = cbind(pid, lat, lon)

style = c("mystyle1", "mystyle2", "mystyle3") #Cycles with pid
altitude = c(2000, 4000, 8000) #Cycles with pid


mykml$addPolygon(x, styleUrl = style, altitude = altitude)

mykml$addLabelStyle(styleid = "mystyle1", color = "red", transparency = .2, scale = 3)
mykml$addLabelStyle(styleid = "mystyle2", color = "green", transparency = 1, scale = 1)
 
#mykml$preview()

}

\name{addLineStyle}
\alias{addLineStyle}
\title{
Add LineStyle to KML Object
}
\description{
After creating a kml object, this method defines a style that can be used by LineString features within the object. When a feature is a LineString, be sure that the feature's styleUrl matches the desired styleid argument. 
}
\usage{
yourKMLobj$addLineStyle(styleid, color, transparency, width, outerColor, outerTransparency, outerPortion, colorMode, labelVisibility)
}

\arguments{

  \item{styleid}{
Mandatory. Define the id for this style. kml features will need to reference this id to use the defined style. You may choose an id that has already been defined in other styles types such as LabelStyle or PolyStyle.
  }
   

  \item{color}{Define the color of the line. This can be one of the predefined colors(), color as hex, or color as rgb object. (Default: NULL)}
   \item{transparency}{numeric. Set the transparency for the color. 0.0 - 1.0, fully opaque:0, solid:1. (Default: 1)}
   \item{width}{numeric. Define the line width. If a single color line is used width is in pixels, if a deul band line is used width is in meters. (Default: 1)}
   \item{outerColor}{Define the outer color band only if a deul band is desired. This can be one of the predefined colors(), color as hex, or color as rgb object. (Default: NULL)}
   \item{outerTransparency}{numeric. Set the transparency for the outerColor. 0.0 - 1.0, fully opaque:0, solid:1. (Default: NULL)}
   \item{outerPortion}{numeric. Define the portion of the width is the outer portion. 0.0 - 1.0. (Default: NULL)}
  \item{colorMode}{character. One of 'normal' or 'random'. Random will sudo-randomly apply a color based on the color argument, use color = white for true random. (Default: "normal")}
  \item{labelVisibility}{Boolean (0-invisible or 1-visible). Specify if a label should be displayed. Label value taken from name argument. (Default: 0)}
}

\references{
\href{https://developers.google.com/kml/documentation/kmlreference\#linestyle}{KML lineStyle Reference}
}
\author{
Brent Cameron\cr
Department of Fisheries and Oceans Canada\cr
Population Ecology Division
}
\note{
\strong{colorMode}\cr
see \href{https://developers.google.com/kml/documentation/kmlreference\#colormode}{color Mode}\cr

\strong{styleid}\cr
 Do not attempt to define more than one linestyle for the same styleid.
 
}


\examples{

mykml = RKmlObject()

pid = c(1, 1, 1, 2, 2, 6, 6, 6, 6)
lat = c(44, 44.1, 44, 44.2, 44.2, 44, 44.3, 44.3, 44)
lon = c(-60, -59.9, -59.8, -60, -59.8, -60.4, -60, -59.8, -59.4)
x = cbind(pid, lat, lon)

mykml$addLineString(x)

#Create styles
mykml$addLineStyle(styleid = "linestyle1", color = "red", transparency = .5, width = 100, outerColor = "orange", outerTransparency = 1, outerPortion = .5)
mykml$addLineStyle(styleid = "linestyle2", color = "yellow", transparency = 1, width = 50, outerColor = "green", outerTransparency = 1, outerPortion = .2)
mykml$addLineStyle(styleid = "linestyle3", color = "#0000ff", transparency = 1, labelVisibility = 1)

#Shift lats up to see style applied next to non style
lat = lat + .02

styleUrl = c("linestyle1", "linestyle1", "linestyle1", "linestyle2", "linestyle2", "linestyle3", "linestyle3", "linestyle3", "linestyle3")
x = cbind(pid, lat, lon, styleUrl)
mykml$addLineString(x, name = "hello world")

#Same as above but without redundant styleURL
#x = cbind(pid, lat,  lon)
#style = c("linestyle1", "linestyle2", "linestyle3") #Cycles with pid
#mykml$addLineString(x, styleUrl = style)
#OR apply 1 altitude to each pid
#mykml$addLineString(x, styleUrl = style, altitude = 1000)

##All optional arguments can be supplied in the function call and will cycle by pid
#mykml$preview()

}

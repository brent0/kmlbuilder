\name{addIconStyle, addIconStyle.RkmlFolder}
\alias{addIconStyle}
\title{
Add IconStyle to KML Object
}
\description{
After creating a kml object, this method defines a style that can be used by Icon features within the object. When a feature uses a icon, be sure that the feature's styleUrl matches the desired styleid argument. 
}
\usage{

yourKMLobj$addIconStyle(styleid, href, color, scale, heading, xunits, yunits, x, y, colorMode)
}

\arguments{

  \item{styleid}{
Mandatory. Define the id for this style. kml features will need to reference this id to use the defined style. You may choose an id that has already been defined in other styles types such as LineStyle or PolyStyle.
  }

\item{href}{character. Path to image, can be HTTP address or a local file.(Default: Google's default pushpin)}

  
  \item{color}{Define the color to be blended with the image. This can be one of the predefined colors(), color as hex, or color as rgb object. (Default: NULL)}
   \item{transparency}{numeric. Set the transparency for the color. 0.0 - 1.0, fully opaque:0, solid:1. (Default: 1)}
   \item{scale}{numeric. Define how large the image will appear. (Default: 1)}
   \item{heading}{numeric. Define a rotation for the image between 0 and 360. (Default: 0)}
   \item{xunits}{character. Define units for specifying the x anchor point in the image. One of 'fraction', 'pixels' or 'insetPixels'. (Default: "fraction")}
  \item{x}{numeric. For fraction units a value from 0 to 1. For pixels and insetPixels units a value from 1 to x resolution. (Default: 0.5)}
\item{yunits}{character. Define units for specifying the y anchor point in the image. One of 'fraction', 'pixels' or 'insetPixels'. (Default: "fraction")}
  \item{y}{numeric. For fraction units a value from 0 to 1. For pixels and insetPixels units a value from 1 to y resolution. (Default: 0.5)}
  \item{colorMode}{character. One of 'normal' or 'random'. Random will sudo-randomly apply a color based on the color argument, use color = white for true random. (Default: "normal")}
}

\references{
\href{https:\/\/developers.google.com/kml/documentation/kmlreference\#iconstyle}{KML iconStyle Reference}
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
 Do not attempt to define more than one iconstyle for the same styleid.
 
}


\examples{

mykml = RKmlObject()

fishpath = "http://maps.google.com/mapfiles/ms/micons/fishing.png" 
mykml$addIconStyle(styleid = "iconstyle1", href = fishpath, color = "red", scale = 5, heading = 180)
mykml$addIconStyle(styleid = "iconstyle2", href = fishpath, color = "blue", scale = 1, heading = 0)
 
x = data.frame(cbind(c(45.9178, 46.807), c(-59.967, -60.321), c("iconstyle1", "iconstyle2"), c("red fishing", "blue fishing"), c("Louisbourg", "Neil's Harbour" )))
names(x) = c("lat", "lon", "styleUrl", "name", "description")
  
mykml$addPoint(x)
mykml$preview()

}

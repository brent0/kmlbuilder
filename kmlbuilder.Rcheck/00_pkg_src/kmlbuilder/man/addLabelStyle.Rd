\name{addLabelStyle}
\alias{addLabelStyle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add LabelStyle to KML Object
}
\description{
After creating a kml object, this method defines a style that can be used by Label features within the object. When a feature uses a label, be sure that the feature's styleUrl matches the desired styleid argument. See references for documentation on how to effectively use the text argument.
}
\usage{

yourKMLobj$addLabelStyle(styleid, color, transparency, colorMode, scale)
}

\arguments{

  \item{styleid}{
Mandatory. Define the id for this style. kml features will need to reference this id to use the defined style. You may choose an id that has already been defined in other styles types such as LineStyle or PolyStyle.
  }

   
  \item{color}{Define the color of the label. This can be one of the predefined colors(), color as hex, or color as rgb object. (Default: "red")}
     \item{transparency}{numeric. Set the transparency for the color. 0.0 - 1.0, fully opaque:0, solid:1. (Default: 1)}
      \item{colorMode}{character. One of 'normal' or 'random'. Random will sudo-randomly apply a color based on the color argument, use color = white for true random. (Default: "normal")}
   \item{scale}{numeric. Define how large the label will appear. (Default: 1)}

 
}

\references{
\href{https://developers.google.com/kml/documentation/kmlreference\#labelstyle}{KML labelStyle Reference}
}
\author{
Brent Cameron\cr
Department of Fisheries and Oceans Canada\cr
Population Ecology Division
}
\note{
\strong{colorMode}\cr
see \href{https://developers.google.com/kml/documentation/kmlreference\#colormode}{KML colormode Reference}
\cr

\strong{styleid}\cr
 Do not attempt to define more than one iconstyle for the same styleid.
 
}


\examples{

mykml = RKmlObject()


mykml$addLabelStyle(styleid = "labelstyle1", color = "red", transparency = .2, scale = 3)
mykml$addLabelStyle(styleid = "labelstyle2", color = "green", transparency = 1, scale = 1)
 
x = data.frame(cbind(c(45.9178, 46.807), c(-59.967, -60.321), c("labelstyle1", "labelstyle2"), c("Example1", "Example2"), c("Louisbourg", "Neil's Harbour" )))
names(x) = c("lat", "lon", "styleUrl", "name", "description")
  
mykml$addPoint(x)
#mykml$preview()

}

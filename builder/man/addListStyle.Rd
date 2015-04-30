\name{addListStyle, addListStyle.RkmlFolder}
\alias{addListStyle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add ListStyle to KML Object
}
\description{
After creating a kml object, this method defines a style that can be used by containers. When a container uses a ListStyle, be sure that the feature's styleUrl matches the desired styleid argument. See references for documentation on how to effectively use the text argument.
}
\usage{

  
yourKMLobj$addListStyle(styleid, listItemTpye, bgColor)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{styleid}{
Mandatory. Define the id for this style. kml containers will need to reference this id to use the defined style. You may choose an id that has already been defined in other styles types such as LineStyle or PolyStyle.
  }

  \item{listItemType}{Define the way elements of a container using this style are displayed. This can be one of check, checkOffOnly, checkHideChildren or radioFolder. (Default: "check)} 
  \item{bgColor}{Define the background color. This can be one of the predefined colors(), color as hex, or color as rgb object. (Default: "white")}
     
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

\strong{styleid}\cr
 Do not attempt to define more than one liststle for the same styleid.
 
}


\examples{

mykml = RKmlObject()

#Create a radio style to allow only one element of a container to be displayed at a time
mykml$addListStyle(styleid = "test", bgColor = "green", listItemType = "radioFolder")
mykml$addPolyStyle(styleid = "mystyle2", color = "yellow", transparency = 1, fill = 0)
mykml$addFolder(fid = "new", styleUrl = "test")
mykml$getFolder("new")$addFolder("1")
mykml$getFolder("new")$addFolder("2")
mykml$getFolder("new")$addFolder("3")
 
mykml$preview()

}

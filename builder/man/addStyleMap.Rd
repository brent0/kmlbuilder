\name{addStyleMap, addStyleMap.RKmlFolder}
\alias{addStyleMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create Mouseover Effects with Style
}
\description{
With this function you can map mouseover and non-mouseover styles to a single style id. The style id can then be referenced by features that you wish to have a mouseover effect. 
}
\usage{
addStyleMap(id, idn, idh)
}

\arguments{
  \item{id}{
Mandatory. Define style id. kml features will need to reference this id to use the defined style. You must choose a unique style id.
  }   
  \item{idn}{Mandatory. Reference the normal(non-highlighted) style id. If you have not yet created this style, you must created it in order for it to be linked. }
  \item{idh}{Mandatory. Reference the highlighted style id. If you have not yet created this style, you must created it in order for it to be linked. }
}

\references{
\href{https:\/\/developers.google.com/kml/documentation/kmlreference\#stylemap}{KML styleMap Reference}
}
\author{
Brent Cameron\cr
Department of Fisheries and Oceans Canada\cr
Population Ecology Division
}

\examples{

mykml = RKmlObject()

#add styles. labelstyle1 will be the normal style, labelstyle2 will be the mouseover style
mykml$addLabelStyle(styleid = "labelstyle1", color = "red", transparency = .5, scale = 1)
mykml$addLabelStyle(styleid = "labelstyle2", color = "green", transparency = 1, scale = 2)

#make icon transparent(invisible) for the mouseover style
mykml$addIconStyle(styleid = "labelstyle2", color = "white", transparency = 0)

#add map style so mouseover effects can be acheived
mykml$addStyleMap(id = "my_mouseover_style", idn = "labelstyle1", idh = "labelstyle2")

#create points so styles can be visualized
x = data.frame(cbind(c(45.9178, 46.807), c(-59.967, -60.321), c("Example1", "Example2"), c("Louisbourg", "Neil's Harbour" )))
names(x) = c("lat", "lon", "name", "description")
mykml$addPoint(x, styleUrl = "my_mouseover_style")

#sample your creation
#mykml$preview()

}

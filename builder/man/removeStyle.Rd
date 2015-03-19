\name{removeStyle, removeStyle.RkmlFolder}
\alias{removeStyle}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove Style
}
\description{
This function is used to remove a style or remove certian style types from a style. 
}
\usage{
yourKMLobj$removeStyle(styleid) #Remove style and all types within
yourKMLobj$removeStyle(styleid, styletype) #Remove an individual style type from a style


}

\arguments{

  \item{styleid}{
Mandatory. The style which you would like to remove from or remove completely.  }
  
     \item{styletype}{Character string or NULL. One of PolyStyle, IconStyle, LineStyle, BalloonStyle or LabelStyle. Specify which styletype to remove from this style. If no styletype provided, the whole style will be removed.(Default = NULL) }
     }

\author{
Brent Cameron \cr
Department of Fisheries and Oceans Canada \cr
Population Ecology Division 
}
\examples{

mykml = RKmlObject()

mykml$addLineStyle(styleid = "mystyle", color = "red", transparency = .5, width = 100)
mykml$addLineStyle(styleid = "yourstyle", color = "#0000ff", transparency = 1)

mykml$addPolyStyle(styleid = "mystyle", color = "red", transparency = 1, fill = 0)
mykml$addPolyStyle(styleid = "yourstyle", color = "yellow", transparency = 1, outline = 0)

mykml$liststyles()

mykml$removeStyle(styleid = 'yourstyle')
mykml$removeStyle(styleid = 'mystyle', styletype = "PolyStyle")

mykml$liststyles()

}

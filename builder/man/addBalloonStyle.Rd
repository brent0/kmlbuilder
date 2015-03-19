\name{addBalloonStyle, addBalloonStyle.RKmlFolder}
\alias{addBalloonStyle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add BalloonStyle to KML Object
}
\description{
After creating a kml object, this method defines a style that can be used by Balloon features within the object. When a feature uses a balloon, be sure that the feature's styleUrl matches the desired styleid argument. See references for documentation on how to effectively use the text argument.
}
\usage{
yourKMLobj$addBalloonStyle(styleid, bgColor, textColor, text, displayMode)
}

\arguments{
  \item{styleid}{
Mandatory. Define the id for this style. kml features will need to reference this id to use the defined style. You may choose an id that has already been defined in other styles types such as LineStyle or PolyStyle.
  }
    \item{bgColor}{
Define the background color of the balloon. This can be one of the predefined colors(), color as hex, or color as rgb object. (Default: "white")
  }
  \item{textColor}{
Define the text color of the balloon. This can be one of the predefined colors(), color as hex, or color as rgb object. (Default: "black")
  }
   \item{text}{
Define what text will appear in the balloon. This can be customized with each feature by using markup within the text. See references for more details. (Default: A CDATA markup that resembles the default kml balloon)
  }
   \item{displayMode}{
Either "display" or "hide". Choose weather balloon will be displayed. (Default: "display")
  }
}
\value{
Does not return a value, simply adds style to kml objects style list
}
\references{
\href{https:\/\/developers.google.com/kml/documentation/kmlreference\#balloonstyle}{KML balloonStyle Reference}
}
\author{
Brent Cameron\cr
Department of Fisheries and Oceans Canada\cr
Population Ecology Division
}
\note{
\strong{styleid}\cr
 Do not attempt to define more than one balloonstyle for the same styleid.
 \cr
\strong{text}\cr
For advanced text arguments, wrap html in CDATA tags. You can use packages to build html such as R2HTML(uses headers that are not required by CDATA) or, if you are comfortable with html, just add your html text inside CDATA enclosures.\cr
\code{text = ''<![CDATA[}\cr
\code{<b><font color='#CC0000' size='+3'>$[name]</font></b>}\cr
\code{<br/><br/>}\cr
\code{<font face='Courier'>$[description]</font>}\cr
\code{]]>''}
    
      
}


\examples{

mykml = RKmlObject()
customtext = "This is the $[name]. <br/> Click on this to look at $[description]. <br/> See references for more custom text insert  options."
 
mykml$addBalloonStyle(styleid = "colorstyle1", bgColor = "violetred4", textColor = rgb(.2, .2, 1))
mykml$addBalloonStyle(styleid = "textstyle1", bgColor = "black", textColor = "white", text = customtext, displayMode = "display")
 
x = data.frame(cbind(c(45.9178, 46.807), c(-59.967, -60.321), c("colorstyle1", "textstyle1"), c("Color Example", "Text Example"), c("Louisbourg", "Neil's Harbour" )))
names(x) = c("lat", "lon", "styleUrl", "name", "description")
  
mykml$addPoint(x)
mykml$preview()

}


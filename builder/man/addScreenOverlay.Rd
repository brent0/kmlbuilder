\name{addScreenOverlay, addScreenOverlay.RkmlFolder}
\alias{addScreenOverlay}
\title{
Add a Screen Overlay to kml
}
\description{
Add a screen overlay to a kml object. This will be usefull when adding titles or legends to your kml. 
}
\usage{
addScreenOverlay(fn = pathtogeoimage)
addGroundOverlay(fn = pathtoimage, overlay_x, overlay_xunit, overlay_y, overlay_yunit, screen_x, screen_Y)
}
\arguments{
  \item{fn}{Mandatory. Specify the path to the image.}
\item{overlay_x}{numeric. Set the x axis anchor position on the image. 0.0 - 1.0 for 'fraction' units, 1 - max(x resolution) for 'pixel' or 'insetpixel' units. (Default: .5)}
  \item{overlay_y}{numeric. Set the x axis anchor position on the image. 0.0 - 1.0 for 'fraction' units, 1 - max(x resolution) for 'pixel' or 'insetpixel' units. (Default: .5)}
  \item{overlay_xunit}{character. One of 'fraction', 'pixels' or 'insetpixels'. Set the units for the overlay_x argument. (Default: 'fraction')}
  \item{overlay_yunit}{character. One of 'fraction', 'pixels' or 'insetpixels'. Set the units for the overlay_y argument. (Default: 'fraction')}
  \item{screen_x}{numeric. Set the x axis anchor position on the screen. 0.0 - 1.0 for 'fraction' units, 1 - max(x resolution) for 'pixel' or 'insetpixel' units. (Default: .5)}
  \item{screen_y}{numeric. Set the x axis anchor position on the screen. 0.0 - 1.0 for 'fraction' units, 1 - max(x resolution) for 'pixel' or 'insetpixel' units. (Default: .5)}
  \item{screen_xunit}{character. One of 'fraction', 'pixels' or 'insetpixels'. Set the units for the screen_x argument. (Default: 'fraction')}
  \item{screen_yunit}{character. One of 'fraction', 'pixels' or 'insetpixels'. Set the units for the screen_y argument. (Default: 'fraction')}
  \item{rotation_x}{numeric. Set the x axis screen position about which the image will be rotated. 0.0 - 1.0 for 'fraction' units, 1 - max(x resolution) for 'pixel' or 'insetpixel' units. (Default: .5)}
  \item{rotation_y}{numeric. Set the y axis screen position about which the image will be rotated. 0.0 - 1.0 for 'fraction' units, 1 - max(x resolution) for 'pixel' or 'insetpixel' units. (Default: .5)}
  \item{rotation_xunit}{character. One of 'fraction', 'pixels' or 'insetpixels'. Set the units for the rotation_x argument. (Default: 'fraction')}
  \item{rotation_yunit}{character. One of 'fraction', 'pixels' or 'insetpixels'. Set the units for the rotation_y argument. (Default: 'fraction')}
  \item{size_x}{numeric. Set the size of the image along the x axis. 0.0 - 1.0 for 'fraction' units, 1 - n for 'pixel' units. (Default: 1)}
  \item{size_y}{numeric. Set the size of the image along the y axis. 0.0 - 1.0 for 'fraction' units, 1 - n for 'pixel' units. (Default: 1)}
  \item{size_xunit}{character. One of 'fraction' or 'pixels'. Set the units for the size_x argument. (Default: 'fraction')}
  \item{size_yunit}{character. One of 'fraction' or 'pixels'. Set the units for the size_y argument. (Default: 'fraction')}  
\item{drawOrder}{ numeric, set the order of the image rendering. (Default:NULL) }
\item{color}{Specify a color to blend with the image. This can be one of the predefined colors(), color as hex, or color as rgb object. (Default: NULL)}
     \item{transparency}{numeric. Set the transparency for the color. 0.0 - 1.0, fully opaque:0, solid:1. (Default: 1)}
       \item{rotation}{numeric. Set the transparency for the color. 0.0 - 1.0, fully opaque:0, solid:1. (Default: 1)}
\item{altitude}{ numeric, meters above/below altiudeMode. 'clamp' altitudeModes ignore altitude. (Default: 0)}
\item{altitudeMode}{one of "clampToGround", "relativeToGround", "absolute", "clampToSeaFloor", "relativeToSeaFloor". (Default:"clampToGround")} 
     \item{name}{Character string or NULL. What the feature will be called in the graphics environment. (Default:NULL) }
     \item{visibility}{Boolean (0-invisible or 1-visible). Specify if the feature will be visible. (Default: 1)}
     \item{open}{ Boolean (0-closed or 1-open). Specify if the feature is open or closed in the kml object tree. (Default: 0)}
     \item{atomauthor}{ Character string or NULL. Text identifying an author relevant to the topic. See Ascription Elements under References (Default: NULL) }
     \item{atomlinkhref}{Character string or NULL. Text identifying a web link relevant to the topic.  See Ascription Elements under References (Default: NULL) }
     \item{address}{Character string or NULL. Specify an unstructured address that is associated with the overlay. (Default:NULL) }
     \item{xalAddressDetails}{Character string or NULL. Specify an structured address in eXtensible Address Language(See xal:AddressDetails under References) that is associated with the overlay. (Default:NULL) }
     \item{phoneNumber}{ Character string or NULL. Text identifying a phone number, useful for mobile apps. (Default:NULL) }
     \item{Snippet}{Character string or NULL. Supply details in addition to description. (Default:NULL)}
     \item{description}{Character string or NULL. Supply details, may contain CDATA. See NOTES section for more info. (Default:NULL)}
     \item{AbstractView}{Character string or NULL. The id of the desired AbstractView. Must create AbstractView with addAbstractView() function. (Default:NULL)}
     \item{TimeStamp}{ Character string of date-time in one of the following formats: (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss). Used to create kml timeseries. (Default:NULL)}
     \item{TimeSpanStart}{ Character string of date-time in one of the following formats(overides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss). Used to create timeseries. (Default:NULL)} 
     \item{TimeSpanEnd}{ Character string of date-time in one of the following formats(overrides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss). Used to create timeseries. (Default:NULL)}  
     \item{styleUrl}{Character string or NULL. The id of the desired Style. Must create a style with one of the addStyle() or interactiveStyle() functions. (Default:NULL)}
     \item{Region}{Currently not supported}
     \item{ExtendedData}{Currently not supported}
}
\references{
\href{https:\/\/developers.google.com/kml/documentation/kmlreference\#screenoverlay}{KML screenoverlay Reference} \cr
\href{https:\/\/developers.google.com/kml/documentation/kmlreference\#sampleattribution}{Ascription Elements}\cr 
\href{http:\/\/www.schemacentral.com/sc/kml22/e-xal_AddressDetails.html}{xal:AddressDetails}
}
\author{
Brent Cameron \cr
Department of Fisheries and Oceans Canada \cr
Population Ecology Division 
}
\note{

\strong{description}\cr
For advanced description arguments, wrap html in CDATA tags. You can use packages to build html such as R2HTML(uses headers that are not required by CDATA) or, if you are comfortable with html, just add your html text inside CDATA enclosures. \cr
\code{text = ''<!\[CDATA\[ }\cr
\code{<b><font color="#CC0000" size='+3'>Example Text</font></b>}\cr
\code{<br/><br/>}\cr
\code{<font face='Courier'>Example Text</font>}\cr
\code{]]>''}\cr
\cr
      See also: \href{https:\/\/developers.google.com/kml/documentation/kmlreference\#descriptionexample}{Description Example}
      
      }
\examples{
png(filename="figure.png", height=200, width=300, 
    bg="transparent")
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend(1, 1, legend = c("Hello", "World"), col=1:2, lwd=2, cex=3, xjust=0.5, yjust=0.5)
dev.off()
mykml = RKmlObject()
fn = file.path(getwd(), "figure.png")
mykml$addScreenOverlay(fn, size_x = .2, size_y = .2)

mykml$preview()

}

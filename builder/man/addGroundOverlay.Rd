\name{addGroundOverlay, addGroundOverlay.RkmlFolder}
\alias{addGroundOverlay}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add a Ground Overlay to kml
}
\description{
Add groundoverlay to a kml object. The function uses rgdal package to read positional metadata from the supplied image. If the image has no positional metadata attached then you will need to supply the east and west longitude bounds plus the north and south latitude bounds.
}
\usage{
addGroundOverlay(fn = pathtogeoimage)
addGroundOverlay(fn = pathtosimpleimage, east = maxlon, west = minlon, north = maxlat, south = minlat)
}

\arguments{

  \item{fn}{Mandatory. Specify the path to the image.}
   \item{x}{Optional dataframe. Allow batch adding of groundoverlays, column names the same as variable names. }
     \item{east}{ Define the eastern most edge of the image. Only required if fn has no positional metadata. }
     \item{west}{ Define the western most edge of the image. Only required if fn has no positional metadata. }
     \item{north}{ Define the northern most edge of the image. Only required if fn has no positional metadata. }
     \item{west}{ Define the southern most edge of the image. Only required if fn has no positional metadata. }


\item{drawOrder}{ numeric, set the order of the image rendering. (Default:NULL) }
\item{color}{Specify a color to blend with the image. This can be one of the predefined colors(), color as hex, or color as rgb object. (Default: NULL)}
     \item{transparency}{numeric. Set the transparency for the color. 0.0 - 1.0, fully opaque:0, solid:1. (Default: 1)}
\item{altitude}{ numeric, meters above/below altiudeMode. 'clamp' altitudeModes ignore altitude. (Default: 0)}
\item{altitudeMode}{one of "clampToGround", "relativeToGround", "absolute", "clampToSeaFloor", "relativeToSeaFloor". (Default:"clampToGround")} 
     \item{name}{Character string or NULL. What the feature will be called in the graphics environment. (Default:NULL) }
     \item{visibility}{Boolean (0-invisible or 1-visible). Specify if these features are visible. (Default: 1)}
     \item{open}{ Boolean (0-closed or 1-open). Specify if the feature is open or closed in the kml object tree. (Default: 0)}
     \item{atomauthor}{ Character string or NULL. Text identifying an author relevant to the topic. See Ascription Elements under References (Default: NULL) }
     \item{atomlinkhref}{Character string or NULL. Text identifying a web link relevant to the topic. See Ascription Elements under References (Default: NULL) }
     \item{address}{Character string or NULL. Specify an unstructured address that is associated with the overlay. (Default:NULL) }
     \item{xalAddressDetails}{Character string or NULL. Specify an structured address in eXtensible Address Language(sSee xal:AddressDetails under References) that is associated with the overlay. (Default:NULL) }
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
\href{https:\/\/developers.google.com/kml/documentation/kmlreference\#groundoverlay}{KML groundoverlay Reference} \cr
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

mykml = RKmlObject()
mykml$addAbstractView(type = "lookat", viewid = "Rview", latitude = 45.5, longitude = -62, range = 1000000)

fn = file.path(R.home(), "doc", "html", "logo.jpg")
mykml$addGroundOverlay(fn = fn, east = -63, west = -61, north = 46.5, south = 44.5, 
  transparency = .3, color = "red", name = "RedR", AbstractView = "Rview" )

mykml$preview()

}


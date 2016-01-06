\name{addPoint}
\alias{addPoint}
\title{
Add Point Features
}
\description{
After creating a kml object or folder, you can add Point features with this function so they are viewable in the graphics window of the google API. A dataframe x must be suplied to this function. At a minimum, x must contain columns lat and lon. To style these features, use addIconStyle and/or addLabelSytle and match styleid to styleUrl. 
}
\usage{
yourKMLobj$addPoint(x, ...)
yourKMLobj$addPoint(x, altitude, styleUrl, extrude, altitudeMode, name, visibility, open, atomauthor, atomlinkhref, address, xalAddressDetails, phoneNumber, Snippet, description, AbstractView, TimeStamp, TimeSpanStart, TimeSpanEnd, styleUrl, Region, ExtendedData)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{x}{Mandatory. A dataframe with columns lat and lon. x may also contain additional columns. All of the listed optional arguments can be supplied in the function call to apply to records of x, or can be a column of x (column name = argument name). This gives the ability to provide specific arguments to specific points.  }
\item{altitude}{numeric. Meters above/below altiudeMode. 'clamp' altitudeModes ignore altitude. (Default: 0)}
\item{extrude}{boolean (0-not extrude or 1-extrude). Choose weather to extrude(connect) the Point to the ground. If you wish to extrude, altitudeMode must be one of relativeToGround, relativeToSeaFloor, or absolute. (Default: 1)}
\item{altitudeMode}{character, one of "clampToGround", "relativeToGround", "absolute", "clampToSeaFloor", "relativeToSeaFloor". (Default:"clampToGround")} 
     \item{name}{character, what the feature will be called in the graphics environment. (Default:NULL) }
     \item{visibility}{boolean (0-invisible or 1-visible). Specify if this feature will be visible. (Default: 1)}
     \item{open}{ boolean (0-closed or 1-open). Specify if the feature is open or closed in the kml object tree. (Default: 0)}
     \item{atomauthor}{ character, text identifying an author relevant to the topic. See Ascription Elements under References(Default: NULL) }
     \item{atomlinkhref}{character, text identifying a web link relevant to the topic. See Ascription Elements under References(Default: NULL) }
     \item{address}{character, specify an unstructured address that is associated with the point. (Default:NULL) }
     \item{xalAddressDetails}{character, specify a structured address in eXtensible Address Language(See xal:AddressDetails under References) that is associated with the point. (Default:NULL) }
     \item{phoneNumber}{ character, represent a phone number, useful for mobile apps. (Default:NULL) }
     \item{Snippet}{character, supply details in addition to description. (Default:NULL)}
     \item{description}{character, supply details relevant to the feature. May contain CDATA. See NOTES section for more info. (Default:NULL)}
     \item{AbstractView}{character, the id of the desired AbstractView. Must create AbstractView with addAbstractView() function. (Default:NULL)}
     \item{TimeStamp}{ character, define a moment of date-time in one of the following formats: (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss). Used to create kml timeseries. (Default:NULL)}
     \item{TimeSpanStart}{ character, define the start of a span of date-time in one of the following formats(overides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss). Used to create timeseries. (Default:NULL)} 
     \item{TimeSpanEnd}{ character, define the end of a span of date-time in one of the following formats(overrides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss). Used to create timeseries. (Default:NULL)}  
     \item{styleUrl}{character, the id of the desired Style. Must create a style with one of the addStyle() or interactiveStyle() functions. (Default:NULL)}
     \item{Region}{Currently not supported}
     \item{ExtendedData}{Currently not supported}
     \item{inFolder}{Allows the adding of data to a specified folder, (EXPERIMENTAL) Usage: inFolder = 'this\\that' }   
You can add styles directly by defining any of the following variables. This is however discouraged as a new style is created for each row of x. These will be ignored if styleid is defined.   
\item{icon_color, icon_href, icon_transparency, icon_scale, icon_heading, icon_xunits, icon_x, icon_yunits, icon_y, icon_colorMode, bal_bgColor, bal_textColor, bal_text,	bal_displayMode, label_color, label_transparency, label_colorMode, label_scale, line_color, line_transparency, line_width, line_outerColor, line_outerTransparency, line_outerPortion, line_colorMode, line_labelVisibility	}{You can define these but it is POOR FORM!!! see icon, balloon, label, and line style documentation for values.   }
}

\references{
\href{https://developers.google.com/kml/documentation/kmlreference\#point}{KML Point Reference} \cr
\href{https://developers.google.com/kml/documentation/kmlreference\#descriptionexample}{Description Example}\cr
\href{https://developers.google.com/kml/documentation/kmlreference\#sampleattribution}{Ascription Elements}\cr 
\href{http://www.schemacentral.com/sc/kml22/e-xal_AddressDetails.html}{xal:AddressDetails}
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
      See also: \href{https://developers.google.com/kml/documentation/kmlreference\#descriptionexample}{Description Example}
      
}



\examples{

mykml = RKmlObject()


imagepath = "http://maps.google.com/mapfiles/kml/paddle/wht-circle-lv.png" 
mykml$addIconStyle(styleid = "mystyle", color = "salmon2", href = imagepath, scale = .5, colorMode = "random")
mykml$addLabelStyle(styleid = "mystyle", color = "#000000", transparency = .8, scale = 1)

lat = c(44, 44.1, 44, 44.2, 44.2, 44, 44.3, 44.3, 44)
lon = c(-60, -59.9, -59.8, -60, -59.8, -60.4, -60, -59.8, -59.4)
extrude = c(0, 0, 1, 1, 1, 0, 0, 1, 0)
altitude = c(1000, 800, 1000, 1000, 1200, 5000, 5000, 10000, 10000)
name = c("p1","p2","p3","p4","p5","p6","p7","p8","p9")
x = cbind(lat, lon, extrude, altitude, name)
  
mykml$addPoint(x, styleUrl = "mystyle", description = "This is an example description applied to all points", altitudeMode = "relativeToGround")
#mykml$preview()
}

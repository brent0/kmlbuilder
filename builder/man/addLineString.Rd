\name{addLineString, addLineString.RkmlFolder}
\alias{addLineString}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add LineString features to a kml
}
\description{
After creating a kml object, you can add LineString features with this function so they are viewable in the graphics window of the google API. A dataframe x must be suplied to this function. At a minimum, x must contain columns pid, lat and lon. Each unique pid represents an individual LineString. To style these features, use addLineStyle and match styleid to styleUrl. 
}
\usage{
yourKMLobj$addLineString(x) 
yourKMLobj$addLineString(x, altitude, styleUrl)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{x}{ Mandatory. A dataframe with columns pid, lat and lon. You may also want to specify altitude for each lat/lon. There must be 2 or more records for each unique pid. x may also contain additional columns. All of the listed optional arguments can be supplied in the function call to apply to records of x, or can be a column of x(column name = argument name) with unique values at each unique pid to apply to that LineString only.  }

\item{altitude}{numeric with 1 or more elements cycled by pid. Meters above/below altiudeMode. 'clamp' altitudeModes ignore altitude. (Default: 0)}
\item{extrude}{boolean (0-not extrude or 1-extrude) with 1 or more elements cycled by pid. Choose weather to extrude(connect) the LineString to the ground. If you wish to extrude, altitudeMode must be one of relativeToGround, relativeToSeaFloor, or absolute. (Default: 1)}
\item{tessellate}{boolean (0-not tesselate or 1-tesselate) with 1 or more elements cycled by pid. Choose weather LineString will tesselate(follow terrain). If you wish to tesselate, altitudeMode must be one of clampToGround or clampToSeaFloor. (Default: 1)}
\item{drawOrder}{ numeric with 1 or more elements cycled by pid, set the order of the feature rendering. (Default:NULL) }
\item{altitudeMode}{character with 1 or more elements cycled by pid, one of "clampToGround", "relativeToGround", "absolute", "clampToSeaFloor", "relativeToSeaFloor". (Default:"clampToGround")} 
     \item{name}{character with 1 or more elements cycled by pid, what the feature will be called in the graphics environment. (Default:NULL) }
     \item{visibility}{boolean (0-invisible or 1-visible) with 1 or more elements cycled by pid. Specify if this feature will be visible. (Default: 1)}
     \item{open}{ boolean (0-closed or 1-open) with 1 or more elements cycled by pid. Specify if the feature is open or closed in the kml object tree. (Default: 0)}
     \item{atomauthor}{ character with 1 or more elements cycled by pid, text identifying an author relevant to the topic. See Ascription Elements under References (Default: NULL) }
     \item{atomlinkhref}{character with 1 or more elements cycled by pid, text identifying a web link relevant to the topic. See Ascription Elements under References (Default: NULL) }
     \item{address}{character with 1 or more elements cycled by pid, specify an unstructured address that is associated with the lineString. (Default:NULL) }
     \item{xalAddressDetails}{character with 1 or more elements cycled by pid, specify a structured address in eXtensible Address Language(See xal:AddressDetails under References) that is associated with the line. (Default:NULL) }
     \item{phoneNumber}{ character with 1 or more elements cycled by pid, represent a phone number, useful for mobile apps. (Default:NULL) }
     \item{Snippet}{character with 1 or more elements cycled by pid, supply details in addition to description. (Default:NULL)}
     \item{description}{character with 1 or more elements cycled by pid, supply details relevant to the feature. May contain CDATA. See NOTES section for more info. (Default:NULL)}
     \item{AbstractView}{character with 1 or more elements cycled by pid, the id of the desired AbstractView. Must create AbstractView with addAbstractView() function. (Default:NULL)}
     \item{TimeStamp}{ character with 1 or more elements cycled by pid, define a moment of date-time in one of the following formats: (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss). Used to create kml timeseries. (Default:NULL)}
     \item{TimeSpanStart}{ character with 1 or more elements cycled by pid, define the start of a span of date-time in one of the following formats(overides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss). Used to create timeseries. (Default:NULL)} 
     \item{TimeSpanEnd}{ character with 1 or more elements cycled by pid, define the end of a span of date-time in one of the following formats(overrides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss). Used to create timeseries. (Default:NULL)}  
     \item{styleUrl}{character with 1 or more elements cycled by pid, the id of the desired Style. Must create a style with one of the addStyle() or interactiveStyle() functions. (Default:NULL)}
     \item{Region}{Currently not supported}
     \item{ExtendedData}{Currently not supported}
     \item{inFolder}{Allows the adding of data to a specified folder, (EXPERIMENTAL) Usage: inFolder = 'this\\that' }   
You can add styles directly by defining any of the following variables. This is however discouraged as a new style is created for each row of x. These will be ignored if styleid is defined.   
\item{icon_color, icon_href, icon_transparency, icon_scale, icon_heading, icon_xunits, icon_x, icon_yunits, icon_y, icon_colorMode, bal_bgColor, bal_textColor, bal_text,  bal_displayMode, label_color, label_transparency, label_colorMode, label_scale, line_color, line_transparency, line_width, line_outerColor, line_outerTransparency, line_outerPortion, line_colorMode, line_labelVisibility	}{You can define these but it is POOR FORM!!! see icon, balloon, label, and line style documentation for values.   }

}

\references{
\href{https:\/\/developers.google.com/kml/documentation/kmlreference\#linestring}{KML lineString Reference} \cr
\href{https:\/\/developers.google.com/kml/documentation/kmlreference\#sampleattribution}{Ascription Elements}\cr 
\href{http:\/\/www.schemacentral.com/sc/kml22/e-xal_AddressDetails.html}{xal:AddressDetails Details}
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

pid = c(1, 1, 1, 2, 2, 6, 6, 6, 6)
lat = c(44, 44.1, 44, 44.2, 44.2, 44, 44.3, 44.3, 44)
lon = c(-60, -59.9, -59.8, -60, -59.8, -60.4, -60, -59.8, -59.4)
altitude = c(1000, 1000, 500, 500, 2000, 2000, 1500, 500, 2000)
x = cbind(pid, lat, lon, altitude)

mykml$addLineString(x, altitudeMode = "relativeToGround")

#Create styles
mykml$addLineStyle(styleid = "linestyle1", color = "red", transparency = .5, width = 100, outerColor = "orange", outerTransparency = 1, outerPortion = .5)
mykml$addLineStyle(styleid = "linestyle2", color = "yellow", transparency = 1, width = 50, outerColor = "green", outerTransparency = 1, outerPortion = .2)
mykml$addLineStyle(styleid = "linestyle3", color = "#0000ff", transparency = 1)

#Shift lats up to see style applied next to non style
lat = lat + .02

x = cbind(pid, lat,  lon)
altitude = c(1000, 500, 0)
style = c("linestyle1", "linestyle2", "linestyle3") #Cycles with pid
mykml$addLineString(x, styleUrl = style, altitude = altitude, altitudeMode = "relativeToGround")
#OR apply 1 altitude to each pid
#mykml$addLineString(x, styleUrl = style, altitude = 1000, altitudeMode = "relativeToGround")

##All optional arguments can be supplied in the function call and will cycle by pid
mykml$preview()
}

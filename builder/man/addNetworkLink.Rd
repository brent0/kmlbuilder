\name{addNetworkLink, addNetworkLink.RkmlFolder}
\alias{addNetworkLink}
\title{
Add NetworkLink
}
\description{
After creating a kml object, you can add a link to an external kml or kmz file. 
}
\usage{
yourKMLobj$addNetworkLink(href)
yourKMLobj$addNetworkLink(href, refreshVisibility, flyToView, refreshMode, refreshInterval, viewRefreshMode, viewRefreshTime, viewBoundScale, viewFormat, httpQuery, name, visibility, open, atomauthor, atomlinkhref, address, xalAddressDetails, phoneNumber, Snippet, description, AbstractView, TimeStamp, TimeSpanStart, TimeSpanEnd, styleUrl, Region, ExtendedData)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{href}{Mandatory. character, a local or network address that points to the kml or kmz file to link this kml.}

\item{refreshVisibility}{boolean (0-user controlled or 1-link controlled). Choose if you wish to control the visibility of features of a networklink or you will except the visibility of the link each time it is refreshed.}
\item{flToView}{boolean (0-user controlled or 1-link controlled). Choose if you wish to fly to the location of the lookAt elements of the networklink initially and on each refresh.}
\item{refreshMode}{character, one of "onChange, "onInterval"", or "onExpire".} 
\item{refreshInterval}{numeric. How many seconds untill next refresh. Only use when refreshMode = "onInterval".}
\item{viewRefreshMode}{character, one of "never"", "onStop"", "onRequest" or "onRegion". Specifies how the link is refreshed when the "camera" changes.} 
\item{viewRefreshTime}{numeric. How many seconds after stopping untill view is refreshed. Only use when viewRefreshMode = "onStop".}
\item{viewBoundScale}{numeric. How much of the current view will be fetched from the server onStop. <1- less than curent screen will be fetched, >1 more than current screen will be fetched. Scales the query (usually BBOX) }
\item{viewFormat}{character. Specifies the format of the query string that is appended to the Link's <href> before the file is fetched. See Notes. }
\item{httpQuery}{character. Additional fetch options added to the query. "[clientVersion]","[kmlVersion]","[clientName]" and "[language]" are supported. }

\item{name}{character, what the feature will be called in the graphics environment. (Default:NULL) }
     \item{visibility}{boolean (0-invisible or 1-visible). Specify if this feature will be visible. (Default: 1)} 
     \item{open}{ boolean (0-closed or 1-open). Specify if the feature is open or closed in the kml object tree. (Default: 0)}
     \item{atomauthor}{ character, text identifying an author relevant to the topic. See Ascription Elements under References (Default: NULL) }
     \item{atomlinkhref}{character, text identifying a web link relevant to the topic. See Ascription Elements under References (Default: NULL) }
     \item{address}{character, specify an unstructured address that is associated with the link. (Default:NULL) }
     \item{xalAddressDetails}{character, specify a structured address in eXtensible Address Language(See xal:AddressDetails under References) that is associated with the link. (Default:NULL) }
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


}

\references{
\href{https:\/\/developers.google.com/kml/documentation/kmlreference\#networklink}{KML networklink Reference}\cr
\href{https:\/\/developers.google.com/kml/documentation/kmlreference\#Link}{KML Link Reference}\cr
\href{http:\/\/geochalkboard.wordpress.com/2007/09/20/the-kml-behind-network-links/}{Network Link Additional Info}\cr
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
      
\strong{viewFormat}\cr
      \code{if viewRefreshMode of onStop, defaults to BBOX=[bboxWest],[bboxSouth],[bboxEast],[bboxNorth]}\cr
      
      You can also specify a custom set of viewing parameters. See \href{https:\/\/developers.google.com/kml/documentation/kmlreference\#Link}{KML Link Reference}
 
      
      
      
}



\examples{
### Simple example ###
mykml = RKmlObject()
mykml$addFolder("MyNetworkLinks", name = "NetworkLinks")

#Environment Canada GoeMet service
path2kmz = "www.ec.gc.ca/meteo-weather/C0D9B3D8-D256-407D-A68F-C606D703105E/GeoMet-E.kmz" 

mykml$getFolder("MyNetworkLinks")$addNetworkLink(href = path2kmz, name= "GeoMet")
mykml$preview()

### End Simple Example ###





}

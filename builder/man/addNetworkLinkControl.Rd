\name{addNetworkLinkControl, addNetworkLinkControl.RkmlFolder}
\alias{addNetworkLinkControl}
\title{
Add NetworkLink Control
}
\description{
Use this function to add control over how this kml will be fetched by other kml/kmz files. This is usually only needed if you plan on hosting this kml. Calling this function overwrites any previously created NetworkLink Controls
}
\usage{
yourKMLobj$addNetworkLinkControl() #EMPTY networkLinkControl
yourKMLobj$addNetworkLinkControl(minRefreshPeriod, maxSessionLength, cookie, message, linkName, linkDescription, linkSnippet, expires, update, AbstractView)
}

\arguments{

  \item{minRefreshPeriod}{numeric, seconds that need to pass before this kml can be fetched again. Helps throtle network.
  }
   
  \item{maxSessionLength}{numeric, seconds to keep the kml linked. A value of -1 means remain open indefinitly. (Default: -1)}
   \item{cookie}{character. text appended to the URL query on the next refresh of the network link. This is usefull whenlinking to cgi scripts that can use conditional file delivery.}
   \item{message}{character. If specified, a pop-up will apear when a client links to this kml with the contents of the text.}
   \item{linkName}{character. Define the name of the networkLink which will appear in the client's file tree.}
   \item{linkDescription}{character. Define a description for this link that will appear in the client's file tree.}
   \item{linkSnippet}{character. Define more information about this link in addition to linkDescription.}
  \item{expires}{character. date/time (formats: (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss) at which the link should be refreshed. This specification is used only when the the client kml has Link argument refreshMode = 'onExpire'. }
  \item{update}{character. xml text specifing a previously loaded kml and actions change, create or delete. The is for advanced users only. See KML networkLinkControll under References for a good description}
    \item{AbstractView}{character. Reference to the id of an Abstract view that has already been created or will soon be created. This is where the clients camera will go when this kml is linked.}
}

\references{
\href{https:\/\/developers.google.com/kml/documentation/kmlreference\#networklinkcontrol}{KML networkLinkControll Reference}
\href{http:\/\/geochalkboard.wordpress.com/2007/09/20/the-kml-behind-network-links/}{Network Link Aditional Info}
}
\author{
Brent Cameron\cr
Department of Fisheries and Oceans Canada\cr
Population Ecology Division
}


\examples{

## Changes view after 20 seconds

#Create the hosted kml
hostkml = RKmlObject()
#add an Astract view
hostkml$addAbstractView(type = "lookat", viewid = "sable_view", latitude = 43.9, longitude = -59.9, range = 100000)
hostkml$addNetworkLinkControl(minRefreshPeriod = 20, maxSessionLength = 60, message = "Welcome to the example Network Control kml. This kml will only stay linked for 60 seconds", linkName = "Sable Island", linkDescription = "You will soon fly to St.Paul's Island", linkSnippet = "buckle your seatbelt", AbstractView = "sable_view")
#hostkml$writekml("hostkml.kml")

#Create a client kml
clientkml = RKmlObject()

#kml client will attempt to refresh every second but will fail due to hosts minRefreshPeriod
clientkml$addNetworkLink(href = "hostkml.kml",flyToView = 1, refreshMode = "onInterval", refreshInterval = 1)
#clientkml$preview()

#Wait a few seconds so that google dosn't try to load this one first
Sys.sleep(10)

#Change the abstract view, should update in the preview of the clientkml after 20 seconds from the clientkml #linking to the host kml

hostkml$addAbstractView(type = "lookat", viewid = "stPaulsView", latitude = 47.2, longitude = -60.15, range = 10000)
hostkml$addNetworkLinkControl(minRefreshPeriod = 20, maxSessionLength = 60, message = "Welcome to the example Network Control kml. This kml will only stay linked for 60 seconds", linkName = "St.Paul's Island", linkDescription = "The link will expire in 40 seconds", linkSnippet = "watch your step", AbstractView = "stPaulsView")

#hostkml$writekml("hostkml.kml")


### End Example ###

}

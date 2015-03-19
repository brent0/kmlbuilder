\name{addAbstractView, addAbstractView.RkmlFolder}
\alias{addAbstractView}
\title{
Add Abstract view to KML Object
}
\description{
After creating a kml object, this method defines an Abstract view that can be used by many elements of a kml. When a using an AbstractView, be sure that the AbstractView value equals the desired viewid argument.
}
\usage{
yourKMLobj$addAbstractView(type = "camera", viewid, TimeStamp, ViewerOptions, longitude, latitude, altitude, heading, tilt, roll, altitudeMode) 
yourKMLobj$addAbstractView(type = "camera", viewid, TimeSpanStart, TimeSpanEnd, ViewerOptions, longitude, latitude, altitude, heading, tilt, roll, altitudeMode)

yourKMLobj$addAbstractView(type = "lookat", viewid, TimeStamp, ViewerOptions, longitude, latitude, altitude, heading, tilt, range, altitudeMode) 

yourKMLobj$addAbstractView(type = "lookat", viewid, TimeSpanStart, TimeSpanEnd, ViewerOptions, longitude, latitude, altitude, heading, tilt, range, altitudeMode) 
}

\arguments{

  \item{type}{
Mandatory. character. either 'lookat' or 'camera', the type of abstract view you would like to create.    
  }
  \item{viewid}{
Mandatory. Define the id for this styleview. kml elements will need to reference this id to use the defined view. 
  }
  \item{TimeStamp}{character. Define a moment in time(formats: (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss). This will affect the features that are displayed, sunlight and historical imagery.}
     \item{TimeSpanStart}{character. Define the start of a span of time(formats: (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss). This will affect the features that are displayed, sunlight and historical imagery.}
    \item{TimeSpanEnd}{character. Define the end of a span of time(formats: (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss). This will affect the features that are displayed, sunlight and historical imagery.}
     
    \item{ViewerOptions}{character vector containing any of 'sunlight', 'historicalimagery' and 'streetview'. By specifing any of these you enable the special viewing modes. }
    
   \item{longitude}{numeric. Define the longitude, for type of 'lookat' this is the longitude of the focal point, for type of 'camera' this is actual longitude of the camera position.}
   
   \item{latitude}{numeric. Define the latitude, for type of 'lookat' this is the latitude of the focal point, for type of 'camera' this is actual latitude of the camera position.}
   \item{altitude}{numeric. Define the altitude, for type of 'lookat' this is the altitude of the focal point, for type of 'camera' this is actual altitude of the camera position.}
    \item{altitudeMode}{character. One of "clampToGround", "relativeToGround", "absolute", "clampToSeaFloor", "relativeToSeaFloor", defining how the altitude of the view is intrepreted } 
    \item{heading}{numeric, 0 - 360. Define the direction that the view will be facing.}
      
 \item{tilt}{numeric, 0-180) for type of 'lookat' define the up/down angle to focal point, for type of 'camera' define the up/down angle of the camera. 0 is looking straight down, 90 looks at horizon, 180 is looking straight up into space.}
 
 \item{range}{numeric. Used only with type of 'lookat', Define how far from the focal point you would like the camera to belarge the label will appear. (Default: 1)}
\item{roll}{numeric, +180 to -180. Used only with type of 'camera', Define the camera left/right roll angle. }
 
}

\references{
\href{https:\/\/developers.google.com/kml/documentation/kmlreference\#abstractview}{KML abstractView Reference}\cr
\href{https:\/\/developers.google.com/kml/documentation/kmlreference\#camera}{KML camera Reference}\cr
\href{https:\/\/developers.google.com/kml/documentation/kmlreference\#lookat}{KML lookat Reference}

}
\author{
Brent Cameron\cr
Department of Fisheries and Oceans Canada\cr
Population Ecology Division
}
\note{
\strong{viewid}\cr
 Do not attempt to define more than one AbstractView for the same viewid.
 }


\examples{
#Create the kml object
twoViewskml = RKmlObject()
#add an Astract view of type lookat. look east, at 70 degrees 
#upward, 1000 meters away from the defined position(latitude, 
#longitude, altitude) 
twoViewskml$addAbstractView(type = "lookat", viewid = "sable_lookat", latitude = 43.94, longitude = -60.1, altitude = 1000, altitudeMode = "relativeToGround", heading = 90, tilt = 70, range = 1000)

#add an Astract view of type camera. position camera at defined position(latitude, 
#longitude, altitude) then point camera east look up 50 degrees and roll to the left slightly  
twoViewskml$addAbstractView(type = "camera", viewid = "sable_camera", latitude = 43.94, longitude = -60.1, altitude = 5000, altitudeMode = "relativeToGround", heading = 90,  tilt = 50, roll = 40)
#Add 2 folders each with one of the above abstractviews associated with it. Click on folder in tree to fly to view.
twoViewskml$addFolder("lookat", name = "lookat example", description = "Click on this folder to fly to the lookat example", AbstractView = "sable_lookat", open = 1)
twoViewskml$addFolder("camera", name = "camera example", description = "Click on this folder to fly to the camera example", AbstractView = "sable_camera", open = 1)
twoViewskml$preview()
}

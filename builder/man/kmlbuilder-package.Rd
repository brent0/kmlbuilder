
\name{kmlbuilder-package}
\alias{kmlbuilder-package}
\alias{kmlbuilder}
\docType{package}
\title{
Create Keyhole Markup Language from R
}
\description{
This package can be used to quickly create kml documents within R. You can easily create simple kml's with little or no styles, or, you can build much more complex kmls that have multiple features, folders and styles. This package supports most of the topics described in the kml reference document found here: https://developers.google.com/kml/documentation/kmlreference.   
}
\details{
\tabular{ll}{
Package: \tab kmlbuilder\cr
Type: \tab Package\cr
Version: \tab 1.1.4\cr
Date: \tab 2014-05-22\cr
License: \tab MIT\cr
}
Use this package if you would like to quickly generate kml documents from data stored in common R data structures
}
\author{
Brent Cameron\cr

Maintainer: brentcameron1@gmail.com

}
\references{
\href{http:\/\/www.google.com/earth/download/ge/agree.html}{Google Earth Download}
\href{https:\/\/developers.google.com/kml/documentation/kmlreference}{KML Reference}
}

\keyword{ package }

\examples{
###  SIMPLE EXAMPLE  ###
mykml = RKmlObject() #Create kml object
lat = lat = rep(seq(44, 45, .1), 11) #latitude 
lon = rep(seq(-60, -61, -.1), 11) #longitude
lon = lon[order(lon)]
mykml$addPoint(x = cbind(lat, lon)) #add point features to kml
mykml$preview() #sample the kml, used default styles

### END SIMPLE EXAMPLE  ###


### STYLE EXAMPLE  ###

#Create a RKmlObject
mykml = RKmlObject()

#Icon for use in styles
fishpath = "http://maps.google.com/mapfiles/ms/micons/fishing.png" 

#Create Styles
mykml$addLabelStyle(styleid = "my_style", color = "red", transparency = .5, scale = .5)
mykml$addIconStyle(styleid = "my_style", href = fishpath, color = "green", scale = 1, heading = 0)
mykml$addPolyStyle(styleid = "my_style", color = "yellow", transparency = .9, outline = 0, fill = 1)
mykml$addPolyStyle(styleid = "my_style2", color = "white", outline = 1, fill = 0)

#Create Folder Structure
mykml$addFolder(fid = "2013", name = "2013") #Demonstrate adding folder
mykml$getFolder("2013")$addFolder(fid = "survey_data", name = "survey data") #Demonstrate adding nested folder

# Assign Folder object to variable
sur2013 = mykml$getFolder("2013")$getFolder("survey_data")

lat = c(45.15, 44.95)
lon = c(-60.55, -60.15)
name = c("location1", "location2" )
TimeStamp = c("2013-09-20", "2013-10-22")

x = cbind(lat, lon, name, TimeStamp)

sur2013$addPoint(x, styleUrl = "my_style") #Demonstrate add data to folder

#one polygon inside another with different altitudes
pid = c(1, 1, 1, 1, 6, 6, 6, 6)
lat = c(45, 45.1, 45.1, 45, 45, 45.1, 45.1, 45)
lon = c(-60.5, -60.5, -60.2, -60.2, -60.5, -60.5, -60.2, -60.2)
x = cbind(pid, lat, lon)

#Define cut out polygon for polygon with pid = 1
pid = c(1, 1, 1)
lat = c(45.03, 45.06, 45.03)
lon = c(-60.3, -60.35, -60.4)
y = cbind(pid, lat, lon)

sty = c("my_style", "my_style2") #Cycles by pid
alt = c(7000, 10000) #Cycles by pid
mykml$addPolygon(x, y, styleUrl = sty, altitude = alt)

mykml$preview()

### END STYLE EXAMPLE ### 
}

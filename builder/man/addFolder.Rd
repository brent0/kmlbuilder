\name{addFolder, getFolder, removeFolder,  addFolder.RkmlFolder, getFolder.RkmlFolder, removeFolder.RkmlFolder}
\alias{addFolder, getFolder, removeFolder}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add Return and Remove Folder to and from KML file Structure
}
\description{
Manipulate the directory structure for your kml. Folders can be created within other folders. Folders must have unique fid within the same directory. 
}
\usage{
yourKMLobj$addFolder(fid, name, visibility, open, atomauthor, atomlinkhref, address, xalAddressDetails, phoneNumber, Snippet, description, AbstractView, TimeStamp, TimeSpanStart, TimeSpanEnd, styleUrl, Region, ExtendedData)
yourKMLobj$getFolder(fid)
yourKMLobj$removeFolder(fid)

}

\arguments{

  \item{fid}{
Mandatory. Define the id for this folder. The id will be used to specify where features are added in the file structure.
  }
  
     \item{name}{Character string or NULL. This is the text that will be displayed in the file tree. (Default = NULL) }
     \item{visibility}{Boolean (0-invisible or 1-visible). Specify if the features contained by this folder are visible. (Default = 1)}
     \item{open}{ Boolean (0-closed or 1-open). Specify if the folder is open or closed in the kml object tree. (Default = 0)}
     \item{atomauthor}{ Character string or NULL. Text identifying an author relevant to the topic. See Ascription Elements under References (Default = NULL) }
     \item{atomlinkhref}{Character string or NULL. Text identifying a web link relevant to the topic. See Ascription Elements under References (Default = NULL) }
     \item{address}{Character string or NULL. Specify an unstructured address that is associated with the folder. (Default = NULL) }
     \item{xalAddressDetails}{Character string or NULL. Specify a structured address in eXtensible Address Language(See xal:AddressDetails under References) that is associated with the folder. (Default = NULL) }
     \item{phoneNumber}{ Character string or NULL. Text identifying a phone number, useful for mobile apps. (Default = NULL) }
     \item{Snippet}{Character string or NULL. Supply details in addition to description. (Default = NULL)}
     \item{description}{Character string or NULL. Supply details, may contain CDATA. See NOTES section for more info. (Default = NULL)}
     \item{AbstractView}{Character string or NULL. The id of the desired AbstractView. Must create AbstractView with addAbstractView() function. (Default = NULL)}
     \item{TimeStamp}{ Character string of date-time in one of the following formats: (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss). Used to create kml timeseries. (Default = NULL)}
     \item{TimeSpanStart}{ Character string of date-time in one of the following formats(overrides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss). Used to create timeseries. (Default = NULL)} 
     \item{TimeSpanEnd}{ Character string of date-time in one of the following formats(overrides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss). Used to create timeseries. (Default = NULL)}  
     \item{styleUrl}{Character string or NULL. The id of the desired Style. Must create a style with one of the addStyle() or interactiveStyle() functions. (Default = NULL)}
     \item{Region}{Currently not supported}
     \item{ExtendedData}{Currently not supported}
     }
\references{
\href{https:\/\/developers.google.com/kml/documentation/kmlreference\#folder}{KML Folder Reference } \cr
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

mykml$addFolder(fid = "2013", name = "2013") #Demonstrate adding folder
mykml$getFolder("2013")$addFolder(fid = "survey_data", name = "survey data") #Demonstrate adding nested folder

##Demonstrate variable assignment of folder object##
sur2013 = mykml$getFolder("2013")$getFolder("survey_data") 
sur2013$addFolder(fid = "harbour")

mykml$getFolder("2013")$removeFolder("survey_data") #Demonstrate remove folder
mykml$getFolder("2013")$addFolder(fid = "survey_data", name = "survey data")
sur2013 = mykml$getFolder("2013")$getFolder("survey_data")
sur2013$addFolder(fid = "ports", name = "ports")

x = data.frame(cbind(c(45.9178, 46.807), c(-59.967, -60.321)))
names(x) = c("lat", "lon")

sur2013$getFolder("ports")$addPoint(x) #Demonstrate add data to folder

#mykml$preview()

}

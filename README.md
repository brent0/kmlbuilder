# kmlbuilder
Create kml, used in google earth, from R dataframes.

INSTALLATION INSTRUCTIONS

clone this repository

locate kmlbuilder_x.x.x.tar.gz. Should be in the projects root directory.

If you do not already have google earth desktop, download it at: Google Earth 

Desktop from http://www.google.com/earth/download/ge/agree.html

Open R Statistical Programming language: 

You may need to install dependencies first, just issue(copy paste)
the folowing commands to R, packages will be installed only if they
do not already exist:

if(!require("rgdal", character.only=T))install.packages("rgdal", dep = T)
if(!require("RCurl", character.only=T))install.packages("RCurl", dep = T)
if(!require("R.oo", character.only=T))install.packages("R.oo", dep = T)

then issue the command:

install.packages(path_to_kmlbuilder_x.x.x.tar.gz, repos = NULL, type="source")

Read documentation

Report any bugs at https://github.com/brent0/kmlbuilder

Contribute code

Contribute your projects to be used as examples, just add them to the projects folder. 

ENJOY

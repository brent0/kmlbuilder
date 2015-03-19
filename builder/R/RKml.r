library("R.oo")
library("rgdal")
library("RCurl")

setConstructorS3("RKmlFolder", function(id = "", parent = NULL) {
  R.oo::extend(Object(), "RKmlFolder",
               .id = id,
               .folders = list(),    #List to hold Folders objects
               .points = list(),     #List to hold points
               .foldertxt = "",
               .parent = parent
               
                           
  )
})


#Construct the Agent Object. The agent object is extended by most 
#other objects in this simulation. They will inherit the folllowing 
#variables and methods listed in this file. 
setConstructorS3("RKmlObject", function(id = "") {
  R.oo::extend(RKmlFolder(), "RKmlObject", 
               .id = id,
               .styles = list(),     #List to hold styles
               .Aviewlist = list(),   #List to hold Abstract views
               .networkcontroltxt = ""
               
  )
})
setMethodS3("styleBuilder", "RKmlFolder", function(this, ...) {
  print("Welcome to the interactive style builder. Plase answer the following questions. Some questions can be skipped by hitting the enter button, defaults will be assumed.")
  id = ""
  while(id == ""){
  id = trim(readline("Please type an id for this style: "))  
  if(id == "")print("You must type an id")
  }
  ida = id
  idh = ""
  idn = ""
    
  tmp = this
  while(!is.null(tmp$.parent)) tmp = tmp$.parent
  while(id %in% names(tmp$.styles)){
    print("This style already exists, please try again.")
    id = readline("Please type an id for this style: ") 
   
  }
  
  mo = toupper(readline("Would you like some features of this style to have mouse over effects, Y or N? "))
  if(mo == "Y") mo = T
  else mo = F
  
  if(mo){
    idn = readline("Please type an id for the non-mouseover style: ")  
    while(id == ""){
    while(idn %in% names(tmp$.styles)){
      print("This style already exists, please try again.")
      idn = readline("Please type an id for the non-mouseover style: ")  
    }
    if(id == "")print("You must type an id")
    }
  idh = readline("Please type an id for the mouseover style: ")
    while(id == ""){
      while(idh %in% names(tmp$.styles)){
        print("This style already exists, please try again.")
        idh = readline("Please type an id for the mouseover style: ")  
      }
      if(id == "")print("You must type an id")
    }

  }
  
style = Style
styleh = style
  
if(mo){
  ind = grep("Style id", style)
  style[ind] = gsub("..rep..", idn, style[ind])  
  ind = grep("Style id", styleh)
  styleh[ind] = gsub("..rep..", idh, styleh[ind])  
}
else{
  ind = grep("Style id", style)
  style[ind] = gsub("..rep..", ida, style[ind])     
}
 
  
if(mo){
  print("You will now create the non-mouseover style")
  id = idn
  end = 2
}
else{
  print("You will now create the style")
  id = ida 
  end = 1
}
   

n=0
while(n < end){
  if(n == 1){
     print("You will now create the mouseover style")
     id = idh
     style = styleh
  }

  again = T
  while(again){  
AA =  toupper(readline("Would you like to customize the balloon style, Y or N ? ")  )
ind = grep("BalloonStyle", style)

switch(AA, 
  Y={
    this$interactiveBalloonStyle(id)
    again = F
  },
  N={
    again = F
  },
  {
  print('You must type either Y or N')
  }
)
}
ind = grep("IconStyle", style)
  
  again = T
  while(again){  
AA = toupper(readline("Would you like to customize the icon style, Y or N ? ")  )
switch(AA, 
  Y={
  this$interactiveIconStyle(id)
  again = F
  },
  N={
  again = F
  },
  {
  print('You must type either Y or N')
  }
)
}
ind = grep("LabelStyle", style)
  
  again = T
  while(again){  
AA = toupper(readline("Would you like to customize the label style, Y or N ? ")  )
switch(AA, 
       Y={
         this$interactiveLabelStyle(id)
         again = F
       },
       N={
        again = F
       },
{
  print('You must type either Y or N')
}
)
}
ind = grep("LineStyle", style)
  
  again = T
  while(again){  
AA =  toupper(readline("Would you like to customize the line style, Y or N ? ")  )
switch(AA, 
       Y={
         this$interactiveLineStyle(id)
         again = F
       },
       N={
         again = F
       },
{
  print('You must type either Y or N')
}
)
}
ind = grep("PolyStyle", style)
  
  again = T
  while(again){  
AA =  toupper(readline("Would you like to customize Polygon style, Y or N ? ")  )
switch(AA, 
  Y={
    this$interactivePolyStyle(id)
    again = F
  },
  N={
    again = F
  },
  {
  print('You must type either Y or N')
  }
)
}
  n = n + 1
}
  if(mo){
    tmp$addStyleMap(id = ida, idn = idn, idh = idh )
  }
  
})
setMethodS3("interactiveLabelStyle", "RKmlFolder", function(this, id, ...) {
  
  ret = ""
  tmp = this
  
  while(!is.null(tmp$.parent)) tmp = tmp$.parent


      mc = readline("   What color would you like the label to be? ")
      mt = readline("   What transparency would you like the label to be (0.0 - 1.0)? ")
      ms = readline("   What scale would you like the label to be (default is 1.0)? ")
    
      colmod = readline("   Would you like to change the color mode to random(random based on color, use white for true random) (Y or N)? ")
      if(colmod == "Y") colmod = "random"
      else colmod = "normal"
          
      tmp$addLabelStyle(styleid = id, color = mc, transparency = mt, colorMode = colmod, scale = ms)
       

  })
setMethodS3("interactivePolyStyle", "RKmlFolder", function(this, id,  ...) {
  
  
  ret = ""
  tmp = this
  
  while(!is.null(tmp$.parent)) tmp = tmp$.parent
  
  
  mc = readline("   What color would you like the polygon to be? ")
  mt = readline("   What transparency would you like the polygon to be (0.0 - 1.0)? ")
  mf = readline("   Would you like to fill the polygon with the specified color (Y or N)? ")
  ml = readline("   Would you like the polygon to be outlined? (uses linestyle, Y or N)? ")
  
  colmod = readline("   Would you like to change the color mode to random(random based on color, use white for true random) (Y or N)? ")
  if(colmod == "Y") colmod = "random"
  else colmod = "normal"
  
  if(mf == "Y") mf = "1"
  else mf = "0"
  
  if(ml == "Y") ml = "1"
  else ml = "0"
  
  tmp$addPolyStyle(styleid = id, color = mc, transparency = mt, colorMode = colmod, fill = mf, outline = ml )
  

})
setMethodS3("interactiveBalloonStyle", "RKmlFolder", function(this, id,  ...) {
  
  ret = ""
  tmp = this
  
  while(!is.null(tmp$.parent)) tmp = tmp$.parent

  mx = readline("   Please type in the text you would like to be displayed in the balloon \n  (See developers.google.com/kml/documentation/kmlreference#balloonstyle\n  for custom text based on placemark variables): ")
  mc = readline("   What color would you like the pop-up's background to be? ")
  mt = readline("   What color would you like the pop-up's text to be?  ")
  ml = readline("   Would you like the balloon to be displayed when clicked (Y or N)? ")
    
  if(ml == "Y") ml = "display"
  else ml = "hide"
  
  tmp$addBalloonStyle(styleid = id, bgColor = mc, textColor = mt, text = mx, displayMode = ml)
  

 
})
setMethodS3("interactiveIconStyle", "RKmlFolder", function(this, id,  ...) {
    
  ret = ""
  tmp = this
  
  while(!is.null(tmp$.parent)) tmp = tmp$.parent
  mfx = ""
  mfxv = ""
  mfy = ""
  mfyv = ""
  mi = ""
  
  while(mi == ""){  
  mi = trim(readline("   Type the url or uri for the image (exa. www.myicons/icon1.png  or C://icons/icon1.png ): "))
  if(mi == "") print("You must enter a url or uri")
  }
  
  
  ms = readline("   What scale would you like the label to be (default is 1.0)? ")
  mh = readline("   Please type in a heading for the icon (0 - 360  default is 0): ")
  mf = readline("   Would you like to specify where the icon will be anchored in relation to the placemarks position (Y or N)? ")
 
  mc = readline("   Type a color to blend with the icon or hit enter for no blending: ")
  if(mc != ""){
  colmod = readline("   Would you like to change the color mode to random(random based on color, use white for true random) (Y or N)? ")
  if(colmod == "Y") colmod = "random"
  else colmod = "normal"
  }
  else colmod = NULL

  if(mf == "Y"){
    mfx = readline("      Type units for x values (pixels, fraction or insetpixels: ")
    if(mfx == "fraction") adder = "(0.0 - 1.0)"
    else adder = "(1 - image's x resolution)"
    mfxv = readline(paste("      Type x value",adder, ": ", sep = ""))
    
    mfy = readline("      Type units for y values (pixels, fraction or insetpixels: ")
    if(mfy == "fraction") adder = "(0.0 - 1.0)"
    else adder = "(1 - image's y resolution)"
    mfyv = readline(paste("      Type y value",adder, ": ", sep = ""))
  } 
  else{
    mfx = "fraction"
    mfxv = .5
    mfy = "fraction"
    mfyv = .5
  } 
  
  
  tmp$addIconStyle(styleid = id, href = mi, color = mc, scale = ms, heading = mh, xunits = mfx, yunits = mfy, x = mfxv, y = mfyv, colorMode = colmod)
  
  
})
setMethodS3("interactiveLineStyle", "RKmlFolder", function(this, id, ...) {
  
  
  ret = ""
  tmp = this
  
  while(!is.null(tmp$.parent)) tmp = tmp$.parent
  mc = NULL
  mt = NULL
  mw = NULL
  
  ow = NULL
  oc = NULL
  ot = NULL
  op = NULL
  
  mv = NULL
  
  ml = readline("   Would you like the line to have a single color line (S) or a line with an inner and an outer color (M)? (S or M)? ")
  if(ml == "S"){
    mc = readline("   What color would you like the line to be? ")
    mt = readline("   What transparency would you like the line to be (0.0 - 1.0)? ")
    mw = readline("   How wide(pixels) would you like the line to be (default is 1.0)? ")
    
  }
  else{
    mc = readline("   What color would you like the inner line to be? ")
    mt = readline("   What transparency would you like the inner line to be (0.0 - 1.0)? ")
    oc = readline("   What color would you like the outer ine to be? ")
    ot = readline("   What transparency would you like the outer line to be (0.0 - 1.0)? ")
    mw = readline("   How wide(meters) would you like the total width to be? ")
    op = readline("   What portion of the total width will be colored with the outer color (0.0 - 1.0)? ")
  }
  
  
  
  colmod = readline("   Would you like to change the color mode to random(random based on color, use white for true random) (Y or N)? ")
  if(colmod == "Y") colmod = "random"
  else colmod = "normal"
  
  lv = readline("   Would you like the line to be labeled with value of the name variable (Y or N)? ")
  if(lv == "Y") lv = "1"
  else lv = "0"
  
  
  tmp$addLineStyle(styleid = id, color = mc, transparency = mt, width = mw, outerColor = oc, outerTransparency = ot, outerPortion = op, colorMode = colmod, labelVisibility = lv)
  

})
setMethodS3("addIconStyle", "RKmlFolder", function(this, styleid = NULL, href = NULL, color = "", transparency = 1, scale = 1, heading = 0, xunits = "fraction", yunits = "fraction", x = .5, y = .5, colorMode = "normal", ...) {
  if(is.null(styleid)) throw("You must define the styleid argument.")
  lstyle = IconStyle

  color = color2kmlcolor(color = color, transparency = transparency)
  
    
  ind = grep("<color>", lstyle)
  if(!is.null(color)){
    lstyle[ind] = gsub("..rep..", color, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  

 
  ind = grep("colorMode", lstyle)
  if(!is.null(colorMode)){
    if((colorMode != "normal" & colorMode != "random")) throw("colorMode must be either 'normal' or 'random'")
    lstyle[ind] = gsub("..rep..", colorMode, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  ind = grep("hotSpot", lstyle)
  if(xunits == "fraction" & yunits == "fraction" & x == .5 &  y == .5){
    lstyle = lstyle[-ind]  
  }
  else{
    
    lstyle[ind] = gsub("..repxu..", xunits, lstyle[ind], fixed = T)
    lstyle[ind] = gsub("..repyu..", yunits, lstyle[ind], fixed = T)
    lstyle[ind] = gsub("..repx..", x, lstyle[ind], fixed = T)
    lstyle[ind] = gsub("..repy..", y, lstyle[ind], fixed = T)
  }

  
  ind = grep("href", lstyle)
  if(!is.null(href)){
    lstyle[ind] = gsub("..rep..", href, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  
  ind = grep("scale", lstyle)

  if(!is.null(scale)){
    lstyle[ind] = gsub("..rep..", scale, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  
  ind = grep("heading", lstyle)
  if(!is.null(heading)){
    lstyle[ind] = gsub("..rep..", heading, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  lstyle = paste(lstyle, collapse = "")
  
  tmp = this
  while(!is.null(tmp$.parent)) tmp = tmp$.parent
  if(styleid %in% names(tmp$.styles)){
    temp = tmp$.styles[[styleid]]
    if(TRUE %in% grepl("IconStyle", temp))throw("The style for this id already contains an IconStyle. Either try again with a new style id or remove current style with mykmlobj$removeStyle(styleid = 'yourid', styletype = 'IconStyle')")
    end = temp[length(temp)]
    temp[length(temp)] = lstyle
    temp[length(temp) + 1] = end
    style = temp
  }
  else{
    style = Style
    ind = grep("Style id", style)
    style[ind] = gsub("..rep..", styleid, style[ind])
    end = style[length(style)]
    style[length(style)] = lstyle
    style[length(style) + 1] = end
  }          
  tmp$.styles[[styleid]] = style 
  
})
setMethodS3("addPolyStyle", "RKmlFolder", function(this, styleid = NULL, color = "red", transparency = 1, colorMode = NULL, fill = "1", outline = "1", ...) {
  if(is.null(styleid)) throw("You must define the styleid argument.")
  lstyle = PolyStyle
  tra = as.hexmode(round(as.numeric(transparency)*255))
  if(color == "")color = NULL
  if(!is.null(color)){
    if(color %in% colors()){
      color = col2rgb(color)
      color = as.character(as.hexmode(color))
      color = paste(color[3], color[2], color[1], sep="")
    }
    else if(nrow(color)>2){
      color = as.character(as.hexmode(color))
      color = paste(color[3], color[2], color[1], sep="")
    } 
    
    color = paste("#", tra, color, sep="")
  }
  
  ind = grep("<color>", lstyle)
  if(!is.null(color)){
    lstyle[ind] = gsub("..rep..", color, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  
  ind = grep("colorMode", lstyle)
  if(!is.null(colorMode)){
    if((colorMode != "normal" & colorMode != "random")) throw("colorMode must be either 'normal' or 'random'")
    lstyle[ind] = gsub("..rep..", colorMode, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  ind = grep("fill", lstyle)
  if(!is.null(fill)){
    lstyle[ind] = gsub("..rep..", fill, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  ind = grep("outline", lstyle)
  if(!is.null(outline)){
    lstyle[ind] = gsub("..rep..", outline, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  lstyle = paste(lstyle, collapse = "")
  
  tmp = this
  while(!is.null(tmp$.parent)) tmp = tmp$.parent
  if(styleid %in% names(tmp$.styles)){
    temp = tmp$.styles[[styleid]]
    if(TRUE %in% grepl("PolyStyle", temp))throw("The style for this id already contains a PolyStyle. Either try again with a new styleid or remove current style with mykmlobj$removeStyle(styleid = 'yourid', styletype = 'PolyStyle')")
    end = temp[length(temp)]
    temp[length(temp)] = lstyle
    temp[length(temp) + 1] = end
    style = temp
  }
  else{
    style = Style
    ind = grep("Style id", style)
    style[ind] = gsub("..rep..", styleid, style[ind])
    end = style[length(style)]
    style[length(style)] = lstyle
    style[length(style) + 1] = end
  }          
  tmp$.styles[[styleid]] = style 

})
setMethodS3("addStyleMap", "RKmlFolder", function(this, id = NULL, idn = NULL, idh = NULL, ...) {
  tmp = this
  if(is.null(id))throw("You must supply an id argument")
  if(is.null(idn))throw("You must supply an idn argument")
  if(is.null(idh))throw("You must supply an idh argument")
  while(!is.null(tmp$.parent)) tmp = tmp$.parent
  stylemap = StyleMap
  ind = grep("StyleMap id", stylemap)
  stylemap[ind] = gsub("..rep..", id, stylemap[ind])
  ind = grep("normal", stylemap)
  stylemap[ind] = gsub("..rep..", idn, stylemap[ind])
  ind = grep("highlight", stylemap)
  stylemap[ind] = gsub("..rep..", idh, stylemap[ind])
  
  tmp$.styles[[id]] = stylemap
})
setMethodS3("addLabelStyle", "RKmlFolder", function(this, styleid = NULL, color = "red", transparency = 1, colorMode = "normal", scale = 1, ...) {
  if(is.null(styleid)) throw("You must define the styleid argument.")
  lstyle = LabelStyle

  color = color2kmlcolor(color = color, transparency = transparency)
    
  ind = grep("<color>", lstyle)
  if(!is.null(color)){
    lstyle[ind] = gsub("..rep..", color, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  
  ind = grep("colorMode", lstyle)
  if(!is.null(colorMode)){
    if((colorMode != "normal" & colorMode != "random")) throw("colorMode must be either 'normal' or 'random'")
    lstyle[ind] = gsub("..rep..", colorMode, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  
  ind = grep("scale", lstyle)
  if(!is.null(scale)){
    lstyle[ind] = gsub("..rep..", scale, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  lstyle = paste(lstyle, collapse = "")
  
  
  tmp = this
  while(!is.null(tmp$.parent)) tmp = tmp$.parent
  if(styleid %in% names(tmp$.styles)){
    temp = tmp$.styles[[styleid]]
    if(TRUE %in% grepl("LabelStyle", temp))throw("The style for this id already contains an IconStyle. Either try again with a new style id or remove current style with mykmlobj$removeStyle(styleid = 'yourid', styletype = 'LabelStyle')")
    end = temp[length(temp)]
    temp[length(temp)] = lstyle
    temp[length(temp) + 1] = end
    style = temp
  }
  else{
    style = Style
    ind = grep("Style id", style)
    style[ind] = gsub("..rep..", styleid, style[ind])
    end = style[length(style)]
    style[length(style)] = lstyle
    style[length(style) + 1] = end
  }          
  tmp$.styles[[styleid]] = style 
  
})
setMethodS3("addBalloonStyle", "RKmlFolder", function(this, styleid = NULL, bgColor = "white", textColor = "black", text = NULL, displayMode = "display", ...) {

  if(is.null(styleid)) throw("You must define the styleid argument.")
  lstyle =BalloonStyle

bgColor = color2kmlcolor(color = bgColor, transparency = 1)

  
  ind = grep("<bgColor>", lstyle)
  if(!is.null(bgColor)){
    lstyle[ind] = gsub("..rep..", bgColor, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
textColor = color2kmlcolor(color = textColor, transparency = 1)
  
  
  ind = grep("<textColor>", lstyle)
  if(!is.null(textColor)){
    lstyle[ind] = gsub("..rep..", textColor, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  ind = grep("<text>", lstyle)
  if(!is.null(text)){
    lstyle[ind] = gsub("..rep..", text, lstyle[ind])
  }
  else{
    text = "<![CDATA[
      <b><font size='+3'>$[name]</font></b>
        <br/><br/>
        <font face='Courier'>$[description]</font>
        <br/><br/>
        <!-- insert the to/from hyperlinks -->
        $[geDirections]
      ]]>"
    lstyle[ind] = gsub("..rep..", text, lstyle[ind])
  }
  
  
  ind = grep("displayMode", lstyle)
  if(!is.null(displayMode)){
    if((displayMode != "display" & displayMode != "hide")) throw("displayMode must be either 'display' or 'hide'")
    lstyle[ind] = gsub("..rep..", displayMode, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  lstyle = paste(lstyle, collapse = "")
  
  
  tmp = this
  while(!is.null(tmp$.parent)) tmp = tmp$.parent
  if(styleid %in% names(tmp$.styles)){
    temp = tmp$.styles[[styleid]]
    if(TRUE %in% grepl("BalloonStyle", temp))throw("The style for this id already contains an IconStyle. Either try again with a new style id or remove current style with mykmlobj$removeStyle(styleid = 'yourid', styletype = 'BalloonStyle')")
    end = temp[length(temp)]
    temp[length(temp)] = lstyle
    temp[length(temp) + 1] = end
    style = temp
  }
  else{
    style = Style
    ind = grep("Style id", style)
    style[ind] = gsub("..rep..", styleid, style[ind])
    end = style[length(style)]
    style[length(style)] = lstyle
    style[length(style) + 1] = end
  }          
  tmp$.styles[[styleid]] = style 
  
})
setMethodS3("addLineStyle", "RKmlFolder", function(this, styleid = NULL, color = "red", transparency = 1, width = 1, outerColor = NULL, outerTransparency = NULL, outerPortion = NULL, colorMode = NULL, labelVisibility = 0, ...) {
  if(is.null(styleid)) throw("You must define the styleid argument.")
  pwidth = NULL
  lstyle = LineStyle
  
  color = color2kmlcolor(color = color, transparency = transparency)
  ind = grep("<color>", lstyle)
  if(!is.null(color)){
    lstyle[ind] = gsub("..rep..", color, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  

  
  if(!is.null(outerColor) & !is.null(outerTransparency)){
    outerColor = color2kmlcolor(color = outerColor, transparency = outerTransparency)
    pwidth = width
    width = NULL
  }
  else{
  outerColor = NULL
  outerPortion = NULL
  pwidth = NULL    
  }
 
  
  ind = grep("outerColor", lstyle)
  if(!is.null(outerColor)){
    lstyle[ind] = gsub("..rep..", outerColor, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  ind = grep("outerWidth", lstyle)
  if(!is.null(outerPortion)){
    lstyle[ind] = gsub("..rep..", outerPortion, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  ind = grep("physicalWidth", lstyle)
  if(!is.null(pwidth)){
    lstyle[ind] = gsub("..rep..", pwidth, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  ind = grep("labelVisibility", lstyle)
  if(!is.null(labelVisibility)){
    lstyle[ind] = gsub("..rep..", labelVisibility, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  ind = grep("colorMode", lstyle)
  if(!is.null(colorMode)){
    if((colorMode != "normal" & colorMode != "random")) throw("colorMode must be either 'normal' or 'random'")
    lstyle[ind] = gsub("..rep..", colorMode, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  
  ind = grep("width", lstyle)
  if(!is.null(width)){
    lstyle[ind] = gsub("..rep..", width, lstyle[ind])
  }
  else lstyle = lstyle[-ind]

  
  lstyle = paste(lstyle, collapse = "")
  
  tmp = this
  while(!is.null(tmp$.parent)) tmp = tmp$.parent
  if(styleid %in% names(tmp$.styles)){
    temp = tmp$.styles[[styleid]]
    if(TRUE %in% grepl("LineStyle", temp))throw("The style for this id already contains an IconStyle. Either try again with a new style id or remove current style with mykmlobj$removeStyle(styleid = 'yourid', styletype = 'LineStyle')")
    end = temp[length(temp)]
    temp[length(temp)] = lstyle
    temp[length(temp) + 1] = end
    style = temp
  }
  else{
    style =Style
    ind = grep("Style id", style)
    style[ind] = gsub("..rep..", styleid, style[ind])
    end = style[length(style)]
    style[length(style)] = lstyle
    style[length(style) + 1] = end
  }          
  tmp$.styles[[styleid]] = style 
  
})
setMethodS3("addListStyle", "RKmlFolder", function(this, styleid = NULL, listItemType = "check", bgColor = "white", ...) {
  lstyle = ListStyle
  
  bgColor = color2kmlcolor(color = bgClor, transparency = 1)
  ind = grep("<bgColor>", lstyle)
  if(!is.null(bgColor)){
    lstyle[ind] = gsub("..rep..", bgColor, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
  
  ind = grep("<listItemType>", lstyle)
  if(!is.null(listItemType)){
    lstyle[ind] = gsub("..rep..", listItemType, lstyle[ind])
  }
  else lstyle = lstyle[-ind]
    
  lstyle = paste(lstyle, collapse = "")
  
  tmp = this
  while(!is.null(tmp$.parent)) tmp = tmp$.parent
  if(styleid %in% names(tmp$.styles)){
    temp = tmp$.styles[[styleid]]
    if(TRUE %in% grepl("ListStyle", temp))throw("The style for this id already contains an IconStyle. Either try again with a new style id or remove current style with mykmlobj$removeStyle(styleid = 'yourid', styletype = 'LineStyle')")
    end = temp[length(temp)]
    temp[length(temp)] = lstyle
    temp[length(temp) + 1] = end
    style = temp
  }
  else{
    style =Style
    ind = grep("Style id", style)
    style[ind] = gsub("..rep..", styleid, style[ind])
    end = style[length(style)]
    style[length(style)] = lstyle
    style[length(style) + 1] = end
  }          
  tmp$.styles[[styleid]] = style 
  
})
setMethodS3("addAbstractView", "RKmlFolder", function(this, viewid = NULL, type = "camera", ViewerOptions = NULL, longitude = NULL, latitude = NULL, altitude = NULL, heading = NULL, tilt = NULL, range = NULL, L, roll = NULL, TimeStamp = NULL, TimeSpanStart = NULL, TimeSpanEnd = NULL, altitudeMode = NULL, ...) {
  if(is.null(altitudeMode)){  
    if(!is.null(altitude)) altitudeMode = "relativeToGround"
    else altitude = 0
  }
  style = ""
  if(type == "camera")style = Camera
  else if(type == "lookat") style = LookAt
  else throw("You must choose either 'lookat' or 'camera' for the type")
  
  if(is.null(viewid)) throw("You must supply a viewid")
  
  ind = grep("ViewerOptions", style)
  if(!is.null(ViewerOptions)){
 
    reptxt = ""
    if("streetview" %in% ViewerOptions) reptxt = paste(reptxt, "<gx:option name= 'streetview' enabled = T />", sep ="")
    if("historicalimagery" %in% ViewerOptions) reptxt = paste(reptxt, "<gx:option name= 'historicalimagery' enabled = T />", sep="")
    if("sunlight" %in% ViewerOptions) reptxt = paste(reptxt, "<gx:option name= 'sunlight' enabled = T />", sep="")
    style[ind] = gsub("..rep..", reptxt, style[ind])
  }
  else style = style[-ind]
  
  ind = grep("longitude", style)
  if(!is.null(longitude)){
    style[ind] = gsub("..rep..", longitude, style[ind])
  }
  else style = style[-ind]
  
  ind = grep("latitude", style)
  if(!is.null(latitude)){
    style[ind] = gsub("..rep..", latitude, style[ind])
  }
  else style = style[-ind]  
  
  ind = grep("altitude", style)
  if(!is.null(altitude)){
    style[ind] = gsub("..rep..", altitude, style[ind])
  }
  else if(length(ind) >0)style = style[-ind]  
  
  ind = grep("heading", style)
  if(!is.null(heading)){
    style[ind] = gsub("..rep..", heading, style[ind])
  }
  else if(length(ind) >0)style = style[-ind]
  
  ind = grep("tilt", style)
  if(!is.null(tilt)){
    style[ind] = gsub("..rep..", tilt, style[ind])
  }
  else if(length(ind) >0)style = style[-ind]  
  


  ind = grep("range", style)
  if(!is.null(range)){
    style[ind] = gsub("..rep..", range, style[ind])
  }
  else if(length(ind) >0)style = style[-ind]
  


  ind = grep("roll", style)

  if(!is.null(roll)){
    style[ind] = gsub("..rep..", roll, style[ind])
  }
  else if(length(ind) >0)style = style[-ind]
  
  
  #TODO Add time format check!
  ind = grep("TimeStamp", style)
  if(!is.null(TimeStamp)) style[ind] = gsub("..rep..", TimeStamp, style[ind])
  else style = style[-ind]
  
  
  #TODO Add time format check!
  ind = grep("TimeSpan", style)
  if(!(is.null(TimeSpanStart) & is.null(TimeSpanEnd))){
    if(!is.null(TimeSpanStart)) style[ind] = gsub("..repa..", TimeStamp, style[ind])
    else gsub("<begin>..repa..</begin>", "", style[ind])
    if(!is.null(TimeSpanEnd)) style[ind] = gsub("..repb..", TimeStamp, style[ind])
    else gsub("<end>..repb..</end>", "", style[ind])
  }
  else style = style[-ind]
  
  
  if(!is.null(altitudeMode)){
    if(!altitudeMode %in% c("clampToGround", "relativeToGround", "absolute", "clampToSeaFloor", "relativeToSeaFloor"))
      throw("altitudeMode must be one of the following: clampToGround, relativeToGround, absolute, clampToSeaFloor, relativeToSeaFloor")
    ind = grep("altitudeMode", style)
    if(altitudeMode == "clampToSeaFloor" || altitudeMode == "relativeToSeaFloor"){
      style[ind] = gsub("altitudeMode", "gx:altitudeMode", style[ind])
    }
    style[ind] = gsub("clampToGround", altitudeMode, style[ind])
  }
  tmp = this
  while(!is.null(tmp$.parent)) tmp = tmp$.parent
  ind = grep("id=", style)
  style[ind] = gsub("..rep..", as.character(viewid), style[ind])
  tmp$.Aviewlist[[viewid]] = style 
  
  })
setMethodS3("addPoint", "RKmlFolder", function(this, x, ...) {
  args = list(...)

  tryCatch({
    x = data.frame(x)
  }, error = function() {
    print("Could not coerce x into dataframe.")
  })
  
  
  
  if(is.null(x$lat) || is.null(x$lon)) throw("Data Frame x must contain columns lat and lon")
  if(is.factor(x$lat) || is.factor(x$lon)){ x$lat = as.character(x$lat); x$lon = as.character(x$lon);}
  if(length(which(is.na(as.numeric(x$lat)))) > 0  || length(which(is.na(as.numeric(x$lon)))) > 0 ) 
    throw("There is a error in lat, lon data. Conversion to numeric failed. Make sure data is in decimal degrees without letters (exa. 45.66666667) ")
  if(length(which(as.numeric(x$lat) > 360 || as.numeric(x$lat) < -360 || as.numeric(x$lon) < -360 || as.numeric(x$lon) > 360)) > 0) 
    throw("There is a error in lat, lon data. Data out of range of expected values (-360, 360). Make sure data is in decimal degrees(exa. 45.66666667) ")
  x$lat = as.numeric(x$lat)
  x$lon = as.numeric(x$lon)
  tryCatch({
    x = data.frame(x)
  }, error = function() {
    print("Could not coerce x into dataframe.")
  })
  

  
  
  #Get Frame for kml point
  mpoints = point
 
  for(i in 1:nrow(x)){
  
  #Variable list
  lat = 0
  lon = 0
  altitude = NULL                    # meters above/below altiudeMode, clamp altitudeModes ignores altitude
  extrude = NULL                  # boolean (0 or 1) draw line from point to altitudeMode setting
  altitudeMode = NULL  # one of "clampToGround", "relativeToGround", "absolute", "clampToSeaFloor", "relativeToSeaFloor" 
  coordinates = NULL              # Taken from x, Must be in LL WGS84  <!-- lon,lat[,alt] -->
  name = NULL                     # string
  visibility = 1                  # boolean (0-invisible or 1-visible)
  open = 0                        # boolean (0-closed or 1-open  in kml object tree)
  atomauthor = NULL  		          # xmlns:atom 
  atomlinkhref = NULL             # xmlns:atom
  address = NULL                  # string
  xalAddressDetails = NULL        # xmlns:xal
  phoneNumber = NULL              # string
  Snippet = NULL                  # string with lines seperated by \n for nice format exa. "Hello World\nThis is my\nPlace 
  description = NULL              # string that may contain CDATA. See CDATA section for more info
  AbstractView = NULL             # string id (CameraID or LookAtID) must add with createCamera(id, ...) or createLookAt(id, ...)
  TimeStamp = NULL                # string of date-time in one of the following formats: (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)
  TimeSpanStart = NULL            # string of date-time in one of the following formats(overides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)
  TimeSpanEnd = NULL              # string of date-time in one of the following formats(overides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)  
  styleUrl = NULL                 # string id (StyleID) must add with createStyle(id, ...)
  id = NULL                       # Point ID
  Region = NULL                   # Currently not supported
  ExtendedData = NULL             # Currently not supported
  
  ##ADDED ABILITY 2015(EXPERIMENTAL) 
  #allow style referenes in dataframe. May remove as using this is bad xml practice 
  
  
  icon_color = NULL
  icon_href = NULL
  icon_transparency = NULL
  icon_scale = NULL
  icon_heading = NULL
  icon_xunits = NULL 
  icon_x = NULL
  icon_yunits = NULL	
  icon_y = NULL	
  icon_colorMode = NULL	
  
  bal_bgColor = NULL  
  bal_textColor = NULL	
  bal_text = NULL	
  bal_displayMode = NULL
  
  label_color = NULL  
  label_transparency = NULL	
  label_colorMode = NULL	
  label_scale = NULL	
  
  line_color = NULL  
  line_transparency = NULL	
  line_width = NULL	
  line_outerColor = NULL	
  line_outerTransparency = NULL	
  line_outerPortion = NULL	
  line_colorMode = NULL	
  line_labelVisibility = NULL	
  
  #allow folder references
  inFolder = NULL
  
  #Assign values to variables

 
    
    points = mpoints
      if(length(args)>0){
      for(j in 1:length(args)){
        assign(names(args[j]), args[[j]][1]) 
      }
    }
   
    for(j in 1:length(x)){
      assign(names(x[j]), x[[names(x)[j]]][i] )
    }
  
  
  if(is.null(altitudeMode)){  
  if(!is.null(altitude)) altitudeMode = "relativeToGround"
  else altitude = 0
  }
  if(!is.null(altitude)){
    if(is.na(as.numeric(altitude))) throw("altitude must be numeric or able to coerce to numeric ")
    altitude = as.numeric(altitude)
  }

  mxalt = F
  
  if(is.null(x$altitude)){
    
    if(!is.null(altitude)){
      
      if(length(altitude) > 1){
        if(i > length(altitude))altitude = altitude[i %% length(altitude)] 
        else altitude = altitude[i]
      }
      
      if(is.na(as.numeric(as.character(altitude)))) throw("altitude must be numeric or able to coerce to numeric ")
      altitude = as.numeric(as.character(altitude))
    }
  }
  else mxalt = T
  
  
  
  
  ind = grep("extrude", points)
  if(!is.null(extrude)){
    extrude = as.numeric(as.character(extrude))
    if(! (extrude==0 | extrude==1)) throw("extrude must be either 0 or 1 (boolean)")

    points[ind] = gsub("..rep..", extrude, points[ind])
  }
  else points = points[-ind]


  if(!is.null(altitudeMode)){
    if(!altitudeMode %in% c("clampToGround", "relativeToGround", "absolute", "clampToSeaFloor", "relativeToSeaFloor"))
      throw("altitudeMode must be one of the following: clampToGround, relativeToGround, absolute, clampToSeaFloor, relativeToSeaFloor")
    ind = grep("altitudeMode", points)
    if(altitudeMode == "clampToSeaFloor" || altitudeMode == "relativeToSeaFloor"){
      points[ind] = gsub("altitudeMode", "gx:altitudeMode", points[ind])
    }
      points[ind] = gsub("clampToGround", altitudeMode, points[ind])
  }

  ind = grep("<name>", points)
  if(!is.null(name)) points[ind] = gsub("..rep..", name, points[ind])
  else points = points[-ind]

  
    if(!is.null(visibility)){
      visibility = as.numeric(as.character(visibility))
      if(! (visibility==0 | visibility==1)) throw("visibility must be either 0 or 1 (boolean)")
      ind = grep("visibility", points)
      points[ind] = gsub("1", visibility, points[ind])
    }
  

    if(!is.null(open)){
      open = as.numeric(as.character(open))
      if(! (open==0 | open==1)) throw("open must be either 0 or 1 (boolean)")
      ind = grep("open", points)
      points[ind] = gsub("0", open, points[ind])
    }

    ind = grep("atom:author", points)
    if(!is.null(atomauthor)) points[ind] = gsub("..rep..", atomauthor, points[ind])
    else points = points[-ind]
   
    ind = grep("atom:link", points)
    if(!is.null(atomlinkhref)) points[ind] = gsub("..rep..", atomlinkhref, points[ind])
    else points = points[-ind]
    
    ind = grep("xal:AddressDetails", points)
    if(!is.null(xalAddressDetails)) points[ind] = gsub("..rep..", xalAddressDetails, points[ind])
    else points = points[-ind]

    ind = grep("address", points)
    if(!is.null(address)) points[ind] = gsub("..rep..", address, points[ind])
    else points = points[-ind]
    
    ind = grep("phoneNumber", points)
    if(!is.null(phoneNumber)) points[ind] = gsub("..rep..", phoneNumber, points[ind])
    else points = points[-ind]

   
     ind = grep("Snippet", points)
     if(!is.null(Snippet)){
      Snippet = as.character(Snippet)
      maxlines = length(unlist(strsplit(Snippet, "\n")))
      points[ind] = gsub("2", maxlines, points[ind])
      points[ind] = gsub("..rep..", Snippet, points[ind])
    }
    else points = points[-ind]
    

    ind = grep("description", points)
    if(!is.null(description)) points[ind] = gsub("..rep..", description, points[ind])
    else points = points[-ind]

 
    ind = grep("AbstractView", points)
    if(!is.null(AbstractView)){
      tmp = this
      while(!is.null(tmp$.parent)) tmp = tmp$.parent
      if(!AbstractView %in% names(tmp$.Aviewlist))warning(paste("No Abstractview id found for '", AbstractView, "'. You must create an abstractview with yourkmlobj$addAbstractView(id = '", AbstractView,"').", sep = ""))
      reptxt = tmp$.Aviewlist[[AbstractView]]
      points[ind] = paste(reptxt, collapse = "")
    }
    else points = points[-ind]

  
    #TODO Add time format check!
    ind = grep("TimeStamp", points)
    if(!is.null(TimeStamp)) points[ind] = gsub("..rep..", TimeStamp, points[ind])
    else points = points[-ind]

    
    #TODO Add time format check!
    ind = grep("TimeSpan", points)
    if(!(is.null(TimeSpanStart) & is.null(TimeSpanEnd))){
      if(!is.null(TimeSpanStart)) points[ind] = gsub("..repa..", TimeStamp, points[ind])
      else gsub("<begin>..repa..</begin>", "", points[ind])
      if(!is.null(TimeSpanEnd)) points[ind] = gsub("..repb..", TimeStamp, points[ind])
      else gsub("<end>..repb..</end>", "", points[ind])
    }
    else points = points[-ind]

###ADDED ABBILITY 2015. 
#some styles in dataframe 
is = F
bs = F
ls = F
lis = F

if(!(is.null(icon_color) & is.null(icon_href) & is.null(icon_transparency) & is.null(icon_scale) & 
    is.null(icon_heading) & is.null(icon_xunits) & is.null(icon_x) & is.null(icon_yunits) & is.null(icon_y) & is.null(icon_colorMode)))
  is = T
if(!(is.null(bal_bgColor) & is.null(bal_textColor) & is.null(bal_text) & is.null(bal_displayMode)))
  bs = T
if(!(is.null(label_color) & is.null(label_transparency) & is.null(label_colorMode) & is.null(label_scale)))
  ls = T
if(!(is.null(line_color) & is.null(line_transparency) & is.null(line_width) & is.null(line_outerColor) & 
       is.null(line_outerTransparency) & is.null(line_outerPortion) & is.null(line_colorMode) & is.null(line_labelVisibility)))
  lis = T

  
  if(is | ls | bs | lis){
         if(is.null(styleUrl)){
         
           tmp = this
           while(!is.null(tmp$.parent)) tmp = tmp$.parent
        
           sid = paste("unnamed_style_", length(tmp$.styles[which(grepl("unnamed_style", names(tmp$.styles)))]), sep = "")
           
           if(is){
           if(is.null(icon_color)) icon_color = ""
           if(is.null(icon_transparency)) icon_transparency = 1
           if(is.null(icon_scale))icon_scale = 1
           if(is.null(icon_heading))icon_heading = 0
           if(is.null(icon_xunits))icon_xunits = "fraction"
           if(is.null(icon_yunits))icon_yunits = "fraction"
           if(is.null(icon_x))icon_x = .5
           if(is.null(icon_y))icon_y = .5
           if(is.null(icon_colorMode))icon_colorMode = "normal"
           tmp$addIconStyle(styleid = sid, href = icon_href, color = icon_color, transparency = icon_transparency, scale = icon_scale, heading = icon_heading, xunits = icon_xunits, x = icon_x, yunits = icon_yunits, y = icon_y, colorMode = icon_colorMode )
           }
           if(bs){
           if(is.null(icon_bgColor))icon_bgColor = "white"
           if(is.null(icon_textColor))icon_textColor = "black"
           if(is.null(icon_displayMode))icon_displayMode = "display"
           tmp$addBalloonStyle(styleid = sid, bgColor = bal_bgColor, textColor = bal_textColor, text = bal_textColor, displayMode = bal_displayMode)   
           }
           if(lis){
           if(is.null(icon_color))icon_color = "red"
           if(is.null(icon_transparency))icon_transparency = 1
           if(is.null(icon_width))icon_width = 1
           if(is.null(icon_labelVisibility))icon_labelVisibility = 0
           tmp$addLineStyle(styleid = sid, color = line_color, transparency = line_transparency, width = line_width, outerColor = line_outerColor, outerTransparency = line_outerTransparency, outerPortion = line_outerPortion, colorMode = line_colorMode, labelVisibility = line_labelVisibility)
           }
           if(ls){
           if(is.null(icon_color))icon_color = "red"
           if(is.null(transparency))transparency = 1
           if(is.null(colorMode))colorMode = "normal"
           if(is.null(scale))scale = 1   
           tmp$addLabelStyle(styleid = sid, color = label_color, transparency = label_transparency, colorMode = label_colorMode, scale = label_scale)
           }
           
           styleUrl = sid
         }
    
  }
#define containing folder   

  if(!is.null(inFolder)){

      ssp = unlist(strsplit(as.character(inFolder), "/"))
      ftmp = this
      while(!is.null(ftmp$.parent)) ftmp = ftmp$.parent
      for(k in 1:length(ssp)){
        if(is.null(ftmp$getFolder(ssp[k])))
          ftmp$addFolder(ssp[k], name = ssp[k])
        ftmp = ftmp$getFolder(ssp[k])
      }
  
  }
    ind = grep("styleUrl", points)
    if(!is.null(styleUrl)){
      points[ind] = gsub("..rep..", styleUrl, points[ind])
      tmp = this
      while(!is.null(tmp$.parent)) tmp = tmp$.parent
      if(!styleUrl %in% names(tmp$.styles))warning(paste("No style id found for '", styleUrl, "'. You must create a style with yourkmlobj$createStyle(id = '", styleUrl,"').", sep = ""))
    }
    else points = points[-ind]

  
  if(mxalt) cord = paste(x$lon[i], x$lat[i], x$altitude[i], sep=",")
  else cord = paste(x$lon[i], x$lat[i], altitude, sep=",")  
  
    ind = grep("coordinates", points)
    points[ind] = gsub("..rep..", cord, points[ind])

    if(!is.null(id)){
      ind = grep("Placemark", points)
      points[ind] = gsub("ID", id, points[ind])
    }
    
    
    
    ind = grep("Region", points)
    if(!is.null(Region)) points[ind] = gsub("..rep..", Region, points[ind])
    else points = points[-ind]
    
    ind = grep("ExtendedData", points)
    if(!is.null(ExtendedData)) points[ind] = gsub("..rep..", ExtendedData, points[ind])
    else points = points[-ind]
    
    
    ind = grep("Placemark id", points)
    if(is.null(id)){
     
##ADDED 2015 condition
      if(is.null(inFolder)){
        points[ind] = gsub("..rep..", as.character(length(this$.points)+1), points[ind])
        this$.points[[as.character(length(this$.points)+1)]] = points
      }
      else{ 
        points[ind] = gsub("..rep..", as.character(length(ftmp$.points)+1), points[ind])
        ftmp$.points[[as.character(length(ftmp$.points)+1)]] = points
      }
    }
    else{
    
##ADDED 2015 condition
      if(is.null(inFolder)){
        points[ind] = gsub("..rep..", as.character(id), points[ind])    
        this$.points[[as.character(id)]] = points
      }
      else{
        points[ind] = gsub("..rep..", as.character(length(ftmp$.points)+1), points[ind])
        ftmp$.points[[as.character(id)]] = points
      }
    }

       
  }
   
})
setMethodS3("addPolygon", "RKmlFolder", function(this, x, y = NULL, ...) {
  args = list(...)
  
  tryCatch({
    x = data.frame(x)
  }, error = function() {
    print("Could not coerce x into dataframe.")
  })

  if(! is.null(y)){
  tryCatch({
    y = data.frame(y)
  }, error = function() {
    print("Could not coerce y into dataframe.")
  })
  }
  

  if(is.null(x$lat) || is.null(x$lon) || is.null(x$pid)) throw("Data Frame x must contain columns lat, lon and pid")
  if(is.factor(x$lat) || is.factor(x$lon) || is.factor(x$pid)){ x$lat = as.character(x$lat); x$lon = as.character(x$lon); x$pid = as.character(x$pid);}
  if(length(which(is.na(as.numeric(x$lat)))) > 0  || length(which(is.na(as.numeric(x$lon)))) > 0 ) 
    throw("There is a error in lat, lon data. Conversion to numeric failed. Make sure data is in decimal degrees without letters (exa. 45.66666667) ")
  if(length(which(is.na(as.numeric(x$pid)))) > 0 ) 
    throw("There is a error in pid data. Conversion to numeric failed. Make sure the data contains no missing values) ")
  if(length(which(as.numeric(x$lat) > 360 || as.numeric(x$lat) < -360 || as.numeric(x$lon) < -360 || as.numeric(x$lon) > 360)) > 0) 
    throw("There is a error in lat, lon data. Data out of range of expected values (-360, 360). Make sure data is in decimal degrees(exa. 45.66666667) ")
  x$lat = as.numeric(x$lat)
  x$lon = as.numeric(x$lon)
  x$pid = as.numeric(x$pid)
  
  
  lt3 = F
  p = split(x$pid, x$pid)
  for(i in 1:length(p)){
    if(length(p[[i]]) < 3){
      lt3 = T
    }
  }
  if(lt3) throw("There is an error in pid data. Each unique pid must have 3 or more data points) ")
 
  tryCatch({
    x = data.frame(x)
  }, error = function() {
    print("Could not coerce x into dataframe.")
  })

  ##Test if y data frame has been supplied. Y data fram defines inner polygons
  if(!is.null(y)){
    if(is.null(y$lat) || is.null(y$lon) || is.null(y$pid)) throw("Data Frame y must contain columns lat, lon and pid")
    if(is.factor(y$lat) || is.factor(y$lon) || is.factor(y$pid)){ y$lat = as.character(y$lat); y$lon = as.character(y$lon); y$lon = as.character(y$pid);}
    if(length(which(is.na(as.numeric(y$lat)))) > 0  || length(which(is.na(as.numeric(y$lon)))) > 0 ) 
      throw("There is a error in inner lat, lon data. Conversion to numeric failed. Make sure data is in decimal degrees without letters (exa. 45.66666667) ")
    if(length(which(is.na(as.numeric(y$pid)))) > 0 ) 
      throw("There is a error in inner pid data. Conversion to numeric failed. Make sure the data contains no missing values) ")
    if(length(which(as.numeric(y$lat) > 360 || as.numeric(y$lat) < -360 || as.numeric(y$lon) < -360 || as.numeric(y$lon) > 360)) > 0) 
      throw("There is a error in inner lat, lon data. Data out of range of expected values (-360, 360). Make sure data is in decimal degrees(exa. 45.66666667) ")
    y$lat = as.numeric(y$lat)
    y$lon = as.numeric(y$lon)
    y$pid = as.numeric(y$pid)
    
    lt3 = F
    q = split(y$pid, y$pid)
    for(i in 1:length(q)){
      if(length(q[[i]]) < 3){
        lt3 = T
      }
    }
    if(lt3) throw("There is an error in inner pid data. Each unique pid must have 3 or more data points) ")
    
    tryCatch({
      y = data.frame(y)
    }, error = function() {
      print("Could not coerce y into dataframe.")
    })
    
    
    
  }
  
  
  
  #Get Frame for kml point
  ppoints = polygon
  
  tx = split(x, x$pid)
  
  for(i in 1:length(tx)){
   points = ppoints
    mx = data.frame(tx[[i]])
   

  #Variable list
  lat = 0
  lon = 0
  altitude = NULL                    # meters above/below altiudeMode, clamp altitudeModes ignores altitude
  tessellate = 0
  extrude = 1                  # boolean (0 or 1) draw line from point to altitudeMode setting
  altitudeMode = NULL  # one of "clampToGround", "relativeToGround", "absolute", "clampToSeaFloor", "relativeToSeaFloor" 
  coordinates = NULL              # Taken from x, Must be in LL WGS84  <!-- lon,lat[,alt] -->
  name = NULL                     # string
  visibility = 1                  # boolean (0-invisible or 1-visible)
  open = 0                        # boolean (0-closed or 1-open  in kml object tree)
  atomauthor = NULL    	          # xmlns:atom 
  atomlinkhref = NULL             # xmlns:atom
  address = NULL                  # string
  xalAddressDetails = NULL        # xmlns:xal
  phoneNumber = NULL              # string
  Snippet = NULL                  # string with lines seperated by \n for nice format exa. "Hello World\nThis is my\nPlace 
  description = NULL              # string that may contain CDATA. See CDATA section for more info
  AbstractView = NULL             # string id (CameraID or LookAtID) must add with createCamera(id, ...) or createLookAt(id, ...)
  TimeStamp = NULL                # string of date-time in one of the following formats: (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)
  TimeSpanStart = NULL            # string of date-time in one of the following formats(overides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)
  TimeSpanEnd = NULL              # string of date-time in one of the following formats(overides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)  
  styleUrl = NULL                 # string id (StyleID) must add with createStyle(id, ...)
  id = NULL                       # Point ID
  Region = NULL                   # Currently not supported
  ExtendedData = NULL             # Currently not supported
  innercoordinates = NULL              # Taken from y, Must be in LL WGS84  <!-- lon,lat[,alt] -->
  
  
  ##ADDED ABILITY 2015(EXPERIMENTAL) 
  #allow style referenes in dataframe. May remove as using this is bad xml practice 
  icon_color = NULL
  icon_href = NULL
  icon_transparency = NULL
  icon_scale = NULL
  icon_heading = NULL
  icon_xunits = NULL 
  icon_x = NULL
  icon_yunits = NULL  
  icon_y = NULL	
  icon_colorMode = NULL	
  
  bal_bgColor = NULL  
  bal_textColor = NULL	
  bal_text = NULL	
  bal_displayMode = NULL
  
  label_color = NULL  
  label_transparency = NULL	
  label_colorMode = NULL	
  label_scale = NULL	
  
  line_color = NULL  
  line_transparency = NULL	
  line_width = NULL	
  line_outerColor = NULL	
  line_outerTransparency = NULL	
  line_outerPortion = NULL	
  line_colorMode = NULL	
  line_labelVisibility = NULL	
    
  #New 2015
  inFolder = NULL

  #Assign values to variables
  
    if(length(args)>0){
      for(j in 1:length(args)){
        assign(names(args[j]), args[[j]]) 
      }
    }
    
    for(j in 1:length(mx)){
      assign(names(mx[j]), mx[[names(mx)[j]]][i] )
    }
    
  
  if(is.null(altitudeMode)){  
    if(!is.null(altitude)) altitudeMode = "relativeToGround"
    else altitude = 0
  }
  

  
  mxalt = F
  
  if(is.null(mx$altitude)){
    if(!is.null(altitude)){
      if(length(altitude) > 1){
        if(i > length(altitude))altitude = altitude[i %% length(altitude)] 
        else altitude = altitude[i]
      }
      if(is.na(as.numeric(as.character(altitude)))) throw("altitude must be numeric or able to coerce to numeric ")
      altitude = as.numeric(as.character(altitude))
    }
  }
  else mxalt = T
  
  ind = grep("extrude", points)
  if(!is.null(extrude)){
    if(length(extrude) > 1){
      if(i > length(extrude))extrude = extrude[i %% length(extrude)] 
      else extrude = extrude[i]
    }
    extrude = as.numeric(as.character(extrude))
    if(! (extrude==0 | extrude==1)) throw("extrude must be either 0 or 1 (boolean)")
    points[ind] = gsub("..rep..", extrude, points[ind])
  }
  else points = points[-ind]
  
  ind = grep("tessellate", points)
  if(!is.null(tessellate)){
    if(length(tessellate) > 1){
      if(i > length(tessellate))tessellate = tessellate[i %% length(tessellate)] 
      else tessellate = tessellate[i]
    }
    tessellate = as.numeric(as.character(tessellate))
    if(! (tessellate==0 | tessellate==1)) throw("tessellate must be either 0 or 1 (boolean)")
    points[ind] = gsub("..rep..", tessellate, points[ind])
  }
  else points = points[-ind]  
  
  if(!is.null(altitudeMode)){
    if(length(altitudeMode) > 1){
      if(i > length(altitudeMode))altitudeMode = altitudeMode[i %% length(altitudeMode)] 
      else altitudeMode = altitudeMode[i]
    }
    if(!altitudeMode %in% c("clampToGround", "relativeToGround", "absolute", "clampToSeaFloor", "relativeToSeaFloor"))
      throw("altitudeMode must be one of the following: clampToGround, relativeToGround, absolute, clampToSeaFloor, relativeToSeaFloor")
    ind = grep("altitudeMode", points)
    if(altitudeMode == "clampToSeaFloor" || altitudeMode == "relativeToSeaFloor"){
      points[ind] = gsub("altitudeMode", "gx:altitudeMode", points[ind])
    }
    points[ind] = gsub("clampToGround", altitudeMode, points[ind])
  }
  
  ind = grep("<name>", points)
  if(!is.null(name)){
    if(length(name) > 1){
      if(i > length(name))name = name[i %% length(name)] 
      else name = name[i]
    }
    points[ind] = gsub("..rep..", name, points[ind])
  }
  else points = points[-ind]
    
  if(!is.null(visibility)){
    if(length(visibility) > 1){
      if(i > length(visibility))visibility = visibility[i %% length(visibility)] 
      else visibility = visibility[i]
    }
    visibility = as.numeric(as.character(visibility))
    if(! (visibility==0 | visibility==1)) throw("visibility must be either 0 or 1 (boolean)")
    
    ind = grep("visibility", points)
    points[ind] = gsub("0", visibility, points[ind])
  }
  if(!is.null(open)){
    if(length(open) > 1){
      if(i > length(open))open = open[i %% length(open)] 
      else open = open[i]
    }
    open = as.numeric(as.character(open))
    if(! (open==0 | open==1)) throw("open must be either 0 or 1 (boolean)")
    
    ind = grep("open", points)
    points[ind] = gsub("0", open, points[ind])
  }
  
  ind = grep("atom:author", points)
  if(!is.null(atomauthor)){
    if(length(atomauthor) > 1){
      if(i > length(atomauthor))atomauthor = atomauthor[i %% length(atomauthor)] 
      else atomauthor = atomauthor[i]
    }
    points[ind] = gsub("..rep..", atomauthor, points[ind])
  }
  else points = points[-ind]
  
  ind = grep("atom:link", points)
  if(!is.null(atomlinkhref)){
    if(length(atomlinkhref) > 1){
      if(i > length(atomlinkhref))atomlinkhref = atomlinkhref[i %% length(atomlinkhref)] 
      else atomlinkhref = atomlinkhref[i]
    }
    points[ind] = gsub("..rep..", atomlinkhref, points[ind])
    
  }
  else points = points[-ind]
  
  ind = grep("xal:AddressDetails", points)
  if(!is.null(xalAddressDetails)){
    if(length(xalAddressDetails) > 1){
      if(i > length(xalAddressDetails))xalAddressDetails = xalAddressDetails[i %% length(xalAddressDetails)] 
      else xalAddressDetails = xalAddressDetails[i]
    }
    points[ind] = gsub("..rep..", xalAddressDetails, points[ind])
  }
  else points = points[-ind]
  
  ind = grep("address", points)
  if(!is.null(address)){
    if(length(address) > 1){
      if(i > length(address))address = address[i %% length(address)] 
      else address = address[i]
    } 
    points[ind] = gsub("..rep..", address, points[ind])
  }
  else points = points[-ind]
  
  ind = grep("phoneNumber", points)
  if(!is.null(phoneNumber)){
    if(length(phoneNumber) > 1){
      if(i > length(phoneNumber))phoneNumber = phoneNumber[i %% length(phoneNumber)] 
      else phoneNumber = phoneNumber[i]
    } 
    points[ind] = gsub("..rep..", phoneNumber, points[ind])
  }
  else points = points[-ind]
  
  
  ind = grep("Snippet", points)
  if(!is.null(Snippet)){
    if(length(Snippet) > 1){
      if(i > length(Snippet))Snippet = Snippet[i %% length(Snippet)] 
      else Snippet = Snippet[i]
    } 
    Snippet = as.character(Snippet)
    maxlines = length(unlist(strsplit(Snippet, "\n")))
    points[ind] = gsub("2", maxlines, points[ind])
    points[ind] = gsub("..rep..", Snippet, points[ind])
  }
  else points = points[-ind]
  
  
  
  ind = grep("description", points)
  if(!is.null(description)){
    if(length(description) > 1){
      if(i > length(description))description = description[i %% length(description)] 
      else description = description[i]
    } 
    points[ind] = gsub("..rep..", description, points[ind])
  }
  else points = points[-ind]
  
  
  
  ind = grep("AbstractView", points)
  if(!is.null(AbstractView)){
    if(length(AbstractView) > 1){
      if(i > length(AbstractView))AbstractView = AbstractView[i %% length(AbstractView)] 
      else AbstractView = AbstractView[i]
    }

    tmp = this
    while(!is.null(tmp$.parent)) tmp = tmp$.parent
    if(!AbstractView %in% names(tmp$.Aviewlist))warning(paste("No Abstractview id found for '", AbstractView, "'. You must create an abstractview with yourkmlobj$addAbstractView(id = '", AbstractView,"').", sep = ""))
    reptxt = tmp$.Aviewlist[[AbstractView]]
    points[ind] = paste(reptxt, collapse = "")
}
  else points = points[-ind]
  
  
  #TODO Add time format check!
  ind = grep("TimeStamp", points)
  if(!is.null(TimeStamp)){
    if(length(TimeStamp) > 1){
      if(i > length(TimeStamp))TimeStamp = TimeStamp[i %% length(TimeStamp)] 
      else TimeStamp = TimeStamp[i]
    }
    points[ind] = gsub("..rep..", TimeStamp, points[ind])
  }
  else points = points[-ind]
  
  
  #TODO Add time format check!
  ind = grep("TimeSpan", points)
  if(!(is.null(TimeSpanStart) & is.null(TimeSpanEnd))){
    if(!is.null(TimeSpanStart)){
      if(length(TimeSpanStart) > 1){
        if(i > length(TimeSpanStart))TimeSpanStart = TimeSpanStart[i %% length(TimeSpanStart)] 
        else TimeSpanStart = TimeSpanStart[i]
      }
      points[ind] = gsub("..repa..", TimeSpanStart, points[ind])
    }
    else gsub("<begin>..repa..</begin>", "", points[ind])
    if(!is.null(TimeSpanEnd)){
      if(length(TimeSpanEnd) > 1){
        if(i > length(TimeSpanEnd))TimeSpanEnd = TimeSpanEnd[i %% length(TimeSpanEnd)] 
        else TimeSpanEnd = TimeSpanEnd[i]
      }
      points[ind] = gsub("..repb..", TimeSpanEnd, points[ind])
    }
    else gsub("<end>..repb..</end>", "", points[ind])
  }
  else points = points[-ind]
  
  
#New 2015. define containing folder   

if(!is.null(inFolder)){
  
  ssp = unlist(strsplit(as.character(inFolder), "/"))
  ftmp = this
  while(!is.null(ftmp$.parent)) ftmp = ftmp$.parent
  for(k in 1:length(ssp)){
    if(is.null(ftmp$getFolder(ssp[k])))
      ftmp$addFolder(ssp[k], name = ssp[k])
    ftmp = ftmp$getFolder(ssp[k])
  }
  
}
###ADDED ABBILITY 2015. 
#some styles in dataframe 
is = F
bs = F
ls = F
lis = F

if(!(is.null(icon_color) & is.null(icon_href) & is.null(icon_transparency) & is.null(icon_scale) & 
       is.null(icon_heading) & is.null(icon_xunits) & is.null(icon_x) & is.null(icon_yunits) & is.null(icon_y) & is.null(icon_colorMode)))
  is = T
if(!(is.null(bal_bgColor) & is.null(bal_textColor) & is.null(bal_text) & is.null(bal_displayMode)))
  bs = T
if(!(is.null(label_color) & is.null(label_transparency) & is.null(label_colorMode) & is.null(label_scale)))
  ls = T
if(!(is.null(line_color) & is.null(line_transparency) & is.null(line_width) & is.null(line_outerColor) & 
       is.null(line_outerTransparency) & is.null(line_outerPortion) & is.null(line_colorMode) & is.null(line_labelVisibility)))
  lis = T


if(is | ls | bs | lis){
  if(is.null(styleUrl)){
    
    tmp = this
    while(!is.null(tmp$.parent)) tmp = tmp$.parent
    
    sid = paste("unnamed_style_", length(tmp$.styles[which(grepl("unnamed_style", names(tmp$.styles)))]), sep = "")
    
    if(is){
      if(is.null(icon_color)) icon_color = ""
      if(is.null(icon_transparency)) icon_transparency = 1
      if(is.null(icon_scale))icon_scale = 1
      if(is.null(icon_heading))icon_heading = 0
      if(is.null(icon_xunits))icon_xunits = "fraction"
      if(is.null(icon_yunits))icon_yunits = "fraction"
      if(is.null(icon_x))icon_x = .5
      if(is.null(icon_y))icon_y = .5
      if(is.null(icon_colorMode))icon_colorMode = "normal"
      tmp$addIconStyle(styleid = sid, href = icon_href, color = icon_color, transparency = icon_transparency, scale = icon_scale, heading = icon_heading, xunits = icon_xunits, x = icon_x, yunits = icon_yunits, y = icon_y, colorMode = icon_colorMode )
    }
    if(bs){
      if(is.null(icon_bgColor))icon_bgColor = "white"
      if(is.null(icon_textColor))icon_textColor = "black"
      if(is.null(icon_displayMode))icon_displayMode = "display"
      tmp$addBalloonStyle(styleid = sid, bgColor = bal_bgColor, textColor = bal_textColor, text = bal_textColor, displayMode = bal_displayMode)   
    }
    if(lis){
      if(is.null(icon_color))icon_color = "red"
      if(is.null(icon_transparency))icon_transparency = 1
      if(is.null(icon_width))icon_width = 1
      if(is.null(icon_labelVisibility))icon_labelVisibility = 0
      tmp$addLineStyle(styleid = sid, color = line_color, transparency = line_transparency, width = line_width, outerColor = line_outerColor, outerTransparency = line_outerTransparency, outerPortion = line_outerPortion, colorMode = line_colorMode, labelVisibility = line_labelVisibility)
    }
    if(ls){
      if(is.null(icon_color))icon_color = "red"
      if(is.null(transparency))transparency = 1
      if(is.null(colorMode))colorMode = "normal"
      if(is.null(scale))scale = 1   
      tmp$addLabelStyle(styleid = sid, color = label_color, transparency = label_transparency, colorMode = label_colorMode, scale = label_scale)
    }
    
    styleUrl = sid
  }
  
}

  ind = grep("styleUrl", points)
  if(!is.null(styleUrl)){
    if(length(styleUrl) > 1){
      if(i > length(styleUrl))styleUrl = styleUrl[i %% length(styleUrl)] 
      else styleUrl = styleUrl[i]
    }
    points[ind] = gsub("..rep..", styleUrl, points[ind])
    tmp = this
    while(!is.null(tmp$.parent)) tmp = tmp$.parent
    if(!styleUrl %in% names(tmp$.styles))warning(paste("No style id found for '", styleUrl, "'. You must create a style with yourkmlobj$createStyle(id = '", styleUrl,"').", sep = ""))
  }
  else points = points[-ind]
   
   cord = ""

   for(k in  1:nrow(mx)){
     if(mxalt) cor = paste(mx$lon[k], mx$lat[k], mx$altitude[k], sep=",")
     else cor = paste(mx$lon[k], mx$lat[k], altitude, sep=",")
    cord = paste (cord, cor, sep = " ")
  }
if(mxalt) cor = paste(mx$lon[1], mx$lat[1], mx$altitude[1], sep=",")
else cor = paste(mx$lon[1], mx$lat[1], altitude, sep=",")

   cord = paste (cord, cor, sep = " ")
   
    ind = grep("outerBoundaryIs", points)
    points[ind] = gsub("..rep..", cord, points[ind])
    
   ind = grep("innerBoundaryIs", points)
   if(!is.null(y)){
     indic = which(y$pid == mx$pid[1])
     
     if(length(indic) > 2){
          ix = y[indic]
          incord = ""
            for(k in 1:nrow(ix)){
              if("altitude" %in% names(ix)) altitude = ix$altitude[k] 
              incor = paste(ix$lon[k], ix$lat[k], altitude, sep=",")
              incord = paste (incord, incor, sep = " ")
            }
          incor = paste(ix$lon[1], ix$lat[1], altitude, sep=",")
          incord = paste (incord, incor, sep = " ")
            points[ind] = gsub("..rep..", incord, points[ind])
          }
          else{
            points = points[-ind]
            if(length(indic > 0))
              warning("Encountered inner boundary with less than 3 data points, check y dataframe.")
          }
   }
   else points = points[-ind]

          
                  
          
    if(!is.null(id)){
      ind = grep("Placemark", points)
      points[ind] = gsub("ID", id, points[ind])
    }
    
    
  ind = grep("Region", points)
  if(!is.null(Region)){
    if(length(Region) > 1){
      if(i > length(Region))Region = Region[i %% length(Region)] 
      else Region = Region[i]
    }
    points[ind] = gsub("..rep..", Region, points[ind])
  }
  else points = points[-ind]
  
  ind = grep("ExtendedData", points)
  if(!is.null(ExtendedData)){
    if(length(ExtendedData) > 1){
      if(i > length(ExtendedData))ExtendedData = ExtendedData[i %% length(ExtendedData)] 
      else ExtendedData = ExtendedData[i]
    }
    points[ind] = gsub("..rep..", ExtendedData, points[ind])
  }
  else points = points[-ind]
    
    
    ind = grep("Placemark id", points)


if(is.null(id)){
  
  ##ADDED 2015 condition
  if(is.null(inFolder)){
    points[ind] = gsub("..rep..", as.character(length(this$.points)+1), points[ind])
    this$.points[[as.character(length(this$.points)+1)]] = points
  }
  else{ 
    points[ind] = gsub("..rep..", as.character(length(ftmp$.points)+1), points[ind])
    ftmp$.points[[as.character(length(ftmp$.points)+1)]] = points
  }
}
else{
  
  ##ADDED 2015 condition
  if(is.null(inFolder)){
    points[ind] = gsub("..rep..", as.character(id), points[ind])    
    this$.points[[as.character(id)]] = points
  }
  else{
    points[ind] = gsub("..rep..", as.character(length(ftmp$.points)+1), points[ind])
    ftmp$.points[[as.character(id)]] = points
  }
}
# 
# if(is.null(id)){
#       points[ind] = gsub("..rep..", as.character(length(this$.points)+1), points[ind])
#       this$.points[[as.character(length(this$.points)+1)]] = points
#     }
#     else{
#       points[ind] = gsub("..rep..", as.character(id), points[ind])
#       this$.points[[as.character(id)]] = points
#     }
#     
    
  }
  
})
setMethodS3("addLineString", "RKmlFolder", function(this, x, ...) {
  args = list(...)
 
  tryCatch({
    x = data.frame(x)
  }, error = function() {
    print("Could not coerce x into dataframe.")
  })
  
  
  
  if(is.null(x$lat) || is.null(x$lon) || is.null(x$pid)) throw("Data Frame x must contain columns lat, lon and pid")
  if(is.factor(x$lat) || is.factor(x$lon) || is.factor(x$pid)){ x$lat = as.character(x$lat); x$lon = as.character(x$lon); x$pid = as.character(x$pid);}
  if(length(which(is.na(as.numeric(x$lat)))) > 0  || length(which(is.na(as.numeric(x$lon)))) > 0 ) 
    throw("There is a error in lat, lon data. Conversion to numeric failed. Make sure data is in decimal degrees without letters (exa. 45.66666667) ")
  if(length(which(is.na(as.numeric(x$pid)))) > 0 ) 
    throw("There is a error in pid data. Conversion to numeric failed. Make sure the data contains no missing values) ")
  if(length(which(as.numeric(x$lat) > 360 || as.numeric(x$lat) < -360 || as.numeric(x$lon) < -360 || as.numeric(x$lon) > 360)) > 0) 
    throw("There is a error in lat, lon data. Data out of range of expected values (-360, 360). Make sure data is in decimal degrees(exa. 45.66666667) ")
  x$lat = as.numeric(x$lat)
  x$lon = as.numeric(x$lon)
  x$pid = as.numeric(x$pid)
  
  
  lt3 = F
  p = split(x$pid, x$pid)
  for(i in 1:length(p)){
    if(length(p[[i]]) < 2){
      lt3 = T
    }
  }
  if(lt3) throw("There is an error in pid data. Each unique pid must have 2 or more data points) ")
  
  tryCatch({
    x = data.frame(x)
  }, error = function() {
    print("Could not coerce x into dataframe.")
  })
  
  
  #Get Frame for kml point
  ppoints = linestring
  
  tx = split(x, x$pid)

  for(i in 1:length(tx)){
    points = ppoints
    mx = data.frame(tx[[i]])
    
    
    #Variable list
    lat = 0
    lon = 0
    altitude = NULL                    # meters above/below altiudeMode, clamp altitudeModes ignores altitude
    extrude = 1                  # boolean (0 or 1) draw line from point to altitudeMode setting
    tessellate = 1                
    drawOrder = NULL 
    altitudeMode = NULL  # one of "clampToGround", "relativeToGround", "absolute", "clampToSeaFloor", "relativeToSeaFloor" 
    coordinates = NULL              # Taken from x, Must be in LL WGS84  <!-- lon,lat[,alt] -->
    name = NULL                     # string
    visibility = 1                  # boolean (0-invisible or 1-visible)
    open = 0                        # boolean (0-closed or 1-open  in kml object tree)
    atomauthor = NULL                # xmlns:atom 
    atomlinkhref = NULL             # xmlns:atom
    address = NULL                  # string
    xalAddressDetails = NULL        # xmlns:xal
    phoneNumber = NULL              # string
    Snippet = NULL                  # string with lines seperated by \n for nice format exa. "Hello World\nThis is my\nPlace 
    description = NULL              # string that may contain CDATA. See CDATA section for more info
    AbstractView = NULL             # string id (CameraID or LookAtID) must add with createCamera(id, ...) or createLookAt(id, ...)
    TimeStamp = NULL                # string of date-time in one of the following formats: (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)
    TimeSpanStart = NULL            # string of date-time in one of the following formats(overides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)
    TimeSpanEnd = NULL              # string of date-time in one of the following formats(overides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)  
    styleUrl = NULL                 # string id (StyleID) must add with createStyle(id, ...)
    id = NULL                       # Point ID
    Region = NULL                   # Currently not supported
    ExtendedData = NULL             # Currently not supported
    
    ##ADDED ABILITY 2015(EXPERIMENTAL) 
    #allow style referenes in dataframe. May remove as using this is bad xml practice 
    
    
    icon_color = NULL
    icon_href = NULL
    icon_transparency = NULL
    icon_scale = NULL
    icon_heading = NULL
    icon_xunits = NULL 
    icon_x = NULL
    icon_yunits = NULL  
    icon_y = NULL	
    icon_colorMode = NULL	
    
    bal_bgColor = NULL  
    bal_textColor = NULL	
    bal_text = NULL	
    bal_displayMode = NULL
    
    label_color = NULL  
    label_transparency = NULL	
    label_colorMode = NULL	
    label_scale = NULL	
    
    line_color = NULL  
    line_transparency = NULL	
    line_width = NULL	
    line_outerColor = NULL	
    line_outerTransparency = NULL	
    line_outerPortion = NULL	
    line_colorMode = NULL	
    line_labelVisibility = NULL	
    
    
    #New 2015
    inFolder = NULL
    #Assign values to variables
    
    
    
    
    if(length(args)>0){
      for(j in 1:length(args)){
        assign(names(args[j]), args[[j]]) 
      }
    }
    
    for(j in 1:length(mx)){
      assign(names(mx[j]), mx[[names(mx)[j]]] )
    }

    
    if(is.null(altitudeMode)){  
      if(!is.null(altitude)) altitudeMode = "relativeToGround"
      else altitude = 0
    }
    
    
    mxalt = F
  
   if(is.null(mx$altitude)){

    if(!is.null(altitude)){
   
      if(length(altitude) > 1){
        if(i > length(altitude))altitude = altitude[i %% length(altitude)] 
        else altitude = altitude[i]
      }
      
      if(is.na(as.numeric(as.character(altitude)))) throw("altitude must be numeric or able to coerce to numeric ")
      altitude = as.numeric(as.character(altitude))
    }
   }
   else mxalt = T
    

    ind = grep("extrude", points)
    if(!is.null(extrude)){
      if(length(extrude) > 1){
        if(i > length(extrude))extrude = extrude[i %% length(extrude)] 
        else extrude = extrude[i]
      }
      extrude = as.numeric(as.character(extrude))
      if(! (extrude==0 | extrude==1)) throw("extrude must be either 0 or 1 (boolean)")
      points[ind] = gsub("..rep..", extrude, points[ind])
    }
    else points = points[-ind]
    
    
    ind = grep("tessellate", points)
    if(!is.null(tessellate)){
      if(length(tessellate) > 1){
        if(i > length(tessellate))tessellate = tessellate[i %% length(tessellate)] 
        else tessellate = tessellate[i]
      }
      tessellate = as.numeric(as.character(tessellate))
      if(! (tessellate==0 | tessellate==1)) throw("tessellate must be either 0 or 1 (boolean)")
      points[ind] = gsub("..rep..", tessellate, points[ind])
    }
    else points = points[-ind]
    
    ind = grep("drawOrder", points)
    if(!is.null(drawOrder)){
      if(length(drawOrder) > 1){
        if(i > length(drawOrder))drawOrder = drawOrder[i %% length(drawOrder)] 
        else drawOrder = drawOrder[i]
      }
      drawOrder = as.numeric(as.character(drawOrder))
      points[ind] = gsub("..rep..", drawOrder, points[ind])
    }
    else points = points[-ind]
    
    
    if(!is.null(altitudeMode)){
      if(length(altitudeMode) > 1){
        if(i > length(altitudeMode))altitudeMode = altitudeMode[i %% length(altitudeMode)] 
        else altitudeMode = altitudeMode[i]
      }
      if(!altitudeMode %in% c("clampToGround", "relativeToGround", "absolute", "clampToSeaFloor", "relativeToSeaFloor"))
        throw("altitudeMode must be one of the following: clampToGround, relativeToGround, absolute, clampToSeaFloor, relativeToSeaFloor")
      ind = grep("altitudeMode", points)
      if(altitudeMode == "clampToSeaFloor" || altitudeMode == "relativeToSeaFloor"){
        points[ind] = gsub("altitudeMode", "gx:altitudeMode", points[ind])
      }
      points[ind] = gsub("clampToGround", altitudeMode, points[ind])
    }
    
    ind = grep("<name>", points)
    if(!is.null(name)){
      if(length(name) > 1){
        if(i > length(name))name = name[i %% length(name)] 
        else name = name[i]
      }
      points[ind] = gsub("..rep..", name, points[ind])
    }
    else points = points[-ind]
    
    if(!is.null(visibility)){
      if(length(visibility) > 1){
        if(i > length(visibility))visibility = visibility[i %% length(visibility)] 
        else visibility = visibility[i]
      }
      visibility = as.numeric(as.character(visibility))
      if(! (visibility==0 | visibility==1)) throw("visibility must be either 0 or 1 (boolean)")
     
      ind = grep("visibility", points)
      points[ind] = gsub("0", visibility, points[ind])
    }
    if(!is.null(open)){
      if(length(open) > 1){
        if(i > length(open))open = open[i %% length(open)] 
        else open = open[i]
      }
      open = as.numeric(as.character(open))
      if(! (open==0 | open==1)) throw("open must be either 0 or 1 (boolean)")
      
      ind = grep("open", points)
      points[ind] = gsub("0", open, points[ind])
    }
    
    ind = grep("atom:author", points)
    if(!is.null(atomauthor)){
      if(length(atomauthor) > 1){
        if(i > length(atomauthor))atomauthor = atomauthor[i %% length(atomauthor)] 
        else atomauthor = atomauthor[i]
      }
      points[ind] = gsub("..rep..", atomauthor, points[ind])
    }
    else points = points[-ind]
    
    ind = grep("atom:link", points)
    if(!is.null(atomlinkhref)){
      if(length(atomlinkhref) > 1){
        if(i > length(atomlinkhref))atomlinkhref = atomlinkhref[i %% length(atomlinkhref)] 
        else atomlinkhref = atomlinkhref[i]
      }
      points[ind] = gsub("..rep..", atomlinkhref, points[ind])
    
    }
      else points = points[-ind]
    
    ind = grep("xal:AddressDetails", points)
    if(!is.null(xalAddressDetails)){
      if(length(xalAddressDetails) > 1){
        if(i > length(xalAddressDetails))xalAddressDetails = xalAddressDetails[i %% length(xalAddressDetails)] 
        else xalAddressDetails = xalAddressDetails[i]
      }
      points[ind] = gsub("..rep..", xalAddressDetails, points[ind])
    }
      else points = points[-ind]
    
    ind = grep("address", points)
    if(!is.null(address)){
      if(length(address) > 1){
        if(i > length(address))address = address[i %% length(address)] 
        else address = address[i]
      } 
      points[ind] = gsub("..rep..", address, points[ind])
    }
      else points = points[-ind]
    
    ind = grep("phoneNumber", points)
    if(!is.null(phoneNumber)){
      if(length(phoneNumber) > 1){
        if(i > length(phoneNumber))phoneNumber = phoneNumber[i %% length(phoneNumber)] 
        else phoneNumber = phoneNumber[i]
      } 
      points[ind] = gsub("..rep..", phoneNumber, points[ind])
    }
      else points = points[-ind]
    
    
    ind = grep("Snippet", points)
    if(!is.null(Snippet)){
      if(length(Snippet) > 1){
        if(i > length(Snippet))Snippet = Snippet[i %% length(Snippet)] 
        else Snippet = Snippet[i]
      } 
      Snippet = as.character(Snippet)
      maxlines = length(unlist(strsplit(Snippet, "\n")))
      points[ind] = gsub("2", maxlines, points[ind])
      points[ind] = gsub("..rep..", Snippet, points[ind])
    }
    else points = points[-ind]
    
    
    ind = grep("description", points)
    if(!is.null(description)){
      if(length(description) > 1){
        if(i > length(description))description = description[i %% length(description)] 
        else description = description[i]
      } 
      points[ind] = gsub("..rep..", description, points[ind])
    }
      else points = points[-ind]
    
   
   ind = grep("AbstractView", points)
   if(!is.null(AbstractView)){
     if(length(AbstractView) > 1){
       if(i > length(AbstractView))AbstractView = AbstractView[i %% length(AbstractView)] 
       else AbstractView = AbstractView[i]
     }
     
     tmp = this
     while(!is.null(tmp$.parent)) tmp = tmp$.parent
     if(!AbstractView %in% names(tmp$.Aviewlist))warning(paste("No Abstractview id found for '", AbstractView, "'. You must create an abstractview with yourkmlobj$addAbstractView(id = '", AbstractView,"').", sep = ""))
     reptxt = tmp$.Aviewlist[[AbstractView]]
     points[ind] = paste(reptxt, collapse = "")
   }
   else points = points[-ind]
   
    
    
    #TODO Add time format check!
    ind = grep("TimeStamp", points)
    if(!is.null(TimeStamp)){
      if(length(TimeStamp) > 1){
        if(i > length(TimeStamp))TimeStamp = TimeStamp[i %% length(TimeStamp)] 
        else TimeStamp = TimeStamp[i]
      }
      points[ind] = gsub("..rep..", TimeStamp, points[ind])
    }
      else points = points[-ind]
    
    
    #TODO Add time format check!
    ind = grep("TimeSpan", points)
    if(!(is.null(TimeSpanStart) & is.null(TimeSpanEnd))){
      if(!is.null(TimeSpanStart)){
        if(length(TimeSpanStart) > 1){
          if(i > length(TimeSpanStart))TimeSpanStart = TimeSpanStart[i %% length(TimeSpanStart)] 
          else TimeSpanStart = TimeSpanStart[i]
        }
        points[ind] = gsub("..repa..", TimeSpanStart, points[ind])
      }
      else gsub("<begin>..repa..</begin>", "", points[ind])
      if(!is.null(TimeSpanEnd)){
        if(length(TimeSpanEnd) > 1){
          if(i > length(TimeSpanEnd))TimeSpanEnd = TimeSpanEnd[i %% length(TimeSpanEnd)] 
          else TimeSpanEnd = TimeSpanEnd[i]
        }
        points[ind] = gsub("..repb..", TimeSpanEnd, points[ind])
      }
        else gsub("<end>..repb..</end>", "", points[ind])
    }
    else points = points[-ind]
    
   ###ADDED ABBILITY 2015. 
   #some styles in dataframe 
   is = F
   bs = F
   ls = F
   lis = F
   
   if(!(is.null(icon_color) & is.null(icon_href) & is.null(icon_transparency) & is.null(icon_scale) & 
          is.null(icon_heading) & is.null(icon_xunits) & is.null(icon_x) & is.null(icon_yunits) & is.null(icon_y) & is.null(icon_colorMode)))
     is = T
   if(!(is.null(bal_bgColor) & is.null(bal_textColor) & is.null(bal_text) & is.null(bal_displayMode)))
     bs = T
   if(!(is.null(label_color) & is.null(label_transparency) & is.null(label_colorMode) & is.null(label_scale)))
     ls = T
   if(!(is.null(line_color) & is.null(line_transparency) & is.null(line_width) & is.null(line_outerColor) & 
          is.null(line_outerTransparency) & is.null(line_outerPortion) & is.null(line_colorMode) & is.null(line_labelVisibility)))
     lis = T
   
   
   if(is | ls | bs | lis){
     if(is.null(styleUrl)){
       
       tmp = this
       while(!is.null(tmp$.parent)) tmp = tmp$.parent
       
       sid = paste("unnamed_style_", length(tmp$.styles[which(grepl("unnamed_style", names(tmp$.styles)))]), sep = "")
       
       if(is){
         if(is.null(icon_color)) icon_color = ""
         if(is.null(icon_transparency)) icon_transparency = 1
         if(is.null(icon_scale))icon_scale = 1
         if(is.null(icon_heading))icon_heading = 0
         if(is.null(icon_xunits))icon_xunits = "fraction"
         if(is.null(icon_yunits))icon_yunits = "fraction"
         if(is.null(icon_x))icon_x = .5
         if(is.null(icon_y))icon_y = .5
         if(is.null(icon_colorMode))icon_colorMode = "normal"
         tmp$addIconStyle(styleid = sid, href = icon_href, color = icon_color, transparency = icon_transparency, scale = icon_scale, heading = icon_heading, xunits = icon_xunits, x = icon_x, yunits = icon_yunits, y = icon_y, colorMode = icon_colorMode )
       }
       if(bs){
         if(is.null(icon_bgColor))icon_bgColor = "white"
         if(is.null(icon_textColor))icon_textColor = "black"
         if(is.null(icon_displayMode))icon_displayMode = "display"
         tmp$addBalloonStyle(styleid = sid, bgColor = bal_bgColor, textColor = bal_textColor, text = bal_textColor, displayMode = bal_displayMode)   
       }
       if(lis){
         if(is.null(icon_color))icon_color = "red"
         if(is.null(icon_transparency))icon_transparency = 1
         if(is.null(icon_width))icon_width = 1
         if(is.null(icon_labelVisibility))icon_labelVisibility = 0
         tmp$addLineStyle(styleid = sid, color = line_color, transparency = line_transparency, width = line_width, outerColor = line_outerColor, outerTransparency = line_outerTransparency, outerPortion = line_outerPortion, colorMode = line_colorMode, labelVisibility = line_labelVisibility)
       }
       if(ls){
         if(is.null(icon_color))icon_color = "red"
         if(is.null(transparency))transparency = 1
         if(is.null(colorMode))colorMode = "normal"
         if(is.null(scale))scale = 1   
         tmp$addLabelStyle(styleid = sid, color = label_color, transparency = label_transparency, colorMode = label_colorMode, scale = label_scale)
       }
       
       styleUrl = sid
     }
     
   }
   
   
    
    ind = grep("styleUrl", points)
    if(!is.null(styleUrl)){
      if(length(styleUrl) > 1){
        if(i > length(styleUrl))styleUrl = styleUrl[i %% length(styleUrl)] 
        else styleUrl = styleUrl[i]
      }
      points[ind] = gsub("..rep..", styleUrl, points[ind])
      tmp = this
      while(!is.null(tmp$.parent)) tmp = tmp$.parent
      if(!styleUrl %in% names(tmp$.styles))warning(paste("No style id found for '", styleUrl, "'. You must create a style with yourkmlobj$createStyle(id = '", styleUrl,"').", sep = ""))
    }
    else points = points[-ind]

    
    cord = ""
    
    for(k in  1:nrow(mx)){
   
      
      if(mxalt) cor = paste(mx$lon[k], mx$lat[k], mx$altitude[k], sep=",")
      else cor = paste(mx$lon[k], mx$lat[k], altitude, sep=",")

      cord = paste (cord, cor, sep = " ")
    }

    
    ind = grep("coordinates", points)
    points[ind] = gsub("..rep..",cord, points[ind])

    
    
    
    if(!is.null(id)){
      ind = grep("Placemark", points)
      points[ind] = gsub("ID", id, points[ind])
    }
    
    
    
    ind = grep("Region", points)
    if(!is.null(Region)){
      if(length(Region) > 1){
        if(i > length(Region))Region = Region[i %% length(Region)] 
        else Region = Region[i]
      }
      points[ind] = gsub("..rep..", Region, points[ind])
    }
      else points = points[-ind]

    ind = grep("ExtendedData", points)
    if(!is.null(ExtendedData)){
      if(length(ExtendedData) > 1){
        if(i > length(ExtendedData))ExtendedData = ExtendedData[i %% length(ExtendedData)] 
        else ExtendedData = ExtendedData[i]
      }
      points[ind] = gsub("..rep..", ExtendedData, points[ind])
    }
      else points = points[-ind]
    
    
   #New 2015. define containing folder   
   
   if(!is.null(inFolder)){
     
     ssp = unlist(strsplit(as.character(inFolder), "/"))
     ftmp = this
     while(!is.null(ftmp$.parent)) ftmp = ftmp$.parent
     for(k in 1:length(ssp)){
       if(is.null(ftmp$getFolder(ssp[k])))
         ftmp$addFolder(ssp[k], name = ssp[k])
       ftmp = ftmp$getFolder(ssp[k])
     }
     
   }
   
    ind = grep("Placemark id", points)
   
   
   if(is.null(id)){
     
     ##ADDED 2015 condition
     if(is.null(inFolder)){
       points[ind] = gsub("..rep..", as.character(length(this$.points)+1), points[ind])
       this$.points[[as.character(length(this$.points)+1)]] = points
     }
     else{ 
       points[ind] = gsub("..rep..", as.character(length(ftmp$.points)+1), points[ind])
       ftmp$.points[[as.character(length(ftmp$.points)+1)]] = points
     }
   }
   else{
     
     ##ADDED 2015 condition
     if(is.null(inFolder)){
       points[ind] = gsub("..rep..", as.character(id), points[ind])    
       this$.points[[as.character(id)]] = points
     }
     else{
       points[ind] = gsub("..rep..", as.character(length(ftmp$.points)+1), points[ind])
       ftmp$.points[[as.character(id)]] = points
     }
   }
   
#     if(is.null(id)){
#       points[ind] = gsub("..rep..", as.character(length(this$.points)+1), points[ind])
#       this$.points[[as.character(length(this$.points)+1)]] = points
#     }
#     else{
#       points[ind] = gsub("..rep..", as.character(id), points[ind])
#       this$.points[[as.character(id)]] = points
#     }
#     
 
  }
  
})
setMethodS3("addGroundOverlay", "RKmlFolder", function(this, x = NULL, fn = NULL, east=NULL, west=NULL, north=NULL, south=NULL, ...) {
  args = list(...)

  print(x)
  
if(is.null(x)){
  x = data.frame(fn, east, west, north, south)
  names(x) = c("fn", "east", "west", "north", "south")
}

if(is.null(fn) & is.null(x$fn))throw("If fn is not supplied, you must supply a data frame with a column named 'fn'")



tryCatch({
  x = data.frame(x)
}, error = function() {
  print("Could not coerce x into dataframe.")
})

if(is.null(x$fn)) x$fn = fn

x$fn = as.character(x$fn)
unfn = unique(x$fn)

  for(i in 1:length(unfn)){
    if(!file.exists(unfn[i]) && !url.exists(unfn[i]))throw("A file location in fn does not exist")
  }
  



#Get Frame for kml point
mpoints = groundoverlay

for(i in 1:nrow(x)){
  
  
 
    
    
      if(!file.exists(x$fn[i])){
        if(is.null(x$east[i]) | is.null(x$west[i]) | is.null(x$north[i]) | is.null(x$south[i]) )
          throw("If you are adding images from a network, you must define east, west, north and south coordinates.")
        
      }
      else{
      inf = GDALinfo(x$fn[i], silent = T)
      if(inf["ll.x"] == 0 | inf["ll.y"] == 0  ){
        if(is.null(x$east[i]) | is.null(x$west[i]) | is.null(x$north[i]) | is.null(x$south[i]) )
          throw(paste("Could not extract geodata from image. This wouldn't be a problem if you define east, west, north and south coordinates. Error at row ", i, sep=""))
      
      }
      else{
        west = inf["ll.x"] 
        east = west + (inf["res.x"] * inf["columns"])
        south = inf["ll.y"] 
        north = south + (inf["res.y"] * inf["rows"])
      }
   }
  
  

  
    
    #Variable list
    id = NULL                       # Point ID
    name = NULL                     # string
    visibility = 1                  # boolean (0-invisible or 1-visible)
    open = 0                        # boolean (0-closed or 1-open  in kml object tree)
    atomauthor = NULL                # xmlns:atom 
    atomlinkhref = NULL             # xmlns:atom
    address = NULL                  # string
    xalAddressDetails = NULL        # xmlns:xal
    phoneNumber = NULL              # string
    Snippet = NULL                  # string with lines seperated by \n for nice format exa. "Hello World\nThis is my\nPlace 
    description = NULL              # string that may contain CDATA. See CDATA section for more info
    AbstractView = NULL             # string id (CameraID or LookAtID) must add with createCamera(id, ...) or createLookAt(id, ...)
    TimeStamp = NULL                # string of date-time in one of the following formats: (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)
    TimeSpanStart = NULL            # string of date-time in one of the following formats(overides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)
    TimeSpanEnd = NULL              # string of date-time in one of the following formats(overides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)  
    styleUrl = NULL                 # string id (StyleID) must add with createStyle(id, ...)
    Region = NULL                   # Currently not supported
    ExtendedData = NULL             # Currently not supported
  drawOrder = NULL 
  color = ""
  transparency = 1
  altitude = NULL                  # meters above/below altiudeMode, clamp altitudeModes ignores altitude
  altitudeMode = NULL  # one of "clampToGround", "relativeToGround", "absolute", "clampToSeaFloor", "relativeToSeaFloor" 
    
  ##ADDED ABILITY 2015(EXPERIMENTAL) 
  #allow style referenes in dataframe. May remove as using this is bad xml practice 
  
  
  icon_color = NULL
  icon_href = NULL
  icon_transparency = NULL
  icon_scale = NULL
  icon_heading = NULL
  icon_xunits = NULL 
  icon_x = NULL
  icon_yunits = NULL  
  icon_y = NULL	
  icon_colorMode = NULL	
  
  bal_bgColor = NULL  
  bal_textColor = NULL	
  bal_text = NULL	
  bal_displayMode = NULL
  
  label_color = NULL  
  label_transparency = NULL	
  label_colorMode = NULL	
  label_scale = NULL	
  
  line_color = NULL  
  line_transparency = NULL	
  line_width = NULL	
  line_outerColor = NULL	
  line_outerTransparency = NULL	
  line_outerPortion = NULL	
  line_colorMode = NULL	
  line_labelVisibility = NULL	
  
  
  #New 2015
  inFolder = NULL
    #Assign values to variables
    
  if(length(args)>0){
    for(j in 1:length(args)){
      assign(names(args[j]), args[[j]][1]) 
    }
  }
  
  for(j in 1:length(x)){
    assign(names(x[j]), x[[names(x)[j]]][i] )
  }
  
   
  
  if(is.null(altitudeMode)){  
    if(!is.null(altitude)) altitudeMode = "relativeToGround"
    else altitude = 0
  }
  
  #Get Frame for kml point
  points = mpoints
  
  
  ind = grep("altitude", points) 
    if(!is.null(altitude)){
   
      if(is.na(as.numeric(as.character(altitude)))) throw("altitude must be numeric or able to coerce to numeric ")
      altitude = as.numeric(as.character(altitude))
      points[ind] = gsub("..rep..", altitude, points[ind])
    }
  else points = points[-ind]
    
  
  ind = grep("drawOrder", points)
  if(!is.null(drawOrder)){
    drawOrder = as.numeric(as.character(drawOrder))
    points[ind] = gsub("..rep..", drawOrder, points[ind])
  }
  else points = points[-ind]
  
    
    
    
    if(!is.null(altitudeMode)){
      if(!altitudeMode %in% c("clampToGround", "relativeToGround", "absolute", "clampToSeaFloor", "relativeToSeaFloor"))
        throw("altitudeMode must be one of the following: clampToGround, relativeToGround, absolute, clampToSeaFloor, relativeToSeaFloor")
      ind = grep("altitudeMode", points)
      if(altitudeMode == "clampToSeaFloor" || altitudeMode == "relativeToSeaFloor"){
        points[ind] = gsub("altitudeMode", "gx:altitudeMode", points[ind])
      }
      points[ind] = gsub("clampToGround", altitudeMode, points[ind])
    }
    
    ind = grep("<name>", points)
    if(!is.null(name)) points[ind] = gsub("..rep..", name, points[ind])
    else points = points[-ind]
    
    if(!is.null(visibility)){
      visibility = as.numeric(as.character(visibility))
      if(! (visibility==0 | visibility==1)) throw("visibility must be either 0 or 1 (boolean)")
      
      ind = grep("visibility", points)
      points[ind] = gsub("1", visibility, points[ind])
    }
    if(!is.null(open)){
      open = as.numeric(as.character(open))
      if(! (open==0 | open==1)) throw("open must be either 0 or 1 (boolean)")
      ind = grep("open", points)
      points[ind] = gsub("0", open, points[ind])
    }
    
    ind = grep("atom:author", points)
    if(!is.null(atomauthor)) points[ind] = gsub("..rep..", atomauthor, points[ind])
    else points = points[-ind]
    
    ind = grep("atom:link", points)
    if(!is.null(atomlinkhref)) points[ind] = gsub("..rep..", atomlinkhref, points[ind])
    else points = points[-ind]
    
    ind = grep("xal:AddressDetails", points)
    if(!is.null(xalAddressDetails)) points[ind] = gsub("..rep..", xalAddressDetails, points[ind])
    else points = points[-ind]
    
    ind = grep("address", points)
    if(!is.null(address)) points[ind] = gsub("..rep..", address, points[ind])
    else points = points[-ind]
    
    ind = grep("phoneNumber", points)
    if(!is.null(phoneNumber)) points[ind] = gsub("..rep..", phoneNumber, points[ind])
    else points = points[-ind]
    
    
    ind = grep("Snippet", points)
    if(!is.null(Snippet)){
      Snippet = as.character(Snippet)
      maxlines = length(unlist(strsplit(Snippet, "\n")))
      points[ind] = gsub("2", maxlines, points[ind])
      points[ind] = gsub("..rep..", Snippet, points[ind])
    }
    else points = points[-ind]
    
    
    ind = grep("description", points)
    if(!is.null(description)) points[ind] = gsub("..rep..", description, points[ind])
    else points = points[-ind]
    
    
  
  ind = grep("AbstractView", points)
  if(!is.null(AbstractView)){
    tmp = this
    while(!is.null(tmp$.parent)) tmp = tmp$.parent
    if(!AbstractView %in% names(tmp$.Aviewlist))warning(paste("No Abstractview id found for '", AbstractView, "'. You must create an abstractview with yourkmlobj$addAbstractView(id = '", AbstractView,"').", sep = ""))
    reptxt = tmp$.Aviewlist[[AbstractView]]
    points[ind] = paste(reptxt, collapse = "")
  }
  else points = points[-ind]
    
    #TODO Add time format check!
    ind = grep("TimeStamp", points)
    if(!is.null(TimeStamp)) points[ind] = gsub("..rep..", TimeStamp, points[ind])
    else points = points[-ind]
    
    
    #TODO Add time format check!
    ind = grep("TimeSpan", points)
    if(!(is.null(TimeSpanStart) & is.null(TimeSpanEnd))){
      if(!is.null(TimeSpanStart)) points[ind] = gsub("..repa..", TimeStamp, points[ind])
      else gsub("<begin>..repa..</begin>", "", points[ind])
      if(!is.null(TimeSpanEnd)) points[ind] = gsub("..repb..", TimeStamp, points[ind])
      else gsub("<end>..repb..</end>", "", points[ind])
    }
    else points = points[-ind]
    
    
    ind = grep("styleUrl", points)
    if(!is.null(styleUrl)){
      points[ind] = gsub("..rep..", styleUrl, points[ind])
      tmp = this
      while(!is.null(tmp$.parent)) tmp = tmp$.parent
      if(!styleUrl %in% names(tmp$.styles))warning(paste("No style id found for '", styleUrl, "'. You must create a style with yourkmlobj$createStyle(id = '", styleUrl,"').", sep = ""))
    }
    else points = points[-ind]
    
  color = color2kmlcolor(color = color, transparency = transparency)
  
  
  ind = grep("<color>", points)
  if(!is.null(color)){
    points[ind] = gsub("..rep..", color, points[ind])
  }
  else points = points[-ind]
    
  box = ""
      
      box = paste(box, "<north>", north, "</north>", sep="")
      box = paste(box, "<south>", south, "</south>", sep="")
      box = paste(box, "<east>", east, "</east>", sep="")
      box = paste(box, "<west>", west, "</west>", sep="")
  
    
    ind = grep("LatLonBox", points)
    points[ind] = gsub("..rep..",box, points[ind])
    
  ##<For non-rectangular image overlays. Not currently supported
  qbox = NULL
  ind = grep("LatLonQuad", points)
  if(!is.null(qbox))
    points[ind] = gsub("..rep..",box, points[ind])
  else points = points[-ind]
  ##/>
  
  ind = grep("Icon", points)
  points[ind] = gsub("..rep..", fn, points[ind])
    
    if(!is.null(id)){
      ind = grep("GroundOverlay", points)
      points[ind] = gsub("ID", id, points[ind])
    }
    
    
    
    ind = grep("Region", points)
    if(!is.null(Region)) points[ind] = gsub("..rep..", Region, points[ind])
    else points = points[-ind]
    
    ind = grep("ExtendedData", points)
    if(!is.null(ExtendedData)) points[ind] = gsub("..rep..", ExtendedData, points[ind])
    else points = points[-ind]
    
    
  #New 2015. Allow define folder 
  if(!is.null(inFolder)){
    
    ssp = unlist(strsplit(as.character(inFolder), "/"))
    ftmp = this
    while(!is.null(ftmp$.parent)) ftmp = ftmp$.parent
    for(k in 1:length(ssp)){
      if(is.null(ftmp$getFolder(ssp[k])))
        ftmp$addFolder(ssp[k], name = ssp[k])
      ftmp = ftmp$getFolder(ssp[k])
    }
    
  }
  
  
    ind = grep("GroundOverlay", points)
  
  
  if(is.null(id)){
    
    ##ADDED 2015 condition
    if(is.null(inFolder)){
      points[ind] = gsub("..rep..", as.character(length(this$.points)+1), points[ind])
      this$.points[[as.character(length(this$.points)+1)]] = points
    }
    else{ 
      points[ind] = gsub("..rep..", as.character(length(ftmp$.points)+1), points[ind])
      ftmp$.points[[as.character(length(ftmp$.points)+1)]] = points
    }
  }
  else{
    
    ##ADDED 2015 condition
    if(is.null(inFolder)){
      points[ind] = gsub("..rep..", as.character(id), points[ind])    
      this$.points[[as.character(id)]] = points
    }
    else{
      points[ind] = gsub("..rep..", as.character(length(ftmp$.points)+1), points[ind])
      ftmp$.points[[as.character(id)]] = points
    }
  }
  
  
#     if(is.null(id)){
#       points[ind] = gsub("..rep..", as.character(length(this$.points)+1), points[ind])
#       this$.points[[as.character(length(this$.points)+1)]] = points
#     }
#     else{
#       points[ind] = gsub("..rep..", as.character(id), points[ind])
#       this$.points[[as.character(id)]] = points
#     }

}
  
})
setMethodS3("addScreenOverlay", "RKmlFolder", function(this, fn = NULL, ...) {
  args = list(...)
  
  if(is.null(fn))throw("fn must not be null")
  else{
    if(!file.exists(fn))throw("file fn does not exist")
  }
  
  
  #Variable list
  id = NULL                       # Point ID
  name = NULL                     # string
  visibility = 1                  # boolean (0-invisible or 1-visible)
  open = 0                        # boolean (0-closed or 1-open  in kml object tree)
  atomauthor = NULL                # xmlns:atom 
  atomlinkhref = NULL             # xmlns:atom
  address = NULL                  # string
  xalAddressDetails = NULL        # xmlns:xal
  phoneNumber = NULL              # string
  Snippet = NULL                  # string with lines seperated by \n for nice format exa. "Hello World\nThis is my\nPlace 
  description = NULL              # string that may contain CDATA. See CDATA section for more info
  AbstractView = NULL             # string id (CameraID or LookAtID) must add with createCamera(id, ...) or createLookAt(id, ...)
  TimeStamp = NULL                # string of date-time in one of the following formats: (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)
  TimeSpanStart = NULL            # string of date-time in one of the following formats(overides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)
  TimeSpanEnd = NULL              # string of date-time in one of the following formats(overides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)  
  styleUrl = NULL                 # string id (StyleID) must add with createStyle(id, ...)
  Region = NULL                   # Currently not supported
  ExtendedData = NULL             # Currently not supported
  drawOrder = NULL 
  color = ""
  transparency = 1
  rotation = 0
  
  
  overlay_x = .5
  overlay_y = .5
  overlay_xunit = "fraction"
  overlay_yunit = "fraction"
  screen_x = .5
  screen_y = .5
  screen_xunit = "fraction"
  screen_yunit = "fraction"
  rotation_x = screen_x
  rotation_y = screen_y
  rotation_xunit = screen_xunit
  rotation_yunit = screen_yunit
  size_x = -1
  size_y = -1
  size_xunit = "fraction"
  size_yunit = "fraction"  
  
  #New 2015
  inFolder = NULL
  
  #Assign values to variables
    
  if(length(args)>0){
    for(j in 1:length(args)){
      assign(names(args[j]), args[[j]][1]) 
    }
  }
  
  
  #Get Frame for kml point
  points = screenoverlay
  
  
  ind = grep("/rotation", points, fixed = T) 
  if(!is.null(rotation)){
    if(is.na(as.numeric(rotation))) throw("rotation must be numeric or able to coerce to numeric ")
    rotation = as.numeric(rotation)
    points[ind] = gsub("..rep..", rotation, points[ind])
  }
  else points = points[-ind]
  
  
  ind = grep("drawOrder", points)
  if(!is.null(drawOrder)){
    drawOrder = as.numeric(as.character(drawOrder))
    points[ind] = gsub("..rep..", drawOrder, points[ind])
  }
  else points = points[-ind]
  
  
  
  
  ind = grep("<name>", points)
  if(!is.null(name)) points[ind] = gsub("..rep..", name, points[ind])
  else points = points[-ind]
  
  if(!is.null(visibility)){
    if(! (as.numeric(visibility)==0 | as.numeric(visibility)==1)) throw("visibility must be either 0 or 1 (boolean)")
    visibility = as.numeric(visibility)
    ind = grep("visibility", points)
    points[ind] = gsub("1", visibility, points[ind])
  }
  if(!is.null(open)){
    if(! (as.numeric(open)==0 | as.numeric(open)==1)) throw("open must be either 0 or 1 (boolean)")
    open = as.numeric(open)
    ind = grep("open", points)
    points[ind] = gsub("0", open, points[ind])
  }
  
  ind = grep("atom:author", points)
  if(!is.null(atomauthor)) points[ind] = gsub("..rep..", atomauthor, points[ind])
  else points = points[-ind]
  
  ind = grep("atom:link", points)
  if(!is.null(atomlinkhref)) points[ind] = gsub("..rep..", atomlinkhref, points[ind])
  else points = points[-ind]
  
  ind = grep("xal:AddressDetails", points)
  if(!is.null(xalAddressDetails)) points[ind] = gsub("..rep..", xalAddressDetails, points[ind])
  else points = points[-ind]
  
  ind = grep("address", points)
  if(!is.null(address)) points[ind] = gsub("..rep..", address, points[ind])
  else points = points[-ind]
  
  ind = grep("phoneNumber", points)
  if(!is.null(phoneNumber)) points[ind] = gsub("..rep..", phoneNumber, points[ind])
  else points = points[-ind]
  
  
  ind = grep("Snippet", points)
  if(!is.null(Snippet)){
    Snippet = as.character(Snippet)
    maxlines = length(unlist(strsplit(Snippet, "\n")))
    points[ind] = gsub("2", maxlines, points[ind])
    points[ind] = gsub("..rep..", Snippet, points[ind])
  }
  else points = points[-ind]
  
  
  ind = grep("description", points)
  if(!is.null(description)) points[ind] = gsub("..rep..", description, points[ind])
  else points = points[-ind]
  
  
  
  ind = grep("AbstractView", points)
  if(!is.null(AbstractView)){
    tmp = this
    while(!is.null(tmp$.parent)) tmp = tmp$.parent
    if(!AbstractView %in% names(tmp$.Aviewlist))warning(paste("No Abstractview id found for '", AbstractView, "'. You must create an abstractview with yourkmlobj$addAbstractView(id = '", AbstractView,"').", sep = ""))
    reptxt = tmp$.Aviewlist[[AbstractView]]
    points[ind] = paste(reptxt, collapse = "")
  }
  else points = points[-ind]
  
  #TODO Add time format check!
  ind = grep("TimeStamp", points)
  if(!is.null(TimeStamp)) points[ind] = gsub("..rep..", TimeStamp, points[ind])
  else points = points[-ind]
  
  
  #TODO Add time format check!
  ind = grep("TimeSpan", points)
  if(!(is.null(TimeSpanStart) & is.null(TimeSpanEnd))){
    if(!is.null(TimeSpanStart)) points[ind] = gsub("..repa..", TimeStamp, points[ind])
    else gsub("<begin>..repa..</begin>", "", points[ind])
    if(!is.null(TimeSpanEnd)) points[ind] = gsub("..repb..", TimeStamp, points[ind])
    else gsub("<end>..repb..</end>", "", points[ind])
  }
  else points = points[-ind]
  
  #New 2015. define containing folder   
  
  if(!is.null(inFolder)){
    
    ssp = unlist(strsplit(as.character(inFolder), "/"))
    ftmp = this
    while(!is.null(ftmp$.parent)) ftmp = ftmp$.parent
    for(k in 1:length(ssp)){
      if(is.null(ftmp$getFolder(ssp[k])))
        ftmp$addFolder(ssp[k], name = ssp[k])
      ftmp = ftmp$getFolder(ssp[k])
    }
    
  }
  
  
  ind = grep("styleUrl", points)
  if(!is.null(styleUrl)){
    points[ind] = gsub("..rep..", styleUrl, points[ind])
    tmp = this
    while(!is.null(tmp$.parent)) tmp = tmp$.parent
    if(!styleUrl %in% names(tmp$.styles))warning(paste("No style id found for '", styleUrl, "'. You must create a style with yourkmlobj$createStyle(id = '", styleUrl,"').", sep = ""))
  }
  else points = points[-ind]
  
  tra = as.hexmode(round(as.numeric(transparency)*255))
  if(color == "")color = NULL
  if(!is.null(color)){
    if(color %in% colors()){
      color = col2rgb(color)
      color = as.character(as.hexmode(color))
      color = paste(color[3], color[2], color[1], sep="")
    }
    else if(nrow(color)>2){
      color = as.character(as.hexmode(color))
      color = paste(color[3], color[2], color[1], sep="")
    } 
    
    color = paste("#", tra, color, sep="")
  }
  
  ind = grep("<color>", points)
  if(!is.null(color)){
    points[ind] = gsub("..rep..", color, points[ind])
  }
  else points = points[-ind]

  
  ind = grep("overlayXY", points)
  points[ind] = sub("..repx..",overlay_x, points[ind], fixed = T)
  points[ind] = sub("..repy..",overlay_y, points[ind], fixed = T)
  points[ind] = sub("..repxu..",overlay_xunit, points[ind], fixed = T)
  points[ind] = sub("..repyu..",overlay_yunit, points[ind], fixed = T)
  ind = grep("screenXY", points)

  points[ind] = sub("..repx..",screen_x, points[ind], fixed = T)
  points[ind] = sub("..repy..",screen_y, points[ind], fixed = T)
  points[ind] = sub("..repxu..",screen_xunit, points[ind], fixed = T)
  points[ind] = sub("..repyu..",screen_yunit, points[ind], fixed = T)
  ind = grep("rotationXY", points)

  points[ind] = sub("..repx..",rotation_x, points[ind], fixed = T)
  points[ind] = sub("..repy..",rotation_y, points[ind], fixed = T)
  points[ind] = sub("..repxu..",rotation_xunit, points[ind], fixed = T)
  points[ind] = sub("..repyu..",rotation_yunit, points[ind], fixed = T)
  ind = grep("size", points)
  points[ind] = sub("..repx..",size_x, points[ind], fixed = T)
  points[ind] = sub("..repy..",size_y, points[ind], fixed = T)
  points[ind] = sub("..repxu..",size_xunit, points[ind], fixed = T)
  points[ind] = sub("..repyu..",size_yunit, points[ind], fixed = T)
  
  
  ind = grep("Icon", points)
  points[ind] = gsub("..rep..", fn, points[ind])
  
  if(!is.null(id)){
    ind = grep("GroundOverlay", points)
    points[ind] = gsub("ID", id, points[ind])
  }
  
  
  
  ind = grep("Region", points)
  if(!is.null(Region)) points[ind] = gsub("..rep..", Region, points[ind])
  else points = points[-ind]
  
  ind = grep("ExtendedData", points)
  if(!is.null(ExtendedData)) points[ind] = gsub("..rep..", ExtendedData, points[ind])
  else points = points[-ind]
  
  
  ind = grep("GroundOverlay", points)
  
  
  if(is.null(id)){
    
    ##ADDED 2015 condition
    if(is.null(inFolder)){
      points[ind] = gsub("..rep..", as.character(length(this$.points)+1), points[ind])
      this$.points[[as.character(length(this$.points)+1)]] = points
    }
    else{ 
      points[ind] = gsub("..rep..", as.character(length(ftmp$.points)+1), points[ind])
      ftmp$.points[[as.character(length(ftmp$.points)+1)]] = points
    }
  }
  else{
    
    ##ADDED 2015 condition
    if(is.null(inFolder)){
      points[ind] = gsub("..rep..", as.character(id), points[ind])    
      this$.points[[as.character(id)]] = points
    }
    else{
      points[ind] = gsub("..rep..", as.character(length(ftmp$.points)+1), points[ind])
      ftmp$.points[[as.character(id)]] = points
    }
  }
  
#   
#   if(is.null(id)){
#     points[ind] = gsub("..rep..", as.character(length(this$.points)+1), points[ind])
#     this$.points[[as.character(length(this$.points)+1)]] = points
#   }
#   else{
#     points[ind] = gsub("..rep..", as.character(id), points[ind])
#     this$.points[[as.character(id)]] = points
#   }
#   
  
  
})
setMethodS3("getFolder", "RKmlFolder", function(this, fid, ...) {
  if(is.null(this$.folders[[fid]])){
    print(paste("Folder '", fid, "' in '", this$.id, "' does not exist", sep = ""))
    return(NULL)
  }
  else{
    print(paste("get Folder '", fid, "' from '", this$.id, "'", sep = "") )
    return(this$.folders[[fid]])
  }
})
setMethodS3("addFolder", "RKmlFolder", function(this, fid, ...) {
  
  
  if(!is.null(this$.folders[[fid]]))
    print("This file alreadys exists at this location. You can add to this folder with getFolder(id)$add.. or you can remove it with removeFolder(id).") 
  else{
    newfol = RKmlFolder(id = fid, parent = this)
  
    args = list(...)
    
    #Variable list

    name = NULL                     # string
    visibility = 1                  # boolean (0-invisible or 1-visible)
    open = 0                        # boolean (0-closed or 1-open  in kml object tree)
    atomauthor = NULL    	          # xmlns:atom 
    atomlinkhref = NULL             # xmlns:atom
    address = NULL                  # string
    xalAddressDetails = NULL        # xmlns:xal
    phoneNumber = NULL              # string
    Snippet = NULL                  # string with lines seperated by \n for nice format exa. "Hello World\nThis is my\nPlace 
    description = NULL              # string that may contain CDATA. See CDATA section for more info
    AbstractView = NULL             # string id (CameraID or LookAtID) must add with createCamera(id, ...) or createLookAt(id, ...)
    TimeStamp = NULL                # string of date-time in one of the following formats: (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)
    TimeSpanStart = NULL            # string of date-time in one of the following formats(overides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)
    TimeSpanEnd = NULL              # string of date-time in one of the following formats(overides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)  
    styleUrl = NULL                 # string id (StyleID) must add with createStyle(id, ...)
    id = fid                       # Point ID
    Region = NULL                   # Currently not supported
    ExtendedData = NULL             # Currently not supported
    
    #Assign values to variables
    
    points =folder
    
   
    if(length(args)>0){
      for(j in 1:length(args)){
        assign(names(args[j]), args[[j]][1]) 
      }
    }
    
    ind = grep("<name>", points)
    if(!is.null(name)) points[ind] = gsub("..rep..", name, points[ind])
    else points = points[-ind]
    
    if(!is.null(visibility)){
      visibility = as.numeric(as.character(visibility))
      if(! (visibility==0 | visibility==1)) throw("visibility must be either 0 or 1 (boolean)")
      ind = grep("visibility", points)
      points[ind] = gsub("1", visibility, points[ind])
    }
    if(!is.null(open)){
      open = as.numeric(as.character(open))
      if(! (as.numeric(open)==0 | as.numeric(open)==1)) throw("open must be either 0 or 1 (boolean)")
      ind = grep("open", points)
      points[ind] = gsub("0", open, points[ind])
    }
    
    ind = grep("atom:author", points)
    if(!is.null(atomauthor)) points[ind] = gsub("..rep..", atomauthor, points[ind])
    else points = points[-ind]
    
    ind = grep("atom:link", points)
    if(!is.null(atomlinkhref)) points[ind] = gsub("..rep..", atomlinkhref, points[ind])
    else points = points[-ind]
    
    ind = grep("xal:AddressDetails", points)
    if(!is.null(xalAddressDetails)) points[ind] = gsub("..rep..", xalAddressDetails, points[ind])
    else points = points[-ind]
    
    ind = grep("address", points)
    if(!is.null(address)) points[ind] = gsub("..rep..", address, points[ind])
    else points = points[-ind]
    
    ind = grep("phoneNumber", points)
    if(!is.null(phoneNumber)) points[ind] = gsub("..rep..", phoneNumber, points[ind])
    else points = points[-ind]
    
    
    ind = grep("Snippet", points)
    if(!is.null(Snippet)){
      Snippet = as.character(Snippet)
      maxlines = length(unlist(strsplit(Snippet, "\n")))
      points[ind] = gsub("2", maxlines, points[ind])
      points[ind] = gsub("..rep..", Snippet, points[ind])
    }
    else points = points[-ind]
    
    
    ind = grep("description", points)
    if(!is.null(description)) points[ind] = gsub("..rep..", description, points[ind])
    else points = points[-ind]
    
    ind = grep("AbstractView", points)
    if(!is.null(AbstractView)){
      tmp = this
      while(!is.null(tmp$.parent)) tmp = tmp$.parent
      if(!AbstractView %in% names(tmp$.Aviewlist))warning(paste("No Abstractview id found for '", AbstractView, "'. You must create an abstractview with yourkmlobj$addAbstractView(id = '", AbstractView,"').", sep = ""))
      reptxt = tmp$.Aviewlist[[AbstractView]]
      points[ind] = paste(reptxt, collapse = "")
    }
    else points = points[-ind]
    
    
    
    #TODO Add time format check!
    ind = grep("TimeStamp", points)
    if(!is.null(TimeStamp)) points[ind] = gsub("..rep..", TimeStamp, points[ind])
    else points = points[-ind]
    
    
    #TODO Add time format check!
    ind = grep("TimeSpan", points)
    if(!(is.null(TimeSpanStart) & is.null(TimeSpanEnd))){
      if(!is.null(TimeSpanStart)) points[ind] = gsub("..repa..", TimeStamp, points[ind])
      else gsub("<begin>..repa..</begin>", "", points[ind])
      if(!is.null(TimeSpanEnd)) points[ind] = gsub("..repb..", TimeStamp, points[ind])
      else gsub("<end>..repb..</end>", "", points[ind])
    }
    else points = points[-ind]
    
    
    ind = grep("styleUrl", points)
    if(!is.null(styleUrl)){
      points[ind] = gsub("..rep..", styleUrl, points[ind])
      tmp = this
      while(!is.null(tmp$.parent)) tmp = tmp$.parent
      if(!styleUrl %in% names(tmp$.styles))warning(paste("No style id found for '", styleUrl, "'. You must create a style with yourkmlobj$createStyle(id = '", styleUrl,"').", sep = ""))
    }
    else points = points[-ind]
    
    
    
    ind = grep("Region", points)
    if(!is.null(Region)) points[ind] = gsub("..rep..", Region, points[ind])
    else points = points[-ind]
    
    ind = grep("ExtendedData", points)
    if(!is.null(ExtendedData)) points[ind] = gsub("..rep..", ExtendedData, points[ind])
    else points = points[-ind]
    
    
    ind = grep("Folder id", points)
    if(!is.null(id)){
      points[ind] = gsub("..rep..", as.character(id), points[ind])
    }
    else throw("Folder must have an id")
    
    newfol$.foldertxt = points
    
    this$.folders[[fid]] = newfol
    print(paste("Folder '", fid, "' to '", this$.id, "' Added", sep = ""))
  }
})
setMethodS3("removeFolder", "RKmlFolder", function(this, fid, ...) {
  this$.folders[[fid]] = NULL
  print(paste("folder '", fid, "' removed from '", this$.id, "'", sep = ""))
})
setMethodS3("removeStyle", "RKmlFolder", function(this, styleid = NULL, styletype = NULL,  ...) {
  tmp = this
  while(!is.null(tmp$.parent)) tmp = tmp$.parent
  
  if(is.null(styleid)) throw("You must suppply the styleid to remove, mykmlobj$listStyles()")
  if(is.null(styletype)){ 
    tmp$.styles[[styleid]] = NULL
    print(paste("Style '", styleid, "' removed", sep = ""))
  }
  else{
    st = c("PolyStyle", "IconStyle", "LineStyle", "BalloonStyle", "LabelStyle")
    if(!styletype %in% st)throw("styletype must be one of PolyStyle, IconStyle, LineStyle, BalloonStyle or LabelStyle")
    temp = tmp$.styles[[styleid]]
    ind = grep(styletype, temp)[1]
    if(!is.na(ind))temp = temp[-ind]
    tmp$.styles[[styleid]] = temp
    print(paste(styletype, " removed from Style '", styleid, ".", sep = ""))
  }

})
setMethodS3("liststyles", "RKmlFolder", function(this, ...) {
  tmp = this
  while(!is.null(tmp$.parent)) tmp = tmp$.parent
  for(i in 1:length(tmp$.styles)){
    print(names(mykml$.styles)[i])
    st = tmp$.styles[[i]]
    j = 2
    while(j < length(st)){
      start = unlist(gregexpr(pattern ='<', st[j]))[1]
      end = unlist(gregexpr(pattern ='>', st[j]))[1]
      print(paste("     ", substr(st[j], start, end)))
      j = j + 1
    }
    
  }
  
  })
setMethodS3("addNetworkLink", "RKmlFolder", function(this, href = NULL, ...) {
  args = list(...)
  
  
  if(is.null(href))throw("You must specify the local or network address href argument")
 
  #Get Frame for kml point
  mpoints = networklink
    #Variable list

       
    name = NULL                     # string
    visibility = 1                  # boolean (0-invisible or 1-visible)
    open = 0                        # boolean (0-closed or 1-open  in kml object tree)
    atomauthor = NULL    	          # xmlns:atom 
    atomlinkhref = NULL             # xmlns:atom
    address = NULL                  # string
    xalAddressDetails = NULL        # xmlns:xal
    phoneNumber = NULL              # string
    Snippet = NULL                  # string with lines seperated by \n for nice format exa. "Hello World\nThis is my\nPlace 
    description = NULL              # string that may contain CDATA. See CDATA section for more info
    AbstractView = NULL             # string id (CameraID or LookAtID) must add with createCamera(id, ...) or createLookAt(id, ...)
    TimeStamp = NULL                # string of date-time in one of the following formats: (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)
    TimeSpanStart = NULL            # string of date-time in one of the following formats(overides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)
    TimeSpanEnd = NULL              # string of date-time in one of the following formats(overides TimeStamp): (YYYY)(YYYY-MM)(YYYY-MM-DD)(YYYY-MM-DDThh:mm:ssZ)(YYYY-MM-DDThh:mm:ss)  
    styleUrl = NULL                 # string id (StyleID) must add with createStyle(id, ...)
    id = NULL                       # Point ID
    Region = NULL                   # Currently not supported
    ExtendedData = NULL             # Currently not supported
    refreshVisibility = NULL
    flyToView = NULL  #0 or 1 1=fly to networklinks controll view if exists eles to first child

    refreshMode = NULL #"onChange" #onChange, onInterval, or onExpire
    refreshInterval = NULL # 4
    viewRefreshMode = NULL #"never" #never, onStop, onRequest, onRegion
  viewRefreshTime = NULL #4
  viewBoundScale = NULL #1
  viewFormat = NULL #BBOX=[bboxWest],[bboxSouth],[bboxEast],[bboxNorth]</viewFormat>
  httpQuery = NULL
  
  #New 2015
  inFolder = NULL
  
  #Assign values to variables
    
    
    
    points = mpoints
    if(length(args)>0){
      for(j in 1:length(args)){
      #  print(names(args[j]))
        assign(names(args[j]), args[[j]][1]) 
      }
    }
 
 
    ind = grep("<name>", points)
    if(!is.null(name)) points[ind] = gsub("..rep..", name, points[ind])
    else points = points[-ind]
    
    
    if(!is.null(visibility)){
      visibility = as.numeric(as.character(visibility))
      if(! (visibility==0 | visibility==1)) throw("visibility must be either 0 or 1 (boolean)")
      ind = grep("visibility", points)
      points[ind] = gsub("1", visibility, points[ind])
    }
    
    
    if(!is.null(open)){
      open = as.numeric(as.character(open))
      if(! (open==0 | open==1)) throw("open must be either 0 or 1 (boolean)")
      ind = grep("open", points)
      points[ind] = gsub("0", open, points[ind])
    }
    
    ind = grep("atom:author", points)
    if(!is.null(atomauthor)) points[ind] = gsub("..rep..", atomauthor, points[ind])
    else points = points[-ind]
    
    ind = grep("atom:link", points)
    if(!is.null(atomlinkhref)) points[ind] = gsub("..rep..", atomlinkhref, points[ind])
    else points = points[-ind]
    
    ind = grep("xal:AddressDetails", points)
    if(!is.null(xalAddressDetails)) points[ind] = gsub("..rep..", xalAddressDetails, points[ind])
    else points = points[-ind]
    
    ind = grep("address", points)
    if(!is.null(address)) points[ind] = gsub("..rep..", address, points[ind])
    else points = points[-ind]
    
    ind = grep("phoneNumber", points)
    if(!is.null(phoneNumber)) points[ind] = gsub("..rep..", phoneNumber, points[ind])
    else points = points[-ind]
    
    
    ind = grep("Snippet", points)
    if(!is.null(Snippet)){
      Snippet = as.character(Snippet)
      maxlines = length(unlist(strsplit(Snippet, "\n")))
      points[ind] = gsub("2", maxlines, points[ind])
      points[ind] = gsub("..rep..", Snippet, points[ind])
    }
    else points = points[-ind]
    
    
    ind = grep("description", points)
    if(!is.null(description)) points[ind] = gsub("..rep..", description, points[ind])
    else points = points[-ind]
    
  ind = grep("AbstractView", points)
  if(!is.null(AbstractView)){
    tmp = this
    while(!is.null(tmp$.parent)) tmp = tmp$.parent
    if(!AbstractView %in% names(tmp$.Aviewlist))warning(paste("No Abstractview id found for '", AbstractView, "'. You must create an abstractview with yourkmlobj$addAbstractView(id = '", AbstractView,"').", sep = ""))
    reptxt = tmp$.Aviewlist[[AbstractView]]
    points[ind] = paste(reptxt, collapse = "")
  }
  else points = points[-ind]
  
    
    #TODO Add time format check!
    ind = grep("TimeStamp", points)
    if(!is.null(TimeStamp)) points[ind] = gsub("..rep..", TimeStamp, points[ind])
    else points = points[-ind]
    
    
    #TODO Add time format check!
    ind = grep("TimeSpan", points)
    if(!(is.null(TimeSpanStart) & is.null(TimeSpanEnd))){
      if(!is.null(TimeSpanStart)) points[ind] = gsub("..repa..", TimeStamp, points[ind])
      else gsub("<begin>..repa..</begin>", "", points[ind])
      if(!is.null(TimeSpanEnd)) points[ind] = gsub("..repb..", TimeStamp, points[ind])
      else gsub("<end>..repb..</end>", "", points[ind])
    }
    else points = points[-ind]
    
  #New 2015. define containing folder   
  
  if(!is.null(inFolder)){
    
    ssp = unlist(strsplit(as.character(inFolder), "/"))
    ftmp = this
    while(!is.null(ftmp$.parent)) ftmp = ftmp$.parent
    for(k in 1:length(ssp)){
      if(is.null(ftmp$getFolder(ssp[k])))
        ftmp$addFolder(ssp[k], name = ssp[k])
      ftmp = ftmp$getFolder(ssp[k])
    }
    
  } 
  
  
    ind = grep("styleUrl", points)
    if(!is.null(styleUrl)){
      points[ind] = gsub("..rep..", styleUrl, points[ind])
      tmp = this
      while(!is.null(tmp$.parent)) tmp = tmp$.parent
      if(!styleUrl %in% names(tmp$.styles))warning(paste("No style id found for '", styleUrl, "'. You must create a style with yourkmlobj$createStyle(id = '", styleUrl,"').", sep = ""))
    }
    else points = points[-ind]
      
    
    ind = grep("Region", points)
    if(!is.null(Region)) points[ind] = gsub("..rep..", Region, points[ind])
    else points = points[-ind]
  
    ind = grep("ExtendedData", points)
    if(!is.null(ExtendedData)) points[ind] = gsub("..rep..", ExtendedData, points[ind])
    else points = points[-ind]
   
  
  ind = grep("refreshVisibility", points)
  
  if(!is.null(refreshVisibility)){
    if(! (refreshVisibility == 0 | refreshVisibility == 1 ))throw("refreshVisibility must be 1 or 0")
    points[ind] = gsub("..rep..", refreshVisibility, points[ind])
  }
  else points = points[-ind]

  ind = grep("flyToView", points)
  if(!is.null(flyToView)){
    if(! (flyToView == 0 | flyToView == 1 ))throw("flyToView must be 1 or 0")
    points[ind] = gsub("..rep..", flyToView, points[ind])
  }
  else points = points[-ind]


  ind = grep("<href>", points)

  if(!is.null(href)) points[ind] = gsub("..rep..", href, points[ind])
  else points = points[-ind]

  ind = grep("refreshMode", points)
  if(!is.null(refreshMode)){
    if(! (refreshMode == "onchange" | refreshMode == "onInterval" | refreshMode == "onExpire" ))throw("refreshMode must be onChange, onInterval oronExpire")
    points[ind] = gsub("..rep..", refreshMode, points[ind])
  }
  else points = points[-ind]
  
  

  ind = grep("viewRefreshMode", points)
  if(!is.null(viewRefreshMode)){
    if(! (viewRefreshMode == "onStop" | viewRefreshMode == "onRequest" | viewRefreshMode == "onRegion" | viewRefreshMode == "never"))throw("viewRefreshMode must be onStop, onRequest, onRegion or never")
    points[ind] = gsub("..rep..", viewRefreshMode, points[ind])
  }
  else points = points[-ind]
  
  
    ind = grep("refreshInterval", points)
  if(!is.null(refreshInterval)){
    if(is.na(as.numeric(as.character(refreshInterval)))) throw("refreshInterval must be numeric or coerceable to numeric")
    points[ind] = gsub("..rep..", refreshInterval, points[ind])
  }
  else points = points[-ind]  
    
    
    
    
  
  ind = grep("viewRefreshTime", points)
  if(!is.null(viewRefreshTime)){
    if(is.na(as.numeric(as.character(viewRefreshTime)))) throw("viewRefreshTime must be numeric or coerceable to numeric")
    
  points[ind] = gsub("..rep..", viewRefreshTime, points[ind])
  }
  else points = points[-ind]

  
  ind = grep("viewBoundScale", points)
  if(!is.null(viewBoundScale)){
    if(is.na(as.numeric(as.character(viewBoundScale)))) throw("viewBoundScale must be numeric or coerceable to numeric")
    
    points[ind] = gsub("..rep..", viewBoundScale, points[ind])
  }
    else points = points[-ind]
  
  ind = grep("viewFormat", points)
  if(!is.null(viewFormat)) points[ind] = gsub("..rep..", viewFormat, points[ind])
  else points = points[-ind]
  
  ind = grep("httpQuery", points)
  if(!is.null(httpQuery)) points[ind] = gsub("..rep..", httpQuery, points[ind])
  else points = points[-ind]
    
  #New 2015. define containing folder   
  
  if(!is.null(inFolder)){
    
    ssp = unlist(strsplit(as.character(inFolder), "/"))
    ftmp = this
    while(!is.null(ftmp$.parent)) ftmp = ftmp$.parent
    for(k in 1:length(ssp)){
      if(is.null(ftmp$getFolder(ssp[k])))
        ftmp$addFolder(ssp[k], name = ssp[k])
      ftmp = ftmp$getFolder(ssp[k])
    }
    
  }
  
    ind = grep("NetworkLink id", points)
    
  if(is.null(id)){
    
    ##ADDED 2015 condition
    if(is.null(inFolder)){
      points[ind] = gsub("..rep..", as.character(length(this$.points)+1), points[ind])
      this$.points[[as.character(length(this$.points)+1)]] = points
    }
    else{ 
      points[ind] = gsub("..rep..", as.character(length(ftmp$.points)+1), points[ind])
      ftmp$.points[[as.character(length(ftmp$.points)+1)]] = points
    }
  }
  else{
    
    ##ADDED 2015 condition
    if(is.null(inFolder)){
      points[ind] = gsub("..rep..", as.character(id), points[ind])    
      this$.points[[as.character(id)]] = points
    }
    else{
      points[ind] = gsub("..rep..", as.character(length(ftmp$.points)+1), points[ind])
      ftmp$.points[[as.character(id)]] = points
    }
  }
  
#   
#   if(is.null(id)){
#       points[ind] = gsub("..rep..", as.character(length(this$.points)+1), points[ind])
#       this$.points[[as.character(length(this$.points)+1)]] = points
#     }
#     else{
#       points[ind] = gsub("..rep..", as.character(id), points[ind])
#       this$.points[[as.character(id)]] = points
#     }
#     
#     
  
  
})
setMethodS3("addNetworkLinkControl", "RKmlFolder", function(this, minRefreshPeriod =NULL, maxSessionLength = -1, cookie = NULL, message = NULL, linkName = NULL, linkDescription = NULL, linkSnippet = NULL, expires = NULL, update = NULL, AbstractView = NULL, ...) {
 
  style = networklinkcontroll

  ind = grep("minRefreshPeriod", style)
  if(!is.null(minRefreshPeriod)){
    if(is.na(as.numeric(as.character(minRefreshPeriod)))) throw("minRefreshPeriod must be numeric or coerceable to numeric")
    style[ind] = gsub("..rep..", minRefreshPeriod, style[ind])
  }
  else style = style[-ind]
  
  ind = grep("maxSessionLength", style)
  if(!is.null(maxSessionLength)){
    if(is.na(as.numeric(as.character(maxSessionLength)))) throw("maxSessionLength must be numeric or coerceable to numeric")
    style[ind] = gsub("..rep..", maxSessionLength, style[ind])
  }
  else style = style[-ind]
  
  ind = grep("cookie", style)
  if(!is.null(cookie)){
      style[ind] = gsub("..rep..", cookie, style[ind])
  }
  else style = style[-ind]
  
  ind = grep("message", style)
  if(!is.null(message)){
    style[ind] = gsub("..rep..", message, style[ind])
  }
  else style = style[-ind]
  
  ind = grep("linkName", style)
  if(!is.null(linkName)){
    style[ind] = gsub("..rep..", linkName, style[ind])
  }
  else style = style[-ind]

  ind = grep("linkDescription", style)
  if(!is.null(linkDescription)){
    style[ind] = gsub("..rep..", linkDescription, style[ind])
  }
  else style = style[-ind]
    
  
  ind = grep("linkSnippet", style)
  if(!is.null(linkSnippet)){
    linkSnippet = as.character(linkSnippet)
    maxlines = length(unlist(strsplit(linkSnippet, "\n")))
    style[ind] = gsub("2", maxlines, style[ind])
    style[ind] = gsub("..rep..", linkSnippet, style[ind])
  }
  else style = style[-ind]
  
  
  ind = grep("expires", style)
  if(!is.null(expires)){
    style[ind] = gsub("..rep..", expires, style[ind])
  }
  else style = style[-ind]

  
  ind = grep("Update", style)
  if(!is.null(update)){
    style[ind] = gsub("..rep..", update, style[ind])
  }
  else style = style[-ind]
  
  
  ind = grep("AbstractView", style)
  if(!is.null(AbstractView)){
    tmp = this
    while(!is.null(tmp$.parent)) tmp = tmp$.parent
    if(!AbstractView %in% names(tmp$.Aviewlist))warning(paste("No Abstractview id found for '", AbstractView, "'. You must create an abstractview with yourkmlobj$addAbstractView(id = '", AbstractView,"').", sep = ""))
    reptxt = tmp$.Aviewlist[[AbstractView]]
    style[ind] = paste(reptxt, collapse = "")
  }
  else style = style[-ind]
  


  tmp = this
  while(!is.null(tmp$.parent)) tmp = tmp$.parent
  

  tmp$.networkcontroltxt = paste(style, collapse = "")

   
  
})
setMethodS3("writekml", "RKmlFolder", function(this, path, ...) {
  
  if(dirname(path) != ".")dir.create(dirname(path), recursive = T, showWarnings = F)


  zz = file(path, "w")
  sta = "<?xml version='1.0' encoding='UTF-8'?>
    <kml xmlns='http://www.opengis.net/kml/2.2' xmlns:gx='http://www.google.com/kml/ext/2.2' xmlns:kml='http://www.opengis.net/kml/2.2' xmlns:atom='http://www.w3.org/2005/Atom'>"
  sta = paste(sta, this$.networkcontroltxt, sep = "")
  sta = paste(sta, "<Document id='..rep..'><open>1</open>", sep = "")
  sta = gsub("..rep..", this$.id, sta)
  writeLines(sta, zz)
  close(zz)
  
  zz = file(path, "a")
  
  i = 1
  while(i <= length(this$.styles)){
    writeLines(this$.styles[[i]], zz)
    i = i + 1
  }


  
  
  i = 1
  while(i <= length(this$.points)){
    writeLines(this$.points[[i]], zz)
    i = i + 1
  }
  close(zz)
  
  i = 1
  
  while(i <= length(this$.folders)){
    this$.folders[[i]]$printcontents(path = path)
    i = i+1
  }
  
  zz = file(path, "a")
  end = "</Document></kml>"
  writeLines(end, zz)
  close(zz)
  print(paste("kml saved at", path))


})
setMethodS3("printcontents", "RKmlFolder", function(this, path, ...) {
 
  zz = file(path, "a")
  
  writeLines(this$.foldertxt, zz)
  
  i = 1
  while(i <= length(this$.points)){
    writeLines(this$.points[[i]], zz)
    i = i + 1
  } 
  close(zz)
  
  i = 1
  while(i <= length(this$.folders)){
    this$.folders[[i]]$printcontents(path = path)
    i = i + 1
  }
  zz = file(path, "a")
  writeLines("</Folder>", zz)
  close(zz)
  
})
setMethodS3("preview", "RKmlFolder", function(this, ...) {

  #zz = tempdir()
  
  #if (!file.exists(zz)){
  #  dir.create(zz, recursive = T)
  #}
  
  #zz = file.path(zz, "tempkml.kml")
   zz = "temp_kmlbuilder.kml"
  this$writekml(zz)

  browseURL(zz)
  

})
color2kmlcolor = function(color = "", transparency = 1) {
  
  if(is.null(transparency)) transparency = 1
  
  if(!is.null(color))if(color == "")color = NULL
  tra = as.hexmode(round(as.numeric(transparency)*255))
  if(nchar(tra) == 1) tra = paste("0", tra, sep = "")
  if(!is.null(color)){
    if(color %in% colors()){
      color = col2rgb(color)
      color = as.character(as.hexmode(color))
      if(nchar(color[1]) == 1) color[1] = paste("0", color[1], sep = "")
      if(nchar(color[2]) == 1) color[2] = paste("0", color[2], sep = "")
      if(nchar(color[3]) == 1) color[3] = paste("0", color[3], sep = "")
      color = paste(color[3], color[2], color[1], sep="")
    }
    else if(length(color)>2){
      color = as.character(as.hexmode(color))
      if(nchar(color[1]) == 1) color[1] = paste("0", color[1], sep = "")
      if(nchar(color[2]) == 1) color[2] = paste("0", color[2], sep = "")
      if(nchar(color[3]) == 1) color[3] = paste("0", color[3], sep = "")
      color = paste(color[3], color[2], color[1], sep="")
    }
    else if(grepl("#", color)){
      color = paste(substr(color, 6, 7), substr(color, 4, 5), substr(color, 2, 3), sep = "")
    }
    color = tolower(paste("#", tra, color, sep=""))
    
  }
return(color)
} 
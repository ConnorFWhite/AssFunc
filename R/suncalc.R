sunCalc<-function(d,lat,long){
  if(any(is.null(c(d,lat,long)))){
    stop("day, latitude and Longitude must be supplied")
  }
  date<-format(d,format="%Y-%m-%d")
  
  d<-as.numeric(format(d,format="%j"))

  ## Function to convert degrees to radians
  rad<-function(x)pi*x/180
  
  ##Radius of the earth (km)
  R=6378
  
  ##Radians between the xy-plane and the ecliptic plane
  epsilon=rad(23.45)
  
  ##Convert observer's latitude to radians
  L=rad(lat)
  
  ## Calculate offset of sunrise based on longitude (min)
  ## If Long is negative, then the mod represents degrees West of
  ## a standard time meridian, so timing of sunrise and sunset should
  ## be made later.
  timezone = -4*(abs(long)%%15)*sign(long)
  
  ## The earth's mean distance from the sun (km)
  r = 149598000
  
  theta = 2*pi/365.25*(d-80)
  
  z.s = r*sin(theta)*sin(epsilon)
  r.p = sqrt(r^2-z.s^2)
  
  t0 = 1440/(2*pi)*acos((R-z.s*sin(L))/(r.p*cos(L)))
  
  ##a kludge adjustment for the radius of the sun
  that = t0+5 
  
  ## Adjust "noon" for the fact that the earth's orbit is not circular:
  n = 720-10*sin(4*pi*(d-80)/365.25)+8*sin(2*pi*d/365.25)
  
  ## now sunrise and sunset are:
  sunrise = (n-that+timezone)/60
  sunset = (n+that+timezone)/60
  
  srH<-floor(sunrise)
  srM<-floor((sunrise-srH)*60)

  ssH<-floor(sunset)
  ssM<-floor((sunset-ssH)*60)
  
  SR<-paste(date,paste(srH, srM,sep=":"),sep=" ")
  SS<-paste(date,paste(ssH, ssM,sep=":"),sep=" ")
  
  return(list("sunrise" = SR,"sunset" = SS))
}
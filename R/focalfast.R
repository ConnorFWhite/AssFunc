focalfast<-function(r,window.size, fun=function(x){mean(x,na.rm=TRUE)}){
  buf<-(window.size-1)/2
  x <- as.matrix(r) #convert raster to a matrix so it can be used in the foreach loop
  r[!is.na(r[])]<-1
  nrowdat <- nrow(x)
  ncoldat <- ncol(x)
  
  y <- t(x) #transpose the matrix so that the correct lines will be read in through the embed function
  x[]<-NA
  col.num = window.size - 1 #the number to add to the columns in the loop
  loc<-(buf+1):(ncoldat-(buf))
  
  for(i in 1:(nrowdat-col.num)){
    x[i+buf,loc]<-apply(embed(y[,i:(i+col.num)], window.size), 1, fun)
  }
  x<-raster(x,template=r)
  x<-x*r
  return(x)
}


focalLocations<-function(focal=1){
  cbind(rep(seq(-1*focal,length.out = (focal*2)+1,by=1),(focal*2 +1)),
        rep(seq(-1*focal,length.out = (focal*2)+1,by=1),each=(focal*2 +1)))
}


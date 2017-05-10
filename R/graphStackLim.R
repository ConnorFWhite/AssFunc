
graphStackLim<-function(min=NULL,max=NULL,data=NULL,perc=NULL){
  if(is.null(min)|is.null(max)){
    min<-min(data,na.rm=TRUE)
    max<-max(data,na.rm=TRUE)
  }
  dif<-(max-min)
  dif<-dif/c(perc[2]-perc[1])
  lims<-c(min-(dif*perc[1]),max+(dif-dif*perc[2]))
  return(lims)
}
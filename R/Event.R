event<-function(x,clip="none",na.rm=FALSE, nas=FALSE,duration=NULL){
  NAs<-is.na(x)
  if(sum(NAs)==length(x)){
    stop("All input values are NA, to identify NA's use is.na(x)")
  }
  if(na.rm==TRUE){
    if(any(NAs==TRUE)){
      NAloc<-which(NAs==TRUE)
      x[NAloc]<-nas
    }
  }else{
    if(any(NAs==TRUE)){stop("There are NA's in the data try na.rm=TRUE")}
  }
  last<-length(x)
  if(clip=="none"){
    firstv<-x[1]
    lastv<-x[last]
    first<-1
    last<-length(x)
    if(firstv==TRUE){stop("First Value cannot be true, try clip=TRUE ")
    }else if(lastv==TRUE){stop("Last Value cannot be true,try clip=TRUE")
    }
  }else if(clip=="buffer"){
    x<-c(FALSE,x,FALSE)
    first<-1
    last<-length(x)
  }else{
    falses<-which(x==FALSE)
    first<-falses[1]
    last<-falses[length(falses)]
  }
  edge<-which(((x[first:(last-1)]==TRUE)+(x[(first+1):last]==TRUE))==1)
  edge<-edge+first-1
  if(length(edge)==0){stop("There are no events where this occurs")}
  start<-edge[seq(1,length(edge),by=2)]
  end<-edge[seq(2,length(edge),by=2)]
  start<-start+1
  if(clip=="buffer"){
    start<-start-1
    end<-end-1
  }
  if(is.numeric(duration)){
    dur<-end-start
    loc<-which(dur>=duration)
    start<-start[loc]
    end<-end[loc]
  }
  return(cbind(start,end))
}


eventInterp<-function(dat,x=NULL,events=NULL,clip="none",na.rm=FALSE,nas=FALSE){
  if(is.numeric(dat)==FALSE){stop("data must be numeric to be interpolated")}
  
  if(!is.null(x)){
    loc<-event(x,clip=clip,nas=nas,na.rm=TRUE)
  }else{
    loc<-events
  }
  
  leng<-loc[,2] - loc[,1]+2
  for(i in 1 : nrow(loc)){
    dif<-(dat[loc[i,2]+1]-dat[loc[i,1]-1])/leng[i]
    if(leng[i]==2){
      dat[loc[i,1]]<-(dat[loc[i,1]-1]+dif)
    }else{
      dat[loc[i,1]:loc[i,2]]<-seq((dat[loc[i,1]-1]+dif),(dat[loc[i,2]+1]-dif), by=dif)
    }
  }
  return(dat)
}



eventFunc<-function(dat,events,fun=function(x){mean(x,na.rm=TRUE)}){
  if(is.numeric(dat)==FALSE){stop("data must be numeric to be interpolated")}
  out<-apply(events,1,FUN = function(rows){
    sub<-dat[rows[1]:rows[2]]
    return(fun(sub))
  })
  return(out)
}



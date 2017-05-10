library(AssFunc)

#Load in acoustic data VUE export
wd<-setwd("C:/Users/Connor/Documents/Particle_Filter/")
setwd(wd)
dat<-read.csv("VUE_Export_cleaned_20150824.csv")

#Turning into POSIX time
dat$DateTime<-as.POSIXct(dat$ï..Date.and.Time..UTC., format="%Y-%m-%d %H:%M:%S",tz="UTC")

#inneficient but convienient way to generate a list of the location of all receivers
recLoc<-locMap(state_rec = dat$Station.Name,Lats = dat$Latitude,Longs = dat$Longitude)

#just subset for one individual
datInd<-dat[which(dat$Transmitter=="A69-1601-19999"),]

#Making a network or transition matrix for the individual selected
#Include the states argument so that is includes all receivers and not jsut the receivers the individual was detected on
tMat<-transMat(State_rec = datInd$Station.Name,States = recLoc[,1])

#Visualizing the network defaults to plotting in a circle
transBub(tMat)

#Can plot them spatially if you supply locs, or add them to a plot if you have add=TRUE
transBub(tMat,locs = recLoc)
points(recLoc[,2]~recLoc[,3],pch=".")


#create log file for mean position
#Time_Step argument says how long you want to generate each step, so hourly would be 3600, daily would be 3600*24
log<-logGen(state_rec = datInd$Station.Name,times = datInd$DateTime,Time_Step = 3600*24,
       states = recLoc[,1],start=as.POSIXct("2014-09-27",tz="UTC"),end=as.POSIXct("2015-09-27",tz="UTC"))

#This generated a matrix with each column representing a receiver and each row representing a time step, so a day
#the matrix is populated by the number of detections on each receiver during that time step


#This function calculates mean position,
#just multiples the position in recLoc by the number of times it was detected
fishMean<-meanPos(log,recLoc = recLoc)

#The above would be calculating the mean position in lat and long, which is wrong since you cant really use
#those means, but the spatial extent we work over it is usually not a bad assumption
#if you change the unties in rec loc to be UTM or somehting it will work better

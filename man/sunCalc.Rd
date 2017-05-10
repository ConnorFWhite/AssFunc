\name{sunCalc}
\alias{sunCalc}

\title{
Sunrise and sunset time
}
\description{
given the latitude and longitude of any day calculate the time of sunrise and sunset. Aquired from..... the internet
}
\usage{
sunCalc(d, lat, long)
}
\arguments{
  \item{d}{
POSIX item representing the day of interest
}
  \item{lat}{
Latitude of location in decimal degrees
}
  \item{long}{
Longitude of location in decimal degrees
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns a list with two levels
  \item{sunrise }{a vector in the list that is composed of the hour of the day in decimal hours that the sun will rise}
  \item{sunset }{a vector in the list that is composed of the hour of the day in decimal hours that the sun will set}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Connor F. White
}
\note{
%%  ~~further notes~~
}


\seealso{

}
\examples{
day<-as.POSIXct("2016-01-01")
sunCalc(day,lat=33.5,long=-118.5)

#if we move the location
sunCalc(day,lat=40.5,long=-115)


#Or the values for the entire year
end<-as.POSIXct("2016-12-31")
days<-seq(day,end,by="day")
sunCalc(d=days,lat=33.5,long=-118.5)




}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

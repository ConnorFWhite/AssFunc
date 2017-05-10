\name{focalfast}
\alias{focalfast}
\title{
Calculate focal Analysis
}
\description{
Execute a funciton over a focal window
}
\usage{
focalfast(r, window.size, fun = function(x) {
    mean(x, na.rm = TRUE)
})
}

\arguments{
  \item{r}{
a raster
}
  \item{window.size}{
the number of cells that you would like the focal window to be, must be odd
}
  \item{fun}{
function to calculate in the focal analysis
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a raster
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Connor F. White and Meghan Blumstein
}
\note{
%%  ~~further notes~~
}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

#library(raster)
#r<-matrix(rnorm(10000),nrow=100,ncol=100)
#r<-raster(r)

#map<-focalfast(r=r,windowsize=4,fun=mean)

#image(map)
}

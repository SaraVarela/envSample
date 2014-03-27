\name{envSample}
\alias{envSample}
\title{resample the raw data to eliminate the biases in the environmental space.}
\usage{
envSample (coord, filters, res, do.plot)
}
\arguments{
  \item{coord}{a dataframe with the coordinates of the raw
  species occurrences. The first column should be the
  longitude (x) and the second column the latitude (y).}

  \item{filters}{a list of vectors with the values of the
  variables for the species occurrences (users can get
  these vectors by using the function extract from the
  raster R-package)}

  \item{res}{a list of numbers that set the size of the
  grid to filter the data for each variable}

  \item{do.plot}{TRUE/FALSE. If FALSE, the function does
  not return a plot showing the raw and filtered data
  sets.}
}
\description{
Ecological Niche Model's predictions could be improved by
using unbiased data sets for calibrating the algorithms.
With this function users could filter their raw species
occurrences to avoid environmental biases. The user should
select which variables (filters) are adequate for filtering
the raw species occurrences: e.g. selecting the variables
that are important for the species, or, in case there is no
information about the importance of the variables,
selecting variables that are not correlated: e.g. annual
mean temperature (bio1) and annual precipitation (bio12).
}
\examples{
\dontrun{
file <- paste(system.file(package="dismo"), "/ex/bradypus.csv", sep="")
bradypus <- read.table(file, header=TRUE, sep=',')
coord<- bradypus [,2:3]
setwd ("yourdirectory/worldclim)
var<- list.files (pattern=".bil")
wc<- stack (var)
data<- extract (wc, coord)
data<- as.data.frame (data)
envSample (coord, filters=list (data$bio1, data$bio12), res=list (20, 200), do.plot=TRUE)
}
}


library (raster)
library (rgdal)
library (sqldf)
library (maps)
library (testthat)
library (roxygen2)

#' resample the raw data to eliminate the biases in the environmental space. 
#' 
#' Ecological Niche Model's predictions could be improved by using unbiased data sets 
#' for calibrating the algorithms. 
#' With this function users could filter their raw species occurrences to avoid environmental biases.
#' The user should select which variables (filters)
#'  are adequate for filtering the raw species occurrences:
#' e.g. selecting the variables that are important for the species, or, in case there is no
#' information about the importance of the variables, selecting variables that are not correlated:
#' e.g. annual mean temperature (bio1) and annual precipitation (bio12).  
#' 
#' @usage envSample (coord, filters, res, do.plot)
#' 
#' @param coord a dataframe with the coordinates of the raw species occurrences. 
#' The first column should be the longitude (x) and the second 
#' column the latitude (y). 
#' @param filters a list of vectors with the values of the variables for the species occurrences 
#' (users can get these vectors by using the function extract from the raster R-package) 
#' @param res a list of numbers that set the size of the grid to filter the data for each variable
#' @param do.plot TRUE/FALSE. If FALSE, the function does not return a plot showing the raw and filtered data sets.
#' 
#' @examples \dontrun{
#' file <- paste(system.file(package="dismo"), "/ex/bradypus.csv", sep="")
#' bradypus <- read.table(file, header=TRUE, sep=',')
#' coord<- bradypus [,2:3]
#' setwd ("yourdirectory/worldclim)
#' var<- list.files (pattern=".bil")
#' wc<- stack (var)
#' data<- extract (wc, coord)
#' data<- as.data.frame (data)
#' envSample (coord, filters=list (data$bio1, data$bio12), res=list (20, 200), do.plot=TRUE)
#' }
#' 

envSample<- function (coord, filters, res, do.plot=TRUE){
  
  n<- length (filters)
  pot_points<- list ()
  for (i in 1:n){
    k<- filters [[i]] [!is.na(filters[[i]])]
    ext1<- range (k)
    ext1 [1]<- ext1[1]- 1
    x<- seq(ext1[1],ext1[2], by=res[[i]])
    pot_points[[i]]<- x
  }
  pot_p<- expand.grid(pot_points)
  
  ends<- NULL
  for (i in 1:n){
    fin<- pot_p [,i] + res[[i]]
    ends<- cbind (ends, fin)
  }
  
  pot_pp<- data.frame (pot_p, ends)
  pot_pp<- data.frame (pot_pp, groupID=c(1:nrow (pot_pp)))
  rows<- length (filters[[1]])
  filter<- data.frame(matrix(unlist(filters), nrow=rows))
  real_p<- data.frame (coord, filter)
  
  names_real<- c("lon", "lat")
  names_pot_st<- NULL
  names_pot_end<- NULL
  sql1<- NULL
  for (i in 1:n){
    names_real<- c(names_real, paste ("filter", i, sep=""))
    names_pot_st<- c(names_pot_st, paste ("start_f", i, sep=""))
    names_pot_end<- c(names_pot_end, paste ("end_f", i, sep=""))
    sql1<- paste (sql1, paste ("real_p.filter", i, sep=""), sep=", ")   
  }
  
  names (real_p)<- names_real
  names (pot_pp)<- c(names_pot_st, names_pot_end, "groupID")
  
  conditions<- paste ("(real_p.filter", 1, "<= pot_pp.end_f", 1,") and (real_p.filter", 1, "> pot_pp.start_f", 1, ")", sep="")
  for (i in 2:n){
    conditions<- paste (conditions, 
                        paste ("(real_p.filter", i, "<= pot_pp.end_f", i,") and (real_p.filter", i, "> pot_pp.start_f", i, ")", sep=""), 
                        sep="and")
  }
  
  selection_NA<- sqldf(paste ("select real_p.lon, real_p.lat, pot_pp.groupID",   
                        sql1, "from pot_pp left join real_p on", conditions, sep=" "))

  selection<- selection_NA [complete.cases(selection_NA),]
  
  final_points<- selection[!duplicated(selection$groupID), ]
  coord_filter<- data.frame (final_points$lon, final_points$lat) 
  names (coord_filter)<- c("lon", "lat")
  
  if (do.plot==TRUE){
    par (mfrow=c(1,2), mar=c(4,4,0,0.5))
  plot (filters[[1]], filters[[2]], pch=19, 
        col="grey50", xlab="Filter 1", ylab="Filter 2")
  points (final_points$filter1, final_points$filter2, 
          pch=19, col="#88000090")
  plot (coord, pch=19, col="grey50")
  map(add=T)
  points (coord_filter, pch=19, col="#88000090")
  
  }
  coord_filter
}




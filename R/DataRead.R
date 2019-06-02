read.timeseries <- function(DateTemp.file, convertDate = TRUE, clean.depths=TRUE, datefmt = "%d/%m/%Y %H:%M", datecol = "Date", div.by=1000, ...){
# Reads-in ground temperature data with a single date column and multiple temperature columns,
  # convert date returns dates as POSIX using the format string specified in datefmt
  # clean.depths converts typical column names (e.g. "2.3m", "X3 m", "5.0000 metres") into numbers for easy plotting
  # datecol  specifies the name of the column with date information
  # div.by permits the conversion of depths to another unit (default input is mm)
  
  soil <- read.csv(DateTemp.file, header=TRUE, sep=',', ...)
  remove.cols <- c("Simulation_Period", "Run", "IDpoint", "No", "X.HK.Bat.V", "X.HK.Temp.oC") # column names to exclude
  soil <- soil[,!names(soil) %in% remove.cols]

  if (clean.depths){
  names(soil) <- clean.depths(names(soil), div.by = div.by)
  }
  if (convertDate){
    soil[,datecol] = as.POSIXct(soil[,datecol], format = datefmt, tz="UTC")
    }
  return(soil)
}

clean.depths <- function(list.of.depths, div.by=1000, ignore=c("Date", "Time")){
 # takes depth values with extra "stuff" and trims them down to numbers
 # Converts from mm to m by default (as per GEOtop)
  clean.depths <- as.numeric( gsub("[A-Za-z]*([0-9]*\\.?[0-9]*).*", "\\1",   #  "[A-Za-z]*([0-9]*)\\.?0*[A-Za-z]*", "\\1",
      list.of.depths[!list.of.depths %in% ignore])) / div.by
  
  clean.depths <- as.character(round(clean.depths, 3))
    
  clean.depths <- c(list.of.depths[list.of.depths %in% ignore], clean.depths)
  return(clean.depths)
  }
 
 
build.trumpet <- function(temperature.df, melt=FALSE) {
    #Build max, mean and min vectors from data frame of temperatures,
#    ignoring any non-finite values in the matrix.
#    names() must be numeric
  # input must be a data frame where the names of the columns are depths (numbers only!)
  # and data are temperature data.
  # melt returns data in long format (1 row = 1 observation, instead of multiple per row)
  #  negative depths are returned  for easy plotting
  
     depths <- -abs(as.numeric(names(temperature.df)))
     max_T  <- sapply(temperature.df, function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA))
     min_T  <- sapply(temperature.df, function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA))
     mean_T <- sapply(temperature.df, function(x) ifelse( !all(is.na(x)), mean(x, na.rm=T), NA))
     PFstat <- cbind(depths,data.frame(max_T,min_T,mean_T))
     PFstat <- PFstat[rev(order(PFstat$depths)),]
     if (melt){
     PFstat <- melt(PFstat, id="depths")
     }
     return(PFstat)
}


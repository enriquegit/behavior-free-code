# Auxiliary functions for Exploring and Visualizing chapter.


computeActivityHour <- function(datapath, type="control"){
  # Computes the mean activity level per hour and returns it as a matrix.
  # type: control or condition.
  
  days <- read.csv(file.path(datapath,"scores.csv"))
  
  filelist <- list.files(file.path(datapath,type))
  
  n <- length(filelist)
  
  TOTAL <- matrix(data = 0,nrow = 24, ncol = 7)
  
  for(f in filelist){
    
    M <- matrix(data = 0,nrow = 24, ncol = 7)
    
    A <- matrix(data = 0.001,nrow = 24, ncol = 7)
      
    df <- read.csv(file.path(datapath,type,f), stringsAsFactors = F)
    
    # Select study days only.
    id <- strsplit(f,"\\.")[[1]][1]
    studydays <- days[which(days$number == id),"days"]
    dates <- unique(df$date)
    ndates <- length(dates)
    ndates <- min(studydays, ndates)
    dates <- dates[1:ndates]
    idxs <- which(df$date %in% dates)
    df <- df[idxs,]
    
    timestamp <- strptime(df$timestamp, "%Y-%m-%d %H:%M:%S", tz = "")
    hour <- as.integer(format(timestamp,"%H"))
    weekday <- as.integer(format(timestamp, "%w"))
    
    for(i in 1:length(hour)){
      M[hour[i]+1,weekday[i]+1] <- df$activity[i] + M[hour[i]+1,weekday[i]+1]
      A[hour[i]+1,weekday[i]+1] <- A[hour[i]+1,weekday[i]+1] + 1
    }
    
    M <- M / A
    
    TOTAL <- TOTAL + M
  }
  
  strWeekdays <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
  colnames(TOTAL) <- strWeekdays
  rownames(TOTAL) <- 0:23
  
  return(TOTAL/n)
}


normalizeMatrices <- function(M1, M2){
  # Normalizes activity level matrices M1 and M2 so both can be compared.
  
  maxv <- max(max(M1), max(M2))
  minv <- min(min(M1), min(M2))
  
  M1 <- (M1 - minv) / (maxv - minv)
  M2 <- (M2 - minv) / (maxv - minv)
  
  return(list(M1=M1, M2=M2))
}


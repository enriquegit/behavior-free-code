source(file.path("..","auxiliary_functions","globals.R"))


# NOTE: This script works on the raw data file WISDM_ar_v1.1_raw.txt.
# Before running this script, two formatting issues need to be corrected in the original file.

# Issue 1: Some lines (approx. 7) are missing the new line separator so they look like these:
# 30,Standing,1729732341000,-1.8,9.28,3.79;34,Walking,295812327000,0.8,18.96,13.57;
# That is, two recordings in one row.
# This can be fixed by finding a ';' followed by a number and inserting a new line before the number.

# Issue 2: Several lines end with ',;' and this can be fixed by replacing ',;' -> ';'

# Finally, replace all ';' with '' (empty string).

# I have made these corrections manually using the find/replace function of a text editor.

# The SMARTPHONE ACTIVITIES dataset distributed with the book is already fixed.

constants <- list()

constants$srcpath <- file.path(datasets_path,"smartphone_activities","WISDM_ar_v1.1_raw.txt")

constants$destpath <- file.path(datasets_path,"smartphone_activities/")

constants$samplingRate <- 20 # hz. For information purposes. Do not change.

constants$winsize <- 100 # Window size.

constants$overlap <- 1 # 1 = no overlap;


computeImage.3d <- function(){
  # Computes the image representation of an activity by stacking x, y and z.
  # Then, it saves the result in images.txt.
  
  dirpath <- file.path(constants$destpath,constants$winsize)
  if(dir.exists(dirpath) == F){
    dir.create(dirpath)
  }
  
  df <- read.table(constants$srcpath, header = F, sep = ",", stringsAsFactors = F, col.names = c("userid","label","timestamp","x","y","z"))
  
  # Just keep users that performed all activities
  t <- as.matrix(table(df$userid, df$label))
  completeUsers <- as.integer(which(apply(t, 1, function(row){length(which(row != 0))}) == 6))
  idxs <- which(df$userid %in% completeUsers)
  df <- df[idxs,]
  
  labels <- unique(df$label)
  
  
  file.remove(file.path(dirpath,"images.txt"))
  fileConn <- file(file.path(dirpath,"images.txt"), open = "wt")
  
  #write header
  vnames <- c(paste0("v",1:(constants$winsize*3)), "userid", "label")
  writeLines(paste(vnames, collapse = ","), fileConn, sep = "\n")
  
  users <- unique(df$userid)
  for(u in users){
    print(paste0("Processing user ", u))
    
    for(label in labels){
      
      tmp <- df[df$label == label & df$userid == u,]
      n <- nrow(tmp)
      lb <- seq(1,n, by = constants$winsize * constants$overlap)
      lb <- lb[1:(length(lb) - 2)]
      
      c <- 0
      total <- length(lb)
      
      for(i in 1:total){
        start <- lb[i]
        block <- tmp[start:(start+constants$winsize-1),]
        
        allAxes <- c(block$x, block$y, block$z, u, label)
        
        writeLines(paste(allAxes, collapse = ","), fileConn, sep = "\n")
        c <- c + 1
      }
      
    }
  }
  close(fileConn)
  
}


computeImage.3d()

corr <- function (directory, threshold = 0) {
# 'directory' is a character vector of length 1 indicating the location 
#  files
        
# 'threshold' is a numeric vector of length 1 indicating the number of 
#  complete observations required to compute the correlation between nitrate and sulfate
#  default is zero
        
# Return a numeric vector of correlations
## some variables
        num_file <- length(dir(directory))
        vect_correlations <- c()
##Let's get the files and the number of complete observations for all files
        complete_observations <- complete(directory, 1:num_file)
## Select the ones where the number of complete observations > threshold
        above_threshold <- complete_observations[complete_observations[,"nobs"]>threshold,]      

## Now in above_threshold we have a data frame, first column is number of file, 
## second one number of complete observations, greater than threshold
         
         num_files <- dim (above_threshold)[1]
         if (num_files <= 0) {                        ## No files with complete obs
                 return(vect_correlations)       ## greater than threshold
         }                                                  
                  
         # Let's find the name of file K
        namefile_k <- function (k) {
                if(k > 99) {
                        namefile_k <- paste(k,".csv", sep="")
                } else {
                        if(k > 9) {
                                namefile_k <- paste("0",k,".csv", sep="")
                        } else {
                                namefile_k <- paste("00",k,".csv",sep="")
                        }
                }
        }
        
        dirinit <- getwd()              # Now we have to go to the directory to open the files
        if (dirinit != directory)  {    # if we're alreay in directory, no need to change
                setwd(directory)        # otherwise change directory
        }
        csulfnit <- c()
        for (i in 1:num_files) {
                j <- above_threshold [i,1]
                file_k <- namefile_k(j)
                data.file_k <- read.csv(file_k)
                NOT_NA_sulfate_in_datafile_k <- data.file_k[!is.na(data.file_k[,"sulfate"]),]
                NOT_NA_pollutant_in_datafile_k <- NOT_NA_sulfate_in_datafile_k[!is.na(NOT_NA_sulfate_in_datafile_k[,"nitrate"]),]
                v_sulfato_k <- NOT_NA_pollutant_in_datafile_k[,-c(1,3,4)]
                v_nitrato_k <- NOT_NA_pollutant_in_datafile_k[,-c(1,2,4)]
                csulfnit_k <- cor(v_sulfato_k,v_nitrato_k)
                csulfnit <- c (csulfnit, csulfnit_k)
        }
        dirnow <- getwd()   # present directory. If we have changed directory at the begining 
                            # let's go back
        if (dirinit != dirnow)  {
                setwd(dirinit) 
        }
        vect_correlations <- csulfnit  
}        
        

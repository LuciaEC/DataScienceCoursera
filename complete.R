complete <- function (directory, id = 1:332) {
        dirinit <- getwd()          # actual directory
        if (dirinit != directory)  {
                setwd(directory)   # change directory if dirinit 
        }                          # isn't directory
       complete_names <- c("id","nobs") # initial variables  
       number_of_files <- length(id)
       my_matrix  <- matrix(0,number_of_files,2)

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
# Let's find the number of complete observations of both pollutants in data.file
        Obs_of_both_pollutant_in_datafile_k <- function (datafile_k) {
                NOT_NA_sulfate_in_datafile_k <- datafile_k[!is.na(datafile_k[,"sulfate"]),]
                NOT_NA_pollutant_in_datafile_k <- NOT_NA_sulfate_in_datafile_k[!is.na(NOT_NA_sulfate_in_datafile_k[,"nitrate"]),]
                Obs_of_both_pollutant_in_datafile_k <- dim(NOT_NA_pollutant_in_datafile_k)
                }
# Let's get for every file in id the number of complete observations of both pollutants
        for(i in 1:number_of_files) {
        j <- id[i]
        filej <- namefile_k(j)
# Open filej
        data.filek <- read.csv(filej)
# Obtain the the number of complete observations of both pollutants in data.namefile
        Obsj <- Obs_of_both_pollutant_in_datafile_k(data.filek)
        my_matrix [i,1] <- j
        my_matrix [i,2] <- Obsj[1]
        }
       
        dirnow <- getwd()              # present directory
        if (dirinit != dirnow)  {
                setwd(dirinit)         #back to original dir
        }
        my_MATRIX <- data.frame(my_matrix)
        colnames(my_MATRIX) <- complete_names
        my_MATRIX
}
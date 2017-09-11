# pollutantmean <- function(directory, pollutant, id = 1:332)
# 'directory' is a character vector of length 1 indicating the location 
# files

# 'pollutant' is a a character vector of length 1 indicating the name
# of the pollutant for wich we will calculate the mean; either "sulfate" or 
# "nitrate"

# id is an integer vector indicating the monitor ID numbers to be used 
# Return the mean of the pollutants across all monitors list in id vector,
# ignoring NA
pollutantmean <- function(directory, pollutant, id = 1:332){
        print(R.version.string)
        dirinit <- getwd()   # actual directory
        if (dirinit != directory)  {
                setwd(directory) 
        }
        pollutant.sum <- 0           # initial variables           
        pollutant.val <- 0
        numberoffiles <- length(id)
        # Let's find the name of file K
        namefile <- function (k) {
                if(k > 99) {
                        namefile <- paste(k,".csv", sep="")
                } else {
                        if(k > 9) {
                                namefile <- paste("0",k,".csv", sep="")
                        } else {
                                namefile <- paste("00",k,".csv",sep="")
                        }
                }
        }
        # Let's find the sum of the non NA values of the pollutant z in data.file
        sumoffileKpollutantz <- function (data.file,z) {
                noNAinpollutant.data <- data.file[!is.na(data.file[,z]),]
                sum(noNAinpollutant.data[,z])
        }
        # Let's find the number of non NA values of the pollutant z in data.file
        lengthoffileKpollutantz <- function (data.file,z) {
                noNAinpollutant.data <- data.file[!is.na(data.file[,z]),]
                length(noNAinpollutant.data[,z])
        }
        # Let's add for every file in id the sum of the non NA values for the pollutant z in data.file
        # and the number of non NA values for the pollutant z in data.file
        for(i in 1:numberoffiles) {
                j <- id[i]
                filej <- namefile(j)
                # Open filej
                data.namefile <- read.csv(filej)
                # increase the sum of non NA values for 'pollutant'
                # increase the number of non NA values for 'pollutant'
                pollutant.sum <- pollutant.sum + sumoffileKpollutantz(data.namefile,pollutant)
                pollutant.val <- pollutant.val + lengthoffileKpollutantz(data.namefile,pollutant)
        }
        dirnow <- getwd()   # present directory
        if (dirinit != dirnow)  {
                setwd(dirinit) 
        }
        pollutant.sum/pollutant.val  # calculate the mean of the non NA values for 'pollutant' in id files
}
	## Name: pollutantmean
	## Author: Dann Hekman (dannhek@gmail.com)
	## Purpose: Read in a series of tables from a given directory
	##          and find the mean of available data for specified columns
	##          full details, specifications and data available here:
	##			https://class.coursera.org/rprog-002/assignment/view?assignment_id=3
	## Parameters:
	##		'directory' is the directory in which to find the ID files. 
	##			If none specified, use the current working directory.
	##		'pollutant' is a character vector of length 1 indicating
	##			the name of the pollutant for which we will calculate the
	## 			mean; either "sulfate" or "nitrate".
	##		'id' is an integer vector indicating the monitor ID numbers
	## 			to be used. Used to read in file names which are in the format
	##			###.csv where ### is a 0-padded, three-digit monitor ID
	## Returns:
	##		'means' The mean of the pollutant across all monitors listed
	## 			in the 'id' vector (ignoring NA values)
pollutantmean <- function(directory, pollutant, id = 1:332) {
    if ((pollutant!="sulfate")&&(pollutant!="nitrate")) return(NA)
	idForFile<-formatC(id,digits=2,format="d",flag=0)
	allNum<-NA
	for(i in seq_along(id)) {
		data<-read.csv(paste(c(directory,"/",idForFile[i],".csv"),collapse=""))
		if (pollutant=="sulfate") {polVect<-data$sulfate}
		else if (pollutant=="nitrate") {polVect<-data$nitrate}
		else {polVect<-vector(NA,length=length(data$Date))}
		allNum<-c(allNum,polVect)
		allNum<-allNum[!is.na(allNum)]
	}
	return(round(mean(allNum,na.rm=TRUE),3))
}

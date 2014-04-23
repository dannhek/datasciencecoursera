	## Name: corr
	## Author: Dann Hekman (dannhek@gmail.com)
	## Purpose: Read in a series of tables from a given directory
	##          and find the corrlation between available data for specified columns
	##          full details, specifications and data available here:
	##			https://class.coursera.org/rprog-002/assignment/view?assignment_id=3
	## Parameters:
	##		'directory' is the directory in which to find the ID files. 
	##			If none specified, use the current working directory.
	##		'threshold' is used to filter out monitors with too few data
	##			points to get a real correlation.
	## Returns:
	##		'corrAry' The correlations between sulfate and nitrate across all 
	##			monitors listed that have enough observations to meet the threshold
	##------------
corr <- function(directory, threshold = 0) {
	id<-1:332
	corrAry<-as.numeric(vector(length=0))
	idForFile<-formatC(id,digits=2,format="d",flag=0)
	numObs<-complete(directory)
	for(i in seq_along(id)) {
		if (numObs[i,2]<=threshold) {corrAry[i]<-NA}
		else {
			data<-read.csv(paste(c(directory,"/",idForFile[i],".csv"),collapse=""))
			dataClean<-subset(data,!is.na(sulfate) & !is.na(nitrate))
			corrAry[i]<-cor(dataClean$sulfate,dataClean$nitrate)
		}
	}
	corrAry<-corrAry[!is.na(corrAry)]
	return(corrAry)
}
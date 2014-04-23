	## Name: complete
	## Author: Dann Hekman (dannhek@gmail.com)
	## Purpose: Read in a series of tables from a given directory
	##          and find the number of rows with complete data for all 
	##			specified columns full details, specifications and data available here:
	##			https://class.coursera.org/rprog-002/assignment/view?assignment_id=3
	## Parameters:
	##		'directory' is the directory in which to find the ID files. 
	##			If none specified, use the current working directory.
	##		'id' is an integer vector indicating the monitor ID numbers
	## 			to be used. Used to read in file names which are in the format
	##			###.csv where ### is a 0-padded, three-digit monitor ID
	## Returns:
	##		'numberObs' is a dataframe with two columns: 
	##		ID and number of complete observations (nObs)
	##------------
complete <- function(directory=getwd(), id = 1:332) {
	idForFile<-formatC(id,digits=2,format="d",flag=0)
	#----Start by Building a Matrix; we'll convert to DF later----
	numberObs<-matrix(nrow=length(id),ncol=2)
	for(i in seq_along(id)) {
		data<-read.csv(paste(c(directory,"/",idForFile[i],".csv"),collapse=""))
		dataClean<-subset(data,!is.na(sulfate) & !is.na(nitrate))
		numberObs[i,1]<-id[i]
		numberObs[i,2]<-length(dataClean$Date)
	}
	#----Now that we've got a matrix with the data, let's coerce it into DF----
	numberObs<-data.frame(numberObs)
	names(numberObs)<-c("id","nobs")
	return(numberObs)
}
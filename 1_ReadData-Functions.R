# 
# 
# Author: E620927
###############################################################################
options(java.parameters = "-Xmx2000m" )
library(ggdendro)
library(RODBC)
library(zoo)
library(xlsx)
library(xts)
library(xtable)
library(ggplot2)
library(gridExtra)
library(grid)
library(car)

convertDateToNumeric <- function(string) {
#	string must be like 
#	string <- Calibration_END_DATE
	#function: convert "2017 Q2" like string to a numeric 2017.25.
	#parameter: string is a date like string in the format of "XXXX QX"
	#return: NULL. Variables are creted in global environment.
	
	return(as.numeric(as.yearqtr(string, format = "%Y Q%q")))
}

convertNumericToDate <- function(number) {
	if (MONTHLY_VERSION) {
		return(as.Date(as.yearmon(number+1/12))-1)
	} else {
		return(as.Date(as.yearqtr(number+0.25))-1)
	}
}

trimSeriesToQuarterStart <- function(series) {
#		series <- data_citi_index
	series_tmp <- na.omit(series)
	firstdate <- index(series_tmp)[1]
	if ((firstdate - as.Date(as.yearqtr(firstdate), frac = 0))/90>0.1) {
		cutoff <- as.Date(as.yearqtr(firstdate)+0.25)
		print(paste0("trimed to quarter start: ",cutoff))
#		series[index(series)<cutoff] <- NA
		return(series[index(series)>=cutoff])
	} else {
		return(series)
	}
}

trimSeriesToMonthStart <- function(series) {
#		series <- data_jpm_index
	firstdate <- index(series)[1]
	if ((firstdate - as.Date(as.yearmon(firstdate), frac = 0))/30>0.2) {
		print(paste0("trimed to month start: ",as.Date(as.yearmon(firstdate)+1/12)))
		return(series[index(series)>as.Date(as.yearmon(firstdate)+1/12)])
	} else {
		return(series)
	}
}

combineCurves <- function(curves){
#	curves <- PC_CURVES
	if (length(curves) > 1){
		for (i in 1:(length(curves)-1)){
#			i <- 1
			if (i == 1) {
				curves_combined = ts.union(curves[[1]],curves[[2]])
				#		colnames(curves_combined)[1:2] = names(curves)[1:2]
			} else {
				curves_combined = ts.union(curves_combined,curves[[i+1]])
				#		colnames(curves_combined)[i+1] = names(curves)[i+1]
			}
		}
		colnames(curves_combined) <- names(curves)
#		curves_combined = na.omit(curves_combined)
		return(curves_combined)
	} else {
		curves_combined = ts.union(curves[[1]],curves[[1]])[,1]
#		curves_combined = na.omit(curves_combined)
		return(curves_combined)
	}
}

selectCurves <- function(CSI_CURVES, csi) {
	csi_tmp <- as.character(csi)[as.character(csi)%in%names(CSI_CURVES)]
	return(CSI_CURVES[csi_tmp])
}

retrieveCSICurves <- function(I_MOD_CSI_AC, I_HIST_CSI) {
#	I_MOD_CSI_AC <- I_MOD_CSI_AC_prev
#	I_HIST_CSI <- I_HIST_CSI_prev
	
	CSI_NAMES_ALL <- unique(I_HIST_CSI$CSI_ID)
	
	CSI_CURVES <- list()
	
	for(i in 1:length(CSI_NAMES_ALL)) {
		#	i = 1;
		csi = as.character(CSI_NAMES_ALL[i])
		history <- I_HIST_CSI[I_HIST_CSI$CSI_ID == csi,]
		history$DATE <- as.Date(history$DATE,format="%m/%d/%Y")
		history <- na.omit(history[order(history$DATE),])
		
		data <- as.numeric(as.character(history$CSI_VALUE))
		CSI_CURVES[[i]] <- ts(data,start=determineTsStartDate(history$DATE),frequency=determineTsFrequency(history$DATE))
		names(CSI_CURVES)[i] <- as.character(csi)
		print(paste(c(csi,range(index(CSI_CURVES[[i]]))),collapse="/"))
	}
	return(CSI_CURVES)
}

extract_ts_from_df <- function(datatable,date_column,value_column,name_column) {
#	datatable <- I_HIST_CSI
	datatable[,date_column] <- as.Date(datatable[,date_column],format="%m/%d/%Y")
	ALL_DATA <- list()
	names <- unique(datatable[,name_column])
	for (i in 1:length(names)) {
#		i <- 1
		timeserires <- datatable[datatable[,name_column]==names[i],]
		timeserires <- timeserires[order(timeserires[,date_column]),]
		ALL_DATA[[i]] <- ts(as.numeric(as.character(timeserires[,value_column])),start=determineTsStartDate(timeserires[,date_column]),frequency=determineTsFrequency(timeserires[,date_column]))
	}
	names(ALL_DATA) <- names
	return(ALL_DATA)
}

extract_xts_from_df <- function(datatable,date_column,value_column,name_column) {
#	datatable <- I_HIST_CSI_prev
	datatable[,date_column] <- as.Date(datatable[,date_column],format="%m/%d/%Y")
	ALL_DATA <- list()
	names <- unique(datatable[,name_column])
	for (i in 1:length(names)) {
		timeserires <- datatable[datatable[,name_column]==names[i],]
		timeserires <- timeserires[order(timeserires[,date_column]),]
		ALL_DATA[[i]] <- as.xts(as.numeric(as.character(timeserires[,value_column])),timeserires[,date_column])
	}
	names(ALL_DATA) <- names
	return(ALL_DATA)
}

xtslist_to_df <- function(xtslist) {
#	xtslist = csi_ts_list
	for (i in 2:length(xtslist)) {
		if (i == 2) {
			Ts_Merge = merge(xtslist[[i-1]],xtslist[[i]])
		} else {
			Ts_Merge = merge(Ts_Merge,xtslist[[i]])
		}
	}
#CSI_ALL_DailyTs_Merge <- data.frame(index(CSI_ALL_DailyTs_Merge),CSI_ALL_DailyTs_Merge)
	
#colnames(CSI_ALL_DailyTs_Merge) <- c("DATE",names(CSI_ALL_DailyTs))
	colnames(Ts_Merge) <- names(xtslist)
	rownames(Ts_Merge) <- index(Ts_Merge)
	return(Ts_Merge)
}

xtslist_to_db <- function(xtslist) {
	TS_DB <- data.frame()
	for (i in 1:length(xtslist)) {
		#	i <- 1
		csi_name <- names(xtslist)[i]
		data <- data.frame(DATE=format(index(xtslist[[i]]),"%m/%d/%Y"),MONTH=as.yearmon(index(xtslist[[i]])),QUARTER=as.yearqtr(index(xtslist[[i]])),CSI_ID=gsub("[.]","-",csi_name),CSI_VALUE=as.vector(xtslist[[i]][,1]))
		TS_DB <- rbind(TS_DB,data)
		#	print(dim(CSI_ALL_DailyDB))
	}
	return(TS_DB)
}


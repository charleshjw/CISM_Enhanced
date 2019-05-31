# To take updated raw downlodas from market research providers (BoA, JPM, Citi
# and Barclays, and standardize observations and save to target Access database.
# It needs to be run once every time data is downloaded or updated. Take 5-10
# minutes to run.
# 
# Author: E620927
###############################################################################

require(outliers)

Output_curve = file.path(Output_root,"CurveInfo")
if(!file.exists(Output_curve)) {
	dir.create(Output_curve)
}

#read data inputs
data_jpm <- read.csv(concat_file_path(tableFilter(list_files,"VariableName","data_jpm")))
data_barclays <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","data_barclays")),sheetIndex=1)
#data_citi <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","data_citi")),sheetIndex=2)
data_boa <- read.csv(concat_file_path(tableFilter(list_files,"VariableName","data_boa")))
data_mmd <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","data_mmd")),sheetIndex=1)
data_armbs_arms <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","data_ambs_arms")),sheetIndex=1)
data_nambs_subprime <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","data_subprime")),sheetIndex=1)

##################################################################################
###Process JPM
data_jpm[data_jpm=="N/A"] <- NA
data_jpm$Date <- as.Date(as.character(data_jpm$Date),format="%d-%b-%y")

outliers <- c("AMBS_CMO_PAC_2Y","AMBS_CMO_SEQ_5Y")

CSI_ALL <- list()
#	j <- 17
for (j in 2:ncol(data_jpm)) {
	csi_name <- colnames(data_jpm)[j]
	data_jpm_index <- as.xts(as.numeric(as.character(data_jpm[,j])),data_jpm[,1])
	file_name = paste(Output_curve,"/",gsub("[.]","-",csi_name),"_daily.png",sep="")
	png(file_name, width=600,height=400)
	plot(na.omit(data_jpm_index),main=paste0("ORIGINAL ",csi_name,"/JPMM"))
	dev.off()
	
	if (csi_name%in%outliers) {
		data_jpm_index <- data_jpm_index[data_jpm_index!=0]
		file_name = paste(Output_curve,"/",gsub("[.]","-",csi_name),"_daily_outliers_removed.png",sep="")
		png(file_name, width=600,height=400)
		plot(na.omit(data_jpm_index),main=paste0("OUTLIER REMOVED ",csi_name,"/JPMM"))
		dev.off()
	}
	
	#trim to quarter start
	data_jpm_index <- trimSeriesToQuarterStart(data_jpm_index)
	
	#trim according to the end date
	data_jpm_index <- data_jpm_index[as.numeric(as.yearqtr(index(data_jpm_index)))<=History_END_DATE]
	
	data_jpm_index <- period.apply(data_jpm_index,INDEX=endpoints(data_jpm_index,'quarters'),FUN=mean_na_ignored)
	data_jpm_index[is.na(data_jpm_index)]<-""
	CSI_ALL <- rbind(CSI_ALL,cbind(DATE=format(index(data_jpm_index),"%m/%d/%Y"),QUARTER=gsub(" ","",as.yearqtr(index(data_jpm_index))),CSI_ID=gsub("[.]","-",csi_name),CSI_VALUE=as.vector(data_jpm_index)))
}

##################################################################################
###Process Barclays
outliers <- as.Date(c("10/12/2001","10/11/2002","05/24/2004","10/12/2004","5/23/2005","10/12/2005"),format="%m/%d/%Y")
#delete rows with n/a
data_barclays<-data_barclays[data_barclays[,2]!="n/a",]
for (j in 2:ncol(data_barclays)) {
#	j <- 2
	csi_name <- colnames(data_barclays)[j]
	data_barclays_index <- as.xts(as.numeric(as.character(data_barclays[,j])),data_barclays[,1])
	file_name = paste(Output_curve,"/",gsub("[.]","-",csi_name),"_daily.png",sep="")
	png(file_name, width=600,height=400)
	plot(na.omit(data_barclays_index),main=paste0("ORIGINAL ",csi_name,"/Barlcays"))
	dev.off()
	#smooth the four outliers
	if (grepl("AD",csi_name)) {
		select <- which(index(data_barclays_index)%in%outliers)
		data_barclays_index[select] <- (as.numeric(data_barclays_index[select-1])+as.numeric(data_barclays_index[select+1]))/2
		file_name = paste(Output_curve,"/",gsub("[.]","-",csi_name),"_daily_outliers_removed.png",sep="")
		png(file_name, width=600,height=400)
		plot(na.omit(data_barclays_index),main=paste0("OUTLIER REMOVED ",csi_name,"/Barlcays"))
		dev.off()
	}
	#trim to quarter start
	data_barclays_index <- trimSeriesToQuarterStart(data_barclays_index)
	
	#trim according to the end date
	data_barclays_index <- data_barclays_index[as.numeric(as.yearqtr(index(data_barclays_index)))<=History_END_DATE]
	
	data_barclays_index <- period.apply(data_barclays_index,INDEX=endpoints(data_barclays_index,'quarters'),FUN=mean_na_ignored)
	data_barclays_index <- data_barclays_index*100
	data_barclays_index[is.na(data_barclays_index)]<-""
	
	CSI_ALL <- rbind(CSI_ALL,cbind(DATE=format(index(data_barclays_index),"%m/%d/%Y"),QUARTER=gsub(" ","",as.yearqtr(index(data_barclays_index))),CSI_ID=gsub("[.]","-",csi_name),CSI_VALUE=as.vector(data_barclays_index)))
}

###################################################################################
####Citi
#data_citi <- data_citi[1:(nrow(data_citi)-1),]
#data_citi$Date <- as.Date(as.numeric(as.character(data_citi$Date)), origin="1899-12-30")
#
#for (j in 2:ncol(data_citi)) {
##	j <- 2
#	csi_name <- colnames(data_citi)[j]
#	data_citi_index <- as.xts(as.numeric(as.character(data_citi[,j])),data_citi[,1])
#	file_name = paste(Output_curve,"/",gsub("[.]","-",csi_name),"_daily.png",sep="")
#	png(file_name, width=600,height=400)
#	plot(na.omit(data_citi_index),main=paste0(csi_name,": Citi"))
#	dev.off()
#
#	#trim to quarter start
#	data_citi_index <- trimSeriesToQuarterStart(data_citi_index)
#	
#	#trim according to the end date
#	data_citi_index <- data_citi_index[as.numeric(as.yearqtr(index(data_citi_index)))<=History_END_DATE]
#	data_citi_index <- period.apply(data_citi_index,INDEX=endpoints(data_citi_index,'quarters'),FUN=mean_na_ignored)
#
#	CSI_ALL <- rbind(CSI_ALL,cbind(DATE=format(index(data_citi_index),"%m/%d/%Y"),QUARTER=gsub(" ","",as.yearqtr(index(data_citi_index))),CSI_ID=gsub("[.]","-",csi_name),CSI_VALUE=as.vector(data_citi_index)))
#}

##################################################################################
###BoA
segments <- unique(data_boa$Bond.Index)
segments <- segments[segments!=""]
data_boa$Date <- as.Date(data_boa$Date,format="%m/%d/%Y")

boa_retrieve_csiname <- function(name) {
	name <- as.character(unique(name))
	name <- name[name!=""]
	if (length(name) != 1) {
		stop("BOA data error. Series name cannot be identified.")
	}
	if (grepl("Euro Covered Bond French Issuers Index",name)) {
		return("CVB_FRA_EUR")
	} else if (grepl("Euro Covered Bond Nordic Issuers Index",name)) {
		return("CVB_NOR_EUR")
	} else if (grepl("Euro Covered Bond UK Issuers Index",name)) {
		return("CVB_UK_EUR")
	} else if (grepl("Sterling Non-Gilts Covered",name)) {
		return("CVB_GBP")
	}
}

for (j in 1:length(segments)) {
#	j <-1
	tmp <- data_boa[data_boa$Bond.Index==segments[j],]
	
	tmp <- tmp[tmp[,"Description"]!="",]
	
	csi_name <- boa_retrieve_csiname(tmp[,"Description"])
	
	data_boa_index <- as.xts(as.numeric(tmp[,"OAS"]),tmp[,"Date"])
	file_name = paste(Output_curve,"/",gsub("[.]","-",csi_name),"_daily.png",sep="")
	png(file_name, width=600,height=400)
	plot(na.omit(data_boa_index),main=paste0(csi_name,": BOA"))
	dev.off()
	
	#trim to quarter start
	data_boa_index <- trimSeriesToQuarterStart(data_boa_index)
	
	#trim according to the end date
	data_boa_index <- data_boa_index[as.numeric(as.yearqtr(index(data_boa_index)))<=History_END_DATE]
	
	#smooth the four outliers
	data_boa_index <- period.apply(data_boa_index,INDEX=endpoints(data_boa_index,'quarters'),FUN=mean_na_ignored)
	CSI_ALL <- rbind(CSI_ALL,cbind(DATE=format(index(data_boa_index),"%m/%d/%Y"),QUARTER=gsub(" ","",as.yearqtr(index(data_boa_index))),CSI_ID=gsub("[.]","-",csi_name),CSI_VALUE=as.vector(data_boa_index)))
}


##################################################################################
###MMD
colnames(data_mmd)[colnames(data_mmd)=="AAA.GO.5.yr"] = "MUNI_GO_AAA_5Y"
colnames(data_mmd)[colnames(data_mmd)=="AA.GO.5.yr"] = "MUNI_GO_AA_5Y"
colnames(data_mmd)[colnames(data_mmd)=="A.GO.5.yr"] = "MUNI_GO_A_5Y"
colnames(data_mmd)[colnames(data_mmd)=="BAA.GO.5.yr"] = "MUNI_GO_BAA_5Y"
colnames(data_mmd)[colnames(data_mmd)=="AAA.GO.10.yr"] = "MUNI_GO_AAA_10Y"
colnames(data_mmd)[colnames(data_mmd)=="AA.GO.10.yr"] = "MUNI_GO_AA_10Y"
data_mmd <- data_mmd[!is.na(data_mmd[,1]),]

#csi_name = "MUNI_GO_AAA_5Y"
mmd_csi_5y <- c("MUNI_GO_AAA_5Y","MUNI_GO_AA_5Y","MUNI_GO_A_5Y","MUNI_GO_BAA_5Y")
for (csi_name in mmd_csi_5y) {
	tmp_data <- 100*(data_mmd[,csi_name]-data_mmd[,"Treasury.5.yr"])
	data_mmd_index <- as.xts(as.numeric(tmp_data),data_mmd[,1])
	file_name = paste(Output_curve,"/",gsub("[.]","-",csi_name),"_daily.png",sep="")
	png(file_name, width=600,height=400)
	plot(na.omit(data_mmd_index),main=paste0(csi_name,": MMD"))
	dev.off()
	
	data_mmd_index <- na.omit(data_mmd_index)
	
	#trim to quarter start
	data_mmd_index <- trimSeriesToQuarterStart(data_mmd_index)
	
	#trim according to the end date
	data_mmd_index <- data_mmd_index[as.numeric(as.yearqtr(index(data_mmd_index)))<=History_END_DATE]
	
	#smooth the four outliers
	data_mmd_index <- period.apply(data_mmd_index,INDEX=endpoints(data_mmd_index,'quarters'),FUN=mean_na_ignored)
	CSI_ALL <- rbind(CSI_ALL,cbind(DATE=format(index(data_mmd_index),"%m/%d/%Y"),QUARTER=gsub(" ","",as.yearqtr(index(data_mmd_index))),CSI_ID=gsub("[.]","-",csi_name),CSI_VALUE=as.vector(data_mmd_index)))
}

mmd_csi_10y <- c("MUNI_GO_AAA_10Y","MUNI_GO_AA_10Y")
for (csi_name in mmd_csi_10y) {
	tmp_data <- 100*(data_mmd[,csi_name]-data_mmd[,"Treasury.10.yr"])
	data_mmd_index <- as.xts(as.numeric(tmp_data),data_mmd[,1])
	file_name = paste(Output_curve,"/",gsub("[.]","-",csi_name),"_daily.png",sep="")
	png(file_name, width=600,height=400)
	plot(na.omit(data_mmd_index),main=paste0(csi_name,": MMD"))
	dev.off()
	
	#trim to quarter start
	data_mmd_index <- trimSeriesToQuarterStart(data_mmd_index)
	
	#trim according to the end date
	data_mmd_index <- data_mmd_index[as.numeric(as.yearqtr(index(data_mmd_index)))<=History_END_DATE]
	
	#smooth the four outliers
	data_mmd_index <- period.apply(data_mmd_index,INDEX=endpoints(data_mmd_index,'quarters'),FUN=mean_na_ignored)
	CSI_ALL <- rbind(CSI_ALL,cbind(DATE=format(index(data_mmd_index),"%m/%d/%Y"),QUARTER=gsub(" ","",as.yearqtr(index(data_mmd_index))),CSI_ID=gsub("[.]","-",csi_name),CSI_VALUE=as.vector(data_mmd_index)))
}

##################################################################################
###Process self constructured ARMBS ARMS indexes for FN GN and FH########
for (j in 2:ncol(data_armbs_arms)) {
#	j <- 2
	csi_name <- colnames(data_armbs_arms)[j]
	data_armbs_arm <- as.xts(as.numeric(as.character(data_armbs_arms[,j])),data_armbs_arms[,1])
	file_name = paste(Output_curve,"/",gsub("[.]","-",csi_name),"_daily.png",sep="")
	png(file_name, width=600,height=400)
	plot(na.omit(data_armbs_arm),main=paste0("ORIGINAL ",csi_name,"/Self-Constructed"))
	dev.off()
	
	#trim to quarter start
	data_armbs_arm <- trimSeriesToQuarterStart(data_armbs_arm)
	
	#trim according to the end date
	data_armbs_arm <- data_armbs_arm[as.numeric(as.yearqtr(index(data_armbs_arm)))<=History_END_DATE]
	
	data_armbs_arm <- period.apply(data_armbs_arm,INDEX=endpoints(data_armbs_arm,'quarters'),FUN=mean_na_ignored)
	data_armbs_arm[is.na(data_armbs_arm)]<-""
	CSI_ALL <- rbind(CSI_ALL,cbind(DATE=format(index(data_armbs_arm),"%m/%d/%Y"),QUARTER=gsub(" ","",as.yearqtr(index(data_armbs_arm))),CSI_ID=gsub("[.]","-",csi_name),CSI_VALUE=as.vector(data_armbs_arm)))
}

##################################################################################
###Process self constructured NAMBS Subprime########
for (j in 2:ncol(data_nambs_subprime)) {
#	j <- 2
	csi_name <- colnames(data_nambs_subprime)[j]
	data_armbs_arm <- as.xts(as.numeric(as.character(data_nambs_subprime[,j])),data_nambs_subprime[,1])
	file_name = paste(Output_curve,"/",gsub("[.]","-",csi_name),"_daily.png",sep="")
	png(file_name, width=600,height=400)
	plot(na.omit(data_armbs_arm),main=paste0("ORIGINAL ",csi_name,"/Self-Constructed"))
	dev.off()
	
	#trim to quarter start
	data_armbs_arm <- trimSeriesToQuarterStart(data_armbs_arm)
	
	#trim according to the end date
	data_armbs_arm <- data_armbs_arm[as.numeric(as.yearqtr(index(data_armbs_arm)))<=History_END_DATE]
	
	data_armbs_arm <- period.apply(data_armbs_arm,INDEX=endpoints(data_armbs_arm,'quarters'),FUN=mean_na_ignored)
	data_armbs_arm[is.na(data_armbs_arm)]<-""
	CSI_ALL <- rbind(CSI_ALL,cbind(DATE=format(index(data_armbs_arm),"%m/%d/%Y"),QUARTER=gsub(" ","",as.yearqtr(index(data_armbs_arm))),CSI_ID=gsub("[.]","-",csi_name),CSI_VALUE=as.vector(data_armbs_arm)))
}


###################################################################################
###Override
CSI_ALL <- as.data.frame(CSI_ALL)
#CSI_ALL[CSI_ALL$CSI_ID=="CLO_2NDBMK06-07VTG_AAA",]
#CSI_ALL[CSI_ALL$CSI_ID==csi_name,]

file_name <- concat_file_path(tableFilter(list_files,"VariableName","data_overrides"))

if (file.exists(file_name)) {
	data_overrides <- read.csv(file_name)
#	data_overrides$DATE <- format(as.Date(data_overrides$DATE,format="%m/%d/%Y"),format="%m/%d/%Y")
	colnames(data_overrides) <- gsub("[.]","-",colnames(data_overrides))
	for (j in 2:ncol(data_overrides)) {
#	j <- 2
		csi_name <- colnames(data_overrides)[j]
		overrides <- na.omit(data_overrides[,c(1,j)])
		if (any(CSI_ALL$CSI_ID==csi_name)) { #if override the existing CSI
			dates <- as.Date(as.yearqtr(unlist(CSI_ALL[CSI_ALL$CSI_ID==csi_name,"QUARTER"])))
			ts_before_overlay <- ts(unlist(CSI_ALL[CSI_ALL$CSI_ID==csi_name,"CSI_VALUE"]),start=determineTsStartDate(dates),frequency=determineTsFrequency(dates))
			for (k in 1:nrow(overrides)) {
#				k <- 1
				CSI_ALL[CSI_ALL$CSI_ID==csi_name&CSI_ALL$QUARTER==overrides[k,1],"CSI_VALUE"] <- overrides[k,2]
			}
			
			ts_after_overlay <- ts(unlist(CSI_ALL[CSI_ALL$CSI_ID==csi_name,"CSI_VALUE"]),start=determineTsStartDate(dates),frequency=determineTsFrequency(dates))
			file_name = paste(Output_curve,"/",gsub("[.]","-",csi_name),"_daily_override.png",sep="")
			png(file_name, width=600,height=400)
			tsPlot(ts.union(ts_after_overlay,ts_before_overlay),dep=csi_name,leg_names=c("After Overide","Before Override"),subtitle="Data Overide")
			dev.off()
		} else { #if CSI did not exist
			overrides_index <- as.xts(as.numeric(as.character(overrides[,2])),as.Date(as.yearqtr(overrides[,1])+0.25)-1)
			overrides_index <- overrides_index[as.numeric(as.yearqtr(index(overrides_index)))<=History_END_DATE]
			CSI_ALL <- rbind(CSI_ALL,cbind(DATE=format(index(overrides_index),"%m/%d/%Y"),QUARTER=gsub(" ","",as.yearqtr(index(overrides_index))),CSI_ID=gsub("[.]","-",csi_name),CSI_VALUE=as.vector(overrides_index)))
			
			ts <- ts(unlist(CSI_ALL[CSI_ALL$CSI_ID==csi_name,"CSI_VALUE"]),start=determineTsStartDate(dates),frequency=determineTsFrequency(dates))
			file_name = paste(Output_curve,"/",gsub("[.]","-",csi_name),"_daily_override.png",sep="")
			png(file_name, width=600,height=400)
			tsPlot(ts,dep=csi_name,subtitle="Data Overide")
			dev.off()
		}
	}
}

table_name = "I_HIST_CSI_APP_PREV"
#try(sqlDrop(dbc, table_name))
CSI_ALL <- data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),CSI_ALL)
saveTable(dbc,table_name,CSI_ALL)

#test if all csi defined in mapping table exists in original download
a <-unlist(unique(CSI_ALL[,"CSI_ID"]))
b <- unique(mapping[,"CSI_ID"])
if (!all(a%in%b)) {
	print("Warning: Several download data spread indexes are not mapped")
	print(a[!a%in%b])
}

if (!all(b%in%a)) {
	print(b[!b%in%a])
	stop("Error: Some indexes in mapping table are not downloaded!!")
}

#memory clean up
CSI_ALL <- c()
data_jpm <- c()
data_barclays <- c()
#data_citi <- c()
data_boa <- c()
data_mmd <- c()
data_armbs_arms <- c()
data_nambs_subprime <- c()
data_overrides <- c()

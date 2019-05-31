# Read python-formatted MAOX files to extract independent variables.
# 
# Author: E620927
###############################################################################
Output_IV <- file.path(Output_root,"MacroFactorVerification")
if (!dir.exists(Output_IV)) {
	dir.create(Output_IV)
}

#read primary IV from orignal MAOX excel files
print("**************Process IV Hisotry and Forecast****************")

IV_HIST <- data.frame()
IV_FCST_ALL <- data.frame()

print(paste0("END Calibration Date is setup as ", Calibration_END_DATE))
print(paste0("END FCST DATE is setup as ", Forecast_END_DATE))

#should update D_IV to include all relevant variables and MAOX mnemonics
D_IV <- fetchTable(dbc,"I_IV",current_version)

tickers <- as.character(D_IV[,"MAOX_CODE"])
tickers <- tickers[tickers!="N/A"]
tickers <- unique(tickers)

#####################################################
####Primary variables
#quarterly Version
FLAG_HISTORY = FALSE #the history of first scenario will be used as the history and assume history across all sceanrios are the same.
for (j in 1:length(ScenarioNames)) {
#	j <- 4
	scenario_name <- ScenarioNames[j]
	IV_FCST <- data.frame()
	
	filename = concat_file_path(tableFilter(list_files,"VariableName",scenario_name))
	if (length(filename) == 0) {
		next
	}
	data_sceanrio <- read.csv(filename)
#	if (j == 1) {
#		mnemonics <- unique(data_sceanrio[,c("Mnemonic","Description")])
#		mnemonics <- mnemonics[order(mnemonics[,"Mnemonic"]),]
##		write.csv(mnemonics,"./Data/UniqueIV.csv")
#	}

	if (!all(tickers%in%unique(data_sceanrio$Mnemonic))) {
		warning("Warning: Not all variables can be found in database")
		tickers_nonexist <- tickers[!tickers%in%unique(data_sceanrio$Mnemonic)]
		print(paste0("**************Mnemonics not in database: ",tickers_nonexist))
		tickers <- tickers[tickers%in%unique(data_sceanrio$Mnemonic)]
	}
	
	for(i in 1:length(tickers)) {
	#	i <- 1
		ticker <- tickers[i]
		data <- data_sceanrio[data_sceanrio$Mnemonic==ticker,]
		if (MONTHLY_VERSION) {
			dates <- as.Date(as.yearmon(data$Date,format="%YM%m")+1/12) - 1
		} else {
			dates <- as.Date(as.yearqtr(data$Date,format="%Y Q%q")+0.25)-1
		}
		
		iv_id <- as.character(D_IV[D_IV[,"MAOX_CODE"]==ticker,"IV_ID"])
		if (length(iv_id)!=1) {
			print(paste0("ERROR: more than one ticker match",iv_id))
			next
		}
		iv <- data.frame(IV_ID=iv_id,DATE=dates,IV_VALUE=as.numeric(as.character(data$Value)))
		iv <- iv[!duplicated(iv$DATE),]
		
		if(FLAG_HISTORY==FALSE) {
			iv_hist <- iv[iv$DATE <= convertNumericToDate(Calibration_END_DATE),]
			iv_hist <- iv_hist[order(iv_hist$DATE),]
			IV_HIST <- rbind(IV_HIST,iv_hist)
		}
		
		#Codes for 2017 DFAST data correction
#		#Manually correct swp spread 5Y and 10Y anomaly
#		if (iv_id %in% c("SWP_SPR_5Y_MAOX","SWP_SPR_10Y_MAOX")) {
#			print("Warning: Manually Correct swap spread anomaly from MAOX files***********")
#			print(paste0(iv_id, ": previous values:"))
#			print(iv_hist[1:2,"IV_VALUE"])
#			iv_hist[1:2,"IV_VALUE"] <- 0
#			print(paste0(iv_id, ": previous values:"))
#			print(iv_hist[1:2,"IV_VALUE"])
#		}
		
		iv_fcst <- iv[iv$DATE > convertNumericToDate(Calibration_END_DATE)&iv$DATE <= convertNumericToDate(Forecast_END_DATE),]
		iv_fcst <- iv_fcst[order(iv_fcst$DATE),]
		IV_FCST <- rbind(IV_FCST,iv_fcst)
	}
	
	IV_FCST[,1] <- as.character(IV_FCST$IV_ID)
	IV_FCST[,2] <- as.character(IV_FCST$DATE)
	IV_FCST[,3] <- as.numeric(as.character(IV_FCST[,3]))
	IV_FCST <- data.frame(IV_FCST, SCEN_ID=scenario_name)
	IV_FCST <- IV_FCST[!is.na(IV_FCST$IV_ID),]
	IV_FCST_ALL <- rbind(IV_FCST_ALL,IV_FCST)
	FLAG_HISTORY = TRUE
}

IV_HIST[,1] <- as.character(IV_HIST[,1])
IV_HIST[,2] <- as.character(IV_HIST[,2])
IV_HIST[,3] <- as.numeric(as.character(IV_HIST[,3]))
IV_HIST <- IV_HIST[!is.na(IV_HIST[,1]),]

table_name <- "I_HIST_IV_PRIM"
#try(sqlDrop(dbc, table_name))
data = data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),IV_HIST)
saveTable(dbc,table_name,data)

print("********Complete IV PRIMARY***********")

#####################################################
#### Seconday Variables
IV <- sqlFetch(dbc, "I_HIST_IV_PRIM")
IV <- IV[IV$VERSION==current_version,]
IV <- IV[,c("IV_ID","DATE","IV_VALUE")]
IV[,1] <- as.character(IV[,1])
IV[,2] <- as.Date(as.character(IV[,2]))
IV[,3] <- as.numeric(as.character(IV[,3]))
IV_DATA <- retrieveIV(IV)

SecTable <- sqlFetch(dbc, "I_IV_SEC")
SecTable <- SecTable[SecTable$VERSION==current_version,]
SecTable <- SecTable[!is.na(SecTable$IV_NAME),]
SecTable[is.na(SecTable)] = ""
#Verify the validity
if (!all(SecTable$IV_ID%in%D_IV$IV_ID)) {
	print("ERROR: SEC Variable not Defined in IV Table")
}
if (!all(SecTable[SecTable$INPUT1!="","INPUT1"]%in%D_IV$IV_ID)) {
	print("ERROR: INPUT 1 not Defined in IV Table")
}
if (!all(SecTable[SecTable$INPUT2!="","INPUT2"]%in%D_IV$IV_ID)) {
	print("ERROR: INPUT 2 not Defined in IV Table")
}
if (!all(SecTable[SecTable$INPUT3!="","INPUT3"]%in%D_IV$IV_ID)) {
	print("ERROR: INPUT 3not Defined in IV Table")
}
if (!all(SecTable[SecTable$INPUT1!="","INPUT1"]%in%names(IV_DATA))) {
	input1 <- SecTable[SecTable$INPUT1!="","INPUT1"]
	input1nonexist <- input1[!input1%in%names(IV_DATA)]
	SecTable <- SecTable[!SecTable[,"INPUT1"]%in%input1nonexist,]
	warning("ERROR: INPUT 1 not Defined in IV Table")
}
if (!all(SecTable[SecTable$INPUT2!="","INPUT2"]%in%names(IV_DATA))) {
	
	a <- SecTable[SecTable$INPUT2!="","INPUT2"]
	b <- names(IV_DATA)
	a[!a%in%b]
	
	stop("ERROR: INPUT 2 not Defined in IV Table")
}
if (!all(SecTable[SecTable$INPUT3!="","INPUT3"]%in%names(IV_DATA))) {
	stop("ERROR: INPUT 3not Defined in IV Table")
}

#Process data extraction
IV_HIST_SEC <- data.frame()
for (i in 1:nrow(SecTable)) {
#	i <- 9
	var_names <- as.character(unlist(SecTable[i,c("INPUT1","INPUT2","INPUT3")]))
	var_names <- as.character(var_names[var_names!=""])
	curves <- na.omit(combineCurves(IV_DATA[c(var_names)]))
	colnames(curves) <- paste0("INPUT",1:ncol(curves))
	data <- eval(parse(text=as.character(SecTable[i,]$FORMULA)), as.data.frame(curves))
	iv_id <- as.character(SecTable[i,"IV_ID"])
	date <- convertNumericToDate(index(curves))
	IV_HIST_SEC <- rbind(IV_HIST_SEC, data.frame(date,iv_id,data))
}
IV_HIST_SEC[,1] <- as.character(IV_HIST_SEC[,1])
IV_HIST_SEC[,2] <- as.character(IV_HIST_SEC[,2])
IV_HIST_SEC[,3] <- as.numeric(IV_HIST_SEC[,3])
colnames(IV_HIST_SEC) <- c("DATE","IV_ID","IV_VALUE")

table_name <- "I_HIST_IV_SEC"
#try(sqlDrop(dbc, table_name))
data = data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),IV_HIST_SEC)
saveTable(dbc,table_name,data)
print("************Finish SECONDARY Variable Calculation****************")

#####################################################
##Fcst for secondary variables
FcstCurves <- list()
for (i in 1:length(ScenarioNames)) {
#	i <- 1
	scenario_name <- ScenarioNames[i]

	FcstCurves[[i]] <- combineCurves(retrieveIV(IV_FCST_ALL[IV_FCST_ALL$SCEN_ID==scenario_name,]))
	names(FcstCurves)[i] <- ScenarioNames[i]
}

IV_FCST_ALL_With_SEC <- IV_FCST_ALL
IV_FCST_ALL_With_SEC$DATE <- as.character(IV_FCST_ALL_With_SEC$DATE)
for (j in 1:length(ScenarioNames)) {
#	j <- 1
	for (i in 1:nrow(SecTable)) {
	#	i <- 1
		var_names <- as.character(unlist(SecTable[i,c("INPUT1","INPUT2","INPUT3")]))
		var_names <- as.character(var_names[var_names!=""])
		
		curves <- na.omit(FcstCurves[[ScenarioNames[j]]][,c(var_names)])
		colnames(curves) <- paste0("INPUT",1:ncol(curves))
		data <- eval(parse(text=as.character(SecTable[i,]$FORMULA)), as.data.frame(curves))
		iv_id <- as.character(SecTable[i,"IV_ID"])
		date <- as.character(convertNumericToDate(index(curves)))
		
		IV_FCST_ALL_With_SEC <- rbind(IV_FCST_ALL_With_SEC,data.frame(IV_ID=iv_id, DATE=date, IV_VALUE=data, SCEN_ID=ScenarioNames[[j]]))
	}
}

table_name <- "I_PREVSCEN_IV_FULL"
#try(sqlDrop(dbc, table_name))
data = data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),IV_FCST_ALL_With_SEC)
saveTable(dbc,table_name,data)
print("************Finish Process Fcst for Primary and Secondary Variables******************")

#write output to csv
file_name <- paste(Output_IV,"/IV_HIST_PRIM.csv",sep="")
write.csv(IV_HIST, file_name)
file_name <- paste(Output_IV,"/IV_HIST_SEC.csv",sep="")
write.csv(IV_HIST_SEC, file_name)
file_name <- paste(Output_IV,"/IV_HIST_FCST.csv",sep="")
write.csv(IV_FCST_ALL_With_SEC, file_name)

#clean up memory
try(rm(list=c("IV","IV_DATA","IV_FCST_ALL","IV_FCST_ALL_With_SEC")))

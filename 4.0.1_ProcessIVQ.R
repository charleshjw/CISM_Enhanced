# 
# 
# Author: E620927
###############################################################################

#read primary IV from orignal MAOX excel files
print("**************Process IV Hisotry and Forecast****************")
ScenarioNames <- c("FRBBASELINE","FRBADVERSE","FRBSEVERELYADVERSE","BHCBASELINE","BHCSTRESS")
#ScenarioNames <- c("FRBBASELINE","FRBADVERSE","FRBSEVERELYADVERSE")
IV_HIST_Q <- data.frame()
IV_FCST_ALL_Q <- data.frame()

END_FCST <- END_DATE+2.25
print(paste0("END HISTORY DATE is setup as ", END_DATE))
print(paste0("END FCST DATE is setup as ", END_FCST))

D_IV <- sqlFetch(dbc,"I_IV")	#should update D_IV to include all relevant variables and MAOX mnemonics
tickers <- as.character(D_IV[,"MAOX CODE"])
tickers <- tickers[tickers!="N/A"]

#quarterly Version
for (j in 1:length(ScenarioNames)) {
#	j <- 1
	scenario_name <- ScenarioNames[j]
	IV_FCST <- data.frame()
	
	#END_DATE
	data_sceanrio <- read.csv(paste0("./Data/IVFromRawQ-",scenario_name,".csv"))
	if (j == 1) {
		mnemonics <- unique(data_sceanrio[,c("Mnemonic","Description")])
		mnemonics <- mnemonics[order(mnemonics[,"Mnemonic"]),]
		write.csv(mnemonics,"./Data/UniqueIVQ.csv")
		
		print(paste0("Mnemonics not in Quarterly database: ",tickers[!tickers%in%unique(data_sceanrio$Mnemonic)]))
		
		tickers <- tickers[tickers%in%unique(data_sceanrio$Mnemonic)]
	}
	
	for(i in 1:length(tickers)) {
		#	i <- 1
		ticker <- tickers[i]
		data <- data_sceanrio[data_sceanrio$Mnemonic==ticker,]
		dates <- as.Date(as.yearmon(data$Date,format="%Y M%m")+1/12)-1
		
		iv_id <- as.character(D_IV[D_IV[,"MAOX CODE"]==ticker,"IV_ID"])
		if (length(iv_id)!=1) {
			print(paste0("ERROR: more than one ticker match",iv_id))
			next
		}
		iv <- data.frame(IV_ID=iv_id,DATE=dates,IV_VALUE=as.numeric(as.character(data$Value)))
		iv <- iv[!duplicated(iv$DATE),]
		
		if (j == 1) {
			iv_hist <- iv[iv$DATE < as.Date(as.yearqtr(END_DATE)),]
			iv_hist <- iv_hist[order(iv_hist$DATE),]
			IV_HIST_Q <- rbind(IV_HIST_Q,iv_hist)			
		}
		
		iv_fcst <- iv[iv$DATE >= as.Date(as.yearqtr(END_DATE))&iv$DATE < as.Date(as.yearqtr(END_FCST)),]
		iv_fcst <- iv_fcst[order(iv_fcst$DATE),]
		IV_FCST <- rbind(IV_FCST,iv_fcst)
	}
	
	IV_FCST[,1] <- as.character(IV_FCST$IV_ID)
	IV_FCST[,2] <- as.character(IV_FCST$DATE)
	IV_FCST[,3] <- as.numeric(as.character(IV_FCST[,3]))
	IV_FCST <- data.frame(IV_FCST, SCEN_ID=getScenId(scenario_name))
	IV_FCST <- IV_FCST[!is.na(IV_FCST$IV_ID),]
	IV_FCST_ALL_Q <- rbind(IV_FCST_ALL_Q,IV_FCST)
}

IV_HIST_Q[,1] <- as.character(IV_HIST_Q[,1])
IV_HIST_Q[,2] <- as.character(IV_HIST_Q[,2])
IV_HIST_Q[,3] <- as.numeric(as.character(IV_HIST_Q[,3]))
IV_HIST_Q <- IV_HIST_Q[!is.na(IV_HIST_Q[,1]),]

table_name <- "I_HIST_IV_Q_PRIM"
try(sqlDrop(dbc, table_name))
sqlSave(dbc, IV_HIST_Q, tablename = table_name, addPK=FALSE, safer = FALSE, rownames=FALSE)

print("********Complete IV PRIMARY***********")
#
#####Calculate Seconday Variables
#IV <- sqlFetch(dbc, "I_HIST_IV_PRIM")
#IV <- IV[,c("IV_ID","DATE","IV_VALUE")]
#IV[,1] <- as.character(IV[,1])
#IV[,2] <- as.Date(as.character(IV[,2]))
#IV[,3] <- as.numeric(as.character(IV[,3]))
#IV_DATA <- retrieveIV(IV)
#
#SecTable <- sqlFetch(dbc, "I_IV_SEC")
#SecTable <- SecTable[!is.na(SecTable$IV_NAME),]
##Verify the validity
#if (!all(SecTable$IV_ID%in%D_IV$IV_ID)) {
#	print("ERROR: SEC Variable not Defined in IV Table")
#}
#if (!all(na.omit(SecTable$INPUT1)%in%D_IV$IV_ID)) {
#	print("ERROR: INPUT 1 not Defined in IV Table")
#}
#if (!all(na.omit(SecTable$INPUT2)%in%D_IV$IV_ID)) {
#	print("ERROR: INPUT 2 not Defined in IV Table")
#}
#if (!all(na.omit(SecTable$INPUT3)%in%D_IV$IV_ID)) {
#	print("ERROR: INPUT 3not Defined in IV Table")
#}
#
##Process data extraction
#IV_HIST_Q_SEC <- data.frame()
#for (i in 1:nrow(SecTable)) {
##	i <- 15
##	SecTable[i,]
#	var_names <- as.character(unlist(SecTable[i,c("INPUT1","INPUT2","INPUT3")]))
#	var_names <- as.character(na.omit(var_names))
#	curves <- na.omit(combineCurves(IV_DATA[c(var_names)]))
#	colnames(curves) <- paste0("INPUT",1:ncol(curves))
#	data <- eval(parse(text=as.character(SecTable[i,]$FORMULA)), as.data.frame(curves))
#	iv_id <- as.character(SecTable[i,"IV_ID"])
#	date <- as.Date(as.yearqtr(index(curves))+0.25)-1
#	IV_HIST_Q_SEC <- rbind(IV_HIST_Q_SEC, data.frame(date,iv_id,data))
#}
#IV_HIST_Q_SEC[,1] <- as.character(IV_HIST_Q_SEC[,1])
#IV_HIST_Q_SEC[,2] <- as.character(IV_HIST_Q_SEC[,2])
#IV_HIST_Q_SEC[,3] <- as.numeric(IV_HIST_Q_SEC[,3])
#colnames(IV_HIST_Q_SEC) <- c("DATE","IV_ID","IV_VALUE")
#
#table_name <- "I_HIST_IV_Q_SEC"
#try(sqlDrop(dbc, table_name))
#sqlSave(dbc, IV_HIST_Q_SEC, tablename = table_name, addPK=FALSE, safer = FALSE, rownames=FALSE)
#
#print("************Finish SECONDARY Variable Calculation****************")
###Fcst for secondary variables
#FcstCurves <- list()
#for (i in 1:length(ScenarioNames)) {
##	i <- 1
#	scenario_name <- ScenarioNames[i]
#	
#	FcstCurves[[i]] <- combineCurves(retrieveIV(IV_FCST_ALL_Q[IV_FCST_ALL_Q$SCEN_ID==getScenId(scenario_name),]))
#	names(FcstCurves)[i] <- ScenarioNames[i]
#}
#
#IV_FCST_ALL_With_SEC <- IV_FCST_ALL_Q
#IV_FCST_ALL_With_SEC$DATE <- as.character(IV_FCST_ALL_With_SEC$DATE)
#for (j in 1:length(ScenarioNames)) {
##	j <- 1
#	for (i in 1:nrow(SecTable)) {
#		#	i <- 1
#		#	SecTable[i,]
#		var_names <- as.character(unlist(SecTable[i,c("INPUT1","INPUT2","INPUT3")]))
#		var_names <- as.character(na.omit(var_names))
#		
#		curves <- na.omit(FcstCurves[[ScenarioNames[j]]][,c(var_names)])
#		colnames(curves) <- paste0("INPUT",1:ncol(curves))
#		data <- eval(parse(text=as.character(SecTable[i,]$FORMULA)), as.data.frame(curves))
#		iv_id <- as.character(SecTable[i,"IV_ID"])
#		date <- as.character(as.Date(as.yearqtr(index(curves))+0.25)-1)
#		
#		IV_FCST_ALL_With_SEC <- rbind(IV_FCST_ALL_With_SEC,data.frame(IV_ID=iv_id, DATE=date, IV_VALUE=data, SCEN_ID=getScenId(ScenarioNames[[j]])))
#	}
#}
##dim(IV_FCST_ALL_Q)
##dim(IV_FCST_ALL_With_SEC)
#
#table_name <- "I_PREVSCEN_IV_Q_FULL"
#try(sqlDrop(dbc, table_name))
#sqlSave(dbc, IV_FCST_ALL_With_SEC, tablename = table_name, addPK=FALSE, safer = FALSE, rownames=FALSE)
#print("************Finish Process Fcst for Primary and Secondary Variables******************")

#write output to csv
file_name <- paste(Output_root,"/IV_HIST_Q_PRIM.csv",sep="")
write.csv(IV_HIST_Q, file_name)
#file_name <- paste(Output_root,"/IV_HIST_Q_SEC.csv",sep="")
#write.csv(IV_HIST_Q_SEC, file_name)
#file_name <- paste(Output_root,"/IV_HIST_Q_FCST.csv",sep="")
#write.csv(IV_FCST_ALL_With_SEC, file_name)

#clean up memory
rm(list=c("IV","IV_all","IV_DATA","IV_FCST_ALL_Q","IV_FCST_ALL_With_SEC"))

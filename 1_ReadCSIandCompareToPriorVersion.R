# Read CSI data from database
#	compare CSI data with prior version to identify data errors
#
# Author: E620927
###############################################################################

Output_curve = file.path(Output_root,"CurveInfo")
if(!file.exists(Output_curve)) {
	dir.create(Output_curve)
}

##Read and Update Mapping Table
I_MOD_CSI_AC <- fetchTable(dbc,"I_MOD_CSI_AC",current_version)[,-1:-2]
I_MOD_CSI_AC <- I_MOD_CSI_AC[order(I_MOD_CSI_AC$AC_ID,I_MOD_CSI_AC$MOD_ID),]
file_name = paste(Output_curve,"/MAP_CURVE_MODEL.tex",sep="")
print(xtable(I_MOD_CSI_AC[,!grepl("MOD_ID",colnames(I_MOD_CSI_AC))],caption="Map of Cluster, Credit Spread Indexes and Asset Class"),file=file_name, tabular.environment = 'longtable', floating=FALSE, include.rownames=FALSE)

#process csi data to required format
I_HIST_CSI <- fetchTable(dbc,I_CSI_TABLE,current_version)[,c("DATE","CSI_ID","CSI_VALUE")]
if (dbHasTable(dbc,"I_HIST_CSI_SEC")) {
	CSI_HIST_SEC <- fetchTable(dbc,"I_HIST_CSI_SEC",current_version)[,c("DATE","CSI_ID","CSI_VALUE")]
	I_HIST_CSI <- rbind(I_HIST_CSI,CSI_HIST_SEC)
}
CSI_CURVES <- extract_ts_from_df(I_HIST_CSI,date_column = "DATE",value_column = "CSI_VALUE",name_column = "CSI_ID")
CSI_NAMES_ALL <- names(CSI_CURVES)

#####################Creat secondary variables
table_name <- "I_CSI_SEC";
if (dbHasTable(dbc,table_name)&!dbHasTable(dbc,"I_HIST_CSI_SEC")) {
	print("****************create secondary CSI*******************")
	SecTable <- fetchTable(dbc, table_name,current_version)
	SecTable <- SecTable[!is.na(SecTable$CSI_ID),]
	SecTable[is.na(SecTable)] = ""
	SecTable <- characterizeTable(SecTable)
	if (!all(SecTable$INPUT1%in%names(csi_ts_list)) | !all(SecTable$INPUT2%in%names(csi_ts_list))) {
		stop("ERROR: some CSI defined in secondary table do not exist")
	}
	CSI_HIST_SEC <- data.frame()
	for (i in 1:nrow(SecTable)) {
		#	i <- 1
		var_names <- as.character(unlist(SecTable[i,c("INPUT1","INPUT2","INPUT3")]))
		var_names <- as.character(var_names[var_names!=""])
		curves <- na.omit(combineCurves(CSI_CURVES[var_names]))
		colnames(curves) <- paste0("INPUT",1:ncol(curves))
		data <- eval(parse(text=as.character(SecTable[i,]$FORMULA)), as.data.frame(curves))
		csi_id <- as.character(SecTable[i,"CSI_ID"])
		date <- convertNumericToDate(index(curves))
		CSI_HIST_SEC <- rbind(CSI_HIST_SEC, data.frame(date,csi_id,data))
	}
	colnames(CSI_HIST_SEC) <- c("DATE","CSI_ID","CSI_VALUE")
	CSI_HIST_SEC$DATE <- format(CSI_HIST_SEC$DATE,"%m/%d/%Y")
	table_name <- "I_HIST_CSI_SEC"
	saveTable(dbc, table_name,versionDataFrame(CSI_HIST_SEC,current_version))
}

#CSI_CURVES <- retrieveCSICurves(I_MOD_CSI_AC, I_HIST_CSI)
#CSI_CURVES is now the list that contains all historical credit spread curve info

#save records to tex file for presentation purpose
csi_names <- names(CSI_CURVES)
fileConn <- file(paste(Output_curve,"/Curves.tex",sep=""),"w")
writeLines(paste0("\\newcommand\\Curves{",paste(csi_names,collapse=","),"}"), fileConn)
writeLines(paste0("\\newcommand\\CurrentVersion{",current_version,"}"), fileConn)
writeLines(paste0("\\newcommand\\PriorVersion{",prior_version,"}"), fileConn)
close(fileConn)

#Plot Curves
if (OUTPUT_CSI_PLOT) {
	for(i in 1:length(csi_names)) {
		#	i = 16;
		file_name = paste(Output_curve,"/",csi_names[i],".png",sep="")
		png(file_name,width=600,height=400)
		ts.plot(na.omit(CSI_CURVES[[i]]),ylab=csi_names[i], main=paste0("CSI_ID: ",csi_names[i]),type="b")
		dev.off()
	}
}

if ("dbc_prev"%in%ls()) {
	
	if (!dbHasTable(dbc_prev,Prior_Table_Name)) {
		print("*****************")
		print("Warning: prior database does not have the CSI table name.")
	}
	
	if (dbHasTable(dbc_prev,Prior_Table_Name)&Frequency==prior_frequency) {
		I_HIST_CSI_prev <- fetchTable(dbc_prev,Prior_Table_Name,prior_version)
		CSI_CURVES_prev <- extract_ts_from_df(I_HIST_CSI_prev,date_column = "DATE",value_column = "CSI_VALUE",name_column = "CSI_ID")
		CSI_NAMES_ALL_prev <- unique(names(CSI_CURVES_prev))
		
		print(paste0("Are previous CSI included in current database? -- ", all(CSI_NAMES_ALL_prev%in%CSI_NAMES_ALL)))
		print(paste0("Are current CSI included in previous database? -- ", all(CSI_NAMES_ALL%in%CSI_NAMES_ALL_prev)))
		print(paste0("Missing previous CSIs are: ",CSI_NAMES_ALL[!CSI_NAMES_ALL%in%CSI_NAMES_ALL_prev]))
		print(paste0("Missing current CSIs are: ",CSI_NAMES_ALL_prev[!CSI_NAMES_ALL_prev%in%CSI_NAMES_ALL]))
		
		csi_names <- CSI_NAMES_ALL[CSI_NAMES_ALL%in%CSI_NAMES_ALL_prev]
		csi_names_new <- CSI_NAMES_ALL[!CSI_NAMES_ALL%in%CSI_NAMES_ALL_prev]
		
		#Plot comparison of Curves
		if (OUTPUT_CSI_PLOT) {
			for(i in 1:length(csi_names)) {
				#	when the csi CAN be found in prior version
				file_name = paste(Output_curve,"/",csi_names[[i]],"-Comparison.png",sep="")
				comparison <- ts.union(curr=CSI_CURVES[[as.character(csi_names[i])]],prev=CSI_CURVES_prev[[as.character(csi_names[[i]])]])
				tsPlotGGplot(comparison,dep=csi_names[i],combined=TRUE,subtitle="Comparison of original time series")
				ggsave(file_name)
				
				file_name = paste(Output_curve,"/",csi_names[[i]],"-Comparison-Diff.png",sep="")
				tsPlotGGplot(diff(comparison),dep=csi_names[i],combined=TRUE,subtitle="Comparison of difference time series")
				ggsave(file_name)
				
				comparison <- ts.union(comparison,diff(comparison),(diff(comparison)[,1]-diff(comparison)[,2])/diff(comparison)[,2])
				comparison <- data.frame(comparison,abs(comparison[,1]-comparison[,2])<0.1,row.names=as.yearperiod(index(comparison)))
				comparison[,5] <- formatPercentages(round(comparison[,5],4))
				colnames(comparison) <- c("curr","prev","curr-diff(A)","prev-diff(B)","(A-B)/B","Comparison")
				file_name = paste(Output_curve,"/",csi_names[[i]],"-Comparison.tex",sep="")
				print(xtable(comparison,caption=paste0("Comparison of Spreads")),file=file_name,tabular.environment = 'longtable', floating=FALSE, include.rownames=TRUE)
			}

			if (length(csi_names_new) > 0) {
				for (i in 1:length(csi_names_new)) {
					#	when csi CANNOT be found in prior version
					file_name = paste(Output_curve,"/",csi_names_new[[i]],"-Comparison.png",sep="")
					comparison <- ts.union(curr=CSI_CURVES[[as.character(csi_names_new[i])]])
					tsPlotGGplot(comparison,dep=csi_names_new[i],combined=TRUE,subtitle="NEW: original time series")
					ggsave(file_name)
					
					file_name = paste(Output_curve,"/",csi_names_new[[i]],"-Comparison-Diff.png",sep="")
					tsPlotGGplot(diff(comparison),dep=csi_names_new[i],combined=TRUE,subtitle="NEW: difference time series")
					ggsave(file_name)
					
					comparison <- data.frame(Date=as.yearperiod(index(comparison)),curr=comparison)
					file_name = paste(Output_curve,"/",csi_names_new[[i]],"-Comparison.tex",sep="")
					print(xtable(comparison,caption=paste0("NEW: Historical Spreads")),file=file_name,tabular.environment = 'longtable', floating=FALSE, include.rownames=TRUE)
				}
			}
		}
		
		#memory clean up
		I_MOD_CSI_AC_prev <- c()
		I_HIST_CSI_prev <- c()
		CSI_CURVES_prev <- c()
		comparison <- c()
	}
}


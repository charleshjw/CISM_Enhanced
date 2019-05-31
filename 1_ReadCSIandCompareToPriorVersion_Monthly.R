# Read CSI data from database on monthly basis
#	compare CSI data with prior version to identify data errors
# 
# Author: E620927
###############################################################################
Output_curve = file.path(Output_root,"CurveInfoMonthly")
if(!file.exists(Output_curve)) {
	dir.create(Output_curve)
}

##Read and Update Mapping Table
I_MOD_CSI_AC <- fetchTable(dbc,"I_MOD_CSI_AC",current_version)[,-1:-2]
I_MOD_CSI_AC <- I_MOD_CSI_AC[order(I_MOD_CSI_AC$AC_ID,I_MOD_CSI_AC$MOD_ID),]
file_name = paste(Output_curve,"/MAP_CURVE_MODEL.tex",sep="")
print(xtable(I_MOD_CSI_AC[,!grepl("MOD_ID",colnames(I_MOD_CSI_AC))],caption="Map of Cluster, Credit Spread Indexes and Asset Class"),file=file_name, tabular.environment = 'longtable', floating=FALSE, include.rownames=FALSE)

#process csi data to required format
I_HIST_CSI <- fetchTable(dbc,I_CSI_TABLE,current_version)[,-1:-2]
CSI_CURVES <- extract_ts_from_df(I_HIST_CSI,date_column = "DATE",value_column = "CSI_VALUE",name_column = "CSI_ID")
CSI_NAMES_ALL <- names(CSI_CURVES)

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
#
#
##I_HIST_CSI_MONTHLY <- fetchTable(dbc,"I_HIST_CSI_MONTHLY",current_version)
##CSI_NAMES_ALL_MONTHLY <- unique(I_HIST_CSI_MONTHLY$CSI_ID)
##CSI_CURVES_MONTHLY <- retrieveCSICurves(I_MOD_CSI_AC, I_HIST_CSI_MONTHLY)
#
#if ("dbc_prev"%in%ls()) {
#	I_MOD_CSI_AC_prev <- sqlFetch(dbc_prev, "I_MOD_CSI_AC")
#	I_MOD_CSI_AC_prev <- tableFilter(I_MOD_CSI_AC_prev,"version",prior_version)
##	I_MOD_CSI_AC_prev <- I_MOD_CSI_AC_prev[I_MOD_CSI_AC_prev$VERSION==prior_version,]
#	
#	if (dbHasTable(dbc_prev,"I_HIST_CSI_MONTHLY")) {
#		I_HIST_CSI_prev <- sqlFetch(dbc_prev, "I_HIST_CSI_MONTHLY")
#		I_HIST_CSI_prev <- tableFilter(I_HIST_CSI_prev,"version",prior_version)
#		
#	#	I_HIST_CSI_prev <- I_HIST_CSI_prev[I_HIST_CSI_prev$VERSION==prior_version,]
#		
#		CSI_NAMES_ALL_prev <- unique(I_HIST_CSI_prev$CSI_ID)
#		CSI_CURVES_prev <- retrieveCSICurves(I_MOD_CSI_AC_prev, I_HIST_CSI_prev)
#		
#		print(paste0("Are previous CSI included in current database? -- ", all(CSI_NAMES_ALL_prev%in%CSI_NAMES_ALL_MONTHLY)))
#		print(paste0("Are current CSI included in previous database? -- ", all(CSI_NAMES_ALL_MONTHLY%in%CSI_NAMES_ALL_prev)))
#		print(paste0("Missing previous CSIs are: ",CSI_NAMES_ALL_MONTHLY[!CSI_NAMES_ALL_MONTHLY%in%CSI_NAMES_ALL_prev]))
#		print(paste0("Missing current CSIs are: ",CSI_NAMES_ALL_prev[!CSI_NAMES_ALL_prev%in%CSI_NAMES_ALL_MONTHLY]))
#		
#		csi_names <- CSI_NAMES_ALL_MONTHLY[CSI_NAMES_ALL_MONTHLY%in%CSI_NAMES_ALL_prev]
#		csi_names_new <- CSI_NAMES_ALL_MONTHLY[!CSI_NAMES_ALL_MONTHLY%in%CSI_NAMES_ALL_prev]
#		
#	#Plot Curves
#		if (OUTPUT_CSI_PLOT) {
#			for(i in 1:length(csi_names)) {
#				#	i = 64;
#				file_name = paste(Output_curve,"/",csi_names[[i]],"-Comparison.png",sep="")
#				png(file_name,width=600,height=400)
#				comparison <- ts.union(curr=CSI_CURVES_MONTHLY[[as.character(csi_names[i])]],prev=CSI_CURVES_prev[[as.character(csi_names[[i]])]])
#				ts.plot(comparison,ylab=csi_names[i], main=paste0("CSI_ID Comparison: ",csi_names[i]), col=1:2,type="b")
#				legend("topleft",c("Current CSI","Previous CSI"),col=1:2,pch=1)
#				dev.off()
#				
#				file_name = paste(Output_curve,"/",csi_names[[i]],"-Comparison-Diff.png",sep="")
#				png(file_name,width=600,height=400)
#				ts.plot(diff(comparison),ylab=csi_names[i], main=paste0("CSI_ID Comparison: ",csi_names[i]), col=1:2,type="b")
#				legend("topleft",c("Current CSI","Previous CSI"),col=1:2,pch=1)
#				dev.off()
#				
#				comparison <- ts.union(comparison,diff(comparison),(diff(comparison)[,1]-diff(comparison)[,2])/diff(comparison)[,2])
#				comparison <- data.frame(comparison,abs(comparison[,1]-comparison[,2])<0.1,row.names=as.yearmon(index(comparison)))
#				comparison[,5] <- formatPercentages(round(comparison[,5],4))
#				colnames(comparison) <- c("curr","prev","curr-diff(A)","prev-diff(B)","(A-B)/B","Comparison")
#				file_name = paste(Output_curve,"/",csi_names[[i]],"-Comparison.tex",sep="")
#				print(xtable(comparison,caption=paste0("Comparison of Spreads")),file=file_name,tabular.environment = 'longtable', floating=FALSE, include.rownames=TRUE)
#			}
#			
#			for (i in 1:length(csi_names_new)) {
##			csi_names_new[i]
#				file_name = paste(Output_curve,"/",csi_names_new[[i]],"-Comparison.png",sep="")
#				comparison <- ts.union(curr=CSI_CURVES_MONTHLY[[as.character(csi_names_new[i])]])
#				tsPlotGGplot(comparison,dep=csi_names_new[i],combined=TRUE,subtitle="NEW: original time series")
#				ggsave(file_name)
#				
#				file_name = paste(Output_curve,"/",csi_names_new[[i]],"-Comparison-Diff.png",sep="")
##			png(file_name,width=600,height=400)
##			ts.plot(diff(comparison),ylab=csi_names[i], main=paste0("CSI_ID Comparison: ",csi_names[i]), col=1:2,type="b")
##			legend("topleft",c("Current CSI","Previous CSI"),col=1:2,pch=1)
##			dev.off()
#				tsPlotGGplot(diff(comparison),dep=csi_names_new[i],combined=TRUE,subtitle="NEW: difference time series")
#				ggsave(file_name)
#				
#				comparison <- data.frame(Date=as.yearqtr(index(comparison)),curr=comparison)
#				file_name = paste(Output_curve,"/",csi_names_new[[i]],"-Comparison.tex",sep="")
#				print(xtable(comparison,caption=paste0("NEW: Historical Spreads")),file=file_name,tabular.environment = 'longtable', floating=FALSE, include.rownames=TRUE)
#			}
#		}
#	}
#}

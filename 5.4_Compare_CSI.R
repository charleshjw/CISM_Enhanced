# 
# 
# Author: e620927
###############################################################################

Output_forecast <- file.path(Output_root,"Forecast-CSI-Comparison")
if (!dir.exists(Output_forecast)) {
	dir.create(Output_forecast)
}

###Comparison 1: Compare credit spreads of current version v.s. prior version
csi_fcst_1 <- fetchTable(dbc, "O_CSI_FCST",version=current_version)
csi_fcst_1$FCST_DATE <- as.Date(as.character(csi_fcst_1$FCST_DATE))
csi_fcst_1$CSI_ID <- as.character(csi_fcst_1$CSI_ID)
csi_fcst_2 <- fetchTable(dbc_prev, "O_CSI_FCST",version=prior_version)
csi_fcst_2$FCST_DATE <- as.Date(csi_fcst_2$FCST_DATE)
csi_fcst_2$CSI_ID <- as.character(csi_fcst_2$CSI_ID)

scenario_mapping <- data.frame(ScenarioNames,ScenarioNames_Prior)
sceaniro_number = 1
for (i in 1:nrow(scenario_mapping)) {
	scenario_1 <- scenario_mapping[i,1]
	scenario_2 <- scenario_mapping[i,2]
	csi_id1 = unique(csi_fcst_1$CSI_ID)
	csi_id2 = unique(csi_fcst_2$CSI_ID)
	
	csi_not_exist = csi_id1[!csi_id1%in%csi_id2]
	csi_new = csi_id2[!csi_id2%in%csi_id1]
	csi_compare = csi_id1[csi_id1%in%csi_id2]
	
	csi_diff = c()
	for(csi in csi_compare) {
	#	csi = csi_compare[6]
		print(csi)
		history <- CSI_CURVES[[as.character(csi)]]
		csi_1 = csi_fcst_1[csi_fcst_1$SCEN_ID == scenario_1 & csi_fcst_1$CSI_ID == csi,]
		csi_2 = csi_fcst_2[csi_fcst_2$SCEN_ID == scenario_2 & csi_fcst_2$CSI_ID == csi,]
		csi_1 = combineCurves(extract_ts_from_df(csi_1,"FCST_DATE","LEVEL","SCEN_ID"))
		csi_2 = combineCurves(extract_ts_from_df(csi_2,"FCST_DATE","LEVEL","SCEN_ID"))
		if (!all(abs(csi_1-csi_2)<0.01)) {
			tmp_data = ts.union(history,csi_1,csi_2)
			colnames(tmp_data) = c("History",as.character(scenario_1),paste0("prior version-",as.character(scenario_2)))
			file_name <- paste(Output_forecast,"/",csi,"_",sceaniro_number,".png",sep="")
			tsPlotGGplot(tmp_data,dep_name=csi,combined=TRUE,subtitle="Comaprison of Credit Spread Forecast")
			ggsave(file_name)
#		dev.off()
			csi_diff = c(csi_diff,csi)
		}
	}
	sceaniro_number = sceaniro_number + 1
}
file_name <- paste(Output_forecast,"/CSI_COMPARE.tex",sep="")
writeVectorToTex(file_name,"CSICOMPARE",csi_diff)

### Comparison 2: Compare BRK versus CSIM
## pair of indices: market value, cumulative peak spread
get_csim_fitting <- function(csim_curve_name) {
	if(csim_curve_name %in%I_MOD_CSI_AC$CSI_ID) {
		csim_mod <- as.character(I_MOD_CSI_AC[I_MOD_CSI_AC$CSI_ID==csim_curve_name,"MOD_ID"])
		csim_fitting <- CSIM_fitting[CSIM_fitting$CLUSTER==csim_mod,]
		if(is.na(csim_fitting$IV2_COEFF)) {
			return(paste0("CSIM: ",round(csim_fitting$IV1_COEFF,4),"*",csim_fitting$Var1, " / R^2:", round(csim_fitting$R_SQ,2)))
		} else {
			return(paste0("CSIM: ",round(csim_fitting$IV1_COEFF,4),"*",csim_fitting$Var1," + ",round(csim_fitting$IV2_COEFF,4),"*",csim_fitting$Var2, " / R^2:", round(csim_fitting$R_SQ,2)))
		}
	} else {
		return("CSIM: No model fitting was found")
	}
}
get_brk_fitting <- function(brk_curve_name) {
	if(brk_curve_name %in%Brk_fitting[,1]) {
		brk_coeff <- Brk_fitting[Brk_fitting[,1]==brk_curve_name,2:ncol(Brk_fitting)]
		brk_coeff <- brk_coeff[,brk_coeff!=0]
		colnames(brk_coeff) <- gsub("[.]","_",colnames(brk_coeff))
		
		paste0("BRK: ",paste(paste0(round(brk_coeff[,colnames(brk_coeff)!="R_SQ",drop=FALSE],4),"*",colnames(brk_coeff[,colnames(brk_coeff)!="R_SQ",drop=FALSE])),collapse=" + ")," / R^2:",round(brk_coeff$R_SQ,2))
	} else {
		return("BRK: No model fitting was found")
	}
}

mapping_brk = tableFilter(list_files,"VariableName","BRK_Mapping")
mapping_csim = tableFilter(list_files,"VariableName","CSIM_Mapping")
fcst_brk = tableFilter(list_files,"VariableName","BRK_Fcst")
fitting_brk = tableFilter(list_files,"VariableName","BRK_Regression_Fitting")

# Comparison based on CSIM's view
if (nrow(mapping_brk)==1&nrow(mapping_csim)==1&nrow(fcst_brk)==1) {
	
	Mapping_brk <- read.csv(concat_file_path(mapping_brk))
	colnames(Mapping_brk)[1:4] = c("Security","ClientClassification","ClientCountry","SpreadType")
	Mapping_csim <- read.csv(concat_file_path(mapping_csim))
	Brk_fitting <- read.csv(concat_file_path(fitting_brk))
	CSIM_fitting <- fetchTable(dbc,"O_MOD_RESULTS_FINAL",current_version)
	
#	head(Mapping_csim)
#	head(Mapping_brk)
#	Mapping_brk <- Mapping_brk[,1:4]
	Fcst_brk <- read.csv(concat_file_path(fcst_brk))
	Fcst_brk$DateID <- convertNumericToDate(as.numeric(as.yearqtr(Fcst_brk$DateID, format = "%YQ%q")))
	
	## write brk forecast to excel and reformat
	Fcst_brk_sevadv <- tableFilter(Fcst_brk,"SCEN_ID","SevereAdverse-2018DFAST")
	CSI_CURVES_BRK <- extract_ts_from_df(Fcst_brk_sevadv,"DateID","CSI_VALUE","CSI_ID")
	csi_brk <- combineCurves(CSI_CURVES_BRK)
	brk_dates <- ts_time_to_date(csi_brk[,1])
	csi_brk <- as.data.frame(csi_brk)
	rownames(csi_brk) <- brk_dates
	file_name <- paste(Output_forecast,"/BRK_CSI_Level.xlsx",sep="")
	write.xlsx(csi_brk,file_name)

	csi_brk <- combineCurves(CSI_CURVES_BRK)
	csi_brk <- diff(csi_brk)
	brk_dates <- ts_time_to_date(csi_brk[,1])
	csi_brk <- as.data.frame(csi_brk)
	rownames(csi_brk) <- brk_dates
	file_name <- paste(Output_forecast,"/BRK_CSI_DIFF.xlsx",sep="")
	write.xlsx(csi_brk,file_name)
	
	## test if scenario names match, otherwise stop
	CSI_NAMES_BRK <- unique(Fcst_brk$CSI_ID)
	a <- unique(csi_fcst_1$SCEN_ID)
	b <- unique(Fcst_brk$SCEN_ID)
	if (!all(a%in%b) & all(b%in%a)) {
		print("***Error: scenario names in CSIM and BRK should match.")
	}
	### derive mapping using CUSIP-level mapping table from CSIM and BRK
	Mapping_merge <- merge(Mapping_csim,Mapping_brk,by=c("Security"))
#	head(Mapping_merge)
	Mapping_merge <- Mapping_merge[c("Security","MarketValue","ASSETCLASSCSIM","SpreadIndex_ID","SpreadType","Currency","WAL","ReferenceRate","CurveUsed")]
	Mapping_merge$WAL <- round(as.numeric(as.character(Mapping_merge$WAL)),1)
	Mapping_merge$MarketValue <- round(as.numeric(as.character(Mapping_merge$MarketValue/1000000)),1)
#	Mapping_merge$MaturityOrig <- round(as.numeric(as.character(Mapping_merge$MaturityOrig)),1)

	market_value <- cast(Mapping_merge[,c("SpreadIndex_ID","SpreadType","MarketValue")],SpreadIndex_ID~SpreadType,fun.aggregate=sum,value="MarketValue")
	market_value <- melt(market_value)
	market_value <- market_value[market_value$value > 0,]
#	market_value <- merge(Mapping_merge[,c("ASSETCLASSCSIM","SpreadIndex_ID")],market_value,by="SpreadIndex_ID")
	
#	market_value <- market_value[order(market_value$value,decreasing = TRUE),]
	rownames(market_value) <- 1:nrow(market_value)
	market_value$PeakDifference <- NA
#	count_mapping <- cast(Mapping_merge[,c("SpreadIndex_ID","SpreadType","MarketValue")],SpreadIndex_ID~SpreadType,fun.aggregate=length,value="MarketValue")
#	count_mapping <- melt(count_mapping)
#	count_mapping <- count_mapping[count_mapping$value > 0,]
#	rownames(count_mapping) <- 1:nrow(count_mapping)

	### Compare the fcst
	unique_csim_csi = unique(market_value$SpreadIndex_ID)
	csi_valid = c()
	for (j in 1:length(unique_csim_csi)) {
#		j = 1
		tmp_mapping <- market_value[market_value$SpreadIndex_ID == as.character(unique_csim_csi[j]),]
		print(as.character(unique_csim_csi[j]))
		
		for (i in 1:nrow(tmp_mapping)) {
#			i = 1
			csim_curve_name = as.character(unique_csim_csi[j])
			brk_curve_name = as.character(tmp_mapping$SpreadType[i])
			
			mv = round(tableFilter(tableFilter(market_value,"SpreadIndex_ID",csim_curve_name),"SpreadType",brk_curve_name)["value"],1)
			
			subtit = paste0(csim_curve_name,"(CSIM) : ",brk_curve_name , "(BRK) - Market Value is ", mv,"MM")
			
			fitting_comparison <- paste0(get_csim_fitting(csim_curve_name),"\n",get_brk_fitting(brk_curve_name))
			
			if(csim_curve_name%in%CSI_NAMES_ALL&brk_curve_name%in%CSI_NAMES_BRK) {
				history <- CSI_CURVES[[csim_curve_name]]
				
				csi_csim = csi_fcst_1[csi_fcst_1$CSI_ID == csim_curve_name,]
				csi_csim = combineCurves(extract_ts_from_df(csi_csim,"FCST_DATE","LEVEL","SCEN_ID"))
				
				brk_curve <- tableFilter(Fcst_brk,"CSI_ID",brk_curve_name)
				CSI_CURVES_BRK <- extract_ts_from_df(brk_curve,"DateID","CSI_VALUE","SCEN_ID")
				csi_brk <- combineCurves(CSI_CURVES_BRK)
				csi_brk <- csi_brk[,colnames(csi_csim)]
				
				#comparison of level values of forecast
				file_name <- paste(Output_forecast,"/",csim_curve_name,"_brk_",i,".png",sep="")
#				tsPlotGGplot(ts.union(history, csi_csim, csi_brk),dep_name="Comparison between CSIM and BRK",combined=TRUE,subtitle=subtit)
#				ggsave(file_name,scale=1.2)
				leges = c("History",paste0("CSIM_",colnames(csi_csim)),paste0("BRK_",colnames(csi_csim)))
				pchs = c(1,rep(2,ncol(csi_csim)),rep(3,ncol(csi_csim)))
				png(file_name,width=800,height=600)
				tsPlot(ts.union(history, csi_csim, csi_brk),dep=paste0("Comparison between CSIM and BRK\n",subtit,"\n"),leg_names=leges,subtitle=fitting_comparison,marks=TRUE,color=c(1,rep(2:(1+ncol(csi_csim)),2)),pch=pchs)
				dev.off()
				
				#comparison of absolute changes of forecast
				csim_last_ob <- as.numeric(tail(history,1))
				brk_last_ob <- as.numeric(csi_brk[1])
				leges = c(paste0("CSIM_",colnames(csi_csim)),paste0("BRK_",colnames(csi_csim)))
				pchs = c(rep(2,ncol(csi_csim)),rep(3,ncol(csi_csim)))
				file_name <- paste(Output_forecast,"/",csim_curve_name,"_brk_",i,"_diff.png",sep="")
				png(file_name,width=800,height=600)
				tsPlot(ts.union(csi_csim-csim_last_ob, csi_brk-brk_last_ob),dep=paste0("Comparison between CSIM and BRK\n",subtit,"\n"),leg_names=leges,subtitle=fitting_comparison,marks=TRUE,color=c(rep(2:(1+ncol(csi_csim)),2)),pch=pchs)
				dev.off()
				
				csi_valid <- c(csi_valid,csim_curve_name)
				market_value[market_value$SpreadIndex_ID==csim_curve_name&market_value$SpreadType==brk_curve_name,"PeakDifference"] = round(max(csi_csim-csim_last_ob)-max(csi_brk-brk_last_ob),1)
			} else if (csim_curve_name%in%CSI_NAMES_ALL&!brk_curve_name%in%CSI_NAMES_BRK) {
#				stop()
				history <- CSI_CURVES[[csim_curve_name]]
				csi_csim = csi_fcst_1[csi_fcst_1$CSI_ID == csim_curve_name,]
				csi_csim = combineCurves(extract_ts_from_df(csi_csim,"FCST_DATE","LEVEL","SCEN_ID"))
				
				#comparison of level values of forecast
				file_name <- paste(Output_forecast,"/",csim_curve_name,"_brk_",i,".png",sep="")
#				tsPlotGGplot(ts.union(history, csi_csim),dep_name="Comparison between CSIM and BRK - forecast from BRK is not available ~~",combined=TRUE,subtitle=subtit)
#				ggsave(file_name,scale=1.2)
				leges = c("History",paste0("CSIM_",colnames(csi_csim)))
				pchs = c(1,rep(2,ncol(csi_csim)))
				png(file_name,width=800,height=600)
				tsPlot(ts.union(history, csi_csim),dep=paste0("Comparison between CSIM and BRK - forecast from BRK is not available ~~\n",subtit,"\n"),leg_names=leges,subtitle=fitting_comparison,marks=TRUE,color=c(1,rep(2:(1+ncol(csi_csim)))),pch=pchs)
				dev.off()
			
				#comparison of absolute changes of forecast
				csim_last_ob <- as.numeric(tail(history,1))
				leges = c(paste0("CSIM_",colnames(csi_csim)))
				pchs = c(rep(2,ncol(csi_csim)))
				file_name <- paste(Output_forecast,"/",csim_curve_name,"_brk_",i,"_diff.png",sep="")
				png(file_name,width=800,height=600)
				tsPlot(ts.union(csi_csim-csim_last_ob),dep=paste0("Comparison between CSIM and BRK - forecast from BRK is not available ~~\n",subtit,"\n"),leg_names=leges,subtitle=fitting_comparison,marks=TRUE,color=c(rep(2:(1+ncol(csi_csim)))),pch=pchs)
				dev.off()
				
				csi_valid <- c(csi_valid,csim_curve_name)
			} else if (!csim_curve_name%in%CSI_NAMES_ALL&brk_curve_name%in%CSI_NAMES_BRK) {
#				stop()
				history <- CSI_CURVES[[csim_curve_name]]
				brk_curve <- tableFilter(Fcst_brk,"CSI_ID",brk_curve_name)
				CSI_CURVES_BRK <- extract_ts_from_df(brk_curve,"DateID","CSI_VALUE","SCEN_ID")
				csi_brk <- combineCurves(CSI_CURVES_BRK)
				
				#comparison of level values of forecast
				file_name <- paste(Output_forecast,"/",csim_curve_name,"_brk_",i,".png",sep="")
#				tsPlotGGplot(csi_brk,dep_name="Comparison between CSIM and BRK - forecast from CSIM is not available ~~",combined=TRUE,subtitle=subtit)
#				ggsave(file_name,scale=1.2)
				leges = c("History",paste0("BRK_",colnames(csi_brk)))
				pchs = c(1,rep(3,ncol(csi_brk)))
				png(file_name,width=800,height=600)
				tsPlot(ts.union(history, csi_brk),dep=paste0("Comparison between CSIM and BRK - forecast from CSIM is not available ~~\n",subtit,"\n"),leg_names=leges,subtitle=fitting_comparison,marks=TRUE,color=c(1,rep(2:(1+ncol(csi_brk)))),pch=pchs)
				dev.off()

				#comparison of absolute changes of forecast
				brk_last_ob <- as.numeric(csi_brk[1])
				leges = c(paste0("BRK_",colnames(csi_brk)))
				pchs = c(rep(3,ncol(csi_brk)))
				file_name <- paste(Output_forecast,"/",csim_curve_name,"_brk_",i,"_diff.png",sep="")
				png(file_name,width=800,height=600)
				tsPlot(ts.union(csi_brk-brk_last_ob),dep=paste0("Comparison between CSIM and BRK - forecast from CSIM is not available ~~\n",subtit,"\n"),leg_names=leges,subtitle=fitting_comparison,marks=TRUE,color=c(rep(2:(1+ncol(csi_brk)))),pch=pchs)
				dev.off()
				
				csi_valid <- c(csi_valid,csim_curve_name)
			} else {
				print("Both unavailable")
			}
			csim_cusips = tableFilter(Mapping_merge,"SpreadIndex_ID",csim_curve_name)
			csim_cusips = tableFilter(csim_cusips,"SpreadType",brk_curve_name)
			if(nrow(csim_cusips)>0) {
#				colnames(csim_cusips) <- c("CUSIP","MarketValue","Asset Class","CSI_CSIM","CSI_BRK","Currency","CreditRt","WAL","MaturityOrig","FixFloat","AFS_Ratio")
				csim_cusips = csim_cusips[order(csim_cusips[,"ASSETCLASSCSIM"], csim_cusips[,"MarketValue"], decreasing=TRUE),]
				rownames(csim_cusips) = 1:nrow(csim_cusips)
#				if (exists("MTM_CUSIP")) {
#					csim_cusips = merge(x = csim_cusips, y = MTM_CUSIP, by = "CUSIP", all.x = TRUE)
#				}
				
#				summary = csim_cusips[1,]
#				summary = c(rep("",ncol(summary)-2),sum(as.numeric(csim_cusips[,"MTMADHOC"]),na.rm=TRUE),sum(as.numeric(csim_cusips[,"MTMBRK"]),na.rm=TRUE))
				
				file_name <- paste(Output_forecast,"/",csim_curve_name,"_brk_",i,".tex",sep="")
				writeMatrixToTex(file_name,cap=paste0("CUSIPs mapped to ",csim_curve_name, " in CSIM. Market Value ",sum(csim_cusips$MarketValue),"MM"),csim_cusips)
			}
		}
	}
	
	market_value <- market_value[,c(1,3,2,4)]
	colnames(market_value) <- c("CSIM_Index","BRK_Index","Market_Value","Peak_CSIMtoBRK_Diff")
	file_name <- paste(Output_forecast,"/MarketValueByPair.tex",sep="")
	writeMatrixToTex(file_name,cap=paste0("Market Value in descending order in USD Millions"),market_value)
	
	file_name <- paste(Output_forecast,"/MarketValueByPair.csv",sep="")
	write.csv(market_value,file_name)
	
	file_name <- paste(Output_forecast,"/CSI_COMPARE_BRK.tex",sep="")
	writeVectorToTex(file_name,"CSICOMPAREBRK",unique_csim_csi)
	
	if (exists("MTM_CUSIP")) {
		Mapping_merge_MTM = merge(x = Mapping_merge, y = MTM_CUSIP, by = "Security", all.x = TRUE)
		Mapping_merge_MTM[,"MTMBRK"] <- as.numeric(as.character(Mapping_merge_MTM[,"MTMBRK"]))
		Mapping_merge_MTM[,"MTMADHOC"] <- as.numeric(as.character(Mapping_merge_MTM[,"MTMADHOC"]))
		
#		Mapping_merge_MTM[Mapping_merge_MTM$Security=="31418V4D1",]
		write.xlsx(Mapping_merge_MTM, paste(Output_forecast,"/MTM_Comparison.xlsx",sep=""))
	}
}

##adhoc analysis
#if(FALSE) {
#	get_brk_fcst <- function(brk_curve_name,normalized=FALSE) {
#		brk_curve <- tableFilter(Fcst_brk,"CSI_ID",brk_curve_name)
#		csi_brk <- combineCurves(extract_ts_from_df(brk_curve,"DateID","CSI_VALUE","SCEN_ID"))
#		if (normalized) {
#			brk_last_ob <- as.numeric(csi_brk[1])
#			return(csi_brk-brk_last_ob)
#		} else {
#			return(csi_brk)
#		}
#	}
#	
#	get_brk_fcsts <- function(brk_curve_names,normalized=FALSE) {
#		csi_brk_union <- NULL
#		for (brk_curve_name in brk_curve_names) {
##			brk_curve_name = brk_curve_names[1]
#			csi_brk_union  <- ts.union(csi_brk_union,get_brk_fcst(brk_curve_name,normalized))
#		}
#		return(csi_brk_union)
#	}
#	
##	brk_curve_names = c("CORP-US-NONFIN-A-10Y","CORP-US-NONFIN-A-5Y")
##	brk_curve_values <- get_brk_fcsts(brk_curve_names,normalized=TRUE)
##	pchs = c(rep(2,3),rep(3,3))
##	png(file_name,width=800,height=600)
##	tsPlot(,dep="",leg_names=leges,subtitle="",marks=TRUE,color=rep(c(2,3,4),2),pch=pchs)
##	dev.off()
#	a <- get_brk_fcsts(brk_curve_names[1],normalized=TRUE)
#	b <- get_brk_fcsts(brk_curve_names[2],normalized=TRUE)
#	ab <- ts.union(a,b)
#	leges<-c(paste0(brk_curve_names,"/",colnames(a)),paste0(brk_curve_names,"/",colnames(b)))
#	colnames(ab) <- leges
#	file_name <- paste(Output_forecast,"/COMPARISON_BRK_SPREAD_CORP_NONFIN_A.xlsx",sep="")
#	write.xlsx(ab,file_name)
#	
##	tsPlot(ts.union(a,b),dep="",leg_names=leges,subtitle="",marks=TRUE,color=rep(c(2,3,4),2),pch=pchs)
##	tsPlotGGplot(ab,dep_name="",combined = TRUE)
#	
#	
#	
#	leges = c("CORP-US-NONFIN-BBB-10Y","CORP-US-NONFIN-BBB-5Y")
#	pchs = c(rep(2,3),rep(3,3))
#	file_name <- paste(Output_forecast,"/COMPARISON_BRK_SPREAD_CORP_NONFIN_BBB.png",sep="")
#	png(file_name,width=800,height=600)
#	tsPlot(get_brk_fcsts(leges,normalized=TRUE),dep="",leg_names=leges,subtitle="",marks=TRUE,color=rep(c(2,3,4),2),pch=pchs)
#	dev.off()
#	
#	
#	leges = c("MUNI-GO-AA-1Y","MUNI-GO-AA-5Y")
#	pchs = c(rep(2,3),rep(3,3))
#	file_name <- paste(Output_forecast,"/COMPARISON_BRK_SPREAD_CORP_NONFIN_BBB.png",sep="")
#	png(file_name,width=800,height=600)
#	tsPlot(ts.union(get_brk_fcst("CORP-US-NONFIN-A-10Y",normalized=TRUE),get_brk_fcst("CORP-US-NONFIN-A-5Y",normalized=TRUE)),dep="",leg_names=leges,subtitle="",marks=TRUE,color=rep(c(2,3,4),2),pch=pchs)
#	dev.off()
#}








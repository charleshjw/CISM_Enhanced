# 
# 
# Author: e620927
###############################################################################

Output_forecast <- file.path(Output_root,"MtM-Comparison-BRK")
if (!dir.exists(Output_forecast)) {
	dir.create(Output_forecast)
}

aggregate_mts <- function(mts_series,element_names=NULL,scenario="BHC Sev Adverse") {
#	mts_series = FV_BRK
#	element_names = select_tickets
	if(is.null(element_names)) {
		element_names = names(mts_series)
	}
	ts_combined <- ""
	for (ticket in element_names) {
		#		ticket = select_tickets[1]
		if (ts_combined=="") {
			ts_combined <- mts_series[[ticket]][,scenario]
		} else {
			ts_combined <- ts.union(ts_combined," " = mts_series[[ticket]][,scenario])
			colnames(ts_combined) <- NULL
		}
	}
	
	if (class(ts_combined)[1]=="ts") {
		return(ts_combined)
	} else {
		colnames(ts_combined) <- element_names
		return(sum_mts(ts_combined))
	}
}

aggregate_by_ccar_sector <- function(db_mts,scenario) {
#		db_mts <- FV_BRK
	mts_series <- ""
	ccar_sectors <- unique(AOCI_tmp$CCAR_Sector)
	for (ccar_sector in ccar_sectors) {
		print(ccar_sector)
		select_tickets <- unique(AOCI_tmp[AOCI_tmp$CCAR_Sector == ccar_sector,"Ticket"])
		fairvalue_brk = aggregate_mts(db_mts,element_names=select_tickets,scenario=scenario)
		if (mts_series ==""){
			mts_series = round(fairvalue_brk/1e6,0)
		} else {
			mts_series <- ts.union(mts_series," "=round(fairvalue_brk/1e6,0))
		}
#			print(mts_series)
	}
	colnames(mts_series) <- ccar_sectors
	return(mts_series)
}

if (!exists("csi_fcst")) {
	csi_fcst <- fetchTable(dbc, "O_CSI_FCST",version=current_version)
	csi_fcst$FCST_DATE <- as.Date(as.character(csi_fcst$FCST_DATE))
	csi_fcst$CSI_ID <- as.character(csi_fcst$CSI_ID)
}

get_peak_incremental_diff_by_quarter_CISM <- function(csim_curve_name,scenario_name=NULL,quarter=NULL) {
	history <- CSI_CURVES[[csim_curve_name]]
	
	csi_csim = csi_fcst[csi_fcst$CSI_ID == csim_curve_name,]
	if (nrow(csi_csim)==0) {
		return(NA)
	} else {
		csi_csim = combineCurves(extract_ts_from_df(csi_csim,"FCST_DATE","LEVEL","SCEN_ID"))
		csim_last_ob <- as.numeric(tail(history,1))
		
		if (is.null(scenario_name)) {
			column_num = grep("Severe",colnames(csi_csim))
			if (is.null(quarter)) {
				quarter = 2
			}
			return(max(csi_csim[quarter,column_num]-csim_last_ob))
		} else {
			if (grepl("Severe",scenario_name)) {
				quarter = 2
			} else if (grepl("Base",scenario_name)) {
				quarter = 2
			} else {
				quarter = 3
			}
			return(max(csi_csim[quarter,scenario_name]-csim_last_ob))
		}
	}
}

get_peak_incremental_diff_CISM <- function(csim_curve_name,scenario_name=NULL) {
	#CSIM
	history <- CSI_CURVES[[csim_curve_name]]
	
	csi_csim = csi_fcst[csi_fcst$CSI_ID == csim_curve_name,]
	if (nrow(csi_csim)==0) {
		return(NA)
	} else {
		csi_csim = combineCurves(extract_ts_from_df(csi_csim,"FCST_DATE","LEVEL","SCEN_ID"))
		csim_last_ob <- as.numeric(tail(history,1))
		
		if (is.null(scenario_name)) {
			column_num = grep("Severe",colnames(csi_csim))
			return(max(csi_csim[,column_num]-csim_last_ob))
		} else {
			return(max(csi_csim[,scenario_name]-csim_last_ob))
		}
	}
}

get_norm_fcst_CSIM <- function(csim_curve_name,scenario_name=NULL) {
	#CSIM
	history <- CSI_CURVES[[csim_curve_name]]
	
	csi_csim = csi_fcst[csi_fcst$CSI_ID == csim_curve_name,]
	if (nrow(csi_csim)==0) {
		return(NA)
	} else {
		csi_csim = combineCurves(extract_ts_from_df(csi_csim,"FCST_DATE","LEVEL","SCEN_ID"))
		csim_last_ob <- as.numeric(tail(history,1))
		
		if (is.null(scenario_name)) {
			column_num = grep("Severe",colnames(csi_csim))
			return(csi_csim[,column_num]-csim_last_ob)
		} else {
			return(csi_csim[,scenario_name]-csim_last_ob)
		}
	}
}

if (!exists("Fcst_brk")) {
	fcst_brk = tableFilter(list_files,"VariableName","BRK_Fcst")
	Fcst_brk <- read.csv(concat_file_path(fcst_brk))
	Fcst_brk$DateID <- convertNumericToDate(as.numeric(as.yearqtr(Fcst_brk$DateID, format = "%YQ%q")))
}

get_norm_fcst_BRK <- function(brk_curve_name,scenario_name=NULL) {
	brk_curve <- tableFilter(Fcst_brk,"CSI_ID",brk_curve_name)
	if (nrow(brk_curve)==0) {
		return(NA)
	} else {
		CSI_CURVES_BRK <- extract_ts_from_df(brk_curve,"DateID","CSI_VALUE","SCEN_ID")
		csi_brk <- combineCurves(CSI_CURVES_BRK)
		brk_last_ob <- as.numeric(csi_brk[1])
		
		if (is.null(scenario_name)) {
			column_num = grep("Severe",colnames(csi_brk))
			return(csi_brk[,column_num]-brk_last_ob)
		} else {
			return(csi_brk[,scenario_name]-brk_last_ob)
		}
	}
}

get_peak_incremental_diff_BRK <- function(brk_curve_name,scenario_name=NULL) {
	#CSIM
#	scenario_name = "SevereAdverse-2018DFAST"
	brk_curve <- tableFilter(Fcst_brk,"CSI_ID",brk_curve_name)
	if (nrow(brk_curve)==0) {
		return(NA)
	} else {
		CSI_CURVES_BRK <- extract_ts_from_df(brk_curve,"DateID","CSI_VALUE","SCEN_ID")
		csi_brk <- combineCurves(CSI_CURVES_BRK)
		brk_last_ob <- as.numeric(csi_brk[1])
		if (is.null(scenario_name)) {
			column_num = grep("Severe",colnames(csi_brk))
			return(max(csi_brk[,column_num]-brk_last_ob))
		} else {
			return(max(csi_brk[,scenario_name]-brk_last_ob))
		}
	}
}

get_incremental_diff_by_quarter_BRK <- function(brk_curve_name,scenario_name=NULL,quarter=NULL) {
	#CSIM
	brk_curve <- tableFilter(Fcst_brk,"CSI_ID",brk_curve_name)
	if (nrow(brk_curve)==0) {
		return(NA)
	} else {
		csi_brk <- combineCurves(extract_ts_from_df(brk_curve,"DateID","CSI_VALUE","SCEN_ID"))
		brk_last_ob <- as.numeric(csi_brk[1])
		
		if (is.null(scenario_name)) {
			column_num = grep("Severe",colnames(csi_brk))
			if (is.null(quarter)) {
				quarter = 3
			}
			return(max(csi_brk[quarter,column_num]-brk_last_ob))
		} else {
			if (grepl("Severe",scenario_name)) {
				quarter = 3
			} else if (grepl("Base",scenario_name)) {
				quarter = 3
			} else {
				quarter = 4
			}
			return(max(csi_brk[quarter,scenario_name]-brk_last_ob))
		}
	}
}

get_ticket_forecast <- function(ticket,scenario = scenario_to_compare) {
	tmp_data = ts.union(AOCI_BRK[[as.character(ticket)]][,scenario],AOCI_Adhoc[[as.character(ticket)]][,scenario])
	colnames(tmp_data) = c("BRK","Adhoc")
	return(tmp_data)
}

get_ticket_fv <- function(ticket) {
	return(FV_BRK[[as.character(ticket)]][1,1])
}

get_csim_spread_name_by_cusip <- function(cusip) {
	tble = tableFilter(Mapping_merge,"Security",as.character(cusip))
	if(nrow(tble)==1) {
		return(tble$SpreadIndex_ID)
	} else {
		return("BRK SPREAD NOT FOUND")
	}
	
}

get_brk_spread_name_by_cusip <- function(cusip) {
#	cusip = as.character(Mapping_Ticket_CUSIP[i,'CUSIP'])
	tble = tableFilter(Mapping_merge,"Security",as.character(cusip))
	if(nrow(tble)==1) {
		return(tble$SpreadType)
	} else {
		return("BRK SPREAD NOT FOUND")
	}
}

get_brk_spreads_by_cusip <- function(cusip_tmp) {
#	cusip_tmp = as.character(Mapping_Ticket_CUSIP[i,'CUSIP'])
	tmp_csim_spread <- get_csim_spread_name_by_cusip(cusip_tmp)
	tmp_brk_spread <- get_brk_spread_name_by_cusip(cusip_tmp)
	spreads <- Mapping_Spread[Mapping_Spread[,1] == tmp_csim_spread & Mapping_Spread[,2] == tmp_brk_spread,]
	if (nrow(spreads)==1) {
		return(spreads[5])
	} else {
		return(NA)
	}
}

get_brk_spread_duration_by_cusip <- function(cusip_tmp) {
#	cusip_tmp = as.character(Mapping_Ticket_CUSIP[i,'CUSIP'])
	tble = tableFilter(Mapping_merge,"Security",as.character(cusip_tmp))
	if(nrow(tble)==1) {
		return(tble$SpreadDuration)
	} else {
		return("BRK SPREAD DURATION NOT FOUND")
	}
}

get_brk_country_by_cusip <- function(cusip_tmp) {
#	cusip_tmp = as.character(Mapping_Ticket_CUSIP[i,'CUSIP'])
	tble = tableFilter(Mapping_merge,"Security",as.character(cusip_tmp))
	if(nrow(tble)==1) {
		return(as.character(tble$ClientCountry))
	} else {
		return("BRK Country not Found")
	}
}

get_csim_spreads_by_cusip <- function(cusip_tmp) {
	tmp_csim_spread <- get_csim_spread_name_by_cusip(cusip_tmp)
	tmp_brk_spread <- get_brk_spread_name_by_cusip(cusip_tmp)
	spreads <- Mapping_Spread[Mapping_Spread[,1] == tmp_csim_spread & Mapping_Spread[,2] == tmp_brk_spread,]
	if (nrow(spreads)==1) {
		return(spreads[6])
	} else {
		return(NA)
	}
}

get_cusip_by_ticket <- function(ticket) {
	return(as.character(tableFilter(Mapping_Ticket_CUSIP,"Ticket",ticket)$CUSIP))
}

get_spreads_by_ticket <- function(ticket) {
#			ticket = ticket
	cusip_tmp <- tableFilter(Mapping_Ticket_CUSIP,"Ticket",ticket)$CUSIP
	return(get_spreads_by_cusip(cusip_tmp))
}

get_spreads_by_cusip <- function(cusip_tmp) {
	tmp_csim_spread <- get_csim_spread_name_by_cusip(cusip_tmp)
	tmp_brk_spread <- get_brk_spread_name_by_cusip(cusip_tmp)
	spreads <- Mapping_Spread[Mapping_Spread[,1] == tmp_csim_spread & Mapping_Spread[,2] == tmp_brk_spread,]
	return(paste0("BRK Spread: ",tmp_csim_spread," - ",round(spreads[3],0),"/CSIM Spread: ",tmp_brk_spread," - ",round(spreads[4],0)))
}

plot_tickets <- function(Mapping_Ticket_CUSIP) {
	tickets <- Mapping_Ticket_CUSIP$Ticket
	for (ticket in tickets) {
#		ticket = "6038000015"
		if (ticket%in%tickets_brk) {
			tmp_data <- get_ticket_forecast(ticket)
			file_name <- paste(Output_forecast,"/Comparison_Level_",cleanString(scenario_to_compare),"_",cleanString(ticket),".png",sep="")
			tsPlotGGplot(tmp_data,dep_name=paste0(scenario_to_compare,": Comaprison of MTM for ticket ", ticket),combined=TRUE,subtitle=get_spreads_by_ticket(ticket))
			ggsave(file_name)
			
			csim_curve_name = as.character(get_csim_spread_name_by_cusip(get_cusip_by_ticket(ticket)))
			brk_curve_name = as.character(get_brk_spread_name_by_cusip(get_cusip_by_ticket(ticket)))
			
			tmp_fcst_csim = get_norm_fcst_CSIM(csim_curve_name)
			tmp_fcst_brk = get_norm_fcst_BRK(brk_curve_name)
			if (!is.na(tmp_fcst_csim) & !is.na(tmp_fcst_brk)) {
				fcst_union = ts.union(tmp_fcst_brk,tmp_fcst_csim)
				fcst_union[is.na(fcst_union)] = 0
				file_name <- paste(Output_forecast,"/Comparison_Spread_",cleanString(scenario_to_compare),"_",cleanString(ticket),".png",sep="")
				tsPlotGGplot(fcst_union,dep_name=paste0(scenario_to_compare,": Comaprison of Spreads for ticket ", ticket),combined=TRUE,subtitle=get_spreads_by_ticket(ticket))
				ggsave(file_name)
			}
		}
	}
	for (ticket in tickets) {
#		ticket = "848000622"
		if (ticket%in%tickets_brk) {

			file_name <- paste(Output_forecast,"/Comparison_Spread_",cleanString(scenario_to_compare),"_",cleanString(ticket),".tex",sep="")
			writeMatrixToTex(file_name,cap="Comparison of Spread and MTM",tableFilter(Mapping_Ticket_CUSIP,"Ticket",ticket))
			
#			file_name <- paste(Output_forecast,"/Comparison_Spread_",cleanString(scenario_to_compare),"_",cleanString(ticket),"_BRK.tex",sep="")
#			writeMatrixToTex(file_name,cap="BRK",tableFilter(Mapping_brk,"Security",get_cusip_by_ticket(ticket)))
#			
#			file_name <- paste(Output_forecast,"/Comparison_Spread_",cleanString(scenario_to_compare),"_",cleanString(ticket),"_Adhoc.tex",sep="")
#			writeMatrixToTex(file_name,cap="Ad hoc",tableFilter(Mapping_csim_info,"Security",get_cusip_by_ticket(ticket)))
		}
	}
}



### Mapping between BRK and CSIM
#get cusip level mapping
mapping_brk = tableFilter(list_files,"VariableName","BRK_Mapping")
mapping_csim = tableFilter(list_files,"VariableName","CSIM_Mapping")
mapping_csim_curve = tableFilter(list_files,"VariableName","CSIM_Mapping_BRK_CURVE")

Mapping_brk <- read.csv(concat_file_path(mapping_brk))
colnames(Mapping_brk)[1:4] = c("Security","ClientClassification","ClientCountry","SpreadType")

Mapping_csim <- read.csv(concat_file_path(mapping_csim))
Mapping_csim <- Mapping_csim[,c('Security','MarketValue','ASSETCLASSCSIM','SpreadIndex_ID')]

Mapping_csim_curve <- read.csv(concat_file_path(mapping_csim_curve))
colnames(Mapping_csim_curve) <- c("ClientClassification","ClientCountry","Security","Currency","CurveUsed","SpreadType")
Mapping_csim_info <- merge(Mapping_csim,Mapping_csim_curve,by=c("Security"))

Mapping_merge <- merge(Mapping_csim,Mapping_brk,by=c("Security"))
#head(Mapping_merge)

Mapping_Spread <- unique(Mapping_merge[,c("SpreadIndex_ID","SpreadType")])
Mapping_Spread["Incremental Peak Spread - BRK"] = 0
Mapping_Spread["Incremental Peak Spread - CSIM"] = 0
Mapping_Spread["Q2 Spread Diff - BRK"] = 0
Mapping_Spread["Q2 Spread Diff - CSIM"] = 0

for (i in 1:nrow(Mapping_Spread)) {
#	i = 1
	brk_curve_name<- as.character(Mapping_Spread$SpreadType[i])
	Mapping_Spread[i,"Incremental Peak Spread - BRK"] <- get_peak_incremental_diff_BRK(brk_curve_name)
	Mapping_Spread[i,"Q2 Spread Diff - BRK"] <- get_incremental_diff_by_quarter_BRK(brk_curve_name)
	
	csim_curve_name <- as.character(Mapping_Spread$SpreadIndex_ID[i])
	Mapping_Spread[i,"Incremental Peak Spread - CSIM"] <- get_peak_incremental_diff_CISM(csim_curve_name)
	Mapping_Spread[i,"Q2 Spread Diff - CSIM"] <- get_peak_incremental_diff_by_quarter_CISM(csim_curve_name)
}

#get ticket to cusip mapping
aoci_brk = tableFilter(list_files,"VariableName","AOCIOutput-BRK")
aoci_adhoc = tableFilter(list_files,"VariableName","AOCIOutput-Adhoc1")

if (nrow(aoci_brk)==1&nrow(aoci_adhoc)==1) {
	
	#read BRK MtM
	AOCI_brk <- read.csv(concat_file_path(aoci_brk))
	AOCI_tmp <- tableFilter(AOCI_brk,"AFS_HTM","AFS")

	AOCI_BRK <- list()
	FV_BRK <- list()
#	BV_BRK <- list() # BookValue_Q0FX 
	
	tickets_brk = unique(AOCI_tmp$Ticket)
	
	for (ticket in tickets_brk) {
		if (dim(AOCI_tmp[AOCI_tmp$Ticket == ticket,])[1] == 30) {
			AOCI_BRK[[as.character(ticket)]] = combineCurves(extract_ts_from_df(datatable=AOCI_tmp[AOCI_tmp$Ticket == ticket,],date_column="HorizonDate",value_column="AOCI_Q0FX",name_column="Scenario"))
			FV_BRK[[as.character(ticket)]] = combineCurves(extract_ts_from_df(datatable=AOCI_tmp[AOCI_tmp$Ticket == ticket,],date_column="HorizonDate",value_column="FairValue_Q0FX",name_column="Scenario"))
		} else {
			print(ticket)
		}
	}
	
	#read Adhoc MtM
	AOCI_adhoc <- read.csv(concat_file_path(aoci_adhoc))
	AOCI_tmp <- tableFilter(AOCI_adhoc,"AFS_HTM","AFS")
	
	AOCI_Adhoc <- list()
	FV_Adhoc <- list()
	
	tickets_adhoc = unique(AOCI_tmp$Ticket)
	for (ticket in tickets_adhoc) {
		if (dim(AOCI_tmp[AOCI_tmp$Ticket == ticket,])[1] == 30) {
			AOCI_Adhoc[[as.character(ticket)]] = combineCurves(extract_ts_from_df(datatable=AOCI_tmp[AOCI_tmp$Ticket == ticket,],date_column="HorizonDate",value_column="AOCI_Q0FX",name_column="Scenario"))
			FV_Adhoc[[as.character(ticket)]] = combineCurves(extract_ts_from_df(datatable=AOCI_tmp[AOCI_tmp$Ticket == ticket,],date_column="HorizonDate",value_column="FairValue_Q0FX",name_column="Scenario"))
		} else {
			print(ticket)
		}
	}
	
	if (!all(tickets_brk %in% tickets_adhoc)|!all(tickets_adhoc %in% tickets_brk)){
		stop("Tickets in the two files are not consistent. Check!")
	}
	
	# get mapping table between ticket and cusip
	Scenarios <- unique(AOCI_tmp$Scenario)
	Scenarios <- Scenarios[3]
	for (scenario_to_compare in Scenarios) {
#		scenario_to_compare <- Scenarios
		Mapping_Ticket_CUSIP <- unique(AOCI_brk[,c("Ticket","CUSIP","CCAR_Sector")])
		Mapping_Ticket_CUSIP["MTM_BRK"] <- NA
		Mapping_Ticket_CUSIP["MTM_ADHOC"] <- NA
		Mapping_Ticket_CUSIP["Spread_BRK"] <-NA
		Mapping_Ticket_CUSIP["Spread_CSIM"] <-NA
		Mapping_Ticket_CUSIP["Spread_Value_BRK"] <- NA
		Mapping_Ticket_CUSIP["Spread_Value_ADHOC"] <- NA
		Mapping_Ticket_CUSIP["Market_Value"] <- NA
		Mapping_Ticket_CUSIP["Spread_Duration"] <- NA
		Mapping_Ticket_CUSIP["Country"] <- NA
		
		#		scenario_to_compare = Scenarios[3]
		ts_aoci_brk = aggregate_by_ccar_sector(AOCI_BRK,scenario=scenario_to_compare)
		ts_aoci_adhoc = aggregate_by_ccar_sector(AOCI_Adhoc,scenario=scenario_to_compare)
		
#		ts_fv_brk = aggregate_by_ccar_sector(FV_BRK,scenario=scenario_to_compare)
#		ts_fv_adhoc = aggregate_by_ccar_sector(FV_Adhoc,scenario=scenario_to_compare)
		
		### Aggregated Level Comparison
		Comparison_MTM <- ts.union("BRS"=sum_mts(ts_aoci_brk),"Adhoc"=sum_mts(ts_aoci_adhoc))
		
		file_name <- paste(Output_forecast,"/Comparison_Level_",cleanString(scenario_to_compare),".png",sep="")
		tsPlotGGplot(Comparison_MTM,dep_name=paste0("MTM Results/",scenario_to_compare),combined=TRUE)
		ggsave(file_name)
		
		Diff_MTM=Comparison_MTM[,1]-Comparison_MTM[,2]
		file_name <- paste(Output_forecast,"/Comparison_Diff_",cleanString(scenario_to_compare),".png",sep="")
		tsPlotGGplot(Diff_MTM,dep_name=paste0("MTM Results/",scenario_to_compare))
		ggsave(file_name)
		
		for (snapshot in time(ts_aoci_brk)) {
#			snapshot = 2018.75
			tmp_select = which(time(ts_aoci_brk)==snapshot)
			tmp_brk <- ts_aoci_brk[tmp_select,]
			tmp_adhoc <- ts_aoci_adhoc[tmp_select,]
			tmp_diff <- tmp_adhoc-tmp_brk
			comparison_table <- data.frame("BRS"=tmp_brk,"Diff"=tmp_diff,"Adhoc"=tmp_adhoc)
			comparison_table <- comparison_table[order(comparison_table['Diff'],decreasing=TRUE),]
			comparison_table = rbind(comparison_table,"Total"=apply(comparison_table,2,sum))
			file_name <- paste(Output_forecast,"/Comparison_",cleanString(scenario_to_compare),"_",cleanString(as.yearqtr(snapshot)),".tex",sep="")
			writeMatrixToTex(file_name,cap=paste0("Comparison for ",gsub(" ", "",as.yearqtr(snapshot))),comparison_table)
		}
		
		### CCAR Sector Level Comparison
		ccar_sectors <- colnames(ts_aoci_brk)
		for (ccar_sector in ccar_sectors) {
#			ccar_sector = ccar_sectors[1]
			Comparison_MTM <- ts.union("BRS"=ts_aoci_brk[,ccar_sector],"Adhoc"=ts_aoci_adhoc[,ccar_sector])
			
			file_name <- paste(Output_forecast,"/Comparison_Level_",cleanString(scenario_to_compare),"_",cleanString(ccar_sector),".png",sep="")
			tsPlotGGplot(Comparison_MTM,dep_name=paste0("MTM Results/",ccar_sector),combined=TRUE)
			ggsave(file_name)
		}
		
#		k = 1
#		VERBOSE_PLOT = FALSE
		for (i in 1:nrow(Mapping_Ticket_CUSIP)) {
#			i =1
			print(i)
			ticket = Mapping_Ticket_CUSIP[i,"Ticket"]
			if (ticket%in%tickets_brk) {
				tmp_data <- get_ticket_forecast(ticket)
				Mapping_Ticket_CUSIP[i,"MTM_BRK"] <- tmp_data[3,"BRK"]
				Mapping_Ticket_CUSIP[i,"MTM_ADHOC"] <- tmp_data[3,"Adhoc"]
				Mapping_Ticket_CUSIP[i,"Spread_BRK"] <- as.character(get_brk_spread_name_by_cusip(as.character(Mapping_Ticket_CUSIP[i,'CUSIP'])))
				Mapping_Ticket_CUSIP[i,"Spread_CSIM"] <- as.character(get_csim_spread_name_by_cusip(as.character(Mapping_Ticket_CUSIP[i,'CUSIP'])))
				Mapping_Ticket_CUSIP[i,"Spread_Value_BRK"] <- get_brk_spreads_by_cusip(as.character(Mapping_Ticket_CUSIP[i,'CUSIP']))
				Mapping_Ticket_CUSIP[i,"Spread_Value_ADHOC"] <- get_csim_spreads_by_cusip(as.character(Mapping_Ticket_CUSIP[i,'CUSIP']))
				Mapping_Ticket_CUSIP[i,"Market_Value"] <- get_ticket_fv(as.character(ticket))
				Mapping_Ticket_CUSIP[i,"Spread_Duration"] <- get_brk_spread_duration_by_cusip(as.character(Mapping_Ticket_CUSIP[i,'CUSIP']))
				Mapping_Ticket_CUSIP[i,"Country"] <- get_brk_country_by_cusip(as.character(Mapping_Ticket_CUSIP[i,'CUSIP']))
			}
		}

		Mapping_Ticket_CUSIP <- Mapping_Ticket_CUSIP[!is.na(Mapping_Ticket_CUSIP["MTM_BRK"]),]
		Mapping_Ticket_CUSIP["Diff in MtM (BRK-Adhoc)"] <- Mapping_Ticket_CUSIP["MTM_BRK"] - Mapping_Ticket_CUSIP["MTM_ADHOC"]
#		Mapping_Ticket_CUSIP["Spread01 * spread diff (BRK-Adhoc)"] <- Mapping_Ticket_CUSIP["Market_Value"]*Mapping_Ticket_CUSIP["Spread_Duration"]*(Mapping_Ticket_CUSIP["Spread_Value_BRK"]-Mapping_Ticket_CUSIP["Spread_Value_ADHOC"])
		
		file_name <- paste(Output_forecast,"/Mapping_Ticket_CSUIP_MTM_",cleanString(scenario_to_compare),"_",Sys.Date(),".xlsx",sep="")
		write.xlsx(Mapping_Ticket_CUSIP,file_name)
		
#		cusips_to_plot_agencymbs <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Agency MBS")
#		plot_tickets(cusips_to_plot_agencymbs$Ticket)
#		
#		cusips_to_plot_nonagencymbs <- Mapping_Ticket_CUSIP[Mapping_Ticket_CUSIP[,"CCAR_Sector"] != "Agency MBS",]
#		plot_tickets(cusips_to_plot_nonagencymbs$Ticket)
#		
#		cusips_to_plot_agencymbs <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Agency MBS")
#		dim(cusips_to_plot_agencymbs)
#		cusips_to_plot_muni <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Municipal Bond")
#		dim(cusips_to_plot_muni)
#		cusips_to_plot_frmbs <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Foreign RMBS")
#		dim(cusips_to_plot_frmbs)
#		cusips_to_plot_corproates <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Corporate Bond")
#		dim(cusips_to_plot_corproates)
#		cusips_to_plot_sovereign <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Sovereign Bond")
#		dim(cusips_to_plot_sovereign)
#		cusips_to_plot_treasuries <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "US Treasuries & Agencies")
#		dim(cusips_to_plot_treasuries)
#		cusips_to_plot_cvb <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Covered Bond")
#		dim(cusips_to_plot_cvb)
#		
#		cusips_to_plot_nonagencymbs <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Domestic Non-Agency RMBS (incl HEL ABS)")
#		dim(cusips_to_plot_nonagencymbs)
#		cusips_to_plot_cmbs <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "CMBS")
#		dim(cusips_to_plot_cmbs)
#		cusips_to_plot_clo <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "CLO")
#		dim(cusips_to_plot_clo)
#		cusips_to_plot_slabs <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Student Loan ABS")
#		dim(cusips_to_plot_slabs)
#		cusips_to_plot_ccabs <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Credit Card ABS")
#		dim(cusips_to_plot_ccabs)
#		cusips_to_plot_alabs <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Auto ABS")
#		dim(cusips_to_plot_alabs)
#		cusips_to_plot_otherabs <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Other ABS (excl HEL ABS)")
#		dim(cusips_to_plot_otherabs)
#		cusips_to_plot_other <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Other")
#		dim(cusips_to_plot_other)

#		plot_tickets(cusips_to_plot_treasuries)
		plot_tickets(Mapping_Ticket_CUSIP)
	}

#	scenario_to_compare = "BHC Adverse"
#	ts_fv_brk_adverse = aggregate_by_ccar_sector(FV_BRK,scenario=scenario_to_compare)
#	ts_fv_adhoc_adverse = aggregate_by_ccar_sector(FV_Adhoc,scenario=scenario_to_compare)
#	ts_aoci_brk_adverse = aggregate_by_ccar_sector(AOCI_BRK,scenario=scenario_to_compare)
#	ts_aoci_adhoc_adverse = aggregate_by_ccar_sector(AOCI_Adhoc,scenario=scenario_to_compare)
#	sum_mts(ts_fv_brk_adverse)
#	names(AOCI_Adhoc) = tickets_adhoc

#	### CCAR Sector Level Comparison
#	all(tickets_brk%in%tickets_adhoc)
#	all(tickets_adhoc%in%tickets_brk)
#	length(tickets_adhoc)
#	length(tickets_brk)
#	mapping_cusip_ticket <- unique(AOCI_tmp[,c("CUSIP","Ticket")])
#	cusips_all <- unique(mapping_cusip_ticket$CUSIP)
##	dim(mapping_cusip_ticket)
#	adhoc_mtm <- c()
#	brk_mtm <- c()
#	market_value <- c()
#	for (k in 1:length(cusips_all)) {
##		k = 1
##		print(k)
#		tickets = tableFilter(mapping_cusip_ticket,"CUSIP",as.character(cusips_all[k]))$Ticket
#		
#		if (length(tickets) == 1) {
#			adhoc_mtm <- c(adhoc_mtm,AOCI_Adhoc[[k]][3,scenario_to_compare])
#			brk_mtm <- c(brk_mtm,AOCI_BRK[[k]][3,scenario_to_compare])
#			market_value <- c(market_value,FV_BRK[[k]][1,scenario_to_compare])
#		} else {
##			break
#			adhoc_tmp <- 0
#			brk_tmp <- 0
#			mv_tmp <- 0
#			for (l in 1:length(tickets)) {
#				ticket = as.character(tickets[l])
#				adhoc_tmp = adhoc_tmp + round(AOCI_Adhoc[[ticket]][3,scenario_to_compare],2)
#				brk_tmp = brk_tmp + round(AOCI_BRK[[ticket]][3,scenario_to_compare],2)
#				mv_tmp = mv_tmp + round(FV_BRK[[ticket]][1,scenario_to_compare],2)
#			}
#			adhoc_mtm <- c(adhoc_mtm,adhoc_tmp)
#			brk_mtm <- c(brk_mtm,brk_tmp)
#			market_value <- c(market_value,mv_tmp)
#		}
#	}
#	
#	MTM_CUSIP = data.frame(cbind(as.character(cusips_all),market_value/1000000,adhoc_mtm/1000000,brk_mtm/1000000))
#	rownames(MTM_CUSIP) <- cusips_all
#	colnames(MTM_CUSIP) <- c("CUSIP","FairValue_BRK","MTM_ADHOC","MTM_BRK")
#	
#	MTM_CUSIP = merge(x = MTM_CUSIP, y = unique(AOCI_brk[,c("CUSIP","CCAR_Sector")]), by = "CUSIP", all.x = TRUE)
#	MTM_CUSIP[,"FairValue_BRK"] <- as.numeric(as.character(MTM_CUSIP[,"FairValue_BRK"]))
#	MTM_CUSIP[,"MTM_BRK"] <- as.numeric(as.character(MTM_CUSIP[,"MTM_BRK"]))
#	MTM_CUSIP[,"MTM_ADHOC"] <- as.numeric(as.character(MTM_CUSIP[,"MTM_ADHOC"]))
#	
#	write.xlsx(MTM_CUSIP, paste(Output_forecast,"/MTM_Comparison.xlsx",sep=""))
}

cusips_to_plot_agencymbs <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Agency MBS")
dim(cusips_to_plot_agencymbs)
cusips_to_plot_muni <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Municipal Bond")
dim(cusips_to_plot_muni)
cusips_to_plot_frmbs <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Foreign RMBS")
dim(cusips_to_plot_frmbs)
cusips_to_plot_corproates <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Corporate Bond")
dim(cusips_to_plot_corproates)
cusips_to_plot_sovereign <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Sovereign Bond")
dim(cusips_to_plot_sovereign)
cusips_to_plot_treasuries <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "US Treasuries & Agencies")
dim(cusips_to_plot_treasuries)
cusips_to_plot_cvb <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Covered Bond")
dim(cusips_to_plot_cvb)

cusips_to_plot_nonagencymbs <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Domestic Non-Agency RMBS (incl HEL ABS)")
dim(cusips_to_plot_nonagencymbs)
cusips_to_plot_cmbs <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "CMBS")
dim(cusips_to_plot_cmbs)
cusips_to_plot_clo <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "CLO")
dim(cusips_to_plot_clo)
cusips_to_plot_slabs <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Student Loan ABS")
dim(cusips_to_plot_slabs)
cusips_to_plot_ccabs <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Credit Card ABS")
dim(cusips_to_plot_ccabs)
cusips_to_plot_alabs <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Auto ABS")
dim(cusips_to_plot_alabs)
cusips_to_plot_otherabs <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Other ABS (excl HEL ABS)")
dim(cusips_to_plot_otherabs)
cusips_to_plot_other <- tableFilter(Mapping_Ticket_CUSIP,"CCAR_Sector", "Other")
dim(cusips_to_plot_other)



file_name <- paste(Output_forecast,"/MTM_COMPARE_BRK.tex",sep="")
writeVectorToTex(file_name,"CCARSectors",cleanString(ccar_sectors))
writeVectorToTex(file_name,"Scenarios",cleanString(Scenarios),mode="a")
writeVectorToTex(file_name,"Periods",cleanString(as.yearqtr(time(ts_aoci_brk))),mode="a")
writeVectorToTex(file_name,"CUSIPSAgencyMBS",cusips_to_plot_agencymbs$Ticket,mode="a")
writeVectorToTex(file_name,"CUSIPSMuniOne",cusips_to_plot_muni[1:600,"Ticket"],mode="a")
writeVectorToTex(file_name,"CUSIPSMuniTwo",cusips_to_plot_muni[601:nrow(cusips_to_plot_muni),"Ticket"],mode="a")
writeVectorToTex(file_name,"CUSIPFRMBS",cusips_to_plot_frmbs$Ticket,mode="a")
writeVectorToTex(file_name,"CUSIPCorporates",cusips_to_plot_corproates$Ticket,mode="a")
writeVectorToTex(file_name,"CUSIPSovereigns",cusips_to_plot_sovereign$Ticket,mode="a")
writeVectorToTex(file_name,"CUSIPTreasuries",cusips_to_plot_treasuries$Ticket,mode="a")
writeVectorToTex(file_name,"CUSIPCoveredBonds",cusips_to_plot_cvb$Ticket,mode="a")
writeVectorToTex(file_name,"CUSIPNAMBS",cusips_to_plot_nonagencymbs$Ticket,mode="a")
writeVectorToTex(file_name,"CUSIPCMBS",cusips_to_plot_cmbs$Ticket,mode="a")
writeVectorToTex(file_name,"CUSIPCLO",cusips_to_plot_clo$Ticket,mode="a")
writeVectorToTex(file_name,"CUSIPSLABS",cusips_to_plot_slabs$Ticket,mode="a")
writeVectorToTex(file_name,"CUSIPCCABS",cusips_to_plot_ccabs$Ticket,mode="a")
writeVectorToTex(file_name,"CUSIPALABS",cusips_to_plot_alabs$Ticket,mode="a")
writeVectorToTex(file_name,"CUSIPOtherABS",cusips_to_plot_otherabs$Ticket,mode="a")
writeVectorToTex(file_name,"CUSIPOthers",cusips_to_plot_other$Ticket,mode="a")


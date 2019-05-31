# To take updated raw downlodas from market research providers (BoA, JPM, Citi
# and Barclays, and standardize observations and save to target Access database.
# It needs to be run once every time data is downloaded or updated. Take 5-10
# minutes to run.
# 
# Author: E620927
###############################################################################

Output_curve = file.path(Output_root,"CurveInfo")
if(!file.exists(Output_curve)) {
	dir.create(Output_curve)
}

access_table_names <- c("I_HIST_CSI_QTRAVE","I_HIST_CSI_MONAVE","I_HIST_CSI_MONEND");
#Process CSI if table names cannot be found in DB or REPROCESS is TRUE
if (!all(dbHasTable(dbc,access_table_names)) | REPROCESS) {
	
#read data inputs
	data_jpm <- read.csv(concat_file_path(tableFilter(list_files,"VariableName","data_jpm")))
	data_barclays <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","data_barclays")),sheetIndex=1)
#data_citi <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","data_citi")),sheetIndex=2)
	data_boa <- read.csv(concat_file_path(tableFilter(list_files,"VariableName","data_boa")))
	data_mmd <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","data_mmd")),sheetIndex=1)
#	data_armbs_arms <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","data_ambs_arms")),sheetIndex=1)
#	data_nambs_subprime <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","data_subprime")),sheetIndex=1)
	
	##
	csi_ts_list <- list()
	
	##################################################################################
	###Process JPM
	data_jpm[data_jpm=="N/A"] <- NA
	data_jpm$Date <- as.Date(as.character(data_jpm$Date),format="%d-%b-%y")
	
#	CSI_ALL_DailyClean <- list()
#j <-5
	for (j in 2:ncol(data_jpm)) {
		csi_name <- colnames(data_jpm)[j]
		csi_name <- gsub("[.]","-",csi_name)
		
		data_jpm_index_original <- as.xts(as.numeric(as.character(data_jpm[,j])),data_jpm[,1])
		data_jpm_index_original <- na.omit(data_jpm_index_original)
		
		file_name = paste(Output_curve,"/",csi_name,"_daily.png",sep="")
		png(file_name, width=600,height=400)
#		file_name = paste(Output_curve,"/",csi_name,"_daily.jpg",sep="")
#		jpeg(file_name, width=600,height=400)
		
#		plot(data_jpm_index_original,main=paste0(csi_name,": JPMM"))
		print(plot.xts(data_jpm_index_original))
		dev.off()
		
		csi_ts_list[[csi_name]] <- data_jpm_index_original
	}
	
	###Process Barclays
#CSI_ALL <- list()
#delete rows with n/a
	
	cutoff_liborOAS <- as.Date("2010-04-01")
	data_barclays<-data_barclays[data_barclays[,2]!="n/a",]
	for (j in 2:ncol(data_barclays)) {
#	j <- 4
		csi_name <- colnames(data_barclays)[j]
		csi_name <- gsub("[.]","-",csi_name)
		
		data_barclays_index_original <- as.xts(as.numeric(as.character(data_barclays[,j])),data_barclays[,1])
		data_barclays_index_original <- na.omit(data_barclays_index_original)
		
		if (grepl("LiborOAS",csi_name)) {
			data_barclays_index_original <- data_barclays_index_original[time(data_barclays_index_original) >= cutoff_liborOAS]
		}
		data_barclays_index_original <- data_barclays_index_original*100
		file_name = paste(Output_curve,"/",csi_name,"_daily.png",sep="")
		png(file_name, width=600,height=400)
		print(plot(data_barclays_index_original,main=paste0("ORIGINAL ",csi_name,"/Barlcays")))
		dev.off()
		
		csi_ts_list[[csi_name]] <- data_barclays_index_original
	}
	csi_ts_list[[1]]
	###################################################################################
	####Citi
	##colnames(data_citi)
	##head(data_citi)
#data_citi <- data_citi[1:(nrow(data_citi)-1),]
#data_citi$Date <- as.Date(as.numeric(as.character(data_citi$Date)), origin="1899-12-30")
#
#for (j in 2:ncol(data_citi)) {
	##	j <- 2
#	csi_name <- colnames(data_citi)[j]
#	data_citi_index_original <- as.xts(as.numeric(as.character(data_citi[,j])),data_citi[,1])
#	file_name = paste(Output_curve,"/",gsub("[.]","-",csi_name),"_daily.png",sep="")
#	png(file_name, width=600,height=400)
#	plot(na.omit(data_citi_index_original),main=paste0(csi_name,": Citi"))
#	dev.off()
#	
#	CSI_ALL_DailyClean <- rbind(CSI_ALL_DailyClean,cbind(DATE=format(index(data_citi_index_original),"%m/%d/%Y"),MONTH=gsub(" ","",as.yearmon(index(data_citi_index_original))),CSI_ID=gsub("[.]","-",csi_name),CSI_VALUE=as.vector(data_citi_index_original)))
#}
	
	##################################################################################
	###BoA
	segments <- unique(data_boa$Bond.Index)
	segments <- segments[segments!=""]
	data_boa$Date <- as.Date(data_boa$Date,format="%m/%d/%Y")
	
	boa_retrieve_csiname <- function(name) {
#	name <- tmp[,"Description"]
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
		} else if (grepl("Australian Quasi Govt",name)) {
			return("QGOV_AUD")
		} else if (grepl("Euro Quasi-Govt",name)) {
			return("QGOV_EUR")
		} else if (grepl("Sterling Quasi Govt",name)) {
			return("QGOV_GBP")
		} else if (grepl("Global Broad Mkt Quasi-Govt",name)) {
			return("QGOV_GLOBE")
		} else if (grepl("MBS GNMA 30 Year Current Coupon",name)) {
			return("AMBS_GNMA_30Y_CurrentCoupon_OAS_ICE")
		} else if (grepl("Mortgages GNMA All 15 Year",name)) {
			return("AMBS_GNMA_15Y_OAS_ICE")
		} else if (grepl("Mortgages GNMA All 30 Year",name)) {
			return("AMBS_GNMA_30Y_OAS_ICE")
		} else if (grepl("Mortgages GNMA 15 & 30 Yr Current Coupon",name)) {
			return("AMBS_GNMA_15Y_30Y_OAS_ICE")
		} else if (grepl("Mortgages GNMA Master",name)) {
			return("AMBS_GNMA_ALL_OAS_ICE")
		} else if (grepl("Mortgages All FHLMC & FNMA 30 Yr",name)) {
			return("AMBS_FNMA_FHLMC_30Y_OAS_ICE")
		} else if (grepl("MBS All FNMA Current Coupon",name)) {
			return("AMBS_FNMA_ALL_CurrentCoupon_OAS_ICE")
		} else if (grepl("Mortgages FNMA All 15 Yr",name)) {
			return("AMBS_FNMA_15Y_OAS_ICE")
		} else if (grepl("Mortgages FNMA All 30 Yr",name)) {
			return("AMBS_FNMA_30Y_OAS_ICE")
		} else if (grepl("Mortgages FNMA Master",name)) {
			return("AMBS_FNMA_ALL_OAS_ICE")
		} else if (grepl("MBS FNMA 15 Year Current Coupon",name)) {
			return("AMBS_FNMA_15Y_CurrentCoupon_OAS_ICE")
		} else if (grepl("MBS FNMA 30 Year Current Coupon",name)) {
			return("AMBS_FNMA_30Y_CurrentCoupon_OAS_ICE")
		} else if (grepl("MBS FHLMC 30 Yr Current Coupon",name)) {
			return("AMBS_FHLMC_30Y_CurrentCoupon_OAS_ICE")
		} else if (grepl("Mortgages FHLMC All 30 Yr",name)) {
			return("AMBS_FHLMC_30Y_OAS_ICE")
		}
	}
	
	for (j in 1:length(segments)) {
#	j <- 10
		print(j)
		tmp <- data_boa[data_boa$Bond.Index==segments[j],]
		tmp <- tmp[tmp[,"Description"]!="",]
		
		csi_name <- boa_retrieve_csiname(tmp[,"Description"])
		csi_name <- gsub("[.]","-",csi_name)
		
		data_boa_index_original <- as.xts(as.numeric(tmp[,"OAS"]),tmp[,"Date"])
		data_boa_index_original <- na.omit(data_boa_index_original)
		file_name = paste(Output_curve,"/",csi_name,"_daily.png",sep="")
		png(file_name, width=600,height=400)
		print(plot(data_boa_index_original, main=paste0(csi_name,": BOA")))
		dev.off()
		csi_ts_list[[csi_name]] <- data_boa_index_original
		
#		if ("Libor.OAS" %in% colnames(tmp) & grepl("OAS",csi_name)) {
#			csi_name <- gsub("OAS","LiborOAS",csi_name)
#			data_boa_index_original <- as.xts(as.numeric(tmp[,"Libor.OAS"]),tmp[,"Date"])
#			data_boa_index_original <- na.omit(data_boa_index_original)
#			file_name = paste(Output_curve,"/",csi_name,"_daily.png",sep="")
#			png(file_name, width=600,height=400)
#			print(plot(data_boa_index_original, main=paste0(csi_name,": BOA")))
#			dev.off()
#			csi_ts_list[[csi_name]] <- data_boa_index_original
#		}
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
#	csi_name <- mmd_csi_5y[1]
		tmp_data <- 100*(data_mmd[,csi_name]-data_mmd[,"Treasury.5.yr"])
		data_mmd_index_original <- as.xts(as.numeric(tmp_data),data_mmd[,1])
		data_mmd_index_original <- na.omit(data_mmd_index_original)
		file_name = paste(Output_curve,"/",csi_name,"_daily.png",sep="")
		png(file_name, width=600,height=400)
		print(plot(data_mmd_index_original,main=paste0(csi_name,": MMD")))
		dev.off()
		
		csi_ts_list[[csi_name]] <- data_mmd_index_original
	}
	
	mmd_csi_10y <- c("MUNI_GO_AAA_10Y","MUNI_GO_AA_10Y")
	for (csi_name in mmd_csi_10y) {
		tmp_data <- 100*(data_mmd[,csi_name]-data_mmd[,"Treasury.10.yr"])
		data_mmd_index_original <- as.xts(as.numeric(tmp_data),data_mmd[,1])
		data_mmd_index_original <- na.omit(data_mmd_index_original)
		file_name = paste(Output_curve,"/",csi_name,"_daily.png",sep="")
		png(file_name, width=600,height=400)
		print(plot(data_mmd_index_original,main=paste0(csi_name,": MMD")))
		dev.off()
		
		csi_ts_list[[csi_name]] <- data_mmd_index_original
	}
	
	###################################################################################
	####Process self constructured NAMBS Subprime########
#for (j in 2:ncol(data_nambs_subprime)) {
	##	j <- 2
#	csi_name <- colnames(data_nambs_subprime)[j]
#	csi_name <- gsub("[.]","-",csi_name)
#	
#	data_armbs_arm <- as.xts(as.numeric(as.character(data_nambs_subprime[,j])),data_nambs_subprime[,1])
#	data_armbs_arm <- na.omit(data_armbs_arm)
#	file_name = paste(Output_curve,"/",gsub("[.]","-",csi_name),"_daily.png",sep="")
#	png(file_name, width=600,height=400)
#	plot(data_armbs_arm,main=paste0("ORIGINAL ",csi_name,"/Self-Constructed"))
#	dev.off()
#	
#	csi_ts_list[[csi_name]] <- data_armbs_arm
#}
	
	##################################################################################
	##Outlier Adjustments
#adjustment for JPM outliers
	outliers <- c("AMBS_CMO_PAC_2Y","AMBS_CMO_SEQ_5Y")
	for (csi_name in outliers) {
#	csi_name = outliers[1]
		orig <- csi_ts_list[[csi_name]]
		
		csi_ts_list[[csi_name]] <- csi_ts_list[[csi_name]][csi_ts_list[[csi_name]]!=0]
		file_name = paste(Output_curve,"/",csi_name,"_daily_outliers_removed.png",sep="")
		png(file_name, width=800,height=600)
		par(mfrow=c(1,2))
		print(plot(orig,main=paste0("Original ",csi_name,"/JPMM")))
		print(plot(csi_ts_list[[csi_name]],main=paste0("OUTLIER REMOVED ",csi_name,"/JPMM")))
		dev.off()
	}
	
#smooth the four outliers for Agency Debts
	outlier_csi <- names(csi_ts_list)[grepl("^AD",names(csi_ts_list))]
	outlier_dates <- as.Date(c("10/12/2001","10/11/2002","05/24/2004","10/12/2004","5/23/2005","10/12/2005"),format="%m/%d/%Y")
	for (csi_name in outlier_csi) {
#	csi_name = outlier_csi[1]
		orig <- csi_ts_list[[csi_name]]
		
		select <- which(index(csi_ts_list[[csi_name]])%in%outlier_dates)
		csi_ts_list[[csi_name]][select] <- (as.numeric(csi_ts_list[[csi_name]][select-1])+as.numeric(csi_ts_list[[csi_name]][select+1]))/2
		
#		csi_ts_list[[csi_name]] <- csi_ts_list[[csi_name]][csi_ts_list[[csi_name]]>=0]
		
		file_name = paste(Output_curve,"/",csi_name,"_daily_outliers_removed.png",sep="")
		png(file_name, width=800,height=600)
		par(mfrow=c(1,2))
		print(plot(orig,main=paste0("Original ",csi_name,"/Barclays")))
		print(plot(csi_ts_list[[csi_name]],main=paste0("OUTLIER REMOVED ",csi_name,"/Barlcays")))
		dev.off()
	}
	
	##################################################################################
	########################overlays
	file_names <- list()
	overrides <- list()
	file_names[[1]] <- concat_file_path(tableFilter(list_files,"VariableName","data_overrides_aux"))
	file_names[[2]] <- concat_file_path(tableFilter(list_files,"VariableName","data_overrides_clo"))
	file_names[[3]] <- concat_file_path(tableFilter(list_files,"VariableName","data_overrides_nambs"))
	
	for (i in 1:length(file_names)) {
#	i <- 3
		if (file.exists(file_names[[i]])) {
			data <- read.xlsx(file_names[[i]],sheetIndex=1)
			for (j in 2:ncol(data)) {
				csi_name <- colnames(data)[j]
				csi_name <- gsub("[.]","-",csi_name)
				overrides[[csi_name]] <- as.xts(as.numeric(as.character(data[,j])),data[,1])
			}
		}
	}
	
	for (i in 1:length(overrides)) {
#	i <- 2
		csi_name = names(overrides)[i]
		csi_name <- gsub("[.]","-",csi_name)
		override <- overrides[[csi_name]]
		orig <- csi_ts_list[[csi_name]]
		
		seg1 <- index(orig)<min(index(override))
		seg2 <- index(orig)>max(index(override))
		tsnew <- rbind(orig[index(orig)<min(index(override))],override,orig[index(orig)>max(index(override))])
		
		file_name = paste(Output_curve,"/",csi_name,"_override.png",sep="")
		png(file_name, width=1200,height=800)
		par(mfrow=c(3,1))
		print(plot(orig,main=paste("Original:",csi_name)))
		print(plot(override,main=paste("Overlays:",csi_name)))
		print(plot(tsnew,main=paste("Overlaid:",csi_name)))
		abline(v=.index(orig)[1],col="red")
		dev.off()
		
		csi_ts_list[[csi_name]] <- tsnew
	}
	
	##################################################################################
	######################Synthetic indexes
	rmbs_fix <- names(csi_ts_list)[grepl("^AMBS_.*_15Y$",names(csi_ts_list))]
	names <- c("GNMA","FHLMC","FNMA")
	
	for (name in names) {
#	name = names[1]
		csi_name <- paste0("AMBS_ARMS_",name)
		csi_ts_list[[csi_name]] <- csi_ts_list[[rmbs_fix[grepl(name,rmbs_fix)]]]-csi_ts_list[["SWAP_SPREAD_5Y"]]
		file_name = paste(Output_curve,"/",csi_name,"_daily.png",sep="")
		png(file_name, width=600,height=400)
		print(plot(csi_ts_list[[csi_name]],main=paste0(csi_name,": Synthetic")))
		dev.off()
	}
	
	#####################Delete Swap Spread 5Y index which is solely used for creating synthetic indexes
	csi_ts_list = csi_ts_list[names(csi_ts_list)!="SWAP_SPREAD_5Y"]
	
	##################################################################################
	###Output the daily data after adjustments for outliers, overlay and synthetic indexes
	CSI_ALL_DailyTs_Merge <- xtslist_to_df(csi_ts_list)
	file_name = paste(Output_curve,"/CSI_ALL_DailyClean.csv",sep="")
	write.csv(as.data.frame(CSI_ALL_DailyTs_Merge), file_name)

	CSI_ALL_DailyDB <- xtslist_to_db(csi_ts_list)
	file_name = paste(Output_curve,"/CSI_ALL_DailyCleanDB.csv",sep="")
	write.csv(CSI_ALL_DailyDB, file_name)
	
	table_name <- "I_HIST_CSI_DailyClean";
	try(sqlDrop(dbc, table_name))
	saveTable(dbc,table_name,data.frame(Date=as.character(index(CSI_ALL_DailyTs_Merge)),CSI_ALL_DailyTs_Merge))
	##################################################################################
#calculation
	CSI_MONTH_END <- list()
	CSI_MONTH_AVE <- list()
	CSI_QUARTER_AVE <- list()
	
	for (i in 1:length(csi_ts_list)) {
#	i <- 2
		csi <- csi_ts_list[[i]]
		csi <- csi[as.numeric(as.yearperiod(index(csi)))<=History_END_DATE]
		
		#Monthly End
		ep1 <- endpoints(csi,on="months")
		csi_monthend <- csi[ep1]
		
		index(csi_monthend) <- as.Date(as.yearmon(index(csi_monthend))+1/12)-1
		
		CSI_MONTH_END[[i]] <- csi_monthend
		
		#Monthly Ave
		#trim to month start
		csi_trimed <- trimSeriesToMonthStart(csi)
		csi_mean_mon <- period.apply(csi_trimed,INDEX=endpoints(csi_trimed,'months'),FUN=mean_na_ignored)
		
		index(csi_mean_mon) <- as.Date(as.yearmon(index(csi_mean_mon))+1/12)-1
#	csi_mean_mon[is.na(csi_mean_mon)]<-""
		CSI_MONTH_AVE[[i]] <- csi_mean_mon
		
		#Quarterly Ave
		#trim to quarter start
		csi_trimed <- trimSeriesToQuarterStart(csi)
		#trim according to the end date
#	csi_trimed <- csi_trimed[as.numeric(as.yearqtr(index(csi_trimed)))<=History_END_DATE]
		csi_mean_qtr <- period.apply(csi_trimed,INDEX=endpoints(csi_trimed,'quarters'),FUN=mean_na_ignored)
#	csi_mean_qtr[is.na(csi_mean_qtr)]<-""
		
		index(csi_mean_qtr) <- as.Date(as.yearqtr(index(csi_mean_qtr))+1/4)-1
		
		CSI_QUARTER_AVE[[i]] <- csi_mean_qtr
		
		csi_merged <- merge(csi_monthend,csi_mean_mon,csi_mean_qtr)
		
		file_name = paste(Output_curve,"/",gsub("[.]","-",names(csi_ts_list)[i]),"_comparison.png",sep="")
		png(file_name, width=600,height=400)
		ts.plot(csi_merged,col=1:3,type="b",main=names(csi_ts_list)[i])
		dev.off()
	}
	names(CSI_MONTH_END) <- names(csi_ts_list)
	names(CSI_MONTH_AVE) <- names(csi_ts_list)
	names(CSI_QUARTER_AVE) <- names(csi_ts_list)
	
#	CSI_MONTH_END_df <- xtslist_to_df(CSI_MONTH_END)
#	CSI_MONTH_AVE_df <- xtslist_to_df(CSI_MONTH_AVE)
#	CSI_QUARTER_AVE_df <- xtslist_to_df(CSI_QUARTER_AVE)
#	
#	file_name = paste0(Output_curve,"/CSI_MONTH_END.csv")
#	write.csv(as.data.frame(CSI_MONTH_END_df),file_name)
#	
#	file_name = paste0(Output_curve,"/CSI_MONTH_AVE.csv")
#	write.csv(as.data.frame(CSI_MONTH_AVE_df),file_name)
#	
#	file_name = paste0(Output_curve,"/CSI_QUARTER_AVE.csv")
#	write.csv(as.data.frame(CSI_QUARTER_AVE_df),file_name)
	
	CSI_MONTH_END_db <- xtslist_to_db(CSI_MONTH_END)
	CSI_MONTH_AVE_db <- xtslist_to_db(CSI_MONTH_AVE)
	CSI_QUARTER_AVE_db <- xtslist_to_db(CSI_QUARTER_AVE)
	
	table_name <- "I_HIST_CSI_QTRAVE";
	deleteTable(dbc,table_name)
	saveTable(dbc,table_name,characterizeTable(versionDataFrame(CSI_QUARTER_AVE_db,current_version)))
	file_name = paste0(Output_curve,"/",table_name,".csv")
	write.csv(CSI_QUARTER_AVE_db,file_name)
	
	table_name <- "I_HIST_CSI_MONAVE";
	deleteTable(dbc,table_name)
	saveTable(dbc,table_name,characterizeTable(versionDataFrame(CSI_MONTH_AVE_db,current_version)))
	file_name = paste0(Output_curve,"/",table_name,".csv")
	write.csv(CSI_MONTH_AVE_db,file_name)
	
	table_name <- "I_HIST_CSI_MONEND";
	deleteTable(dbc,table_name)
	saveTable(dbc,table_name,characterizeTable(versionDataFrame(CSI_MONTH_END_db,current_version)))
	file_name = paste0(Output_curve,"/",table_name,".csv")
	write.csv(CSI_MONTH_END_db,file_name)
}

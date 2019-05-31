# 
# 
# Author: E620927
###############################################################################

Output_forecast <- file.path(Output_root,"Forecast-CSI")
if (!dir.exists(Output_forecast)) {
	dir.create(Output_forecast)
}

Model_Final <- fetchTable(dbc,"I_MOD_SPECIFICATION_FINAL",current_version)
Model_Final <- clean_test_Model_Final(Model_Final)

MULTIPLIER <- fetchTable(dbc, "O_MULTIPLIER_ALL",version=current_version)
MULTIPLIER$MOD_ID <- cleanString(MULTIPLIER$MOD_ID)

if (!Do_Not_Recalibrate) {
	print("Forecast_CSI.R: recalibrate and then forecast the index spread.")
	
	result <- data.frame()
	
	for (i in 1:nrow(Model_Final)) {
		#	i <- 66
		dep <- Model_Final[i,]$CLUSTER
		ind <- c(Model_Final[i,]$Var1,Model_Final[i,]$Var2,Model_Final[i,]$Var3)
		ind <- as.character(na.omit(ind))
		ind <- ind[ind!=""]
		dep_var <- Dep_var[,dep,drop=FALSE]
		ind_var <- Ind_var[,ind,drop=FALSE]
		
		if (i == 1) {
			result <- CSIForecast(dep_var, ind_var, Output_forecast,MULTIPLIER)
		} else {
			result <- rbind(result, CSIForecast(dep_var, ind_var, Output_forecast,MULTIPLIER))
		}
	}
	
# save forecast to excel file
	file_name <- paste(Output_forecast,paste0("/CSI_FCST_",current_version,".xlsx"),sep="")
	write.xlsx(result,file_name,row.names=FALSE)
	
# save forecast to database
	result <- data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),result)
	table_name <- "O_CSI_FCST";
	for (j in 1:ncol(result)) {
		result[,j] <- as.character(result[,j])
	}
	saveTable(dbc,table_name,result)
	
	file_name = paste(Output_forecast,"/CSIAll.tex",sep="")
	writeVectorToTex(file_name,"CSI",unique(result$CSI_ID))
	
} else {
	print("Forecast_CSI.R: did not recalibrate for index spread forecast.")
	
	# If prior calibration file and multipliers exist
	file_name_calib = tableFilter(list_files,"VariableName","PriorCalibration")
	file_name_multiplier = tableFilter(list_files,"VariableName","PriorMultiplier")
	if (nrow(file_name_calib)==1&nrow(file_name_multiplier)==1) {
		#if we can find both prior recalibration file and prior multiplier file, we use prior calibration.
		MULTIPLIER_Prior <- read.csv(concat_file_path(file_name_multiplier))
		MULTIPLIER_Prior$MOD_ID <- cleanString(MULTIPLIER_Prior$MOD_ID)
		
		Model_Final_Prior <- read.csv(concat_file_path(file_name_calib))
		Model_Final_Prior <- characterizeTable(Model_Final_Prior)
		
		if (!all(Model_Final_Prior$CLUSTER%in%Model_Final$CLUSTER)&all(Model_Final$CLUSTER%in%Model_Final_Prior$CLUSTER)) {
			stop("The prior calibration file does not include all clusters for current run.")
		}
		if (!all(MULTIPLIER_Prior$MOD_ID%in%MULTIPLIER$MOD_ID)) {
			stop("The prior multiplier file does not include all clusters.")
		}
		
		for (i in 1:nrow(Model_Final_Prior)) {
			#	i <- 1
			dep <- Model_Final_Prior[i,]$CLUSTER
			ind <- c(Model_Final_Prior[i,]$Var1,Model_Final_Prior[i,]$Var2,Model_Final_Prior[i,]$Var3)
			ind <- as.character(na.omit(ind))
			ind <- ind[ind!=""]
			dep_var <- Dep_var[,dep,drop=FALSE]
			ind_var <- Ind_var[,ind,drop=FALSE]	
			prior_calibration <- Model_Final_Prior[Model_Final_Prior$CLUSTER==dep,c("Var1Coef","Var2Coef")]
			colnames(prior_calibration) <- ind
			if (i == 1) {
				result_PriorCalibration <- CSIForecast_With_PriorCalibration(dep_var, ind_var,prior_calibration, Output_forecast,MULTIPLIER_Prior)
			} else {
				result_PriorCalibration <- rbind(result_PriorCalibration, CSIForecast_With_PriorCalibration(dep_var, ind_var,prior_calibration, Output_forecast,MULTIPLIER_Prior))
			}
		}
		
		file_name <- paste(Output_forecast,paste0("/CSI_FCST_",current_version,"_PriorCalibration.xlsx"),sep="")
		write.xlsx(result_PriorCalibration,file_name,row.names=FALSE)
		
		result_PriorCalibration <- data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),result_PriorCalibration)
		for (j in 1:ncol(result_PriorCalibration)) {
			result_PriorCalibration[,j] <- as.character(result_PriorCalibration[,j])
		}
		
		table_name <- "O_CSI_FCST";
		try(sqlDrop(dbc,table_name))
		saveTable(dbc,table_name,result_PriorCalibration)
		
		#compare two forecasts
		result$FCST_DATE = format(as.Date(result$FCST_DATE),format="%m/%d/%Y")
		result_PriorCalibration$FCST_DATE = format(as.Date(result_PriorCalibration$FCST_DATE),format="%m/%d/%Y")
		
		indices <- unique(result$CSI_ID)
		scenas <- unique(result$SCEN_ID)
		for(i in 1:length(indices)) {
#		i = 1
			data1 = result[result$CSI_ID==indices[i],c("CSI_ID","SCEN_ID","FCST_DATE","CSI_FCST")]
			data1_ts = extract_ts_from_df(data1,"FCST_DATE","CSI_FCST","SCEN_ID")
			file_name = paste(Output_forecast,"/",indices[i],"-Calibrated.png",sep="")
			tsPlotGGplot(combineCurves(data1_ts),dep_name=indices[i],combined=TRUE,subtitle="Forecast without Calibrated Coefficients")
			ggsave(file_name)
			
			data2 = result_PriorCalibration[result_PriorCalibration$CSI_ID==indices[i],c("CSI_ID","SCEN_ID","FCST_DATE","CSI_FCST")]
			data2_ts = extract_ts_from_df(data2,"FCST_DATE","CSI_FCST","SCEN_ID")
			file_name = paste(Output_forecast,"/",indices[i],"-NotCalibrated.png",sep="")
			tsPlotGGplot(combineCurves(data2_ts),dep_name=indices[i],combined=TRUE,subtitle="Forecast with Calibrated Coefficients")
			ggsave(file_name)
		}
		
		#regression table for Sanjib's team
		result_tmp = data.frame(matrix(rep(-1,8*nrow(MULTIPLIER_Prior)),ncol=8))
		colnames(result_tmp) = c("IV1_ID","IV1_TRANS","IV1_LAG","IV1_COEFF","IV2_ID","IV2_TRANS","IV2_LAG","IV2_COEFF")
		for (i in 1:nrow(MULTIPLIER_Prior)) {
#		i = 2
			if (sum(Model_Final_Prior[,"CLUSTER"]==MULTIPLIER_Prior[i,"MOD_ID"])==1) {
				multiplier = MULTIPLIER_Prior[i,"MULTIPLIER"]
				
				tmp = Model_Final_Prior[Model_Final_Prior[,"CLUSTER"]==MULTIPLIER_Prior[i,"MOD_ID"],]
				
				coef1 = as.numeric(tmp[,"Var1Coef"])*as.numeric(multiplier)
				coef2 = as.numeric(tmp[,"Var2Coef"])*as.numeric(multiplier)
				result_tmp[i,] = c(str_split_fixed(separate_variable_name(tmp[,"Var1"]), "[|]", 3),coef1,str_split_fixed(separate_variable_name(tmp[,"Var2"]), "[|]", 3),coef2)
			}
		}
		
		result_tmp = cbind(MULTIPLIER_Prior[result_tmp[,1]!=-1,c("CSI_ID", "AC_ID", "MOD_ID", "MULTIPLIER")],result_tmp[result_tmp[,1]!=-1,])
		file_name <- paste(Output_forecast,paste0("/Forecast-Statistics_CSI.xlsx"),sep="")
		write.xlsx(result_tmp,file_name,row.names=FALSE)
	} else {
		stop("Forecast_CSI.R: you specified NO Recalibration, but did not specify the prior model results. Check.")
	}
}

########################################################
#### Reformated table for discounting purposes
# decide if use prior calibration

if (Do_Not_Recalibrate) {
	print("Forecast_CSI.R: did not recalibrate for index spread forecast.")
	file_name_calib = tableFilter(list_files,"VariableName","PriorCalibration")
	file_name_multiplier = tableFilter(list_files,"VariableName","PriorMultiplier")
	if (nrow(file_name_calib)==1&nrow(file_name_multiplier)==1) {
		MULTIPLIER_Prior <- read.csv(concat_file_path(file_name_multiplier))
		MULTIPLIER_Prior$MOD_ID <- cleanString(MULTIPLIER_Prior$MOD_ID)
		
		Model_Final_Prior <- read.csv(concat_file_path(file_name_calib))
		Model_Final_Prior <- clean_test_Model_Final(Model_Final_Prior)
		
		if (!"AssetClass"%in%colnames(MULTIPLIER_Prior)) {
			MULTIPLIER_Prior <- unique(merge(MULTIPLIER_Prior,MULTIPLIER[,c("CSI_ID","AssetClass")],all.x=TRUE))
		}
		MULTIPLIER <- MULTIPLIER_Prior
		Model_Final <- Model_Final_Prior
	}
}

result_reformat <- data.frame()
result_discounttable <- data.frame()

for (i in 1:nrow(Model_Final)) {
	#	i <- 2
	dep <- Model_Final[i,]$CLUSTER
	ind <- c(Model_Final[i,]$Var1,Model_Final[i,]$Var2,Model_Final[i,]$Var3)
	ind <- as.character(na.omit(ind))
	ind <- ind[ind!=""]
	dep_var <- Dep_var[,dep,drop=FALSE]
	ind_var <- Ind_var[,ind,drop=FALSE]
	
	# need to reformat the prior multiplier file!!! fuck
	result_tmp <- CSIForecast_reformated(dep_var, ind_var, Output_forecast,MULTIPLIER)
	if (i == 1) {
		result_reformat <- result_tmp[[1]]
		result_discounttable <- result_tmp[[2]]
	} else {
		result_reformat <- rbind(result_reformat, result_tmp[[1]])
		result_discounttable <- rbind(result_discounttable, result_tmp[[2]])
	}
}
colnames(result_reformat) <- c("Scenario", "Index_Name", "Q0", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9")
table_name <- "Index_Spreads_Forecasts";
if (!"FG_Austria"%in%unique(result_reformat$Index_Name)) {
	# to be replaced once Foreign Gov curve is available
	fg_curve <- read.csv(concat_file_path(tableFilter(list_files,"VariableName","FG_Curve")))
	result_reformat <- rbind(result_reformat,fg_curve)
}
saveTable(dbc,table_name,versionDataFrame(result_reformat,current_version))

result_discounttable <- unique(result_discounttable)
colnames(result_discounttable) <- c("IndexMapping","AssetClass","GroupName","Load_Factor","Index_start_spread_BPS", "Index_Historic_Minimum")
#head(result_discounttable)

if (!"FG_Austria"%in%unique(result_discounttable$Index_Name)) {
	# to be replaced once Foreign Gov curve is available
	fg_curve <- read.csv(concat_file_path(tableFilter(list_files,"VariableName","FG_Curve")))
	fg_discount <- as.data.frame(matrix(nrow=length(unique(fg_curve$Index_Name)),ncol=ncol(result_discounttable)))
	colnames(fg_discount) <- colnames(result_discounttable)
	fg_discount$IndexMapping <- unique(fg_curve$Index_Name)
	fg_discount$AssetClass <- "Sovereign Bond"
	
	result_discounttable <- rbind(result_discounttable,fg_discount)
}

#try(sqlDrop(dbc,table_name))
table_name <- "Index_Spreads_To_Group_Map"
saveTable(dbc,table_name,versionDataFrame(unique(result_discounttable),current_version))


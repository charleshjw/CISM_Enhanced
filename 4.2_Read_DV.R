# Read Dependent Variables
#	perform stationarity test.
#	perform ACF and PACF of DV
# 
# Author: E620927
###############################################################################

######Auto correlation Analysis
Output_Corr = file.path(Output_root,"CSICluster")
if(!file.exists(Output_Corr)) {
	dir.create(Output_Corr)
}

if (FLAG_READ_PRIOR_VERSION) {
	dbc_dv = dbc_prev
	version_dv = prior_version
} else {
	dbc_dv = dbc
	version_dv = current_version
}

##############################
##Read Dependent Variables
for (i in 1:length(modelid_columns)) {
#	i <- 1
	modelid_column = modelid_columns[i]
	if (i == 1) {
		table_name <- paste0("O_PC1LEVEL_FINAL_",modelid_column)
		PCLEVEL <- fetchTable(dbc_dv, table_name,version_dv)
		PCLEVEL <- retrievePC(PCLEVEL)
		PC_level <- combineCurves(PCLEVEL)
		colnames(PC_level) <- cleanString(colnames(PC_level))
		
		table_name <- paste0("O_PC1TS_FINAL_",modelid_column)
		PC1 <- fetchTable(dbc_dv, table_name,version_dv)
		PC_DATA <- retrievePC(PC1)
		Dep_var <- combineCurves(PC_DATA)
		colnames(Dep_var) <- cleanString(colnames(Dep_var))
	} else {
		table_name <- paste0("O_PC1LEVEL_FINAL_",modelid_column)
		PCLEVEL <- fetchTable(dbc_dv, table_name,version_dv)
		PCLEVEL <- retrievePC(PCLEVEL)
		PC_level_tmp <- combineCurves(PCLEVEL)
		PC_level_tmp <- PC_level_tmp[,!colnames(PC_level_tmp)%in%colnames(PC_level)]
		colnames_tmp <- c(colnames(PC_level),cleanString(colnames(PC_level_tmp)))
		PC_level <- ts.union(PC_level,PC_level_tmp)
		colnames(PC_level) <- colnames_tmp
		
		table_name <- paste0("O_PC1TS_FINAL_",modelid_column)
		PC1 <- fetchTable(dbc_dv, table_name,version_dv)
		PC_DATA <- retrievePC(PC1)
		Dep_var_tmp <- combineCurves(PC_DATA)
		Dep_var_tmp <- Dep_var_tmp[,!colnames(Dep_var_tmp)%in%colnames(Dep_var)]
		colnames_tmp <- c(colnames(Dep_var),cleanString(colnames(Dep_var_tmp)))
		Dep_var <- ts.union(Dep_var,Dep_var_tmp)
		colnames(Dep_var) <- colnames_tmp
	}
}

#table_name <- "O_PC1LEVEL_FINAL"
#PCLEVEL <- fetchTable(dbc, table_name,current_version)
#PCLEVEL <- retrievePC(PCLEVEL)
#PC_level <- combineCurves(PCLEVEL)
#colnames(PC_level) <- cleanString(colnames(PC_level))
#
#table_name <- "O_PC1TS_FINAL"
#PC1 <- fetchTable(dbc, table_name,current_version)
#PC_DATA <- retrievePC(PC1)
#Dep_var <- combineCurves(PC_DATA)
#colnames(Dep_var) <- cleanString(colnames(Dep_var))

##verification
if (!all(colnames(Dep_var)%in%colnames(MOD_IV))) {
	print("Warning: Some clusters do not have relevant variables specified in MOD_IV table")
	print(colnames(Dep_var)[!colnames(Dep_var)%in%colnames(MOD_IV)])
}
if (!all(colnames(MOD_IV)%in%colnames(Dep_var))) {
	print("Warning: Some clusters do not have history read")
	print(colnames(MOD_IV)[!colnames(MOD_IV)%in%colnames(Dep_var)])
}

###verification
if (!all(colnames(Ind_var)%in%MOD_IV[,"IV_NAME"])) {
	print(colnames(Ind_var)[!colnames(Ind_var)%in%MOD_IV[,"IV_NAME"]])
	stop("Warning: Some indepedent variables are not included in variable mapping table")
}

if (DV_ANALYSIS) {
#	table_name <- "O_PC1TS_FINAL_DATAFRAME"
#	try(sqlDrop(dbc, table_name))
#	sqlSave(dbc, as.data.frame(round(Dep_var,digits=5)), tablename = table_name, addPK=FALSE, safer = FALSE, rownames=TRUE)
	file_name <- paste0(Output_Corr,"/ALLCSIClusters.xlsx")
	write.xlsx(Dep_var,file_name)

	##############################
	#ACF and PACF of DV
	Num_variables = length(Dep_var[1,])
	test_results = matrix(NA,1+6*Num_variables,4+length(Dep_var[,1]))
	test_results[1,seq(1,4+length(Dep_var[,1]))] = c("Type", "Variable", "# obs.", seq(0,length(Dep_var[,1])))
	max_columns = -1
	
	# Note that NA values are scrubbed out, if NA's exist in the middle of the data series, 
	#	it will be removed.
	for (i in 1:Num_variables) {
	#	i <- 2
		temp_var = Dep_var[,i]
		
		lag_max =min(sum(1*(!is.na(temp_var)))-1,10*log10(sum(1*!is.na(temp_var)))) 
		
		file_name = paste(Output_Corr,"/ACF_",colnames(Dep_var)[i],".png",sep="")
		png(file_name)
		ACF_temp = acf(temp_var, na.action = na.pass,lag.max = lag_max,main=paste("ACF_",colnames(Dep_var)[i]))
		dev.off()
		test_results[2+6*(i-1),seq(1,3+length(ACF_temp$acf))] = c("ACF",colnames(Dep_var)[i],sum(1*(!is.na(temp_var))),ACF_temp$acf)
		
		Error_term = qnorm(c(0.025, 0.975))/sqrt(sum(1*(!is.na(temp_var)))) 
		test_results[3+6*(i-1),seq(1,3+length(ACF_temp$acf))] = c("ACF","","Error band",rep(Error_term[1],length(ACF_temp$acf)))
		test_results[4+6*(i-1),seq(1,3+length(ACF_temp$acf))] = c("ACF","","Error band",-rep(Error_term[1],length(ACF_temp$acf)))
		
		file_name = paste(Output_Corr,"/PACF_",colnames(Dep_var)[i],".png",sep="")
		png(file_name)
		PACF_temp = pacf(temp_var, na.action = na.pass,lag.max = lag_max,main=paste("PACF_",colnames(Dep_var)[i]))
		dev.off()
		test_results[5+6*(i-1),seq(1,4+length(PACF_temp$acf))] = c("PACF",colnames(Dep_var)[i],sum(1*(!is.na(temp_var))),"#N/A",PACF_temp$acf)	
		test_results[6+6*(i-1),seq(1,4+length(PACF_temp$acf))] = c("PACF","","Error band","#N/A",rep(Error_term[1],length(PACF_temp$acf)))
		test_results[7+6*(i-1),seq(1,4+length(PACF_temp$acf))] = c("PACF","","Error band","#N/A",-rep(Error_term[1],length(PACF_temp$acf)))
		
		if (max(3+length(ACF_temp$acf),4+length(PACF_temp$acf)) > max_columns) max_columns = max(3+length(ACF_temp$acf),4+length(PACF_temp$acf))
	}
	# Cut down the matrix to maximum size
	test_results = test_results[,seq(1,max_columns)]
	
#	table_name <- "O_IV_AUTOCORR"
#	try(sqlDrop(dbc, table_name))
#	sqlSave(dbc, as.data.frame(test_results), tablename = table_name, addPK=FALSE, safer = FALSE, rownames=TRUE)
	file_name <- paste0(Output_Corr,"/AutoCorrelation.xlsx")
	write.xlsx(as.data.frame(test_results),file_name)
	
	##############################
	#stationarity test with no lags
	Num_variables <- length(Dep_var[1,])
	test_results <- matrix(NA,Num_variables,11)
	
	for (i in 1:Num_variables) {
	#	i <- 1
		op <- options(warn = (-1)) 
		
		temp_var <- try(na.omit(Dep_var[,i],silent = TRUE))
		
		if (inherits(temp_var,"try-error")) {
			temp_var <- Dep_var[,i]
			temp_var <- temp_var[!is.na(temp_var)]
			test_results[i,2] <- "Internal missing values omitted"
		}
		
		options(op) 
		
		#If there are no infinite values in data
		if(sum(temp_var) < Inf) {
			test_results[i,1] <- length(temp_var)
			
			op <- options(warn = (-1)) 
			stats_adf0 <- adf.test(temp_var, alternative = "stationary")$p.value
			stats_adf1 <- adf.test(temp_var, alternative = "stationary")$p.value
			stats_pp1 <- pp.test(temp_var, alternative = "stationary")$p.value
			stats_kpss1 <- kpss.test(temp_var, null = "Level")$p.value
			
			# Verdict for level stationarity tests
			if ((min(stats_adf1,stats_pp1)<0.05)&(stats_kpss1>0.05)) {
				test_results[i,7] <- "Level Stationary"
			} else if ((min(stats_adf1,stats_pp1)>0.05)&(stats_kpss1<0.05)) {
				test_results[i,7] <- "Not Level Stationary"
			} else if ((min(stats_adf1,stats_pp1)>0.05)&(stats_kpss1>0.05)) {
				test_results[i,7] <- "Inconclusive Level Stationarity Tests"
			} else if ((min(stats_adf1,stats_pp1)<0.05)&(stats_kpss1<0.05)) {
				test_results[i,7] <- "Conflicting Level Stationarity Tests"
			}
			
			adf2_test <- ur.df(temp_var, type = "trend", lags = trunc((length(temp_var)-1)^(1/3)))
			
			# Classify the test results for adf-test:
			adf2_class <- sum(1*(adf2_test@teststat[1,1] < adf2_test@cval[1,]))
			if (adf2_class == 3) {
				stats_adf2 <- "Less than 1%"
				stats_adf2_ex <- 0.005
			} else if (adf2_class == 2) {
				stats_adf2 <- "Between 1% and 5%"
				stats_adf2_ex <- 0.025
			} else if (adf2_class == 1) {
				stats_adf2 <- "Between 5% and 10%"
				stats_adf2_ex <- 0.075
			} else if (adf2_class == 0) {
				stats_adf2 <- "Over 10%"
				stats_adf2_ex <- 0.45
			}
			
			pp2_test <- ur.pp(temp_var, model = "trend", lags = "short", type = "Z-tau")
			# Classify the test results for pp-test:
			pp2_class <- sum(1*(pp2_test@teststat < pp2_test@cval))
			if (pp2_class == 3) {
				stats_pp2 <- "Less than 1%"
				stats_pp2_ex <- 0.005
			} else if (pp2_class == 2) {
				stats_pp2 <- "Between 1% and 5%"
				stats_pp2_ex <- 0.025
			} else if (pp2_class == 1) {
				stats_pp2 <- "Between 5% and 10%"
				stats_pp2_ex <- 0.075
			} else if (pp2_class == 0) {
				stats_pp2 <- "Over 10%"
				stats_pp2_ex <- 0.45
			}
			
			kpss2_test <- ur.kpss(temp_var, type = "tau", lags = "short")
			kpss2_class <- sum(1*(kpss2_test@teststat < kpss2_test@cval[-3]))
			if (kpss2_class == 3) {
				stats_kpss2 <- "Less than 1%"
				stats_kpss2_ex <- 0.005
			} else if (kpss2_class == 2) {
				stats_kpss2 <- "Between 1% and 5%"
				stats_kpss2_ex <- 0.025
			} else if (kpss2_class == 1) {
				stats_kpss2 <- "Between 5% and 10%"
				stats_kpss2_ex <- 0.075
			} else if (kpss2_class == 0) {
				stats_kpss2 <- "Over 10%"
				stats_kpss2_ex <- 0.45
			}
			
			options(op) 
			
			# Verdict for Trend stationarity tests
			if ((min(stats_adf2_ex,stats_pp2_ex)<0.05)&(stats_kpss2_ex>0.05)) {
				test_results[i,11] <- "Trend Stationary"
			} else if ((min(stats_adf2_ex,stats_pp2_ex)>0.05)&(stats_kpss2_ex<0.05)) {
				test_results[i,11] <- "Not Trend Stationary"
			} else if ((min(stats_adf2_ex,stats_pp2_ex)>0.05)&(stats_kpss2_ex>0.05)) {
				test_results[i,11] <- "Inconclusive Trend Stationarity Tests"
			} else if ((min(stats_adf2_ex,stats_pp2_ex)<0.05)&(stats_kpss2_ex<0.05)) {
				test_results[i,11] <- "Conflicting Trend Stationarity Tests"
			}
			
			test_results[i,3:6] <- round(c(stats_adf0,stats_adf1,stats_pp1,stats_kpss1),digits=3)
			test_results[i,8:10] <- c(stats_adf2,stats_pp2,stats_kpss2)
			
		} else {
			test_results[i,7] <- "Undesirable (Infinite value in time series)"
			test_results[i,11] <- "Undesirable (Infinite value in time series)"
		}
	}
	colnames(test_results) <- c("Obs", "Internal NAs?","ADF p-val (Trapletti reference)","ADF p-val (drift)","PP p-val (drift)", "KPSS p-val (drift)","Mean Stationarity Test Verdict","ADF p-val (trend)","PP p-val (trend)", "KPSS p-val (trend)","Trend Stationarity Test Verdict")
	rownames(test_results) <- colnames(Dep_var)
	
#	table_name <- "O_PC1TS_DV_Stationarity_nolag"
#	try(sqlDrop(dbc, table_name))
#	sqlSave(dbc, as.data.frame(test_results), tablename = table_name, addPK=FALSE, safer = FALSE, rownames=TRUE)
	file_name <- paste0(Output_Corr,"/ALLCSI_Stationarity_nolag.xlsx")
	write.xlsx(as.data.frame(test_results),file_name)
	
	##################################
	####Stationarity test with lags	
	Num_variables = length(Dep_var[1,])
	test_results = matrix(NA,Num_variables,18)
	
	for (i in 1:Num_variables) {
		op <- options(warn = (-1)) 
		
		temp_var = try(na.omit(Dep_var[,i],silent = TRUE))
		
		if (inherits(temp_var,"try-error")) {
			temp_var = Dep_var[,i]
			temp_var = temp_var[!is.na(temp_var)]
			test_results[i,2] = "Internal missing values omitted"
		}
		
		options(op) 
		
		#If there are no infinite values in data
		if(sum(temp_var) < Inf) {
			
			op <- options(warn = (-1)) 
			
			#Length of dependent variable
			DV_len = length(temp_var)
			test_results[i,1] = DV_len
			
			########## ADF test with drift ##############
			if (DV_len < 50) {
				max_lag = floor(4*(DV_len/100)^(1/4)) #Use Schwart's l4
			} else {
				max_lag = floor(12*(DV_len/100)^(1/4)) #Use Schwart's l12
			}
			stationarity = ur.df(temp_var, type = "drift", lags = max_lag, selectlags = "BIC")
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_adf1 = approx(x = cval, y =c(0.01,0.05,0.10), xout = test_stat, method = "linear", rule = 2)$y
			stats_adf1_lag = length(rownames(stationarity@testreg$coef)) - 2
			
			########## PP test with drift using Schwert's lag##############
			if (DV_len < 50) {
				max_lag = floor(4*(DV_len/100)^(1/4)) #Use Schwart's l4
				stationarity = ur.pp(temp_var, type = "Z-tau", model = "constant", lags = "short") #Use Schwart's l4
			} else {
				max_lag = floor(12*(DV_len/100)^(1/4)) #Use Schwart's l12
				stationarity = ur.pp(temp_var, type = "Z-tau", model = "constant", lags = "long")
			}
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_pp1 = approx(x = cval, y =c(0.01,0.05,0.10), xout = test_stat, method = "linear", rule = 2)$y
			stats_pp1_lag = max_lag
			
			########## KPSS test with drift using Schwert's lag##############
			
			if (DV_len < 50) {
				max_lag = floor(4*(DV_len/100)^(1/4)) #Use Schwart's l4
				stationarity = ur.kpss(temp_var, type = "mu", lags = "short") #Use Schwart's l4
			} else {
				max_lag = floor(12*(DV_len/100)^(1/4)) #Use Schwart's l12
				stationarity = ur.kpss(temp_var, type = "mu", lags = "long")
			}
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_kpss1 = approx(x = cval, y =c(0.1,0.05,0.025, 0.01), xout = test_stat, method = "linear", rule = 2)$y
			stats_kpss1_lag = max_lag
			
			########## DF-GLS test with drift ##############
			#http://www.stata.com/manuals13/tsdfgls.pdf
			if (DV_len < 50) {
				max_lag = floor(4*(DV_len/100)^(1/4)) #Use Schwart's l4
			} else {
				max_lag = floor(12*(DV_len/100)^(1/4)) #Use Schwart's l12
			}
			select_lag = -1
			select_BIC = Inf
			for (k in 1:max_lag) {
				
				stationarity = ur.ers(temp_var, type = "DF-GLS", model = "constant", lag.max = k)
				residuals = stationarity@testreg$residuals
				if (max_lag > k) {
					residuals = residuals[-1*seq(1,max_lag - k)]
				}
				BIC = log(1/((DV_len-1) - max_lag)*sum(residuals^2)) + (k+1)*log((DV_len-1)-max_lag)/((DV_len-1)-max_lag)
				if (BIC < select_BIC) {
					select_BIC = BIC
					select_lag = k
				}
			}
			stationarity = ur.ers(temp_var, type = "DF-GLS", model = "constant", lag.max = select_lag)
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_dfgls1 = approx(x = cval, y =c(0.01,0.05,0.10), xout = test_stat, method = "linear", rule = 2)$y
			stats_dfgls1_lag = select_lag
			
			########## ADF test with TREND ##############
			if (DV_len < 50) {
				max_lag = floor(4*(DV_len/100)^(1/4)) #Use Schwart's l4
			} else {
				max_lag = floor(12*(DV_len/100)^(1/4)) #Use Schwart's l12
			}
			stationarity = ur.df(temp_var, type = "trend", lags = max_lag, selectlags = "BIC")
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_adf2 = approx(x = cval, y =c(0.01,0.05,0.10), xout = test_stat, method = "linear", rule = 2)$y
			stats_adf2_lag = length(rownames(stationarity@testreg$coef)) - 3
			
			########## PP test with TREND using Schwert's lag##############
			if (DV_len < 50) {
				max_lag = floor(4*(DV_len/100)^(1/4)) #Use Schwart's l4
				stationarity = ur.pp(temp_var, type = "Z-tau", model = "trend", lags = "short") #Use Schwart's l4
			} else {
				max_lag = floor(12*(DV_len/100)^(1/4)) #Use Schwart's l12
				stationarity = ur.pp(temp_var, type = "Z-tau", model = "trend", lags = "long")
			}
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_pp2 = approx(x = cval, y =c(0.01,0.05,0.10), xout = test_stat, method = "linear", rule = 2)$y
			stats_pp2_lag = max_lag
			
			########## KPSS test with TREND using Schwert's lag##############
			
			if (DV_len < 50) {
				max_lag = floor(4*(DV_len/100)^(1/4)) #Use Schwart's l4
				stationarity = ur.kpss(temp_var, type = "tau", lags = "short") #Use Schwart's l4
			} else {
				max_lag = floor(12*(DV_len/100)^(1/4)) #Use Schwart's l12
				stationarity = ur.kpss(temp_var, type = "tau", lags = "long")
			}
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_kpss2 = approx(x = cval, y =c(0.1,0.05,0.025, 0.01), xout = test_stat, method = "linear", rule = 2)$y
			stats_kpss2_lag = max_lag
			
			########## DF-GLS test with TREND ##############
			#http://www.stata.com/manuals13/tsdfgls.pdf
			if (DV_len < 50) {
				max_lag = floor(4*(DV_len/100)^(1/4)) #Use Schwart's l4
			} else {
				max_lag = floor(12*(DV_len/100)^(1/4)) #Use Schwart's l12
			}
			
			select_lag = -1
			select_BIC = Inf
			for (k in 1:max_lag) {
				
				stationarity = ur.ers(temp_var, type = "DF-GLS", model = "trend", lag.max = k)
				residuals = stationarity@testreg$residuals
				if (max_lag > k) {
					residuals = residuals[-1*seq(1,max_lag - k)]
				}
				BIC = log(1/((DV_len-1) - max_lag)*sum(residuals^2)) + (k+1)*log((DV_len-1)-max_lag)/((DV_len-1)-max_lag)
				if (BIC < select_BIC) {
					select_BIC = BIC
					select_lag = k
				}
			}
			stationarity = ur.ers(temp_var, type = "DF-GLS", model = "trend", lag.max = select_lag)
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_dfgls2 = approx(x = cval, y =c(0.01,0.05,0.10), xout = test_stat, method = "linear", rule = 2)$y
			stats_dfgls2_lag = length(rownames(stationarity@testreg$coef)) - 1
			
			test_results[i,3:18] = c(stats_adf1,stats_adf1_lag,
					stats_pp1,stats_pp1_lag,
					stats_kpss1,stats_kpss1_lag,
					stats_dfgls1,stats_dfgls1_lag,
					stats_adf2,stats_adf2_lag,
					stats_pp2,stats_pp2_lag,
					stats_kpss2,stats_kpss2_lag,
					stats_dfgls2,stats_dfgls2_lag)
			
		} else {
			test_results[i,2] = "Infinite value in time series"
		}
		
		colnames(test_results) = c("Obs","Internal NAs?","ADF p-val (drift)","ADF lag (drift)",
				"PP p-val (drift)","PP lag (drift)",
				"KPSS p-val (drift)","KPSS lag (drift)",
				"DF-GLS p-val (drift)","DF_GLS lag (drift)",
				"ADF p-val (trend)","ADF lag (trend)",
				"PP p-val (trend)","PP lag (trend)",
				"KPSS p-val (trend)","KPSS lag (trend)",
				"DF-GLS p-val (trend)","DF_GLS lag (trend)")
		rownames(test_results) <- colnames(Dep_var)
		
	}
	
#	table_name <- "O_PC1TS_DV_Stationarity_wlags"
#	try(sqlDrop(dbc, table_name))
#	sqlSave(dbc, as.data.frame(round(test_results,digits=3)), tablename = table_name, addPK=FALSE, safer = FALSE, rownames=TRUE)

	file_name <- paste0(Output_Corr,"/ALLCSI_Stationarity_wlags.xlsx")
	write.xlsx(as.data.frame(round(test_results,digits=3)),file_name)
}

test_results <- c()

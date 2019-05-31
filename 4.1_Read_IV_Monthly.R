#
# 
# Author: E620927
###############################################################################

Output_IV <- file.path(Output_root,"MacroFactorVerificationMonthly")
if (!dir.exists(Output_IV)) {
	dir.create(Output_IV)
}

##Fetch Independent Variables --- History
IV <- fetchTable(dbc,"I_HIST_IV_PRIM_MONTHLY",current_version)
#IV <- sqlFetch(dbc, "I_HIST_IV_PRIM")
#IV <- IV[IV$VERSION==current_version,]
IV <- IV[,c("IV_ID","DATE","IV_VALUE")]
IV[,1] <- as.character(IV[,1])
IV[,2] <- as.Date(as.character(IV[,2]))
IV[,3] <- as.numeric(as.character(IV[,3]))

IV_sec <- fetchTable(dbc,"I_HIST_IV_SEC_MONTHLY",current_version)
#IV_sec <- sqlFetch(dbc, "I_HIST_IV_SEC")
#IV_sec <- IV_sec[IV_sec$VERSION==current_version,]
IV_sec <- IV_sec[,c("IV_ID","DATE","IV_VALUE")]
IV_sec[,1] <- as.character(IV_sec[,1])
IV_sec[,2] <- as.Date(as.character(IV_sec[,2]))
IV_sec[,3] <- as.numeric(as.character(IV_sec[,3]))

IV_all <- rbind(IV, IV_sec)
#IV[1,]
#IV[IV$IV_ID=="CORP_SPR_SWP_5Y_AAA",]
#IV_sec[IV_sec$IV_ID=="CORP_SPR_SWP_5Y_AAA",]

IV_DATA <- retrieveIV(IV_all)
IV_Names <- names(IV_DATA)

file_name <- paste(Output_IV,"/IVALL.tex",sep="")
print(xtable(as.data.frame(IV_Names),caption=paste0("All Independent Variables")),file=file_name,tabular.environment = 'longtable', floating=FALSE, include.rownames=TRUE)

##Fetch Independent Variables --- Fcst
scenario_data <- sqlFetch(dbc, "I_PREVSCEN_IV_FULL_MONTHLY")
scenario_data <- scenario_data[scenario_data$VERSION==current_version,]
Scenario_Names <- as.character(unique(scenario_data$SCEN_ID))
iv_names <- as.character(unique(scenario_data$IV_ID))
Scenario_Data_Names <- c()
if(!all(IV_Names%in%iv_names)) {
	stop("ERROR: Not all variables have forecast data")
}

for (i in 1:length(iv_names)) {
#	i<-1
	iv <- iv_names[i]
	for (j in 1:length(Scenario_Names)) {
#		j <- 1
		sce <- Scenario_Names[j]
		data <- scenario_data[scenario_data$SCEN_ID==sce&scenario_data$IV_ID==iv,c("DATE","IV_VALUE")]
		data <- data[order(data$DATE),]
		if (i==1&j==1) {
			Scenario_Data <- ts(data$IV_VALUE,start=determineTsStartDate(data$DATE),frequency=determineTsFrequency(data$DATE))
		} else {
			Scenario_Data <- ts.union(Scenario_Data,ts(data$IV_VALUE,start=determineTsStartDate(data$DATE),frequency=determineTsFrequency(data$DATE)))
		}
		Scenario_Data_Names <- c(Scenario_Data_Names, paste(iv,Scenario_Names[j],sep="_"))
	}
}
colnames(Scenario_Data) <- Scenario_Data_Names

#Do IV transformation - history and fcst
IV_transform <- sqlFetch(dbc, "I_IV_TRANSFORMS")
IV_transform <- IV_transform[IV_transform$VERSION==current_version,]
IV_transform <- IV_transform[IV_transform$IV_ID%in%IV_Names,]

colnames = c()
colnames_fcst <- c()
#Initiate the matrix storing the variable transformations, lags, and group
IV_ID = matrix(ncol = 5, nrow = 0)
for (i in 1:nrow(IV_transform)) {
#	i <- 1
	#Identify the independent variable
	temp_ts = IV_DATA[[as.character(IV_transform$IV_ID[i])]]
#	if ("END_DATE" %in% ls())
#		temp_ts <- window(temp_ts,end=END_DATE)
	#Create transformations
	tranform_columns <- grep("Transform",colnames(IV_transform))
	for (j in tranform_columns) {
		# j <- 7
		
		if (IV_transform[i,j]==""|is.na(IV_transform[i,j])) {
			next
		}
		for (l in 0:Num_lags) {
#			l <- 0
			for (k in 1:length(Scenario_Names)) {
#				k <- 1
				tmp_fcst <- transformTS(as.ts(c(zoo(temp_ts),zoo(Scenario_Data[,paste(IV_transform$IV_ID[i],Scenario_Names[k],sep="_")]))), IV_transform[i,j], l)
#				tmp_fcst <- window(tmp_fcst,start=max(index(temp_ts))+0.00001,end=END_DATE+2)
				tmp_fcst <- window(tmp_fcst,start=max(index(temp_ts))+0.00001)
				
				if ((i == 1)&(j==tranform_columns[1])&(l==0)&(k==1)) {
					Ind_var_fcst = tmp_fcst
				} else {
					Ind_var_fcst = ts.union(Ind_var_fcst,tmp_fcst)
				}
				colname_fcst <- paste(IV_transform$IV_ID[i],IV_transform[i,j],lags[1+l],Scenario_Names[k],sep="")
				colnames_fcst <- c(colnames_fcst, colname_fcst)
			}
			
			temp <- transformTS(temp_ts, IV_transform[i,j], l)
			# Create a matrix storing all the transformed variables
			if ((i == 1)&(j==tranform_columns[1])&(l==0)) {
				Ind_var = temp
			} else {
				Ind_var = ts.union(Ind_var,temp)
			}
			colname <- paste(IV_transform$IV_ID[i],IV_transform[i,j],lags[1+l],sep="")
			colnames <- c(colnames, colname)
			IV_ID <- rbind(IV_ID,c(as.character(IV_transform$IV_ID[i]),as.character(IV_transform[i,j]),lags[l+1],colname,as.character(IV_transform[i,]$IV_GROUP)))
		}
	}
}

IV_ID <- as.data.frame(IV_ID)
colnames(IV_ID) <- c("CSI_ID", "TRANSFORM", "LAG", "IV_NAME","IV_GROUP")
colnames(Ind_var) = colnames
rownames(Ind_var) <- as.yearmon(index(Ind_var))
colnames(Ind_var_fcst) <- colnames_fcst
rownames(Ind_var_fcst) <- as.yearmon(index(Ind_var_fcst))
#write.csv(Ind_var,"./test1.csv")
#write.csv(Ind_var_fcst,"./test2.csv")
#
#if ("dbc_prev"%in%ls()&IV_ANALYSIS) {
#	print("Verification: Current version versus Prior Version")
#	
##	IV_prev <- sqlFetch(dbc_prev, "I_HIST_IV_PRIM")
###	IV_prev <- IV_prev[IV_prev$VERSION==prior_version,]
##	IV_prev <- tableFilter(IV_prev,"version",prior_version)
##	
##	IV_prev <- IV_prev[,c("IV_ID","DATE","IV_VALUE")]
##	IV_prev[,1] <- as.character(IV_prev[,1])
##	IV_prev[,2] <- as.Date(as.character(IV_prev[,2]))
##	IV_prev[,3] <- as.numeric(as.character(IV_prev[,3]))
##	IV_sec_prev <- sqlFetch(dbc_prev, "I_HIST_IV_SEC")
###	IV_sec_prev <- IV_sec_prev[IV_sec_prev$VERSION==prior_version,]
##	IV_sec_prev <- tableFilter(IV_sec_prev,"version",prior_version)
##	
##	IV_sec_prev <- IV_sec_prev[,c("IV_ID","DATE","IV_VALUE")]
##	IV_sec_prev[,1] <- as.character(IV_sec_prev[,1])
##	IV_sec_prev[,2] <- as.Date(as.character(IV_sec_prev[,2]))
##	IV_sec_prev[,3] <- as.numeric(as.character(IV_sec_prev[,3]))
#	
##	IV_all_prev <- rbind(IV_prev, IV_sec_prev)
##	IV_DATA_prev <- retrieveIV(IV_all_prev)
##	IV_Names_prev <- names(IV_DATA_prev)
#	
##	which("CRE_PRICE_IND"==iv_names)
##	iv_names <- IV_Names[IV_Names%in%IV_Names_prev]
##	for (i in 1:length(iv_names)) {
###		i <- 17
##		file_name <- paste(Output_IV,"/",iv_names[i],".png",sep="")
##		png(file_name,width=800,height=600)
##		tmp_data <- ts.union(IV_DATA[[iv_names[i]]],IV_DATA_prev[[iv_names[i]]])
##		ts.plot(tmp_data,col=1:2,main=iv_names[i],type="b")
###		legend("topleft",c("Q22017","Q42016"),col=1:2,pch=1)
##		legend("topleft",c("Current Variable","Prior Variable"),col=1:2,pch=1)
###		abline(v=c(2016.9,2017.3))
##		dev.off()
##		tmp_data <- cbind(tmp_data,tmp_data[,1]-tmp_data[,2],(tmp_data[,1]-tmp_data[,2])/tmp_data[,2])
##		tmp_data <- cbind(time(tmp_data),tmp_data)
##		colnames(tmp_data) <- c("Time","Current","Prior","Diff","Percentage")
##		file_name <- paste(Output_IV,"/",iv_names[i],".xlsx",sep="")
##		write.xlsx(tmp_data,file_name)
##		
##		file_name <- paste(Output_IV,"/",iv_names[i],".tex",sep="")
##		print(xtable(tmp_data),file=file_name,tabular.environment = 'longtable', floating=FALSE, include.rownames=TRUE)
##	}
##	
##	fileConn<-file(paste(Output_IV,"/IVFactors.tex",sep=""),"w")
##	writeLines(paste0("\\newcommand\\IVFactors{",paste(iv_names,collapse=","),"}"), fileConn)
##	close(fileConn)
#}

##compare monthly and quarterly version
#try(IV_Q <- sqlFetch(dbc, "I_HIST_IV_Q_PRIM"))
#if ("IV_Q"%in%ls()) {
#	print("Verification: Monthly versus Quarterly")
#	Output_IV <- file.path(Output_root,"M_Q")
#	if (!dir.exists(Output_IV)) {
#		dir.create(Output_IV)
#	}
#	
#	IV_Q <- IV_Q[,c("IV_ID","DATE","IV_VALUE")]
#	IV_Q[,1] <- as.character(IV_Q[,1])
#	IV_Q[,2] <- as.Date(as.character(IV_Q[,2]))
#	IV_Q[,3] <- as.numeric(as.character(IV_Q[,3]))
#	
#	IV_DATA_Q <- retrieveIV(IV_Q)
#	IV_Names_Q <- names(IV_DATA_Q)
#	
#	iv_names <- IV_Names[IV_Names%in%IV_Names_Q]
#	
#	targets <- c("CORP_SPR_TSY_5Y_AAA","CORP_SPR_TSY_5Y_AA","CORP_SPR_TSY_5Y_A","CORP_SPR_TSY_5Y_BBB","CORP_SPR_TSY_5Y_BB_PLUS",	"CORP_SPR_TSY_10Y_AAA",
#			"CORP_SPR_TSY_10Y_AA",	"CORP_SPR_TSY_10Y_A",	"CORP_BOND_YLD_10Y_BBB",	"CORP_SPR_TSY_10Y_BB_PLUS",	"SWP_RATE_5Y",
#			"SWP_RATE_10Y",	"TSY_YLD_5Y","TSY_YLD_10Y")
#	print(targets[!targets%in%iv_names])
#	iv_names <- targets[targets%in%iv_names]
#	
#	
#	for (i in 1:length(iv_names)) {
##		i <- 1
#		#PNG version
#		file_name <- paste(Output_IV,"/",iv_names[i],"-M-Q.png",sep="")
#		png(file_name,width=800,height=600)
#		tmp_data <- as.ts(merge(Quarterly = as.zoo(IV_DATA[[iv_names[i]]]), Monthly = as.zoo(IV_DATA_Q[[iv_names[i]]])))
#		ts.plot(tmp_data,col=1:2,main=iv_names[i],type="b")
#		legend("topleft",c("Q42016","Q42015"),col=1:2,pch=1)
#		dev.off()
#		
#		#Excel Version
#		tmp_data <- cbind(tmp_data,tmp_data[,1]-tmp_data[,2],(tmp_data[,1]-tmp_data[,2])/tmp_data[,2])
#		tmp_data <- cbind(time(tmp_data),tmp_data)
#		colnames(tmp_data) <- c("Time","Current","Prior","Diff","GrowthRate")
#		file_name <- paste(Output_IV,"/",iv_names[i],"-M-Q.xlsx",sep="")
#		write.xlsx(tmp_data,file_name)
#	}
#}

if (IV_ANALYSIS) {
	
	file_name <- paste0(Output_IV,"/O_IV_VAR_MONTHLY.xlsx")
#	try(sqlDrop(dbc, table_name))
#	sqlSave(dbc, as.data.frame(t(Ind_var)), tablename = table_name, addPK=FALSE, safer = FALSE, rownames=TRUE)
	data = data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),as.data.frame(t(Ind_var)))
#	saveTable(dbc,table_name,data)
	write.xlsx(data,file_name)
	
	file_name <- paste0(Output_IV,"/O_IV_VAR_FCST_MONTHLY.xlsx")
#	table_name <- paste0(current_version,"_O_IV_VAR_FCST")
#	try(sqlDrop(dbc, table_name))
#	sqlSave(dbc, as.data.frame(t(Ind_var_fcst)), tablename = table_name, addPK=FALSE, safer = FALSE, rownames=TRUE)
	data = data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),as.data.frame(t(Ind_var_fcst)))
#	saveTable(dbc,table_name,data)
	write.xlsx(data,file_name)
	
	##Correlation Matrix
	file_name <- paste0(Output_IV,"/IV_Correlation_MONTHLY.xlsx")
	Ind_var_L0=Ind_var[,grep("L0",colnames(Ind_var))]
	Correlation_matrix = cor(Ind_var_L0, use = "pairwise.complete.obs")
#	table_name <- paste0(current_version,"_O_IV_CORRELATION")
#	try(sqlDrop(dbc, table_name))
#	sqlSave(dbc, as.data.frame(round(Correlation_matrix,digits=3)), tablename = table_name, addPK=FALSE, safer = FALSE, rownames=TRUE)
	data = data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),as.data.frame(round(Correlation_matrix,digits=3)))
#	saveTable(dbc,table_name,data)
	write.xlsx(data,file_name)
	
	#Stationarity Test
	Num_variables = length(Ind_var[1,])
	test_results = matrix(NA,Num_variables,10)
	
	for (i in 1:Num_variables) {
		#	i <- 1
		op <- options(warn = (-1)) 
		
		temp_var = try(na.omit(Ind_var[,i],silent = TRUE))
		
		if (inherits(temp_var,"try-error")) {
			temp_var = Ind_var[,i]
			temp_var = temp_var[!is.na(temp_var)]
			test_results[i,1] = "Internal missing values omitted"
		}
		
		options(op) 
		
		#If there are no infinite values in data
		if(sum(temp_var) < Inf&sum(temp_var) > -Inf) {
			
			op <- options(warn = (-1)) 
			stats_adf0 = AdfTest(temp_var,simpleoutput=FALSE)$p.value
			stats_adf1 = AdfTest(temp_var,simpleoutput=FALSE)$p.value
			stats_pp1 = PPTest_tseries(temp_var,simpleoutput=FALSE)$p.value
			stats_kpss1 = KPSSTest_tseries(temp_var,simpleoutput=FALSE)$p.value
			
			# Verdict for level stationarity tests
			if ((min(stats_adf1,stats_pp1)<0.05)&(stats_kpss1>0.05)) {
				test_results[i,6] = "Level Stationary"
			} else if ((min(stats_adf1,stats_pp1)>0.05)&(stats_kpss1<0.05)) {
				test_results[i,6] = "Not Level Stationary"
			} else if ((min(stats_adf1,stats_pp1)>0.05)&(stats_kpss1>0.05)) {
				test_results[i,6] = "Inconclusive Level Stationarity Tests"
			} else if ((min(stats_adf1,stats_pp1)<0.05)&(stats_kpss1<0.05)) {
				test_results[i,6] = "Conflicting Level Stationarity Tests"
			}
			
			adf2_test = ur.df(temp_var, type = "trend", lags = trunc((length(temp_var)-1)^(1/3)))
			
			# Classify the test results for adf-test:
			adf2_class = sum(1*(adf2_test@teststat[1,1] < adf2_test@cval[1,]))
			if (adf2_class == 3) {
				stats_adf2 = "Less than 1%"
				stats_adf2_ex = 0.005
			} else if (adf2_class == 2) {
				stats_adf2 = "Between 1% and 5%"
				stats_adf2_ex = 0.025
			} else if (adf2_class == 1) {
				stats_adf2 = "Between 5% and 10%"
				stats_adf2_ex = 0.075
			} else if (adf2_class == 0) {
				stats_adf2 = "Over 10%"
				stats_adf2_ex = 0.45
			}
			
			pp2_test = ur.pp(temp_var, model = "trend", lags = "short", type = "Z-tau")
			# Classify the test results for pp-test:
			pp2_class = sum(1*(pp2_test@teststat < pp2_test@cval))
			if (pp2_class == 3) {
				stats_pp2 = "Less than 1%"
				stats_pp2_ex = 0.005
			} else if (pp2_class == 2) {
				stats_pp2 = "Between 1% and 5%"
				stats_pp2_ex = 0.025
			} else if (pp2_class == 1) {
				stats_pp2 = "Between 5% and 10%"
				stats_pp2_ex = 0.075
			} else if (pp2_class == 0) {
				stats_pp2 = "Over 10%"
				stats_pp2_ex = 0.45
			}
			
			kpss2_test = ur.kpss(temp_var, type = "tau", lags = "short")
			kpss2_class = sum(1*(kpss2_test@teststat < kpss2_test@cval[-3]))
			if (kpss2_class == 3) {
				stats_kpss2 = "Less than 1%"
				stats_kpss2_ex = 0.005
			} else if (kpss2_class == 2) {
				stats_kpss2 = "Between 1% and 5%"
				stats_kpss2_ex = 0.025
			} else if (kpss2_class == 1) {
				stats_kpss2 = "Between 5% and 10%"
				stats_kpss2_ex = 0.075
			} else if (kpss2_class == 0) {
				stats_kpss2 = "Over 10%"
				stats_kpss2_ex = 0.45
			}
			
			options(op) 
			
			# Verdict for trend stationarity tests
			if ((min(stats_adf2_ex,stats_pp2_ex)<0.05)&(stats_kpss2_ex>0.05)) {
				test_results[i,10] = "Trend Stationary"
			} else if ((min(stats_adf2_ex,stats_pp2_ex)>0.05)&(stats_kpss2_ex<0.05)) {
				test_results[i,10] = "Not Trend Stationary"
			} else if ((min(stats_adf2_ex,stats_pp2_ex)>0.05)&(stats_kpss2_ex>0.05)) {
				test_results[i,10] = "Inconclusive Trend Stationarity Tests"
			} else if ((min(stats_adf2_ex,stats_pp2_ex)<0.05)&(stats_kpss2_ex<0.05)) {
				test_results[i,10] = "Conflicting Trend Stationarity Tests"
			}
			
			test_results[i,2:5] = round(c(stats_adf0,stats_adf1,stats_pp1,stats_kpss1),digits=3)
			test_results[i,7:9] = c(stats_adf2,stats_pp2,stats_kpss2)
			
		} else {
			test_results[i,6] = "Undesirable (Infinite value in time series)"
			test_results[i,10] = "Undesirable (Infinite value in time series)"
		}
	}
	
	colnames(test_results) = c("Internal NAs?","ADF p-val","ADF p-val","PP p-val", "KPSS p-val","Level Stationarity Test Verdict","ADF p-val (trend)","PP p-val (trend)", "KPSS p-val (trend)","Trend Stationarity Test Verdict")
	rownames(test_results) = colnames(Ind_var)
	test_results = cbind(IV_NAME=as.character(colnames(Ind_var)),test_results)
	file_name <- paste0(Output_IV,"/IV_Stationarity_nolag_MONTHLY.xlsx")
	data = data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),as.data.frame(test_results))
	write.xlsx(data,file_name)
	
	colnames(data) <- gsub("[.]","",colnames(data))
	table_name <- "O_IV_Stationarity_nolags_MONTHLY"
	saveTable(dbc,table_name,data)
	
	##With lags
	Num_variables = length(Ind_var[1,])
	test_results = matrix(NA,Num_variables,18)
	
	for (i in 1:Num_variables) {
		op <- options(warn = (-1)) 
		
		temp_var = try(na.omit(Ind_var[,i],silent = TRUE))
		
		if (inherits(temp_var,"try-error")) {
			temp_var = Ind_var[,i]
			temp_var = temp_var[!is.na(temp_var)]
			test_results[i,2] = "Internal missing values omitted"
		}
		
		options(op) 
		
		#If there are no infinite values in data
		if(sum(temp_var) < Inf&sum(temp_var) > -Inf) {
			
			op <- options(warn = (-1)) 
			
			#Length of dependent variable
			test_results[i,1] = length(temp_var)
			#Length of dependent variable
			IV_len = length(temp_var)
			test_results[i,1] = IV_len
			
			########## ADF test with drift ##############
			if (IV_len < 50) {
				max_lag = floor(4*(IV_len/100)^(1/4)) #Use Schwart's l4
			} else {
				max_lag = floor(12*(IV_len/100)^(1/4)) #Use Schwart's l12
			}
			stationarity = ur.df(temp_var, type = "drift", lags = max_lag, selectlags = "BIC")
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_adf1 = approx(x = cval, y =c(0.01,0.05,0.10), xout = test_stat, method = "linear", rule = 2)$y
			stats_adf1_lag = length(rownames(stationarity@testreg$coef)) - 2
			
			########## PP test with drift using Schwert's lag##############
			if (IV_len < 50) {
				max_lag = floor(4*(IV_len/100)^(1/4)) #Use Schwart's l4
				stationarity = ur.pp(temp_var, type = "Z-tau", model = "constant", lags = "short") #Use Schwart's l4
			} else {
				max_lag = floor(12*(IV_len/100)^(1/4)) #Use Schwart's l12
				stationarity = ur.pp(temp_var, type = "Z-tau", model = "constant", lags = "long")
			}
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_pp1 = approx(x = cval, y =c(0.01,0.05,0.10), xout = test_stat, method = "linear", rule = 2)$y
			stats_pp1_lag = max_lag
			
			########## KPSS test with drift using Schwert's lag##############
			
			if (IV_len < 50) {
				max_lag = floor(4*(IV_len/100)^(1/4)) #Use Schwart's l4
				stationarity = ur.kpss(temp_var, type = "mu", lags = "short") #Use Schwart's l4
			} else {
				max_lag = floor(12*(IV_len/100)^(1/4)) #Use Schwart's l12
				stationarity = ur.kpss(temp_var, type = "mu", lags = "long")
			}
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_kpss1 = approx(x = cval, y =c(0.1,0.05,0.025, 0.01), xout = test_stat, method = "linear", rule = 2)$y
			stats_kpss1_lag = max_lag
			
			########## DF-GLS test with drift ##############
			#http://www.stata.com/manuals13/tsdfgls.pdf
			if (IV_len < 50) {
				max_lag = floor(4*(IV_len/100)^(1/4)) #Use Schwart's l4
			} else {
				max_lag = floor(12*(IV_len/100)^(1/4)) #Use Schwart's l12
			}
			select_lag = -1
			select_BIC = Inf
			for (k in 1:max_lag) {
				
				stationarity = ur.ers(temp_var, type = "DF-GLS", model = "constant", lag.max = k)
				residuals = stationarity@testreg$residuals
				if (max_lag > k) {
					residuals = residuals[-1*seq(1,max_lag - k)]
				}
				BIC = log(1/((IV_len-1) - max_lag)*sum(residuals^2)) + (k+1)*log((IV_len-1)-max_lag)/((IV_len-1)-max_lag)
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
			if (IV_len < 50) {
				max_lag = floor(4*(IV_len/100)^(1/4)) #Use Schwart's l4
			} else {
				max_lag = floor(12*(IV_len/100)^(1/4)) #Use Schwart's l12
			}
			stationarity = ur.df(temp_var, type = "trend", lags = max_lag, selectlags = "BIC")
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_adf2 = approx(x = cval, y =c(0.01,0.05,0.10), xout = test_stat, method = "linear", rule = 2)$y
			stats_adf2_lag = length(rownames(stationarity@testreg$coef)) - 3
			
			########## PP test with TREND using Schwert's lag##############
			if (IV_len < 50) {
				max_lag = floor(4*(IV_len/100)^(1/4)) #Use Schwart's l4
				stationarity = ur.pp(temp_var, type = "Z-tau", model = "trend", lags = "short") #Use Schwart's l4
			} else {
				max_lag = floor(12*(IV_len/100)^(1/4)) #Use Schwart's l12
				stationarity = ur.pp(temp_var, type = "Z-tau", model = "trend", lags = "long")
			}
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_pp2 = approx(x = cval, y =c(0.01,0.05,0.10), xout = test_stat, method = "linear", rule = 2)$y
			stats_pp2_lag = max_lag
			
			########## KPSS test with TREND using Schwert's lag##############
			
			if (IV_len < 50) {
				max_lag = floor(4*(IV_len/100)^(1/4)) #Use Schwart's l4
				stationarity = ur.kpss(temp_var, type = "tau", lags = "short") #Use Schwart's l4
			} else {
				max_lag = floor(12*(IV_len/100)^(1/4)) #Use Schwart's l12
				stationarity = ur.kpss(temp_var, type = "tau", lags = "long")
			}
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_kpss2 = approx(x = cval, y =c(0.1,0.05,0.025, 0.01), xout = test_stat, method = "linear", rule = 2)$y
			stats_kpss2_lag = max_lag
			
			########## DF-GLS test with TREND ##############
			#http://www.stata.com/manuals13/tsdfgls.pdf
			if (IV_len < 50) {
				max_lag = floor(4*(IV_len/100)^(1/4)) #Use Schwart's l4
			} else {
				max_lag = floor(12*(IV_len/100)^(1/4)) #Use Schwart's l12
			}
			
			select_lag = -1
			select_BIC = Inf
			for (k in 1:max_lag) {
				
				stationarity = ur.ers(temp_var, type = "DF-GLS", model = "trend", lag.max = k)
				residuals = stationarity@testreg$residuals
				if (max_lag > k) {
					residuals = residuals[-1*seq(1,max_lag - k)]
				}
				BIC = log(1/((IV_len-1) - max_lag)*sum(residuals^2)) + (k+1)*log((IV_len-1)-max_lag)/((IV_len-1)-max_lag)
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
			
			
			test_results[i,3:18] = round(c(stats_adf1,stats_adf1_lag,
							stats_pp1,stats_pp1_lag,
							stats_kpss1,stats_kpss1_lag,
							stats_dfgls1,stats_dfgls1_lag,
							stats_adf2,stats_adf2_lag,
							stats_pp2,stats_pp2_lag,
							stats_kpss2,stats_kpss2_lag,
							stats_dfgls2,stats_dfgls2_lag),digits=3)
			
		} else {
			test_results[i,7] = "Undesirable (Infinite value in time series)"
			test_results[i,11] = "Undesirable (Infinite value in time series)"
		}
	}
	
	colnames(test_results) = c("Obs","Internal NAs?","ADF p-val (drift)","ADF lag (drift)",
			"PP p-val (drift)","PP lag (drift)",
			"KPSS p-val (drift)","KPSS lag (drift)",
			"DF-GLS p-val (drift)","DF_GLS lag (drift)",
			"ADF p-val (trend)","ADF lag (trend)",
			"PP p-val (trend)","PP lag (trend)",
			"KPSS p-val (trend)","KPSS lag (trend)",
			"DF-GLS p-val (trend)","DF_GLS lag (trend)")
	
	test_results_clean <- cbind(IV_NAME=colnames(Ind_var),test_results)
	
	file_name <- paste0(Output_IV,"/IV_Stationarity_wlags_MONTHLY.xlsx")
	data = data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),as.data.frame(test_results_clean))
	write.xlsx(data,file_name)
	
	table_name <- "O_IV_Stationarity_wlags_MONTHLY"
	colnames(data) <- gsub("[.]","",colnames(data))
	
#	try(sqlDrop(dbc, table_name))
	saveTable(dbc,table_name,data)
}
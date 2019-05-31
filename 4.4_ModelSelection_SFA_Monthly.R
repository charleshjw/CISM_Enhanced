# Perform singple factor analysis
# 
# Author: E620927
###############################################################################
Output_sfa <- file.path(Output_root,"SFA_MONTHLY")
if (!dir.exists(Output_sfa)) {
	dir.create(Output_sfa)
}

#get un-stationary independent variables and remove them
remove_IV_list <- getNonStationaryTSNames(FALSE,QUARTERLY=FALSE)
Ind_var_clean <- Ind_var[,!colnames(Ind_var) %in% remove_IV_list]

for (i in 1:ncol(Dep_var)) {
	ptm = proc.time()
#	i <- 1
	dep = colnames(Dep_var)[i]
	print(dep)
	dep_var = Dep_var[,i,drop=FALSE]
	
	file_name <- paste(Output_sfa,"/SimpleReg-",dep,".csv",sep="")
	RERUN_SFA <- RERUN_SFA_FLAG
	REPLOT_SFA <- REPLOT_SFA_FLAG
	
	if (RERUN_SFA | !file.exists(file_name)) {
		REPLOT_SFA = TRUE
		
		temp_matrix = matrix(ncol=23,nrow=0)
		for (j in 1:ncol(Ind_var_clean)) {
		#	j <- 1;
		#	colnames(Ind_var_clean)[j]
			ind_var = Ind_var_clean[,j,drop=FALSE]
			data <- ts.union(dep_var,ind_var)
			colnames(data) <- c(colnames(Dep_var)[i],colnames(Ind_var_clean)[j])
			data <- na.omit(data)
			if(!all(is.finite(data[,2]))) {
				print(paste("Infinite Data observed:",colnames(Ind_var_clean)[j]))
				next();
			}
			
			# building the regression models
			temp_reg = lm(paste(colnames(data)[1], "~ 0+", colnames(data)[2]),data)
			result = summary(temp_reg)
			
			temp_res = temp_reg$residuals
			temp_fit = temp_reg$fitted
			
			#Number of observations used
			temp_length = length(temp_fit)
			
			temp_AIC = AIC(temp_reg)
			temp_BIC = BIC(temp_reg)	
			
			temp_rmse = sqrt(mean((temp_fit-temp_reg$model[,1])^2))
			
			temp_coef = temp_reg$coefficients[1]
			
			temp_p.value = summary(temp_reg)$coefficients[1,4]
			
			rob_stat = coeftest(temp_reg,vcov = vcovHAC)[,4]
			
			R2adj = summary(temp_reg)$adj.r.squared
			R2 = summary(temp_reg)$r.squared
			
			F_stat =1-pf(result$fstatistic[1],result$fstatistic[2],result$fstatistic[3])
	
			stats_adf1 = AdfTest(temp_res,simpleoutput=FALSE)$p.value
			stats_pp = PPTest_tseries(temp_res,simpleoutput=FALSE)$p.value
			stats_kpss = KPSSTest_tseries(temp_res,simpleoutput=FALSE)$p.value
			
			stationary_residuals = stationaryTest(temp_res)
			if (stats_kpss < 0.1) {
				stationary_residuals2 = "Fail"
			} else {
				stationary_residuals2 = ""
			}
			
			if (stats_kpss < 0.05) {
				stationary_residuals3 = "Fail"
			} else {
				stationary_residuals3 = ""
			}
			
			# calculate BG statistics 
			BG_stat = bgtest(temp_reg)$p.value
			
			DW_upper_stat = dwtest(temp_reg,alternative = c("greater"))$p.value
			#DW_upper_stat = NA
			
			DW_lower_stat = dwtest(temp_reg,alternative = c("less"))$p.value
			#DW_lower_stat = NA
			
			BP_stat = BPTest(temp_reg, simpleoutput=FALSE)
			
			RMSE = outSampleTest(dep_var, ind_var, OutSamplePeriod=9)
			
			temp_matrix = rbind(temp_matrix,
				c(colnames(Dep_var)[i],colnames(Ind_var_clean)[j], temp_length, 
						R2, R2adj, temp_AIC,temp_BIC, F_stat, temp_rmse, temp_coef, 
						temp_p.value, rob_stat,BG_stat,
						DW_upper_stat, DW_lower_stat,BP_stat,RMSE,
						stats_adf1,
						stats_pp,
						stats_kpss,
						stationary_residuals, 
						stationary_residuals2,
						stationary_residuals3)
			)
		}
		colnames(temp_matrix) = c("DV", "Variable", "# datapoints", "R2", "adjR2", 
				"AIC", "BIC", "F", "RMSE", "Coef", "p-value","Robst p","BG_stat", "DW_upper_stat", "DW_lower_stat","BP_stat","RMSE_OS",
				"ADF pvalue", "PP pvalue",
				"KPSS pvalue", "Criteria1",
				"Criteria2", "Criteria3")
		
		temp_matrix <- temp_matrix[order(as.numeric(temp_matrix[,"adjR2"]),decreasing = TRUE),]
		
		#################################
		###Filter out unsatisfactory models 
		#condidtion1: pass heter and autocorrelation tests or robostness test
		attri <- c("BG_stat","DW_upper_stat","DW_lower_stat","BP_stat")
		flag <- rep(FALSE,nrow(temp_matrix))
		for (j in 1:nrow(temp_matrix)) {
			if (any(as.numeric(temp_matrix[j,attri])<pthreshold,na.rm=TRUE)) {
				flag[j] <- as.numeric(temp_matrix[j,"Robst p"]) < pthreshold
			} else {
				flag[j] <- TRUE
			}
		}
		temp_matrix <- temp_matrix[flag,,drop=FALSE]
		
		#condition2: R^2 > 0.3
		temp_matrix = temp_matrix[temp_matrix[,"adjR2"]>0.3,,drop=FALSE]
		
		#output to tex files
		temp_matrix = as.data.frame(temp_matrix)
		columns = c("Variable","adjR2","Coef", "Robst p", "AIC", "RMSE_OS", "Criteria1","Criteria2", "Criteria3")
		temp_matrix_tex = temp_matrix[1:min(max_model_to_test,nrow(temp_matrix)),colnames(temp_matrix)%in%columns]
		attri <- c("adjR2","Coef","Robst p","AIC","RMSE_OS")
		for (k in 1:length(attri)) {
			temp_matrix_tex[,attri[k]]=round(as.numeric(as.character(temp_matrix_tex[,attri[k]])),2)
		}
		
		file_name <- paste(Output_sfa,"/SimpleReg-",colnames(Dep_var)[i],".tex",sep="")
		print(xtable(temp_matrix_tex,caption=paste0("Single Factor Analysis-",colnames(Dep_var)[i])),file=file_name,tabular.environment = "longtable", floating=FALSE, include.rownames=TRUE)
	
		#output to excel files
		file_name <- paste(Output_sfa,"/SimpleReg-",colnames(Dep_var)[i],".csv",sep="")
		write.csv(temp_matrix, file = file_name)
	} else if (REPLOT_SFA) {
		temp_matrix <- read.csv(file_name,check.names=FALSE)
	} else {
		print("Skip")
	}
	
	if (REPLOT_SFA) {
		###run backtest and out-of-sample tests for top models
		Output_sfa_model <- paste0(Output_sfa,"/",colnames(Dep_var)[i])
		if (!dir.exists(Output_sfa_model)) {
			dir.create(Output_sfa_model)
		}
		
		if (nrow(temp_matrix) == 0) {
			next()
		}
		
		for (j in 1:min(max_model_to_test,nrow(temp_matrix))) {
		#	j <- 1
			dep <- as.character(temp_matrix[j,]$DV)
			ind <- as.character(temp_matrix[j,]$Variable)
			ind <- ind[!is.na(ind)]
			ind <- as.character(ind[ind!=""])
			dep_var <- Dep_var[,dep,drop=FALSE]
			ind_var <- Ind_var[,ind,drop=FALSE]
			pc_level <- PC_level[,dep]
			data <- ts.union(dep_var,ind_var)
			colnames(data) <- c(colnames(dep_var),colnames(ind_var))
			
			# building the regression models
			temp_reg = lm(paste(colnames(data)[1], "~ 0+", colnames(data)[2]),data)
			temp_res = temp_reg$res
			temp_fit = temp_reg$fitted
			ts_fitted <- fittedValueToTS(temp_fit, data)

			#calculate forecast differenced value
			fcst_ts_all <- retrieveFcst(ind,Ind_var_fcst,temp_reg)
			ts_to_plot <- ts.union(dep_var,ts_fitted,fcst_ts_all)
			leg_names <- c("History","FittedValue",Scenario_Names)
			colnames(ts_to_plot) <- paste(dep,leg_names,sep="_");
			
			file_name <- paste(Output_sfa_model,"/",j,"_insample_diff.png",sep="")
			png(file_name, width=800, height=600)
			tsPlot(ts_to_plot,dep,leg_names,retrieveSubtitle(temp_reg),marks=TRUE)
			dev.off()
			
			#convert back to level value
			pc_level_SOP <- head(na.omit(pc_level),1)
			leg_names <- c(Scenario_Names,"FittedValue","History")
			levels <- diffToLevelMultipleTS(ts_to_plot,pc_level_SOP,leg_names,History_END_DATE)
			
			file_name <- paste(Output_sfa_model,"/",j,"_insample_level.png",sep="")
			png(file_name, width=800, height=600)
			tsPlot(levels[,!grepl("FittedValue",colnames(levels))],dep,leg_names[!grepl("FittedValue",colnames(levels))],marks=TRUE)
			dev.off()
			
			#out of sample testing
			outSampleTesting(dep_var, ind_var, Output_sfa_model,filenameseg=j,OutSamplePeriods, OUTPUT_VERBOSE = FALSE)
		}
	}
	
	print(proc.time() - ptm)
}

###Output to Database
#Reg_results_ACCESS <- Reg_results[[1]]
#for (i in 2:length(Reg_results)) {
#	Reg_results_ACCESS <- rbind(Reg_results_ACCESS, Reg_results[[i]])
#}
#for (i in 1:length(Reg_results)) {
#	mod <- Reg_results[[i]][1,"DV"]
#	print(mod)
#	table_name <- paste("O_SFA_RESULT_",mod,sep="")
#	sqlSave(dbc, as.data.frame(Reg_results[[i]]), tablename = table_name, addPK=FALSE, safer = FALSE, rownames=TRUE)
#}

file_name = paste(Output_sfa,"/ClustersAll.tex",sep="")
writeVectorToTex(file_name,"CLUSTERS",colnames(Dep_var))

temp_matrix <- c()
temp_matrix_tex <- c()
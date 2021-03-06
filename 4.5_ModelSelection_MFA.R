# Perform two-facotr analysis and plot graphs for top hundreds of models
# 
# Author: E620927
###############################################################################
Output_mfa <- file.path(Output_root,"MFA")
if (!dir.exists(Output_mfa)) {
	dir.create(Output_mfa)
}

#remove un-stationary independent variables
remove_IV_list <- getNonStationaryTSNames(FALSE)
Ind_var <- Ind_var[,!colnames(Ind_var) %in% c(remove_IV_list,"TSY_3M_FEDQoQPercentChangeL0","TSY_3M_FEDQoQPercentChangeL1")]

#get relevant variables for each cluster
IV_MOD <- fetchTable(dbc,"I_MOD_IV",current_version)
#head(IV_MOD)
#deleteTable(dbc,"I_MOD_IV")
#head(IV_MOD)
for (i in 1:ncol(Dep_var)) {
#	i <- 1
	print(i)
	print(colnames(Dep_var)[i])
	ptm = proc.time()
	
	dep = colnames(Dep_var)[i]
#	if (!grepl("LiborOAS",dep)) {
#		next
#	}

	Output_mfa_model <- paste0(Output_mfa,"/",colnames(Dep_var)[i])
	if (!dir.exists(Output_mfa_model)) {
		dir.create(Output_mfa_model)
	}
	RERUN_MFA <- RERUN_MFA_FLAG
	REPLOT_MFA <- REPLOT_MFA_FLAG
	
	tryCatch({
		if (!colnames(Dep_var)[i]%in%colnames(IV_MOD)) {
			#if no relevant variable defined, use AD_SBA's relevant variables as default
			print(paste0("Warning: no relevant variables defined. ", colnames(Dep_var)[i]))
			default_variable_selection <- colnames(IV_MOD)[grep("AD",colnames(IV_MOD))[1]]
			
			iv_list <- as.character(na.omit(IV_MOD$IV_NAME[IV_MOD[,colnames(IV_MOD)==default_variable_selection]!="NONE"]))
			iv_plus_list <- as.character(na.omit(IV_MOD$IV_NAME[IV_MOD[,colnames(IV_MOD)==default_variable_selection]=="PLUS"]))
			iv_minus_list <- as.character(na.omit(IV_MOD$IV_NAME[IV_MOD[,colnames(IV_MOD)==default_variable_selection]=="MINUS"]))
		} else {
			iv_list <- as.character(na.omit(IV_MOD$IV_NAME[IV_MOD[,colnames(IV_MOD)==colnames(Dep_var)[i]]!="NONE"]))
			iv_plus_list <- as.character(na.omit(IV_MOD$IV_NAME[IV_MOD[,colnames(IV_MOD)==colnames(Dep_var)[i]]=="PLUS"]))
			iv_minus_list <- as.character(na.omit(IV_MOD$IV_NAME[IV_MOD[,colnames(IV_MOD)==colnames(Dep_var)[i]]=="MINUS"]))
		}
		Ind_var_clean <- Ind_var[,colnames(Ind_var) %in% iv_list]
		
		#write relevant variables to tex file for presentation purpose
		if (ncol(Ind_var_clean)%%3!=0) {
			relevant_variables <- matrix(c(colnames(Ind_var_clean),rep("",3-ncol(Ind_var_clean)%%3)),ncol=3)
		} else {
			relevant_variables <- matrix(colnames(Ind_var_clean),ncol=3)
		}
		file_name = paste(Output_mfa_model,"/MFA_RelevantVariables_",colnames(Dep_var)[i],".tex",sep="")
		writeMatrixToTex(file_name,paste0("Relevant Variables: ",colnames(Dep_var)[i]),relevant_variables) 
		
		#get all feasible combination of two variables
		VarList <- generateVarList(Ind_var_clean)
#		VarList <- VarList[,1:200]
		Dep_local = Dep_var[,i,drop=FALSE]
		data <- ts.union(Dep_local,Ind_var_clean)
		colnames(data) <- c(colnames(Dep_var)[i],colnames(Ind_var_clean))
		
		file_name = paste(Output_mfa_model,"/MFA_",colnames(Dep_var)[i],".csv",sep="")
		if (RERUN_MFA | !file.exists(file_name)) {
			REPLOT_MFA = TRUE
			##Run from scratch
			##Create Empty Storages for results
			IV_coef = matrix(NA, nrow = ncol(VarList), ncol = 2*(Max_var+1))
			temporary_name = rbind(paste("Var",1:Max_var,sep=""), paste(paste("Var",1:Max_var,sep=""), "p_val")) 
			dim(temporary_name) <- NULL
			colnames(IV_coef) = c("Intercept", "Intercept p-val", temporary_name)
			
			#Standardized Betas
			Standardized = matrix(NA, nrow = ncol(VarList), ncol = Max_var)
			colnames(Standardized) = paste(paste("Var",1:Max_var,sep=""), "Beta")
			
			#robust p value
			Robust_stat = matrix(NA, ncol = Max_var,nrow = ncol(VarList))
			temporary_name = t(paste("Var",seq(1:Max_var),"rob p_val",sep = " "))
			colnames(Robust_stat) = temporary_name
			
			#Model fit 
			fitted = matrix(NA, ncol = length(data[,1]),nrow = ncol(VarList))   # date from 2001Q1 to 2015Q1
			colnames(fitted) <- index(data[,1])
			
			#Results
			Width = 33
			Reg_results = matrix(NA, ncol = Width, nrow = ncol(VarList))
			
			for (j in 1:ncol(VarList)) {
#			for (j in 1:100) {
					
#				j <- 1
				varlist <- na.omit(VarList[,j])
				formu <- as.formula(paste(colnames(Dep_var)[i], " ~ 0 + ", paste(varlist,collapse=" + "),sep=""))
				temp_reg <- lm(formu, data)
				
				result = summary(temp_reg)
				
				# if one of the coefficients is NA, skip this model 
				if (sum(is.na(temp_reg$coef))>0) {         
					next
				}
				
				coeff_pval = t(result$coefficients[,c(1,4)])[,1:(length(result$coefficients[,1]))]
				dim(coeff_pval) <- NULL
				
				IV_coef[j,1] = 0
				IV_coef[j,3:ncol(IV_coef)] = c(coeff_pval, rep(NA,2*Max_var-length(coeff_pval)))
				
				# Calculate VIFs
				if (length(temp_reg$coef) >= 2 ){
					vif = max(vif(temp_reg))
				} else {
					vif = 1
				}
				
				F_stat =1-pf(result$fstatistic[1],result$fstatistic[2],result$fstatistic[3])
				
				# Calculate standardized coefficients
				betas = result$coef[,1]*sapply(temp_reg$model[-1],sd)/sapply(temp_reg$model[1],sd)
				Standardized[j,]=c(betas,  rep(NA, Max_var-length(betas)))
				
				# Calculate robust p-values without the intercept 
				rob_stat = coeftest(temp_reg,vcov = vcovHAC)[,4]
				Robust_stat[j,1:length(rob_stat)]=rob_stat
				
				# Calculate additional statistics on residuals
				residuals = result$residuals
				
				#Indices of non-NA datapoints
				dates_used = as.numeric(names(result$residuals))
				#If there is a gap between indices, the time-series is not continous
				Largest_gap = max(dates_used[-1] - dates_used[-length(dates_used)])
				
				if (Largest_gap > 1) {
					Gap = "Gap in time-series"
				} else {
					Gap = "No gap"
				}
				
				# calculate BG statistics 
				BG_stat = bgtest(temp_reg)$p.value
				
				DW_upper_stat = dwtest(temp_reg,alternative = c("greater"))$p.value
				
				DW_lower_stat = dwtest(temp_reg,alternative = c("less"))$p.value
				
				BP_stat = bptest(temp_reg)$p.value
				
				model = temp_reg$model
				colnames(model)= c("y", paste("x", seq(1,length(model[1,])-1),sep=""))
				lmobj = lm(y~., data = model)
				White_stat = white_test(lmobj)$p.value
				
				SW_stat = shapiro.test(residuals)$p.value
				
				JB_stat = jarque.bera.test(residuals)$p.value
				
				RESET_stat = resettest(temp_reg)$p.value
				
				#Calculate R^2, AIC, BIC, etc.
				r2 = result$r.squared
				r2adj = result$adj.r.squared
				aic = AIC(temp_reg)
				bic = BIC(temp_reg)	
				
				# Calculate average fitted R2
				#		R2_ex_dummy = no_dummy_r_sq(temp_reg = temp_reg, Temp_dummies=Dummies)
				R2_ex_dummy = NA
				
				########################################################################################################
				# Suppress warnings where p-values are interpolated
				op <- options(warn = (-1)) 
				### ADF test 
				stats_adf1 = AdfTest(residuals,simpleoutput=FALSE)$p.value
				
				### PP test  
				# PP test with Schwert's lag selection
				stats_pp = PPTest_tseries(residuals,simpleoutput=FALSE)$p.value
				
				### KPSS test
				stats_kpss = KPSSTest_tseries(residuals,simpleoutput=FALSE)$p.value
				
				### stationarity of residuals with three different criteria
				#criteria 1
				stationary_residuals = stationaryTest(residuals)
				
				#criteria 2
				if (stats_kpss < 0.1) {
					stationary_residuals2 = "Fail"
				} else {
					stationary_residuals2 = ""
				}
				
				#criteria 3
				if (stats_kpss < 0.05) {
					stationary_residuals3 = "Fail"
				} else {
					stationary_residuals3 = ""
				}

				# RMSE of out sample
				RMSE = outSampleTest(Dep_local, Ind_var_clean[,varlist], OutSamplePeriod=9)
				
				# Turn warnings back on
				options(op) 
				
				# Calculate fitted values
				fit = temp_reg$fitted.values
				fit_index = as.numeric(names(fit))
				
				fitted[j,fit_index] = fit
				
				# Store the results
				Reg_results[j,] = c(
					r2, r2adj, R2_ex_dummy, aic, bic, F_stat,
					length(residuals), Gap,
					vif,
					BG_stat, DW_upper_stat, DW_lower_stat,
					BP_stat, White_stat, 
					SW_stat, JB_stat,
					RESET_stat,
					stats_adf1,
					stats_pp,
					stats_kpss,
					stationary_residuals, 
					stationary_residuals2,
					stationary_residuals3,
					RMSE, 
					"","","","","","","","",""
					)
			}
			
			colnames(Reg_results) = c(
					"r2", "r2adj", "r2 ex dummy", "aic", "bic", "F", 
					"Num_obs", "Gap",
					"Max VIF",
					"BG_stat", "DW_upper_stat", "DW_lower_stat",
					"BP_stat", "White_stat", 
					"SW_stat", "JB_stat",
					"RESET_stat",
					"ADF p (noconst)", "PP p (noconst)",
					"KPSS lag (const)", "Criteria1",
					"Criteria2", "Criteria3",
					"RMSE",
					"", "",
					"", "",
					"", "",
					"", "",
					"")
			
			output = cbind( t(VarList) , IV_coef,
					Standardized, Robust_stat,
					Reg_results)
			 
			output_tmp=as.data.frame(output)
			
			####Filter out un-satisfactory models
			## condition1: pass (autocorrelation test and heteroscha test) or (significance test and VIF)
			attri <- c("BG_stat","DW_upper_stat","DW_lower_stat","BP_stat")
			flag <- rep(FALSE,nrow(output_tmp))
			for (j in 1:nrow(output_tmp)) {
#				j <- 1
				if (any(as.numeric(as.character(Reg_results[j,attri]))<pthreshold,na.rm=TRUE)) {
					flag[j] <- all(as.numeric(Robust_stat[j,]) < pthreshold,na.rm=TRUE)&(as.numeric(Reg_results[j,"Max VIF"]) < 5)	
				} else {
					flag[j] <- TRUE
				}
			}
			output_tmp_deleted <- cbind(output_tmp[!flag,,drop=FALSE],"FailReason"=rep("FailStatisticTest",length(sum(!flag))))
			output_tmp <- output_tmp[flag,,drop=FALSE]
			
			## condition2: R^2 > 0.4
			rthreshold = 0.4
			flag = as.numeric(as.character(output_tmp[,"r2adj"]))>rthreshold
			while (sum(flag) < 5 & rthreshold >= 0.2) {
				rthreshold <- rthreshold - 0.05
				flag = as.numeric(as.character(output_tmp[,"r2adj"]))>rthreshold
			}
			if (sum(!flag)>0) {
				output_tmp_deleted <- rbind(output_tmp_deleted,cbind(output_tmp[!flag,,drop=FALSE],"FailReason"=rep(paste0("R2adj<",rthreshold),length(sum(!flag)))))
			}
			output_tmp = output_tmp[flag,,drop=FALSE]
			
			## condition3: positive and negative of coefficients
#			head(output_tmp)
			flag <- rep(TRUE,nrow(output_tmp))
			for (j in 1:nrow(output_tmp)) {
#				j = 49
				if (output_tmp[j,"V1"]%in%iv_plus_list) {
					if (as.numeric(as.character(output_tmp[j,"Var1"]))<=0) {
						flag[j] = FALSE
						next
					}
				} else if(output_tmp[j,"V1"]%in%iv_minus_list) {
					if (as.numeric(as.character(output_tmp[j,"Var1"]))>=0) {
						flag[j] = FALSE
						next
					}
				}
				if (output_tmp[j,"V2"]%in%iv_plus_list) {
					if (as.numeric(as.character(output_tmp[j,"Var2"]))<=0) {
						flag[j] = FALSE
						next
					}
				} else if(output_tmp[j,"V2"]%in%iv_minus_list) {
					if (as.numeric(as.character(output_tmp[j,"Var2"]))>=0) {
						flag[j] = FALSE
						next
					}
				}
			}
			
			if (sum(!flag)>0) {
				output_tmp_deleted <- rbind(output_tmp_deleted,cbind(output_tmp[!flag,,drop=FALSE],"FailReason"=rep("CoeffSign",length(sum(!flag)))))
			}
			output_tmp = output_tmp[flag,,drop=FALSE]
			
			####output to excel file
			output_tmp=output_tmp[order(as.numeric(as.character(output_tmp$r2adj)),decreasing=TRUE),]
			file_name = paste(Output_mfa_model,"/MFA_",colnames(Dep_var)[i],".csv",sep="")
			write.csv(output_tmp,file = file_name)
			
			file_name = paste(Output_mfa_model,"/MFA_",colnames(Dep_var)[i],"_deleted_models.csv",sep="")
			write.csv(output_tmp_deleted,file = file_name)
			
			####output to tex file
			columns = c("V1","V2","r2adj","Var1","Var2","aic","RMSE","Criteria1","Criteria2","Criteria3")
			output_tmp_tex=output_tmp[1:min(nrow(output_tmp),max_model_to_test),columns]
			output_tmp_tex <-characterizeTable(output_tmp_tex)
			attri <- c("r2adj","Var1","Var2","aic","RMSE")
			for (k in 1:length(attri)) {
				output_tmp_tex[,attri[k]]=round(as.numeric(as.character(output_tmp_tex[,attri[k]])),2)
			}
			rownames(output_tmp_tex) <- 1:nrow(output_tmp_tex)
			file_name = paste(Output_mfa_model,"/MFA_",colnames(Dep_var)[i],".tex",sep="")
			print(xtable(output_tmp_tex,caption=paste0("Two Factor Analysis-",colnames(Dep_var)[i])),file=file_name,tabular.environment = "longtable", floating=FALSE, include.rownames=TRUE)
			
		} else if (REPLOT_MFA){
			output_tmp <- read.csv(file_name,check.names=FALSE)
		} else {
			print("skip")
		}
		
		if (REPLOT_MFA) {
			# run tests for top models
			dep <- colnames(Dep_var)[i]
			
			if (nrow(output_tmp)==0) {
				next()
			}
			
			for (j in 1:min(nrow(output_tmp),max_model_to_test)) {
			#	j <- 1
				# construct data frame
				ind <- output_tmp[j,c("V1","V2")]
				ind <- ind[!is.na(ind)]
				ind <- as.character(ind[ind!=""])
				dep_var <- Dep_var[,dep,drop=FALSE]
				ind_var <- Ind_var[,ind,drop=FALSE]
				data <- ts.union(dep_var,ind_var)
				colnames(data) <- c(colnames(dep_var),colnames(ind_var))
				
				# building the regression models
				formu <- as.formula(paste(colnames(data), " ~ 0 + ", paste(colnames(data),collapse=" + "),sep=""))
				temp_reg = lm(formu,data)
				temp_res = temp_reg$res
				
				# Graph1: backtesting and forecasting on differenced values				
				temp_fit = temp_reg$fitted
				ts_fitted <- fittedValueToTS(temp_fit, data)
				fcst_ts_all <- retrieveFcst(ind,Ind_var_fcst,temp_reg)
				ts_to_plot <- ts.union(dep_var,ts_fitted,fcst_ts_all)
				leg_names <- c("History","FittedValue",Scenario_Names)
				colnames(ts_to_plot) <- paste(dep,leg_names,sep="_");
				
				file_name <- paste(Output_mfa_model,"/",j,"_insample_diff.png",sep="")
				png(file_name, width=800, height=600)
				tsPlot(ts_to_plot,dep,leg_names,retrieveSubtitle(temp_reg),marks=TRUE)
				dev.off()
				
				# Graph2: backtesting and forecasting when convertted back to level value
				tryCatch({
					pc_level <- PC_level[,dep]
					pc_level_SOP <- head(na.omit(pc_level),1)
					leg_names <- c(Scenario_Names,"FittedValue","History")
					levels <- diffToLevelMultipleTS(ts_to_plot,pc_level_SOP,leg_names,History_END_DATE)
					
					file_name <- paste(Output_mfa_model,"/",j,"_insample_level.png",sep="")
					png(file_name, width=800, height=600)
					tsPlot(levels[,!grepl("FittedValue",colnames(levels))],dep,leg_names[!grepl("FittedValue",colnames(levels))],marks=TRUE)
					dev.off()
				}, error=function(e) e)
				# Graph3: plot residuals and stationarity test
				stats_adf = round(AdfTest(temp_res,simpleoutput=FALSE)$p.value,3)
				stats_pp = round(PPTest_tseries(temp_res,simpleoutput=FALSE)$p.value,3)
				stats_kpss = round(KPSSTest_tseries(temp_res,simpleoutput=FALSE)$p.value,3)
				
				file_name <- paste(Output_mfa_model,"/",j,"_insample_residuals.png",sep="")
				png(file_name, width=800, height=600)
				plot(temp_res,main=dep,sub=retrieveSubtitle(temp_reg))
				legend("bottomright", paste0(stats_adf,"/",stats_pp,"/",stats_kpss), pch = NA, title = "ADF/PP/KPSS")
				dev.off()
				
				# Graph4: out of sample testing
				outSampleTesting(dep_var, ind_var, Output_mfa_model,filenameseg=j,OutSamplePeriods, OUTPUT_VERBOSE=FALSE)
				
				# Graph5: effect of each variables
				tryCatch({
					fcst_ts_all_variables <- retrieveFcstByVariable(ind,Ind_var_fcst,temp_reg)
					if (!is.na(fcst_ts_all_variables)) {
						file_name <- paste(Output_mfa_model,"/",j,"_insample_level_by_variables.png",sep="")
						png(file_name, width=1600, height=600)
						par(mfrow=c(1,2))
						for (i in 1:length(fcst_ts_all_variables)) {
						#	i <- 1
							ts_to_plot <- ts.union(dep_var,ts_fitted,fcst_ts_all_variables[[i]])
							leg_names <- c("History","FittedValue",Scenario_Names)
							colnames(ts_to_plot) <- paste(dep,leg_names,sep="_");
							pc_level_SOP <- head(na.omit(pc_level),1)
							leg_names <- c(Scenario_Names,"FittedValue","History")
							levels <- diffToLevelMultipleTS(ts_to_plot,pc_level_SOP,leg_names,History_END_DATE)
							tsPlot(levels[,!grepl("FittedValue",colnames(levels))],dep,leg_names[!grepl("FittedValue",colnames(levels))],subtitle=names(fcst_ts_all_variables)[i],marks=TRUE)
						}
						dev.off()
					}
				}, error=function(e) e)
			}
		}
	}, finally = {
#		print(colnames(Dep_var)[i])
	})
	print(proc.time() - ptm)
}
	
#memory clean up
Ind_var_clean = c()
relevant_variables = c()
VarList = c()
Dep_local = c()
data = c()
IV_coef = c()
Standardized = c()
Robust_stat = c()
fitted = c()
Reg_results = c()
output = c()
output_tmp = c()
output_tmp_tex = c()
	
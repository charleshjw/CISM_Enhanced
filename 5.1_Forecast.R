#
# 
# Author: E620927
###############################################################################

Output_forecast <- file.path(Output_root,"Forecast")
if (!dir.exists(Output_forecast)) {
	dir.create(Output_forecast)
}

Model_Final <- fetchTable(dbc,"I_MOD_SPECIFICATION_FINAL",current_version)
Model_Final <- clean_test_Model_Final(Model_Final)

#if(!all(Model_Final[Model_Final$Var3!="",]$Var3%in%colnames(Ind_var))) {
#	stop("ERROR: Specification is Not Right")
#}
if(!all(Model_Final$CLUSTER%in%colnames(Dep_var))) {
	print(Model_Final$CLUSTER[!Model_Final$CLUSTER%in%colnames(Dep_var)])
	stop("ERROR: Final Model Specification has more cluster than dependent variables")
}

if(!all(colnames(Dep_var)%in%Model_Final$CLUSTER)) {
	print(colnames(Dep_var)[!colnames(Dep_var)%in%Model_Final$CLUSTER])
	print("Warning: dependent variables have more variables that are not specified in final model specification")
}

##Create Empty Storages for results
#Intercept and coefficients
IV_coef = matrix(NA, nrow = nrow(Model_Final), ncol = 2*(Max_var+1))
temporary_name = rbind(paste("Var",1:Max_var,"Coef",sep=""), paste("Var",1:Max_var,"p_val",sep="")) 
dim(temporary_name) <- NULL
colnames(IV_coef) = c("Intercept", "Interceptpval", temporary_name)

#Standardized Betas
Standardized = matrix(NA, nrow = nrow(Model_Final), ncol = Max_var)
colnames(Standardized) = paste0(paste("Var",1:Max_var,sep=""), "Beta")

#robust p value
Robust_stat = matrix(NA, ncol = Max_var,nrow = nrow(Model_Final))
temporary_name = t(paste("Var",seq(1:Max_var),"rob_p_val",sep = " "))
colnames(Robust_stat) = temporary_name

#Results
Width = 21
Reg_results = matrix(NA, ncol = Width, nrow = nrow(Model_Final))
colnames(Reg_results) = c(
		"r2", "r2adj", "r2 ex dummy", "aic", "bic", "F", 
		"Num_obs", "Gap",
		"Max VIF",
		"BG_stat", "DW_upper_stat", "DW_lower_stat",
		"BP_stat", "White_stat", 
		"SW_stat", "JB_stat",
		"RESET_stat",
		"ADF (Residual)", "PP (Residual)",
		"KPSS (Residual)","Empty")

#sensitivity results for both variables
sensitivity_var1_all <- data.frame(matrix(rep("",nrow(Model_Final)*(length(Scenario_Names)+6)),nrow=nrow(Model_Final)),stringsAsFactors=FALSE)
sensitivity_var2_all <- data.frame(matrix(rep("",nrow(Model_Final)*(length(Scenario_Names)+6)),nrow=nrow(Model_Final)),stringsAsFactors=FALSE)

for (i in 1:nrow(Model_Final)) {
#	i <- 70
	dep <- Model_Final[i,]$CLUSTER
	ind <- c(Model_Final[i,]$Var1,Model_Final[i,]$Var2,Model_Final[i,]$Var3)
	ind <- ind[!is.na(ind)]
	ind <- as.character(ind[ind!=""])
	if (!dep%in%colnames(Dep_var)) {
		warning(paste0("Forecast.R: Model_Final_Specification specified a dependent variable that is not existed. Check"))
		next()
	}
	dep_var <- Dep_var[,dep,drop=FALSE]
	ind_var <- Ind_var[,ind,drop=FALSE]
	pc_level <- PC_level[,dep]
	data <- ts.union(dep_var,ind_var)
	colnames(data) <- c(colnames(dep_var),colnames(ind_var))
	
	file_name <- paste(Output_forecast,"/",dep,"_regression_data.csv",sep="")
	write.csv(data,file_name)
	
	if (i == 1) {
		#Model fit 
		fitted = matrix(NA, ncol = nrow(data), nrow = nrow(Model_Final))   # date from 2001Q1 to 2015Q1
		colnames(fitted) <- index(data[,1])
	}
	formu <- as.formula(paste(colnames(dep_var), " ~ 0 + ", paste(colnames(ind_var),collapse=" + "),sep=""))
	temp_reg <- lm(formu, data)
	
	result = summary(temp_reg)
	
	# if one of the coefficients is NA, skip this model 
	if (sum(is.na(temp_reg$coef))>0) {   
		print(colnames(dep_var))
		print("ERROR:Coefficient has NA")
		next
	}
	
	coeff_pval = t(result$coefficients[,c(1,4)])
	#		coeff_pval = rbind(coeff_pval,result$coefficients[1,4])
	dim(coeff_pval) <- NULL
	
	IV_coef[i,1] = 0
	IV_coef[i,3:ncol(IV_coef)] = c(coeff_pval, rep(NA,2*Max_var-length(coeff_pval)))
	
	# Calculate VIFs
	if (length(temp_reg$coef) > 2){
		#vif = max(vif(lm(as.formula(temp_expr)))$VIF.Eigenvalue[,2])
		vif = max(vif(temp_reg))
	} else {
		vif = 1
	}
	
	F_stat =1-pf(result$fstatistic[1],result$fstatistic[2],result$fstatistic[3])
	
	# Calculate standardized coefficients
	betas = result$coef[,1]*sapply(temp_reg$model[-1],sd)/sapply(temp_reg$model[1],sd)
	Standardized[i,]=c(betas,  rep(NA, Max_var-length(betas)))
	
	# Calculate robust p-values
	# without the intercept 
	rob_stat = coeftest(temp_reg,vcov = vcovHAC)[,4,drop=FALSE]
	Robust_stat[i,1:length(rob_stat)]=rob_stat
	
	#Calculate additional statistics
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
	#DW_upper_stat = NA
	
	DW_lower_stat = dwtest(temp_reg,alternative = c("less"))$p.value
	#DW_lower_stat = NA
	
	bptest = BPTest(temp_reg,simpleoutput=FALSE)
	if (is.na(bptest)) {
		BP_stat = NA
	} else {
		BP_stat = bptest$p.value
	}
	#BP_stat = bptest(temp_reg)$p.value
	#BP_stat = NA
	
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
	########################Stationarity of Residuals
	op <- options(warn = (-1)) 
	### ADF test 
	stats_adf1 = AdfTest(residuals,simpleoutput=FALSE)$p.value
	stats_adf1_lag = NA
	
	stats_adf2_lag = NA
	stats_adf2 = NA
	
	### PP test  
	# PP test with Schwert's lag selection
	stats_pp = PPTest_tseries(residuals,simpleoutput=FALSE)$p.value
	stats_pp_lag = NA
	
	### KPSS test
	stats_kpss = KPSSTest_tseries(residuals,simpleoutput=FALSE)$p.value
	stats_kpss_lag = NA
	
	# DF-GLS test using Schwert's (not using)
	stats_dfgls = NA
	stats_dfgls_lag = NA
	
	# Engle-Granger using ADF's BIC lag selection (not using)
	stats_eg = NA	
	stats_eg_lag = NA
	
	# Phillips-Ouliaris test not used 
	stats_po = NA
	stats_po_lag = NA
	
	# Turn warnings back on
	options(op) 
	
	# Calculate fitted values
	#fit = temp_reg$model[,1]-temp_reg$residual
	fit = temp_reg$fitted.values
	
	fit_index = as.numeric(names(fit))
	
	fitted[i,fit_index] = fit

	# Store the results
	Reg_results[i,] = c(
			r2, r2adj, R2_ex_dummy, aic, bic, F_stat,
			length(residuals), Gap,
			vif,
			BG_stat, DW_upper_stat, DW_lower_stat,
			BP_stat, White_stat, 
			SW_stat, JB_stat,
			RESET_stat,
			stats_adf1,stats_pp,stats_kpss,NA)
	
	###################Regression Fit Output to Tex and PNG
	file_name <- paste(Output_forecast,"/",dep,"_RegressionResult.tex",sep="")
	writeFitToTex(file_name,temp_reg)
	##Diagnostic Test Outpuit
	reg_result <- round(c(r2, r2adj, as.vector(rob_stat), vif,
					BG_stat, DW_upper_stat, DW_lower_stat,
					BP_stat, White_stat, 
					SW_stat, JB_stat,
					stats_adf1,
					stats_pp,
					stats_kpss),2)
	
	threshold <- 0.05
	rob_flag <- rep("Not Robustly Significant",length(rob_stat))
	rob_flag[rob_stat<threshold] <- "Robustly Significant"
	
	if (BG_stat < threshold) {	
		bg_flag <- "Exhibit Serial Correlation"
	} else {
		bg_flag <- "Do not Exhibit Serial Correlation"
	}
	if (DW_upper_stat < threshold) {	
		dw_flag1 <- "Exhibit Positive AutoCorrelation"
	} else {
		dw_flag1 <- "Do not Exhibit Positive AutoCorrelation"
	}
	if (DW_lower_stat < threshold) {	
		dw_flag2 <- "Exhibit Negative AutoCorrelation"
	} else {
		dw_flag2 <- "Do not Exhibit Negative AutoCorrelation"
	}
	if (is.na(BP_stat)) {
		bp_flag <- "NA"
	} else {
		if (BP_stat < threshold) {	
			bp_flag <- "Exhibit Heteroscedasticity"
		} else {
			bp_flag <- "Do not Exhibit Heteroscedasticity"
		}
	}
	if (White_stat < threshold) {	
		white_flag <- "Exhibit Heteroscedasticity"
	} else {
		white_flag <- "Do not Exhibit Heteroscedasticity"
	}
	if (SW_stat < threshold) {	
		sw_flag <- "Residuals are not normally distributed"
	} else {
		sw_flag <- "Residuals are normally distributed"
	}
	if (JB_stat < threshold) {	
		jb_flag <- "Residuals are not normally distributed"
	} else {
		jb_flag <- "Residuals are normally distributed"
	}
	if (stats_adf1 < threshold) {	
		adf_flag <- "Residuals is Stationary"
	} else {
		adf_flag <- "Residuals is not Stationary"
	}
	if (stats_pp < threshold) {	
		pp_flag <- "Residuals is Stationary"
	} else {
		pp_flag <- "Residuals is not Stationary"
	}
	if (stats_kpss < threshold) {	
		kpss_flag <- "Residuals is not Stationary"
	} else {
		kpss_flag <- "Residuals is Stationary"
	}
	
	reg_flag <- c("","", rob_flag, "", 
			bg_flag,dw_flag1,dw_flag2,
			bp_flag,white_flag,
			sw_flag,jb_flag,
			adf_flag,
			pp_flag,
			kpss_flag)
	
	reg_result <- cbind(reg_result,reg_flag)
	
	rownames(reg_result) <- c("Rsquared","Rsquared_Adj",rownames(rob_stat),"VIF_Statistic",
			"BG_pvalue","DW1_pvalue","DW2_pvalue",
			"BP_pvalue","White_pvalue",
			"Shapiro_pvalue","JB_pvalue",
			"ADF_pvalue",
			"PP_pvalue",
			"KPSS_pvalue")
	colnames(reg_result) <- c("Statistic","Test Conclusion")
	file_name <- paste(Output_forecast,"/",dep,"_RegressionTestResult.tex",sep="")
	writeMatrixToTex(file_name,paste0(dep," Diagnostic Tests. Thershold ",threshold),as.data.frame(reg_result))
	
	#calculate fitted values
	ts_fitted <- fittedValueToTS(fit, data)
	#calculate forecast differenced value
	fcst_ts_all <- retrieveFcst(ind,Ind_var_fcst,temp_reg)
	ts_to_plot <- ts.union(dep_var,ts_fitted,fcst_ts_all)
	leg_names <- c("History","FittedValue",Scenario_Names)
	colnames(ts_to_plot) <- paste(dep,leg_names,sep="_");
#	colnames(ts_to_plot) <- leg_names
	file_name <- paste(Output_forecast,"/",dep,"_insample_diff.png",sep="")
	png(file_name, width=800, height=600)
	tsPlot(ts_to_plot,dep,leg_names,retrieveSubtitle(temp_reg),mark=TRUE)
	dev.off()
	
	file_name <- paste(Output_forecast,"/",dep,"_insample_diff.csv",sep="")
	write.csv(ts_to_plot, file_name)
	
	file_name <- paste(Output_forecast,"/",dep,"_insample_diff.tex",sep="")
	ts_to_tex <- ts_to_plot
	colnames(ts_to_tex) <- leg_names
	xtb <- xtable(ts_to_tex,caption="Numerical Results of Differenced Values")
#	align(xtb) <- "p{0.065\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}"
	print(xtb,file=file_name,tabular.environment = "longtable", floating=FALSE, include.rownames=TRUE)
	
	#save differenced values to MS ACCESS
	if (i == 1) {
		Fcst_All <- ts_to_plot
		Fcst_All_Names <- colnames(ts_to_plot)
	} else {
		Fcst_All <- ts.union(Fcst_All, ts_to_plot)
		Fcst_All_Names <- c(Fcst_All_Names,colnames(ts_to_plot))
	}
	
	#convert back to level value
	pc_level_SOP <- head(na.omit(pc_level),1)
	leg_names <- c(Scenario_Names,"FittedValue","History")
	levels <- diffToLevelMultipleTS(ts_to_plot,pc_level_SOP,leg_names,History_END_DATE)
	
	file_name <- paste(Output_forecast,"/",dep,"_insample_level.png",sep="")
	png(file_name, width=800, height=600)
	tsPlot(levels[,!grepl("FittedValue",colnames(levels))],dep,leg_names[!grepl("FittedValue",colnames(levels))],mark=TRUE)
	dev.off()
	
	file_name <- paste(Output_forecast,"/",dep,"_insample_level.csv",sep="")
	write.csv(levels,file_name)
	
	file_name <- paste(Output_forecast,"/",dep,"_insample_level.tex",sep="")
	levels_to_tex <- levels
	colnames(levels_to_tex) <- leg_names
	xtb <- xtable(as.data.frame(levels_to_tex,row.names=as.character(as.yearperiod(index(levels_to_tex)))),caption="Numerical Results of Level Values")
#	align(xtb) <- "p{0.065\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}"
	print(xtb,file=file_name,tabular.environment = "longtable", floating=FALSE, include.rownames=TRUE)
	
	if (i == 1) {
		Fcst_Level_All <- levels
		Fcst_Level_All_Names <- colnames(levels)
	} else {
		Fcst_Level_All <- ts.union(Fcst_Level_All, levels)
		Fcst_Level_All_Names <- c(Fcst_Level_All_Names,colnames(levels))
	}
	
	# Sensitivity Analysis: 1 and 2 times the standard deviation + adverse scenario + severely adverse scenarios
	sensitivity_std <- data.frame(matrix(rep(-1,length(Scenario_Names)*length(ind)),ncol=length(ind)))
	colnames(sensitivity_std) <- ind
	for (j in 1:length(ind)) {
		ind_fcst <- apply(expand.grid(ind[j], Scenario_Names),1,paste,collapse="")
		ind_var_fcst <- Ind_var_fcst[,ind_fcst,drop=FALSE]
#		sensitivity_std[,j] <- apply(ind_var_fcst*temp_reg$coefficients[ind[j]], 2, function(data){max(data,na.rm=TRUE)})
		max_move = ind_var_fcst*temp_reg$coefficients[ind[j]]
		sensitivity_std[,j] <- apply(apply(max_move,2,cumsum), 2, function(data){max(data,na.rm=TRUE)})
	}
	sensitivity_std = rbind(2*abs(temp_reg$coefficients*apply(data[,ind,drop=FALSE],2,sd_na_ignored)),sensitivity_std)
	sensitivity_std = rbind(abs(temp_reg$coefficients*apply(data[,ind,drop=FALSE],2,sd_na_ignored)),sensitivity_std)
	rownames(sensitivity_std) <- c("OneSD","TwoSD",Scenario_Names)
	
	for (j in 1:length(ind)) {
		file_name <- paste(Output_forecast,"/",dep,"_sensitivity_analysis_",j,".png",sep="")
		png(file_name, width=800, height=600)
		tsPlot(ts_to_plot,dep,leg_names,paste0("Sensitivity Analysis: ",ind[j]),mark=TRUE)
		worse_time = index(ts_to_plot)[which.max(ts_to_plot[,1])]
		points(rep(worse_time,nrow(sensitivity_std)),sensitivity_std[,j], pch=23, col = 1:nrow(sensitivity_std))
		text(rep(worse_time,nrow(sensitivity_std)),sensitivity_std[,j], paste(rownames(sensitivity_std),round(sensitivity_std[,j],1),sep=":"), pos=4, col="black")
		dev.off()
	}
#	file_name <- paste(Output_forecast,"/",dep,"_sensitivity_analysis.csv",sep="")
#	write.csv(sensitivity_std,file_name)
	sensitivity_var1_all[i,] = c(dep,str_split_fixed(separate_variable_name(colnames(sensitivity_std)[1]), "[|]", 3)[1:2], temp_reg$coefficients[1],t(sensitivity_std)[1,])
	if (length(ind)>1) {
		sensitivity_var2_all[i,] = as.character(c(dep,str_split_fixed(separate_variable_name(colnames(sensitivity_std)[2]), "[|]", 3)[1:2], temp_reg$coefficients[2],t(sensitivity_std)[2,]))
	} else {
		sensitivity_var2_all[i,] = as.character(c(dep,rep("",nrow(sensitivity_std)+3)))
	}
	
	#Residuals
	fcts_periods = length(fcst_ts_all[,1])
	path = 10000
	random_residuals = matrix(rnorm(path*fcts_periods,mean(temp_reg$residuals,na.rm=TRUE),sd(temp_reg$residuals,na.rm=TRUE)),nrow=path,ncol=fcts_periods)
	uncertainty = data.frame(matrix(rep(-1,2*fcts_periods),ncol=2))
	for (j in 1:ncol(random_residuals)) {
#		i = 3
		if (j == 1) {
			cum_residuals = random_residuals[,1:j]
		}else {
			cum_residuals = apply(random_residuals[,1:j],1,sum)
		}
		uncertainty[j,] = quantile(cum_residuals,c(0.25,0.75))
	}
	
	for (sce in 3:ncol(ts_to_plot)) {
#		sce = 7
		levels <- diffToLevelMultipleTS_WithSensitivity(ts_to_plot[,1],ts_to_plot[,sce],pc_level_SOP,c(Scenario_Names[sce-2],"Plus25thPerc","Plus75thPerc"),History_END_DATE,uncertainty)
		file_name <- paste(Output_forecast,"/",dep,"_sensitivity_",sce,".png",sep="")
		png(file_name, width=800, height=600)
		tsPlot(levels,dep=dep,leg_names=colnames(levels), subtitle=paste0("Sensitivity For ",colnames(ts_to_plot)[sce]),marks=TRUE)
		dev.off()
	}

	#contribution of each variables
	fcst_ts_all_variables <- retrieveFcstByVariable(ind,Ind_var_fcst,temp_reg)
	if (!is.na(fcst_ts_all_variables)) {
		file_name <- paste(Output_forecast,"/",dep,"_insample_level_by_variables.png",sep="")
		png(file_name, width=1600, height=600)
		par(mfrow=c(1,2))
		for (j in 1:length(fcst_ts_all_variables)) {
	#					i <- 1
			ts_to_plot <- ts.union(dep_var,ts_fitted,fcst_ts_all_variables[[j]])
			leg_names <- c("History","FittedValue",Scenario_Names)
			colnames(ts_to_plot) <- paste(dep,leg_names,sep="_");
			pc_level_SOP <- head(na.omit(pc_level),1)
			leg_names <- c(Scenario_Names,"FittedValue","History")
			levels <- diffToLevelMultipleTS(ts_to_plot,pc_level_SOP,leg_names,History_END_DATE)
			tsPlot(levels[,!grepl("FittedValue",colnames(levels))],dep,leg_names[!grepl("FittedValue",colnames(levels))],subtitle=names(fcst_ts_all_variables)[j],marks=TRUE)
		}
		dev.off()
	}
	
#	###insample performance, cumulative 9q performance across 3 periods, 2007, 2010, 2013
#	p1 <- 2007
#	pc_level_SOP <- pc_level[min(which(index(pc_level)>=p1&!is.na(pc_level)))]
#	leg_names <- c(Scenario_Names,"FittedValue","History")	
#	level1 <- diffToLevelMultipleTS(window(ts_to_plot,start=p1),pc_level_SOP,leg_names,History_END_DATE)
#	file_name <- paste(Output_forecast,"/",dep,"_insample_level_Period_",p1,".png",sep="")
#	png(file_name, width=800, height=600)	
#	tsPlot(level1,dep,leg_names)
#	dev.off()
#	
#	p1 <- 2010
#	pc_level_SOP <- pc_level[min(which(index(pc_level)>=p1&!is.na(pc_level)))]
#	leg_names <- c(Scenario_Names,"FittedValue","History")	
#	level2 <- diffToLevelMultipleTS(window(ts_to_plot,start=p1),pc_level_SOP,leg_names,History_END_DATE)
#	file_name <- paste(Output_forecast,"/",dep,"_insample_level_Period_",p1,".png",sep="")
#	png(file_name, width=800, height=600)	
#	tsPlot(level2,dep,leg_names)
#	dev.off()
#	
#	p1 <- 2013
#	pc_level_SOP <- pc_level[min(which(index(pc_level)>=p1&!is.na(pc_level)))]
#	leg_names <- c(Scenario_Names,"FittedValue","History")	
#	level3 <- diffToLevelMultipleTS(window(ts_to_plot,start=p1),pc_level_SOP,leg_names,History_END_DATE)
#	file_name <- paste(Output_forecast,"/",dep,"_insample_level_Period_",p1,".png",sep="")
#	png(file_name, width=800, height=600)	
#	tsPlot(level3,dep,leg_names)
#	dev.off()
}

colnames(sensitivity_var1_all) <- c("CLUSTER","IV1_ID","IV1_TRANS","IV1_COEFF",rownames(sensitivity_std))
colnames(sensitivity_var2_all) <- c("CLUSTER","IV2_ID","IV2_TRANS","IV2_COEFF",rownames(sensitivity_std))
file_name = paste(Output_forecast,"/Sensitivity_Var1.csv",sep="")
write.csv(sensitivity_var1_all,file_name)
file_name = paste(Output_forecast,"/Sensitivity_Var2.csv",sep="")
write.csv(sensitivity_var2_all,file_name)

file_name = paste(Output_forecast,"/ClustersAll.tex",sep="")
writeVectorToTex(file_name,"CLUSTERS",Model_Final$CLUSTER)

var1 = str_split_fixed(lapply(Model_Final$Var1,separate_variable_name), "[|]", 3)
colnames(var1) = c("IV1_ID","IV1_TRANS","IV1_LAG")
var2 = str_split_fixed(lapply(Model_Final$Var2,separate_variable_name), "[|]", 3)
colnames(var2) = c("IV2_ID","IV2_TRANS","IV2_LAG")
var3 = str_split_fixed(lapply(Model_Final$Var3,separate_variable_name), "[|]", 3)
colnames(var3) = c("IV3_ID","IV3_TRANS","IV3_LAG")
var_info = cbind(var1,var2,var3)

#rename column names
colnames(IV_coef) <- c("Intercept","Interceptpval","IV1_COEFF","IV1_PVAL","IV2_COEFF","IV2_PVAL")
colnames(Robust_stat) <- c("IV1_ROBPVAL","IV2_ROBPVAL")
colnames(Reg_results) <- c("R_SQ","ADJ_R_SQ","r2exdummy","AIC","BIC","F","NUM_OBS","Gap","MAX_VIF","BG_STAT","DW_UP_STAT","DW_LOW_STAT","BP_STAT","WHITE_STAT","SW_STAT","JB_STAT","RESET_STAT","ADF_PVAL","PP_PVAL","KPSS_PVAL","Empty")
#names(Model_Final)[names(Model_Final) == 'CLUSTER'] <- 'MOD_ID'
###
output = cbind(Model_Final, var_info, IV_coef,
		Standardized, Robust_stat,
		Reg_results,
		fitted)

file_name = paste(Output_forecast,"/Forecast-Statistics.csv",sep="")
write.csv(output,file = file_name)

table_name <- "O_MOD_RESULTS_FINAL"
output = cbind(Model_Final, var_info, IV_coef,
		Standardized, Robust_stat,
		Reg_results)
#deleteTable(dbc,table_name)
saveTable(dbc,table_name,output)

colnames(Fcst_All) <- Fcst_All_Names
time <- as.yearperiod(index(Fcst_All))
Fcst_All <- as.data.frame(Fcst_All)
rownames(Fcst_All) <- time

#table_name <- "O_MOD_FORECAST_DIFF"
#try(sqlDrop(dbc, table_name))
#sqlSave(dbc, Fcst_All, tablename = table_name, addPK=FALSE, safer = FALSE, rownames=TRUE)
#data <- Fcst_All
#saveTable(dbc,table_name,data)
file_name = paste(Output_forecast,"/Forecast-O_MOD_FORECAST_DIFF.csv",sep="")
write.csv(Fcst_All,file = file_name)

colnames(Fcst_Level_All) <- Fcst_Level_All_Names
time <- as.yearperiod(index(Fcst_Level_All))
Fcst_Level_All <- as.data.frame(Fcst_Level_All)
rownames(Fcst_Level_All) <- time
table_name <- "O_MOD_FORECAST_LEVEL"
Fcst_Level_All <- cbind(Date=rownames(Fcst_Level_All),Fcst_Level_All)

#data <- Fcst_Level_All
#saveTable(dbc,table_name,data)
#sqlDrop(dbc,table_name)
file_name = paste(Output_forecast,"/Forecast-O_MOD_FORECAST_LEVEL.csv",sep="")
write.csv(Fcst_Level_All,file = file_name)

file_name = paste(Output_forecast,"/Regression-Summary-Result.tex",sep="")
writeMatrixToTex(file_name,"Summary of Regression Results",cbind(Model_Final[,c("CLUSTER","Var1","Var2")], IV_coef[,c("IV1_COEFF","IV1_PVAL","IV2_COEFF","IV2_PVAL")]))

file_name = paste(Output_forecast,"/Regression-Summary-Diagnostics.tex",sep="")
writeMatrixToTex(file_name,"Summary of Disgnostic Results",cbind(Model_Final, Reg_results))

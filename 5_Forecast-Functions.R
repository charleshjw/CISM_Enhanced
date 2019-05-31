# 
# 
# Author: E620927
###############################################################################

library(forecast)
library(stringr)

clean_test_Model_Final <- function(Model_Final) {
#clean up the model specification table
	Model_Final <- characterizeTable(Model_Final)
	varcol <- grep("Var",colnames(Model_Final))
	delrow <- c()
	for (i in 1:nrow(Model_Final)) {
#	i <- 8
		if(all(Model_Final[i,varcol]%in%c("",NA))) {
			delrow <- c(delrow,i)
		}
	}
	if (length(delrow) > 0) {
		print("Warnings: some models are not specified variables to be predictors. Delete.")
		print(Model_Final[delrow,"CLUSTER"])
		Model_Final <- Model_Final[-delrow,]
	}
	
	variables <- Model_Final$Var1
	variables <- na.omit(variables)
	variables <- variables[variables!=""]
	if(!all(Model_Final$Var1%in%colnames(Ind_var))) {
		stop("ERROR: Variables1 are not specified correctly. Some are not in database")
	}
	
	variables <- Model_Final$Var2
	variables <- na.omit(variables)
	variables <- variables[variables!=""]
	if(!all(variables%in%colnames(Ind_var))) {
#	stop("ERROR: Variables2 are not specified correctly. Some are not in database")
		print("ERROR: Variables2 are not specified correctly. Some are not in database. Continue")
		nonexist <- variables[!variables%in%colnames(Ind_var)]
		print(nonexist)
		Model_Final <- Model_Final[!Model_Final$Var2%in%nonexist,]
	}
	variables <- Model_Final$Var3
	variables <- na.omit(variables)
	variables <- variables[variables!=""]
	if(!all(variables%in%colnames(Ind_var))) {
		stop("ERROR: Variables3 are not specified correctly. Some are not in database")
	}
	
	Model_Final = unique(Model_Final)
	return(Model_Final)
}

### separate variable names to three columns
separate_variable_name <- function (x) {
#	x <- model_info$Var2[1]
	x = gsub("eL0","e|0",x)
	x = gsub("eL1","e|1",x)
	x = gsub("QoQ","|QoQ",x)
	return(x)
}

fittedValueToTS <- function(fit, data){
	tmp <- na.omit(data)
	if ("mts"%in%class(data)) {
		if(length(tmp[,1])!=length(fit)){
			stop("ERROR: fit and data dimension not match")
		}
	} else if ("ts"%in%class(ts_to_plot[,sce])) {
		if(length(tmp)!=length(fit)){
			stop("ERROR: fit and data dimension not match")
		}
	}
	
	return(ts(fit,start=determineTsStartDate(as.Date(as.yearperiod(index(tmp)))),frequency=determineTsFrequency(as.Date(as.yearperiod(index(tmp))))))
}


#ts_to_plot = na.remove(backtest)
#dep = gsub("[.]","-",csi_id)
#leg_names = c("History","FittedValue")
#subtitle = retrieveSubtitle(temp_reg)
#marks = TRUE

tsPlot <- function(ts_to_plot,dep=NULL,leg_names=NULL,subtitle=NULL,marks=FALSE,color=NULL,...) {
#	ts_to_plot <- ts.union(csi_csim-csim_last_ob)
#	subtitle <- retrieveSubtitle(temp_reg)
	range <- as.yearperiod(round(range(index(na.omit(ts_to_plot[,1]))),2))
	if (is.null(color)) {
		color = 1:ncol(ts_to_plot)
	}
	
	par(xpd = T, mar = par()$mar + c(0,0,0,10))
	ts.plot(ts_to_plot, col=color,type="b",main=paste(dep,paste(range,collapse="-")),xlab=NULL,ylab="spread in basis points",gpars = list(...))
#	ts.plot(ts_to_plot, col=color,type="b",main=paste(dep,paste(range,collapse="-")))
	
	if (!is.null(leg_names))
#		legend("topleft",leg_names,col=1:ncol(ts_to_plot),pch=1)
		legend(par("usr")[2],par("usr")[4],leg_names,col=color,cex =0.7,...)
	if (!is.null(subtitle))
		title(sub=subtitle, font.sub =2 , cex.sub=1)
	par(mar=c(5, 4, 4, 2) + 0.1)
	
	if (marks) {
		
		if (min(time(ts_to_plot))<=History_END_DATE) {
			ts_to_plot_bf <- window(ts_to_plot,end=History_END_DATE)
			
			for (i in 1:ncol(ts_to_plot_bf)) {
	#			i <- 8
	#			if (length(ts_to_plot_bf[!is.na(ts_to_plot_bf[,i]),i]) <= 6) {
	#				next
	#			}
				maximum <- max(ts_to_plot_bf[,i],na.rm=TRUE)
	#			min(ts_to_plot[,i],na.rm=TRUE)
				date <- index(ts_to_plot_bf)[which(ts_to_plot_bf[,i]==maximum)[1]]
				text(date,maximum,round(maximum,1),cex=1.5)
			}
		}
		
		if(max(time(ts_to_plot)) > History_END_DATE+0.0001) {
			ts_to_plot_af <- window(ts_to_plot,start=History_END_DATE+0.0001)
			for (i in 1:ncol(ts_to_plot_af)) {
	#			i <- 7
				maximum <- max(ts_to_plot_af[,i],na.rm=TRUE)
	#			min(ts_to_plot[,i],na.rm=TRUE)
				date <- index(ts_to_plot_af)[which(ts_to_plot_af[,i]==maximum)[1]]
				text(date,maximum,round(maximum,1),cex=1.5)
			}
		}
	}
}

diffToLevelMultipleTS <- function(ts_to_plot,pc_level_SOP,leg_names,History_END_DATE) {
	history_data <- zoo(na.omit(window(ts_to_plot[,1],end=History_END_DATE)))
	for (j in 3:ncol(ts_to_plot)) {
#		j <- 3
		forecast_data <- zoo(na.omit(window(ts_to_plot[,j],start=History_END_DATE+0.0001)))
		level <- as.ts(c(history_data,forecast_data))
		
#		level <- as.ts(c(zoo(na.omit(ts_to_plot[,1])),zoo(na.omit(ts_to_plot[,j]))))
		
		if (j == 3) {
			levels <- diffToLevel(level,pc_level_SOP)
		} else {
			levels <- ts.union(levels, diffToLevel(level,pc_level_SOP))
		}
	}
	levels <- ts.union(levels,diffToLevel(na.omit(ts_to_plot[,2]),pc_level_SOP))
	levels <- ts.union(levels,diffToLevel(na.omit(ts_to_plot[,1]),pc_level_SOP))
	colnames(levels) <- paste(dep,leg_names,sep="_");
	return(levels)
}

diffToLevelMultipleTS_WithSensitivity <- function(history,forecast,pc_level_SOP,leg_names,History_END_DATE,uncertainty) {
	history_data <- zoo(na.omit(window(history,end=History_END_DATE)))
	forecast_data <- zoo(na.omit(window(forecast,start=History_END_DATE+0.0001)))
	forecast_data_level <- as.ts(c(history_data,forecast_data))
	forecast_data_level <- diffToLevel(forecast_data_level,pc_level_SOP)
	levels <- forecast_data_level
	
	for(j in 1:ncol(uncertainty)) {
#		j = 1
		history = window(forecast_data_level,end=History_END_DATE)
		forecast_data_shifted = window(forecast_data_level,start=History_END_DATE+0.0001)+fittedValueToTS(uncertainty[1:length(forecast_data),j],window(forecast,start=History_END_DATE+0.0001))
		level <- as.ts(c(history_data,forecast_data_shifted))
		levels <- ts.union(levels, as.ts(c(zoo(history),zoo(forecast_data_shifted))))
	}
	levels <- ts.union(levels,history)
	
	colnames(levels) <- c(paste(dep,leg_names,sep="_"),"history");
	return(levels)
}

retrieveFcst <- function(ind,Ind_var_fcst,temp_reg) {
	ind_fcst <- apply(expand.grid(ind, Scenario_Names),1,paste,collapse="")
	ind_var_fcst <- Ind_var_fcst[,ind_fcst[order(ind_fcst)],drop=FALSE]
	
	for (j in 1:length(Scenario_Names)) {
#		j <- 1
		scen <- Scenario_Names[j]
		tmp_colnames <- colnames(ind_var_fcst)[grep(scen,colnames(ind_var_fcst))]
		fcst_data <- ind_var_fcst[,tmp_colnames,drop=FALSE]
		colnames(fcst_data) <- gsub(scen,"",tmp_colnames)
		
		fcast <- forecast(temp_reg, newdata=as.data.frame(fcst_data))
		if (j == 1) {
			fcst_ts_all <- ts(fcast$mean,start=determineTsStartDate(as.Date(as.yearperiod(index(fcst_data)))),frequency=determineTsFrequency(as.Date(as.yearperiod(index(fcst_data)))))
		} else {
			fcst_ts_all <- ts.union(fcst_ts_all,ts(fcast$mean,start=determineTsStartDate(as.Date(as.yearperiod(index(fcst_data)))),frequency=determineTsFrequency(as.Date(as.yearperiod(index(fcst_data))))))
		}
	}
	colnames(fcst_ts_all) <- Scenario_Names
	
	return(fcst_ts_all)
}

retrieveFcstByVariable <- function(ind,Ind_var_fcst,temp_reg) {
	if (length(ind) == 1) {
		print("Warning: Single variable analysis. Do not need to run analysis");
		return(NA)
	}
	fcst_ts_all_variables <- list()
	for (i in 1:length(ind)) {
		ind_fcst <- apply(expand.grid(ind, Scenario_Names),1,paste,collapse="")
		ind_var_fcst <- Ind_var_fcst[,ind_fcst[order(ind_fcst)],drop=FALSE]
		ind_var_fcst[,!grepl(ind[i],colnames(ind_var_fcst))]=0
		
		for (j in 1:length(Scenario_Names)) {
			#		j <- 1
			scen <- Scenario_Names[j]
			tmp_colnames <- colnames(ind_var_fcst)[grep(scen,colnames(ind_var_fcst))]
			fcst_data <- ind_var_fcst[,tmp_colnames,drop=FALSE]
			colnames(fcst_data) <- gsub(scen,"",tmp_colnames)
			
			fcast <- forecast(temp_reg, newdata=as.data.frame(fcst_data))
			if (j == 1) {
				fcst_ts_all <- ts(fcast$mean,start=determineTsStartDate(as.Date(as.yearperiod(index(fcst_data)))),frequency=determineTsFrequency(as.Date(as.yearperiod(index(fcst_data)))))
			} else {
				fcst_ts_all <- ts.union(fcst_ts_all,ts(fcast$mean,start=determineTsStartDate(as.Date(as.yearperiod(index(fcst_data)))),frequency=determineTsFrequency(as.Date(as.yearperiod(index(fcst_data))))))
			}
		}
		colnames(fcst_ts_all) <- Scenario_Names
		fcst_ts_all_variables[[i]] <- fcst_ts_all
	}
	names(fcst_ts_all_variables) <- ind
	return(fcst_ts_all_variables)
}

retrieveSubtitle <- function(temp_reg) {
	tmp <- coef(temp_reg)
	return(paste("R^2: ",round(summary(temp_reg)$adj.r.squared,digits=2),"/",paste(paste(round(tmp,4),names(tmp),sep=" * "),collapse=" + "),sep=""))
}

outSampleTest <- function(dep_var, ind_var, OutSamplePeriod=9) {
	dep <- colnames(dep_var)
	data <- ts.union(dep_var,ind_var)
	data <- na.omit(data)
	colnames(data) <- c(colnames(dep_var),colnames(ind_var))
	
	formu <- as.formula(paste(colnames(dep_var), " ~ 0 + ", paste(colnames(ind_var),collapse=" + "),sep=""))
	
	time_segments <- OutSamplePeriods*0.25
	
	end_time <- max(time(data))-time_segments
	if (sum(end_time>time(data))<5) {
		next
	}
	data_in <- window(data,end=end_time)
	
	temp_reg <- lm(formu, data_in)
	
	if (any(is.na(coefficients(temp_reg)))) {
		return(9999)
	}
	
	if (end_time >= max(time(data))) {
		next
	}
	data_out <- window(data,start=end_time+0.001)
	fcast <- forecast(temp_reg, newdata=as.data.frame(data_out))
	outSample <- ts(fcast$mean,start=determineTsStartDate(as.Date(as.yearperiod(index(data_out)))),frequency=determineTsFrequency(as.Date(as.yearperiod(index(data_out)))))
	
#	ts_to_plot <- ts.union(dep_var, ts_fitted, outSample)
#	leg_names <- c("History", "FittedValue", "OutSample")
#	colnames(ts_to_plot) <- paste(dep,leg_names,sep="_")
#	tsPlot(ts_to_plot,dep,leg_names,retrieveSubtitle(temp_reg))
#	abline(v=end_time+0.1)
	
	dep_out <- window(dep_var,start=end_time+0.001)
	error = outSample-dep_out
	return(sqrt(mean((error)^2)))
}

outSampleTesting <- function(dep_var, ind_var, Output_OutSample, filenameseg = "", OutSamplePeriods=0:9, OUTPUT_VERBOSE = TRUE) {
	dep <- colnames(dep_var)
	data <- ts.union(dep_var,ind_var)
	data <- na.omit(data)
	colnames(data) <- c(colnames(dep_var),colnames(ind_var))
	
	formu <- as.formula(paste(colnames(dep_var), " ~ 0 + ", paste(colnames(ind_var),collapse=" + "),sep=""))
	
	time_segments <- OutSamplePeriods[order(OutSamplePeriods,decreasing=TRUE)]*0.25
	coeff <- matrix(rep(NA,(3+ncol(ind_var))*length(time_segments)),ncol=(ncol(ind_var)+3))
	for (i in 1:length(time_segments)) {
#		i <- 1
		end_time <- max(time(data))-time_segments[i]
		if (sum(end_time>time(data))<5) {
			#if time window is too short, skip
			next
		}
		data_in <- window(data,end=end_time)
		temp_reg <- lm(formu, data_in)
		if (any(is.na(coefficients(temp_reg)))) {
			return(9999)
		}
		
		result <- summary(temp_reg)
		fit <- temp_reg$fitted.values
		ts_fitted <- fittedValueToTS(fit, data_in)
		coeff[i,] <- c(as.character(as.yearperiod(range(index(data_in)))),as.vector(temp_reg$coefficients),result$adj.r.squared)

		if (end_time >= max(time(data))) {
			next
		}
		data_out <- window(data,start=end_time+0.001)
		fcast <- forecast(temp_reg, newdata=as.data.frame(data_out))
		outSample <- ts(fcast$mean,start=determineTsStartDate(as.Date(as.yearperiod(index(data_out)))),frequency=determineTsFrequency(as.Date(as.yearperiod(index(data_out)))))
		
		ts_to_plot <- ts.union(dep_var, ts_fitted, outSample)
		leg_names <- c("History", "FittedValue", "OutSample")
		colnames(ts_to_plot) <- paste(dep,leg_names,sep="_")
		file_name <- paste(Output_OutSample,"/",dep,filenameseg,"-Recent-",OutSamplePeriods[i],".png",sep="")
		png(file_name,width=800,height=600)
#		ts_to_plot_tmp <- ts_to_plot[! apply( ts_to_plot , 1 , function(x) all(is.na(x)) ) ,]
		tsPlot(ts_to_plot,dep,leg_names,retrieveSubtitle(temp_reg))
		abline(v=end_time+0.1)
		dev.off()
	}
	
	if (OUTPUT_VERBOSE) {
#		coeff <- coeff[order(coeff[,2]),]
		colnames(coeff) <- c("Start","End",names(temp_reg$coefficient),"R2adj")
		file_name <- paste(Output_OutSample,"/",dep,filenameseg,"-ImpactOfRecentPeriods.csv",sep="")
		write.csv(coeff,file_name)
	
		file_name <- paste(Output_OutSample,"/",dep,filenameseg,"-ImpactOfRecentPeriods.png",sep="")
		png(file_name,height=400,width=500*(ncol(coeff)-2))
#		if (ncol(ind_var) == 1) {
#			plot(coeff[,3],type="b",ylab=colnames(coeff)[3],main=colnames(coeff)[3],xlab="Most Recent Periods Excluded",xaxt = "n")
#			axis(1, at=1:nrow(coeff),coeff[,2])
#		} else {
		par(mfrow=c(1,(ncol(coeff)-2)))
		for (i in 3:ncol(coeff)) {
			plot(coeff[,i],type="b",ylab=colnames(coeff)[i],main=colnames(coeff)[i],xlab="Most Recent Periods Excluded",xaxt = "n")
			axis(1, at=1:nrow(coeff),coeff[,2])
		}
#		}
		dev.off()
	}
	
	
	time_segments <- OutSamplePeriods*0.25
	coeff <- matrix(rep(NA,(3+ncol(ind_var))*length(time_segments)),ncol=(ncol(ind_var)+3))
	for (i in 1:length(time_segments)) {
#		i <- 2
		start_time <- min(time(data))+time_segments[i]
		if (sum(start_time<time(data))<5) {
			next
		}
		
		data_in <- window(data,start=start_time)
		temp_reg <- lm(formu, data_in)
		result <- summary(temp_reg)
		fit <- temp_reg$fitted.values
		ts_fitted <- fittedValueToTS(fit, data_in)
		coeff[i,] <- c(as.character(as.yearperiod(range(index(data_in)))),as.vector(temp_reg$coefficients),result$adj.r.squared)
		
		if (start_time <= min(time(data))) {
			next
		}
		data_out <- window(data,end=start_time-0.001)
		fcast <- forecast(temp_reg, newdata=as.data.frame(data_out))
		outSample <- ts(fcast$mean,start=determineTsStartDate(as.Date(as.yearperiod(index(data_out)))),frequency=determineTsFrequency(as.Date(as.yearperiod(index(data_out)))))
		
		ts_to_plot <- ts.union(dep_var, ts_fitted, outSample)
		leg_names <- c("History", "FittedValue", "OutSample")
		colnames(ts_to_plot) <- paste(dep,leg_names,sep="_")
		
		file_name <- paste(Output_OutSample,"/",dep,filenameseg,"-Acient-",OutSamplePeriods[i],".png",sep="")
		png(file_name,width=800,height=600)
#			ts_to_plot_tmp <- ts_to_plot[! apply( ts_to_plot , 1 , function(x) all(is.na(x)) ) ,]
		tsPlot(ts_to_plot,dep,leg_names,retrieveSubtitle(temp_reg))
		abline(v=start_time-0.1)
		dev.off()
	}
	if (OUTPUT_VERBOSE) {
		colnames(coeff) <- c("Start","End",names(temp_reg$coefficient),"R2adj")
		file_name <- paste(Output_OutSample,"/",dep,filenameseg,"-ImpactOfAcientPeriods.csv",sep="")
		write.csv(coeff,file_name)
		
		file_name <- paste(Output_OutSample,"/",dep,filenameseg,"-ImpactOfAcientPeriods.png",sep="")
		png(file_name,height=400,width=500*(ncol(coeff)-2))
#		if (ncol(ind_var) == 1) {
#			plot(coeff[,3],type="b",ylab=colnames(coeff)[3],main=colnames(coeff)[3],xlab="Most Acient Periods Excluded",xaxt = "n")
#			axis(1, at=1:nrow(coeff),coeff[,1])
#		} else {
#			par(mfrow=c(1,(ncol(coeff)-2)))
#			for (i in 3:ncol(coeff)) {
#				plot(coeff[,i],type="b",ylab=colnames(coeff)[i],main=colnames(coeff)[i],xlab="Most Acient Periods Excluded",xaxt = "n")
#				axis(1, at=1:nrow(coeff),coeff[,1])
#			}
#		}
		par(mfrow=c(1,(ncol(coeff)-2)))
		for (i in 3:ncol(coeff)) {
			plot(coeff[,i],type="b",ylab=colnames(coeff)[i],main=colnames(coeff)[i],xlab="Most Acient Periods Excluded",xaxt = "n")
			axis(1, at=1:nrow(coeff),coeff[,1])
		}
		
		dev.off()
	}

}

CSIForecast <- function(dep_var, ind_var, Output_forecast, MULTIPLIER) {
	dep <- colnames(dep_var)
	ind <- colnames(ind_var)
	
	multipliers <- MULTIPLIER[MULTIPLIER$MOD_ID==dep,]
	if (nrow(multipliers) == 0) {
		# if cannot find multiplier, search CSI ID
		multipliers <- MULTIPLIER[MULTIPLIER$CSI_ID==dep,]
	}
	if (nrow(multipliers) == 0) {
		stop("CSIForecast: cannot find multiplier by searching MOD_ID and CSI_ID. Check")
	}
	
	data <- ts.union(dep_var,ind_var)
	data <- na.omit(data)
	colnames(data) <- c(colnames(dep_var),colnames(ind_var))
	
	formu <- as.formula(paste(colnames(dep_var), " ~ 0 + ", paste(colnames(ind_var),collapse=" + "),sep=""))
	
	temp_reg <- lm(formu, data)
	fit <- temp_reg$fitted.values
	ts_fitted <- fittedValueToTS(fit, data)
	fcst_ts_all <- retrieveFcst(ind,Ind_var_fcst,temp_reg)
	ts_to_plot <- ts.union(dep_var,ts_fitted,fcst_ts_all)
	leg_names <- c("History","FittedValue",Scenario_Names)
	colnames(ts_to_plot) <- paste(dep,leg_names,sep="_");
	
	for (i in 1:nrow(multipliers)) {
	#		i <- 1
		pc_level_SOP <- head(na.omit(CSI_CURVES[[as.character(multipliers$CSI_ID[i])]]),1)
		pc_level_EOP <- tail(na.omit(CSI_CURVES[[as.character(multipliers$CSI_ID[i])]]),1)
		multiplier <- multipliers$MULTIPLIER[i]
		csi_id <- multipliers$CSI_ID[i]
		
		#backtest on differenced values
		backtest <- ts_to_plot[,1:2]
		file_name <- paste(Output_forecast,"/",gsub("[.]","-",csi_id),"_backtest_diff.png",sep="")
		png(file_name,width=800,height=600)
		tsPlot(na.remove(backtest),gsub("[.]","-",csi_id),c("History","FittedValue"),retrieveSubtitle(temp_reg),mark=FALSE)
		dev.off()
		
		#backtest on level values
		level <- na.omit(CSI_CURVES[[as.character(multipliers$CSI_ID[i])]])						#column1: history
		level <- ts.union(level,diffToLevel(na.omit(ts_to_plot[,2]*multiplier),pc_level_SOP))	#column2: fitted value
		for (j in 3:ncol(ts_to_plot)) {															#column3-N: forecast for each scenario
	#			j <- 3
			fcst <- diffToLevel(na.omit(window(ts_to_plot[,j],start=History_END_DATE+0.0001)*multiplier),pc_level_EOP)
			level <- ts.union(level,fcst)
		}
		leg_names <- c("History","FittedValue",Scenario_Names)
		colnames(level) <- leg_names
		
		backtest <- level[,c("History","FittedValue")]
		file_name <- paste(Output_forecast,"/",gsub("[.]","-",csi_id),"_backtest_level.png",sep="")
		png(file_name,width=800,height=600)
		tsPlot(na.remove(backtest),gsub("[.]","-",csi_id),c("History","FittedValue"),retrieveSubtitle(temp_reg),mark=TRUE)
		dev.off()
		
		level = level[,!grepl("FittedValue",colnames(level))]
		if (OUTPUT_VERBOSE) {
			file_name <- paste(Output_forecast,"/",gsub("[.]","-",csi_id),"_insample_level.png",sep="")
			png(file_name,width=800,height=600)
			tsPlot(level[,!grepl("FittedValue",colnames(level))],gsub("[.]","-",csi_id),leg_names[!grepl("FittedValue",leg_names)],retrieveSubtitle(temp_reg),mark=TRUE)
			dev.off()
			
			file_name <- paste(Output_forecast,"/",gsub("[.]","-",csi_id),"_insample_level.csv",sep="")
			write.csv(level,file_name)
			
			file_name <- paste(Output_forecast,"/",gsub("[.]","-",csi_id),"_insample_level.tex",sep="")
			xtb <- xtable(as.data.frame(level,row.names=as.character(as.yearperiod(index(level)))),caption="Numerical Results of Level Values")
	#			align(xtb) <- "p{0.065\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}"
			print(xtb,file=file_name,tabular.environment = "longtable", floating=FALSE, include.rownames=TRUE)
		}
		
		#Floored level forecast
		min_history <- min(na.omit(CSI_CURVES[[as.character(multipliers$CSI_ID[i])]]))
		k <- 1;
		for (j in 3:ncol(ts_to_plot)) {
	#			j <- 3
			fcst <- na.omit(window(ts_to_plot[,j]*multiplier,start=History_END_DATE+0.0001))
			quarter <- gsub(" ","",as.yearqtr(time(fcst)))
			month <- gsub(" ","",as.yearmon(time(fcst)))
			fcst_date <- ts_time_to_date(fcst)
			base_date <- convertNumericToDate(Calibration_END_DATE)
			
			ac_id <- as.character(multipliers[i,]$AC_ID)
			mod_id <- as.character(multipliers[i,]$MOD_ID)
			csi_id <- as.character(multipliers[i,]$CSI_ID)
			
			scen_id <- Scenario_Names[k]
			k <- k+1
			
			level <- as.matrix(diffToLevel(na.omit(window(ts_to_plot[,j],start=History_END_DATE+0.0001)*multiplier),pc_level_EOP))
			if (grepl("MUNI",dep)) {
				#Muni does not have to be floored by historical low
				floored_level <- as.matrix(diffToLevel(fcst,pc_level_EOP))
			} else {
				floored_level <- as.matrix(diffToLevel(fcst,pc_level_EOP,min_history))
			}
			starting_point <-c(pc_level_EOP,rep(NA,length(fcst_date)-1))
			min_h <- c(min_history,rep(NA,length(fcst_date)-1))

			if (i==1&j==3) {
				result <- data.frame(FCST_DATE=fcst_date,BASE_DATE=base_date,AC_ID=ac_id,QUARTER=quarter,MONTH=month,SCEN_ID=scen_id,CSI_ID=csi_id,MOD_ID=mod_id,CSI_FCST=as.matrix(fcst),STARTINGPOINT=starting_point,LEVEL=level,HISTORICALMINIMUM=min_h,FLOOREDLEVEL=floored_level)
			} else {
				result <- rbind(result,data.frame(FCST_DATE=fcst_date,BASE_DATE=base_date,AC_ID=ac_id,QUARTER=quarter,MONTH=month,SCEN_ID=scen_id,CSI_ID=csi_id,MOD_ID=mod_id,CSI_FCST=as.matrix(fcst),STARTINGPOINT=starting_point,LEVEL=level,HISTORICALMINIMUM=min_h,FLOOREDLEVEL=floored_level))
			}
		}
	}
	return(result)
}

CSIForecast_With_PriorCalibration <- function(dep_var, ind_var,prior_calibration, Output_forecast, MULTIPLIER) {
#	MULTIPLIER = MULTIPLIER_Prior
	dep <- colnames(dep_var)
	ind <- colnames(ind_var)
	multipliers <- MULTIPLIER[MULTIPLIER$MOD_ID==dep,]
	
	data <- ts.union(dep_var,ind_var)
	data <- na.omit(data)
	colnames(data) <- c(colnames(dep_var),colnames(ind_var))
	
	formu <- as.formula(paste(colnames(dep_var), " ~ 0 + ", paste(colnames(ind_var),collapse=" + "),sep=""))
	
	temp_reg <- lm(formu, data)
	
	for (i in 1:ncol(prior_calibration)) {
		temp_reg$coefficients[colnames(prior_calibration)[i]] <- as.numeric(prior_calibration[1,i])
	}

	fit <- temp_reg$fitted.values
	ts_fitted <- fittedValueToTS(fit, data)
	fcst_ts_all <- retrieveFcst(ind,Ind_var_fcst,temp_reg)
	ts_to_plot <- ts.union(dep_var,ts_fitted,fcst_ts_all)
	leg_names <- c("History","FittedValue",Scenario_Names)
	colnames(ts_to_plot) <- paste(dep,leg_names,sep="_");
	
	for (i in 1:nrow(multipliers)) {
		#		i <- 1
		pc_level_SOP <- head(na.omit(CSI_CURVES[[as.character(multipliers$CSI_ID[i])]]),1)
		pc_level_EOP <- tail(na.omit(CSI_CURVES[[as.character(multipliers$CSI_ID[i])]]),1)
		multiplier <- multipliers$MULTIPLIER[i]
		csi_id <- multipliers$CSI_ID[i]
		
		level <- na.omit(CSI_CURVES[[as.character(multipliers$CSI_ID[i])]])						#column1: history
		level <- ts.union(level,diffToLevel(na.omit(ts_to_plot[,2]*multiplier),pc_level_SOP))	#column2: fitted value
		for (j in 3:ncol(ts_to_plot)) {															#column3-N: forecast for each scenario
			#			j <- 5
			fcst <- diffToLevel(na.omit(window(ts_to_plot[,j],start=History_END_DATE+0.0001)*multiplier),pc_level_EOP)
			level <- ts.union(level,fcst)
		}
		leg_names <- c("History","FittedValue",Scenario_Names)
		colnames(level) <- leg_names
		level = level[,!grepl("FittedValue",colnames(level))]
		
		if (OUTPUT_VERBOSE) {
			file_name <- paste(Output_forecast,"/",gsub("[.]","-",csi_id),"_insample_level.png",sep="")
			png(file_name,width=800,height=600)
			tsPlot(level[,!grepl("FittedValue",colnames(level))],gsub("[.]","-",csi_id),leg_names[!grepl("FittedValue",leg_names)],retrieveSubtitle(temp_reg),mark=TRUE)
			dev.off()
			
			file_name <- paste(Output_forecast,"/",gsub("[.]","-",csi_id),"_insample_level.csv",sep="")
			write.csv(level,file_name)
			
			file_name <- paste(Output_forecast,"/",gsub("[.]","-",csi_id),"_insample_level.tex",sep="")
			xtb <- xtable(as.data.frame(level,row.names=as.character(as.yearperiod(index(level)))),caption="Numerical Results of Level Values")
			#			align(xtb) <- "p{0.065\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}|p{0.14\\textwidth}"
			print(xtb,file=file_name,tabular.environment = "longtable", floating=FALSE, include.rownames=TRUE)
		}
		
		#Floored level forecast
		min_history <- min(na.omit(CSI_CURVES[[as.character(multipliers$CSI_ID[i])]]))
		k <- 1;
		for (j in 3:ncol(ts_to_plot)) {
			#			j <- 3
			fcst <- na.omit(window(ts_to_plot[,j]*multiplier,start=History_END_DATE+0.0001))
			quarter <- gsub(" ","",as.yearqtr(time(fcst)))
			month <- gsub(" ","",as.yearmon(time(fcst)))
			fcst_date <- ts_time_to_date(fcst)
			base_date <- convertNumericToDate(Calibration_END_DATE)
			
			ac_id <- as.character(multipliers[i,]$AC_ID)
			mod_id <- as.character(multipliers[i,]$MOD_ID)
			csi_id <- as.character(multipliers[i,]$CSI_ID)
			
			scen_id <- Scenario_Names[k]
			k <- k+1
			
			level <- as.matrix(diffToLevel(na.omit(window(ts_to_plot[,j],start=History_END_DATE+0.0001)*multiplier),pc_level_EOP))
			if (grepl("MUNI",dep)) {
				#Muni does not have to be floored by historical low
				floored_level <- as.matrix(diffToLevel(fcst,pc_level_EOP))
			} else {
				floored_level <- as.matrix(diffToLevel(fcst,pc_level_EOP,min_history))
			}
			starting_point <-c(pc_level_EOP,rep(NA,length(fcst_date)-1))
			min_h <- c(min_history,rep(NA,length(fcst_date)-1))
			
			if (i==1&j==3) {
				result <- data.frame(FCST_DATE=fcst_date,BASE_DATE=base_date,AC_ID=ac_id,QUARTER=quarter,MONTH=month,SCEN_ID=scen_id,CSI_ID=csi_id,MOD_ID=mod_id,CSI_FCST=as.matrix(fcst),STARTINGPOINT=starting_point,LEVEL=level,HISTORICALMINIMUM=min_h,FLOOREDLEVEL=floored_level)
			} else {
				result <- rbind(result,data.frame(FCST_DATE=fcst_date,BASE_DATE=base_date,AC_ID=ac_id,QUARTER=quarter,MONTH=month,SCEN_ID=scen_id,CSI_ID=csi_id,MOD_ID=mod_id,CSI_FCST=as.matrix(fcst),STARTINGPOINT=starting_point,LEVEL=level,HISTORICALMINIMUM=min_h,FLOOREDLEVEL=floored_level))
			}
		}
	}
	return(result)
}

CSIForecast_reformated <- function(dep_var, ind_var, Output_forecast, MULTIPLIER) {
	dep <- colnames(dep_var)
	ind <- colnames(ind_var)
	multipliers <- MULTIPLIER[MULTIPLIER$MOD_ID==dep,]
	
	data <- ts.union(dep_var,ind_var)
	data <- na.omit(data)
	colnames(data) <- c(colnames(dep_var),colnames(ind_var))
	
	formu <- as.formula(paste(colnames(dep_var), " ~ 0 + ", paste(colnames(ind_var),collapse=" + "),sep=""))
	
	temp_reg <- lm(formu, data)
	fit <- temp_reg$fitted.values
	ts_fitted <- fittedValueToTS(fit, data)
	fcst_ts_all <- retrieveFcst(ind,Ind_var_fcst,temp_reg)
	ts_to_plot <- ts.union(dep_var,ts_fitted,fcst_ts_all)
	leg_names <- c("History","FittedValue",Scenario_Names)
	colnames(ts_to_plot) <- paste(dep,leg_names,sep="_");
	result <- data.frame(matrix(nrow=0,ncol=12))
	result_discoutning <- data.frame(matrix(nrow=0,ncol=6))
	k <- 1;
	for (i in 1:nrow(multipliers)) {
		#		i <- 1
#		pc_level_SOP <- head(na.omit(CSI_CURVES[[as.character(multipliers$CSI_ID[i])]]),1)
		pc_level_EOP <- tail(na.omit(CSI_CURVES[[as.character(multipliers$CSI_ID[i])]]),1)
		multiplier <- multipliers$MULTIPLIER[i]
		csi_id <- multipliers$CSI_ID[i]

		#Floored level forecast
		min_history <- min(na.omit(CSI_CURVES[[as.character(multipliers$CSI_ID[i])]]))
		for (j in 3:ncol(ts_to_plot)) {
			#			j <- 3
			fcst <- na.omit(window(ts_to_plot[,j]*multiplier,start=History_END_DATE+0.0001))
			quarter <- gsub(" ","",as.yearqtr(time(fcst)))
			month <- gsub(" ","",as.yearmon(time(fcst)))
			fcst_date <- ts_time_to_date(fcst)
			base_date <- convertNumericToDate(Calibration_END_DATE)
			
			ac_id <- as.character(multipliers[i,]$AC_ID)
			mod_id <- as.character(multipliers[i,]$MOD_ID)
			csi_id <- as.character(multipliers[i,]$CSI_ID)
			assetclass <- as.character(multipliers[i,]$AssetClass)
			scen_id <- Scenario_Names[j-2]
			
			level <- as.matrix(diffToLevel(na.omit(window(ts_to_plot[,j],start=History_END_DATE+0.0001)*multiplier),pc_level_EOP))
			if (grepl("MUNI",dep)) {
				#Muni does not have to be floored by historical low
				floored_level <- diffToLevel(fcst,pc_level_EOP)
				result_discoutning[k,] <- c(csi_id,assetclass,mod_id,multiplier,pc_level_EOP,-9999)
			} else {
				floored_level <- diffToLevel(fcst,pc_level_EOP,min_history)
				result_discoutning[k,] <- c(csi_id,assetclass,mod_id,multiplier,pc_level_EOP,min_history)
			}
			
			result[k,] <- c(scen_id,csi_id,pc_level_EOP,floored_level)	
			k <- k+1
		}
	}
	return(list(result,result_discoutning))
}










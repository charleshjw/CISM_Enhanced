# 
# 
# Author: E620927
###############################################################################

Output_forecast <- file.path(Output_root,"Forecast-CSI")
if (!dir.exists(Output_forecast)) {
	dir.create(Output_forecast)
}

MULTIPLIER <- fetchTable(dbc, "O_MULTIPLIER_MONTHLY_ALL",version=current_version)
MULTIPLIER$MOD_ID <- cleanString(MULTIPLIER$MOD_ID)

for (i in 1:nrow(Model_Final)) {
#	i <- 2
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

file_name <- paste(Output_forecast,paste0("/CSI_FCST_",current_version,".xlsx"),sep="")
write.xlsx(result,file_name,row.names=FALSE)

result <- data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),result)
table_name <- "O_CSI_FCST_MONTHLY";
for (j in 1:ncol(result)) {
	result[,j] <- as.character(result[,j])
}
saveTable(dbc,table_name,result)

file_name = paste(Output_forecast,"/CSIAll.tex",sep="")
writeVectorToTex(file_name,"CSI",unique(result$CSI_ID))



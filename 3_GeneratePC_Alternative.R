# First decide the clustering of CSIs. Read decisions from table I_MOD_CSI_AC.
# Run PCA analysis.
# 
# Author: E620927
###############################################################################

MULTIPLIER <- data.frame()

for (modelid_column in modelid_columns) {
#	modelid_column <- modelid_columns[1]
	Output_PCA <- file.path(Output_root,paste0("PCA_",modelid_column))
	if (!dir.exists(Output_PCA)) {
		dir.create(Output_PCA)
	}
	
	M_CSI <- fetchTable(dbc,"I_MOD_CSI_AC",current_version)
	M_CSI <- M_CSI[!is.na(M_CSI[,modelid_column]),]
	
	if (!all(M_CSI$CSI_ID%in%names(CSI_CURVES))) {
		stop("Stop some credit spread indices defined is not sourced. Check")		
		print(M_CSI$CSI_ID[!M_CSI$CSI_ID%in%names(CSI_CURVES)])
	}
	
	MODEL_ID <- na.omit(unique(M_CSI[,modelid_column]))
	CSI_Eigenvectors <- data.frame()
	PC1TS <- data.frame()
	PC1LEVEL <- data.frame()
	model_ids <- c()
	for (i in 1:length(MODEL_ID)) {
#	i = 50
		print(i)
		model <- MODEL_ID[i]
		csi <- M_CSI[M_CSI[,modelid_column]==model,]$CSI_ID
		ac_id <- M_CSI[M_CSI[,modelid_column]==model,]$AC_ID[1]
		curves_mod <- combineCurves(selectCurves(CSI_CURVES, csi))
		first_diff_ts <- diff(curves_mod)
		
		subgroup_no_pca <- prcomp(na.omit(first_diff_ts),center = FALSE,scale = FALSE)
		
		total = sum(subgroup_no_pca$sdev^2)  # total variance 
		var_pc1 = (subgroup_no_pca$sdev[1])^2     
		percent = var_pc1/total
		
		PCA_rotation <- subgroup_no_pca$rotation[,1,drop=FALSE]
		PC1_no_series <- subgroup_no_pca$x[,1,drop=FALSE]
		
		if (all(PCA_rotation<0)) {
			PCA_rotation <- -PCA_rotation
			PC1_no_series <- -PC1_no_series
		}
		
		PC1_no_series <- ts(PC1_no_series,start=determineTsStartDate(as.yearperiod(index(na.omit(first_diff_ts)))),frequency=determineTsFrequency(as.yearperiod(index(na.omit(first_diff_ts)))))
		
		pc1_ts <- PC1_no_series/sum(PCA_rotation)
		pc1_frame <- data.frame(PERIOD=as.yearperiod(index(na.omit(first_diff_ts))),AC_ID=ac_id,MOD_ID=model,PC1=pc1_ts,row.names=NULL)
		PC1TS <- rbind(PC1TS, pc1_frame)
		
		multiplier <- PCA_rotation*sum(PCA_rotation)
		rotation <- data.frame(PCA_rotation, multiplier)
		rownames(rotation) <- csi
		colnames(rotation) <- c("EIGENVECTOR","MULTIPLIER")
		
		CSI_Eigenvectors <- rbind(CSI_Eigenvectors,rotation)
		
		pca_level <- ts(curves_mod%*%PCA_rotation/sum(PCA_rotation),start=determineTsStartDate(as.Date(as.yearperiod(index(curves_mod)))),frequency=determineTsFrequency(as.Date(as.yearperiod(index(curves_mod)))))
		pc1level <- data.frame(PERIOD=as.yearperiod(index(curves_mod)),AC_ID=ac_id,MOD_ID=model,PC1=pca_level,row.names=NULL)
		PC1LEVEL <- rbind(PC1LEVEL, pc1level)
		
		model <- cleanString(model)
		model_ids <- c(model_ids,model)
		file_name <- paste(Output_PCA,"/",model,"_leveldata.png",sep="")
		png(file_name,width=600,height=400)
		ts.plot(curves_mod,ylab="spread",main=paste0("MOD_ID: ",model, " - CSI level"), col=1:length(csi),type="b")
		legend("topleft",legend=csi,col=1:length(csi),pch=1)
		dev.off()
		
		file_name <- paste(Output_PCA,"/",model,"_diffdata.csv",sep="")
		write.csv(first_diff_ts,file_name)
		
		file_name <- paste(Output_PCA,"/",model,"_diffdata.png",sep="")
		png(file_name,width=600,height=400)
		ts.plot(first_diff_ts,ylab="differenced spread",main=paste0("MOD_ID: ",model," - CSI first differenced"), col=1:length(csi),type="b")
		legend("topleft",legend=csi,col=1:length(csi),pch=1)
		dev.off()
		
		file_name <- paste(Output_PCA,"/",model,"_pc1.csv",sep="")
		write.csv(pc1_frame,file_name)
		
		file_name <- paste(Output_PCA,"/",model,"_pc1data.png",sep="")
		png(file_name,width=600,height=400)
		ts.plot(pc1_ts,ylab="differenced spread",main=paste0("MOD_ID: ",model," - PC1 first differenced"),type="b",sub=paste0("Total Variance Explained By PC1 is ",formatPercentages(round(percent,3))))
		dev.off()
		
		file_name <- paste(Output_PCA,"/",model,"_pc1level.csv",sep="")
		write.csv(pc1level,file_name)
		
		file_name <- paste(Output_PCA,"/",model,"_pc1level.png",sep="")
		png(file_name,width=600,height=400)
		ts.plot(pca_level,ylab="spread",main=paste0("MOD_ID: ",model," - PC1 level"),type="b")
		dev.off()
		
		file_name <- paste(Output_PCA,"/",model,"_RotationAndMultplier.csv",sep="")
		write.csv(rotation,file_name)
		
		file_name <- paste(Output_PCA,"/",model,"_PCAOutput.tex",sep="")
		print(xtable(rotation,caption=paste0("PCA Output: ",model)),file=file_name, floating=FALSE, include.rownames=TRUE)
		
		ts_tmp <- na.omit(ts.union(first_diff_ts,pc1_ts))
		colnames(ts_tmp) <- c(as.character(csi),"pc1")
		cor <- round(cor(ts_tmp,method="kendall"),2)
		file_name <- paste(Output_PCA,"/",model,"_RetainedCorrelation_kendall.png",sep="")
		png(file_name,width=900)
		grid.arrange(tableGrob(cor),top=textGrob("Kendall Correlation"))
		dev.off()
		
		file_name <- paste(Output_PCA,"/",model,"_RetainedCorrelation_spearman.png",sep="")
		cor <- round(cor(ts_tmp,method="spearman"),2)
		png(file_name,width=900)
		grid.arrange(tableGrob(cor),top=textGrob("Spearman Correlation"))
		dev.off()
	}
	
	# add dependent variables
	added_dependent_variables <- names(CSI_CURVES)[!names(CSI_CURVES)%in%M_CSI$'CSI_ID']
	if (length(added_dependent_variables) > 0) {
		for (j in 1:length(added_dependent_variables)) {
			curves_mod <- CSI_CURVES[[added_dependent_variables[j]]]
			first_diff_ts <- diff(curves_mod)
			pc1_frame <- data.frame(PERIOD=as.yearperiod(index(first_diff_ts)),AC_ID="Added",MOD_ID=added_dependent_variables[j],PC1=first_diff_ts,row.names=NULL)
			PC1TS <- rbind(PC1TS, pc1_frame)
#			PC1TS <- rbind(PC1TS, characterizeTable(pc1_frame))
			pc1level <- data.frame(PERIOD=as.yearperiod(index(curves_mod)),AC_ID="Added",MOD_ID=added_dependent_variables[j],PC1=curves_mod,row.names=NULL)
			PC1LEVEL <- rbind(PC1LEVEL,pc1level)
#			PC1LEVEL <- rbind(PC1LEVEL,characterizeTable(pc1level))
		}
	}
	
	colnames(CSI_Eigenvectors) <- c("EIGENVECTOR","MULTIPLIER")
	#CSI_Eigenvectors = data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),CSI_ID=rownames(CSI_Eigenvectors),CSI_Eigenvectors,row.names=NULL)
	CSI_Eigenvectors = data.frame(CSI_ID=rownames(CSI_Eigenvectors),CSI_Eigenvectors,row.names=NULL)
	
	dat = merge(M_CSI,CSI_Eigenvectors,by="CSI_ID")
	if ("AssetClass"%in%colnames(dat)) {
		dat <- dat[,c("CSI_ID","AssetClass",modelid_column,"AC_ID","EIGENVECTOR","MULTIPLIER")]
	} else {
		dat <- dat[,c("CSI_ID",modelid_column,"AC_ID","EIGENVECTOR","MULTIPLIER")]
	}
	colnames(dat)[colnames(dat) == modelid_column] <- modelid_columns[1]
	MULTIPLIER = rbind(MULTIPLIER,dat)
	
	if (length(added_dependent_variables) > 0) {
		MULTIPLIER_addition <- data.frame(added_dependent_variables,"Added","Added","Added",1,1)
		colnames(MULTIPLIER_addition) <- c("CSI_ID","AssetClass",modelid_column,"AC_ID","EIGENVECTOR","MULTIPLIER")
		MULTIPLIER <- rbind(MULTIPLIER,MULTIPLIER_addition)
	}
	table_name <- paste0("O_PC1TS_FINAL_",modelid_column)
#	try(sqlDrop(dbc, table_name))
	PC1TS[,1] <- as.character(PC1TS[,1])
	saveTable(dbc,table_name,versionDataFrame(PC1TS,current_version))
	
#	try(sqlDrop(dbc, table_name))
	PC1LEVEL[,1] <- as.character(PC1LEVEL[,1])
	table_name <- paste0("O_PC1LEVEL_FINAL_",modelid_column)
	saveTable(dbc,table_name,versionDataFrame(PC1LEVEL,current_version))
	
#	file_name = paste(Output_PCA,"/EigenvectorAndMultiplier.tex",sep="")
#	print(xtable(dat[,4:ncol(dat)],caption="Eigenvector and Multiplier of PCA Analysis"),file=file_name, tabular.environment = 'longtable', floating=FALSE, include.rownames=FALSE)
	
	file_name = paste(Output_PCA,"/ClustersAll.tex",sep="")
	writeVectorToTex(file_name,"CLUSTERS",model_ids,command="renewcommand")
}

MULTIPLIER <- unique(MULTIPLIER)
MULTIPLIER <- versionDataFrame(MULTIPLIER,current_version)
MULTIPLIER <- MULTIPLIER[order(MULTIPLIER[,"AC_ID"]),]
#write.csv(MULTIPLIER[order(MULTIPLIER$MOD_ID),],"./test.csv")

table_name <- "O_MULTIPLIER_ALL"
saveTable(dbc,table_name,MULTIPLIER,prohibitaddition=FALSE)

file_name = paste(Output_root,"/O_MULTIPLIER_ALL.csv",sep="")
write.csv(MULTIPLIER,file = file_name)

# Run hierarchical clustering analysis
# 
# Author: E620927
###############################################################################

#CSI_CURVES from read data
###Cluster analysis


AC_ID <- as.character(unique(I_MOD_CSI_AC$AC_ID))

#CSI_Eigenvectors <- data.frame()
#PC1TS <- data.frame()

for (i in 1:length(AC_ID)) {
#	i<-1
	ac_id <- AC_ID[i]
	csi_tmp <- as.character(I_MOD_CSI_AC[I_MOD_CSI_AC$AC_ID==ac_id,]$CSI_ID)
	select_tmp <- unique(csi_tmp[csi_tmp%in%names(CSI_CURVES)]);
	if (length(select_tmp) > 1) {
		curves <- CSI_CURVES[select_tmp]
		curves_ac <- combineCurves(curves)
		curves_ac <- na.omit(curves_ac)
		result <- cluster_analysis(curves_ac, ac_id, Output_cluster)
#		PC1TS <- rbind(PC1TS,result[[1]])
#		CSI_Eigenvectors <- rbind(CSI_Eigenvectors,result[[2]])
		
		curves <- c()
		curves_ac <- c()
		result <- c()
	}
}

#table_name = "O_CLUSTER_PC1TS"
#data = data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),PC1TS)
#saveTable(dbc,table_name,data)
#
#table_name = "O_CLUSTER_EIGENVECTORS"
#data = data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),CSI_Eigenvectors)
#saveTable(dbc,table_name,data)

file_name <- paste(Output_cluster,"/AssetClassAll.tex",sep="")
writeVectorToTex(file_name,"ASSETCLASS",AC_ID)

####Cluster analysis of Monthly Version
#Output_cluster = file.path(Output_root,"ClusterAnalysis_Monthly")
#if(!file.exists(Output_cluster)) {
#	dir.create(Output_cluster)
#}
#AC_ID <- as.character(unique(I_MOD_CSI_AC$AC_ID))
##paste(AC_ID,collapse=",")
#
#CSI_Eigenvectors <- data.frame()
#PC1TS <- data.frame()
#for (i in 1:length(AC_ID)) {
##	i <- 1
#	ac_id <- AC_ID[i]
#	csi_tmp <- as.character(I_MOD_CSI_AC[I_MOD_CSI_AC$AC_ID==ac_id,]$CSI_ID)
#	select_tmp <- csi_tmp[csi_tmp%in%names(CSI_CURVES_MONTHLY)];
#	if (length(select_tmp) > 1) {
#		curves <- CSI_CURVES_MONTHLY[select_tmp];
#		curves_ac <- combineCurves(curves)
#		curves_ac <- na.omit(curves_ac)
#		result <- cluster_analysis(curves_ac, ac_id, Output_cluster)
#		PC1TS <- rbind(PC1TS,result[[1]])
#		CSI_Eigenvectors <- rbind(CSI_Eigenvectors,result[[2]])
#	}
#}
#
##	try(sqlDrop(dbc, table_name))
#table_name = "O_CLUSTER_PC1TS_MONTHLY"
#data = data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),PC1TS)
#saveTable(dbc,table_name,data)
#
#table_name = "O_CLUSTER_EIGENVECTORS_MONTHLY"
##	try(sqlDrop(dbc, table_name))
#data = data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),CSI_Eigenvectors)
##sqlSave(dbc, CSI_Eigenvectors, tablename = table_name, addPK=FALSE, safer = FALSE)
#saveTable(dbc,table_name,data)
#
#file_name <- paste(Output_cluster,"/AssetClassAll_MONTHLY.tex",sep="")
#writeVectorToTex(file_name,"ASSETCLASS_MONTHLY",AC_ID)


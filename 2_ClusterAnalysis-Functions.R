# 
# 
# Author: E620927
###############################################################################
library(ggplot2)
library(reshape)
library(RODBC)
library(xtable)
#method to calculate the distance between series
calculate_distance_cor <- function(first_diff, orig=TRUE) {
	if (orig) {
		corr_matrix = cor(first_diff)
		dissimilarity = 1-corr_matrix
		return(as.dist(dissimilarity))	
	} else {
		return(dist(t(first_diff), method = "euclidean"))
	}
}

#core code for clustering analysis
cluster_analysis <- function(curves_ac, ac_id, Output_cluster) {
#	curves_ac <- spreads
#	ac_id <- "CMBS-by-Maturity-Monthly"
	
	first_diff_ts = diff(curves_ac)
	first_diff = as.data.frame(first_diff_ts)
	freqcy = determineTsFrequency(as.Date(as.yearperiod(index(first_diff_ts))))
	rownames(first_diff) <- as.yearperiod(index(first_diff_ts))
	
	# clustering
	Entireperiod_Clustering  = hclust(calculate_distance_cor(first_diff), method = "ward.D")
	#	plot(hclust(calculate_distance_cor(first_diff), method = "ward.D"),hang = -1)
	#	plot(hclust(calculate_distance_cor(first_diff,FALSE), method = "ward.D"),hang = -1)
	
	# the number of groups is also height of clustering tree
	numberofgroups = ncol(curves_ac)
	
	clustering_result <- t(cutree(Entireperiod_Clustering,k=1:numberofgroups))
	
	percent_stat = as.data.frame(matrix(NA,numberofgroups,4))
	colnames(percent_stat) = c("percent","where is minimum","min Spearman corr","min Kendall corr")
	
# calculate the PCA statistics
	for(i in 1:numberofgroups){
#		i = 1
		max_group_num = max(clustering_result[i,])
		
		min_percent = 1
		min_percent_index = 1
		min_spearman_corr = 1
		min_kendall_corr = 1
#		percentages <- c()
		for(j in 1:max_group_num){
#			j <- 1
			# names of indices within same cluster
			names = colnames(clustering_result)[clustering_result[i,]==j]
			cluster_data = first_diff[names] 
			
			PCA = prcomp(cluster_data, center = FALSE,scale = FALSE)
			
			# calculate porpation of variance can be explained by PC1
			total = sum(PCA$sdev^2)  # total variance 
			var_pc1 = (PCA$sdev[1])^2     
			percent = var_pc1/total
			if(percent< min_percent){
				min_percent = percent
				min_percent_index = j
			}
			
			#calculate retained correlations
			PCA_rotation <- PCA$rotation[,1,drop=FALSE]
			PC1_no_series <- PCA$x[,1,drop=FALSE]
			
			if (all(PCA_rotation<0)) {
				PCA_rotation <- -PCA_rotation
				PC1_no_series <- -PC1_no_series
			}
			
			date <- as.Date(as.yearperiod(rownames(PC1_no_series)))
			
			PC1_no_series <- ts(PC1_no_series,start=determineTsStartDate(date),frequency=determineTsFrequency(date))
			pc1_ts <- PC1_no_series/sum(PCA_rotation)
			
			ts_tmp <- na.omit(ts.union(cluster_data,pc1_ts))
			cor <- round(cor(ts_tmp,method="spearman"),2)
			retained_corr = min(cor[,"pc1_ts"])
			if (retained_corr < min_spearman_corr) {
				min_spearman_corr <- retained_corr
			}
			
			cor <- round(cor(ts_tmp,method="kendall"),2)
			retained_corr = min(cor[,"pc1_ts"])
			if (retained_corr < min_kendall_corr) {
				min_kendall_corr <- retained_corr
			}
		}
		percent_stat[i,1] = min_percent
		percent_stat[i,2] = min_percent_index
		percent_stat[i,3] = min_spearman_corr
		percent_stat[i,4] = min_kendall_corr
	}
	percent_stat = round(percent_stat,3)
	clustering_result_stat = cbind(clustering_result,percent_stat)
	
	if (OUTPUT_VERBOSE) {
#		dat = as.data.frame(t(clustering_result_stat))
#		table_name = paste("O_ClusterAnalysis_",ac_id,sep="")
#		sqlSave(dbc, dat, tablename = table_name, addPK=FALSE, safer = FALSE)
#		file_name <- paste(Output_cluster,"/clustering_result-",ac_id,"-statistics.xlsx",sep="")
#		write.xlsx(t(clustering_result_stat), file_name,col.names=FALSE)
	#		print(clustering_result_stat)
		file_name <- paste(Output_cluster,"/clustering_result-",ac_id,"-data.xlsx",sep="")
		curves_ac_data = as.data.frame(curves_ac)
		rownames(curves_ac_data) <- as.yearperiod(index(curves_ac))
		
		write.xlsx(curves_ac_data, file_name)
		
		file_name <- paste(Output_cluster,"/clustering_result-",ac_id,"-diffdata.xlsx",sep="")
		first_diff_ts_data = as.data.frame(first_diff_ts)
		rownames(first_diff_ts_data) <- as.yearperiod(index(first_diff_ts))
		
		write.xlsx(first_diff_ts_data, file_name)
		
		file_name = paste(Output_cluster,"/",ac_id,"-diffdata.tex",sep="")
		print(xtable(first_diff,caption=paste0("Differenced Spread Data")),file=file_name,tabular.environment = "tabularx", width = "\\textwidth", floating=FALSE, include.rownames=TRUE)
		
		#plot 1:trend
		plot_name <- paste(Output_cluster,"/clustering_result-",ac_id,"-trend.png",sep="")
		png(plot_name,width=800,height=600)
		ts.plot(curves_ac,col=1:ncol(curves_ac),type="b",main=paste("Asset Class:",ac_id))
		legend("topright",colnames(curves_ac),col=1:ncol(curves_ac),pch=1)
		dev.off()
		
		#plot 2: write to excel
		file_name <- paste(Output_cluster,"/clustering_result-",ac_id,"-matrix.csv",sep="")
		if (ncol(clustering_result)>5) {
			clustering_result <- clustering_result[,order(clustering_result[2,],clustering_result[3,],clustering_result[4,],clustering_result[5,])]
		}
		cluster_matrix <- rbind(t(clustering_result),"Variance% of PC1"=formatPercentages(percent_stat[,"percent"]),"Retained Correlation"=formatPercentages(percent_stat[,"min Spearman corr"]))
		write.csv(cluster_matrix,file_name)
		
		file_name <- paste(Output_cluster,"/clustering_result-",ac_id,"-matrix.tex",sep="")
		writeMatrixToTex(file_name,paste0("Cluster Analysis - ",ac_id),cluster_matrix)
		
		#plot 3: heatmap
		plot_name <- paste(Output_cluster,"/clustering_result-",ac_id,"-heatmap.png",sep="")
		clustering_result_m <- melt(clustering_result)
		ggplot(data = clustering_result_m, aes(x = X2, y = X1)) +
				ggtitle(ac_id) +
				labs(x = "", y = "") +
				geom_tile(aes(fill = value), col="grey") +
				scale_fill_gradient(low="#0A2F5C",high="#E4ECF0") + 
				theme(panel.grid.major = element_blank(),
						axis.text.y=element_blank(),
						axis.ticks.y=element_blank(),
						legend.position="none",
						axis.text.x = element_text(angle = 45)) +
				scale_x_discrete(position = "top") + 
				geom_text(label=clustering_result_m$value, col="#f56e00") + 
				scale_y_continuous(trans = "reverse") + theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
				annotate("text", x = -0.7, y=1:ncol(clustering_result), label = "") +
				annotate("text", x = -0.2, y=0:ncol(clustering_result), label = c("Variance%",formatPercentages(percent_stat[,"percent"])), angle =15) + 
				annotate("text", x = ncol(clustering_result)+3, y=1:ncol(clustering_result), label = "") +
				annotate("text", x = ncol(clustering_result)+1.2, y=0:ncol(clustering_result), label = c("Spearman",formatPercentages(percent_stat[,"min Spearman corr"])), angle =15) + 
				annotate("text", x = ncol(clustering_result)+2.2, y=0:ncol(clustering_result), label = c("Kendall",formatPercentages(percent_stat[,"min Kendall corr"])), angle =15)
		ggsave(plot_name)
	}

#	CSI_Eigenvectors <- data.frame()
#	PC1TS <- data.frame()
#	for (i in 1:nrow(clustering_result)) {
#		if (percent_stat$percent[i] > PARAM_CLUSTER) {
##			i <- 3
##			print(i)
#			max_group_num <- max(clustering_result[i,])
#			for (j in 1:max_group_num) {
##				j <- 3;
#				cluster_data = first_diff[colnames(clustering_result)[clustering_result[i,]==j]]
#				subgroup_no_pca = prcomp(cluster_data,center = FALSE,scale = FALSE)
#				
#				PC1_no_series = round(subgroup_no_pca$x[,1,drop=FALSE],digits=5)
#				PC1TS <- rbind(PC1TS, data.frame(QUARTER=rownames(PC1_no_series),AC_ID=ac_id,CLUSTER=paste(ac_id,"-Group", j, sep=""),PC1=PC1_no_series,row.names=NULL))
##				center = subgroup_no_pca$center
##				scale = subgroup_no_pca$scale
##				subgroup_no_pca$rotation
#				PCA_rotation = t(subgroup_no_pca$rotation[,1,drop=FALSE])
#				PCA_rotation <- round(PCA_rotation,digits=5)
#				if (all(PCA_rotation<0)) {
#					PCA_rotation = -PCA_rotation
#				}
#				
#				CSI_Eigenvectors <- rbind(CSI_Eigenvectors,data.frame(CSI_ID=colnames(PCA_rotation),CLUSTER=paste(ac_id,"-Group", j, sep=""),AC_ID=ac_id,EIGENVECTOR=t(PCA_rotation),row.names=NULL))
#			}
#			break
#		}
#	}
#	
#	return(list(PC1TS,CSI_Eigenvectors))
}


# 
# 
# Author: E620927
###############################################################################


Output_OutSample <- file.path(Output_root,"OutSample")
if (!dir.exists(Output_OutSample)) {
	dir.create(Output_OutSample)
}

for (i in 1:nrow(Model_Final)) {
#	i <-1
	dep <- Model_Final[i,]$CLUSTER
	ind <- c(Model_Final[i,]$Var1,Model_Final[i,]$Var2,Model_Final[i,]$Var3)
	ind <- as.character(na.omit(ind))
	ind <- ind[ind!=""]
	ind <- as.character(ind[ind!=""])
	
	dep_var <- Dep_var[,dep,drop=FALSE]
	ind_var <- Ind_var[,ind,drop=FALSE]
	
	outSampleTesting(dep_var, ind_var, Output_OutSample, OutSamplePeriods=OutSamplePeriods, OUTPUT_VERBOSE=OUTPUT_VERBOSE)
}

#file_name = paste(Output_OutSample,"/ClustersAll.tex",sep="")
#writeVectorToTex(file_name,"CLUSTERS",model_ids)


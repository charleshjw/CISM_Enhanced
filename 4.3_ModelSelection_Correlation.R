# 
# 
# Author: E620927
###############################################################################


Num_dep = ncol(Dep_var)
Num_ind = ncol(Ind_var)

Correlation_matrix = matrix(NA,Num_ind, Num_dep)

Corr_option = "pearson"
for (i in 1:Num_ind) {
	tmp_matrix <- ts.union(Ind_var[,i],Dep_var)
	colnames(tmp_matrix) <- c(colnames(Ind_var)[i],colnames(Dep_var))
	Correlation_matrix[i,] <- cor(tmp_matrix,method = Corr_option,use = "complete.obs")[1,2:ncol(tmp_matrix)]
}

colnames(Correlation_matrix) <- colnames(Dep_var)
rownames(Correlation_matrix) <- colnames(Ind_var)

file_name <- paste(Output_root,"/CorrelationMatrix_Dep_VS_Indep.xlsx",sep="")
write.xlsx(Correlation_matrix, file_name)

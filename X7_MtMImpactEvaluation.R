# 
# 
# Author: e620927
###############################################################################

re_import = FALSE
source("./X0_ConfigDiscounting.R")

source("./X0.1_Supporting functions.R")

cluster <- "AMBS_GNMA_30Y"

CUSIP_information_eval <- fetchTable(dbc,"CUSIP_Master_Table_Permanent",current_version)
CUSIP_information_eval <- CUSIP_information_eval[,c("CUSIPs","IndexMapping")]

# read independnet/dependent variables and credit spread indices
IV_ANALYSIS <- FALSE
DV_ANALYSIS <- FALSE
source("./4.1_Read_IV.R")

FLAG_READ_PRIOR_VERSION = FALSE 
source("./4.2_Read_DV.R")

OUTPUT_CSI_PLOT = FALSE;
Prior_Table_Name = "I_HIST_CSI_QTRAVE"
source("./1_ReadCSIandCompareToPriorVersion.R")

Output_folder <- file.path(Output_root,"Discounting")
if (!dir.exists(Output_folder)) {
	dir.create(Output_folder)
}

# read MFA file
filename_mfa <- file.path(Output_root,"MFA",cluster,paste0("MFA_",cluster,".csv"))
if (file.exists(filename_mfa)) {
	
	mfa <- read.csv(filename_mfa)
	mfa <- head(mfa,3) ## need to specify how many top models to run
	Model_Final <- fetchTable(dbc,"I_MOD_SPECIFICATION_FINAL",current_version)
	Model_Final <- clean_test_Model_Final(Model_Final)
	
	for (i in 1:nrow(mfa)) {
#		i = 2
		# replace the variable
		Model_Final_selected <- Model_Final[Model_Final$CLUSTER == cluster,]
		Model_Final_selected[,"Var1"] = as.character(mfa[i,c("V1")])
		Model_Final_selected[,"Var2"] = as.character(mfa[i,c("V2")])
		# forecast CSI
		MULTIPLIER <- fetchTable(dbc, "O_MULTIPLIER_ALL",version=current_version)
		MULTIPLIER$MOD_ID <- cleanString(MULTIPLIER$MOD_ID)
		
		########################################################
		#### Reformated table for discounting purposes
		result_reformat <- data.frame()
		
		for (j in 1:nrow(Model_Final_selected)) {
			#	j <- 1
			dep <- Model_Final_selected[j,]$CLUSTER
			ind <- c(Model_Final_selected[j,]$Var1,Model_Final_selected[j,]$Var2,Model_Final_selected[j,]$Var3)
			ind <- as.character(na.omit(ind))
			ind <- ind[ind!=""]
			dep_var <- Dep_var[,dep,drop=FALSE]
			ind_var <- Ind_var[,ind,drop=FALSE]
			
			result_tmp <- CSIForecast_reformated(dep_var, ind_var, Output_forecast,MULTIPLIER)
			if (i == 1) {
				result_reformat <- result_tmp[[1]]
			} else {
				result_reformat <- rbind(result_reformat, result_tmp[[1]])
			}
		}
		colnames(result_reformat) <- c("Scenario", "Index_Name", "Q0", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9")
		
		table_name <- "Index_Spreads_Forecasts";
		result_reformat_orig <- fetchTable(dbc,table_name,current_version)
		result_reformat_orig <- rbind(result_reformat_orig[!(result_reformat_orig$Scenario%in%result_reformat$Scenario & result_reformat_orig$Index_Name%in%result_reformat$Index_Name),],versionDataFrame(result_reformat,current_version))
		saveTable(dbc,table_name,result_reformat_orig)

#		file_name <- file.path(Output_folder,"Selected_CUSIPS.xlsx")
#		write.xlsx(CUSIP_information_eval[CUSIP_information_eval$IndexMapping%in%unique(result_reformat$Index_Name),],file_name)
		selected_cusips <- CUSIP_information_eval[CUSIP_information_eval$IndexMapping%in%unique(result_reformat$Index_Name),"CUSIPs"]
#		CUSIP_information_eval[CUSIP_information_eval$IndexMapping%in%unique(result_reformat$Index_Name),]
#		file_name <- file.path(Output_folder,"selected_cusips.csv")
#		write.csv(selected_cusips, file_name, row.names=FALSE)
		
		export_to_db <- "Yes" # "Yes" to write results in database tables
		source("./X3_Spread Projections.R")
		
		export_to_db <- "No" # "Yes" to write results in database tables
		run_main_portfolio <- "Yes" # "Yes" to run the main IP discounting
		run_reinvestments <- "No" # "Yes" to run reinvestments discounting
		source("./X5_DCF.R")
		
		export_CUSIP_Level <- "No" # "Yes" to export. 
		export_Asset_Class_Level <- "No" # "Yes" to export at asset class level summarized
		export_Index_Level <- "No" # "Yes" to export at index level summarized
		File_Output_Key <- "_test_"
		fair_values_table <- Fair_Value_ALL
		source("./X6_Summary_for_Documentation.R")
	}
}



# 
# 
# Author: e620927
###############################################################################

Root_Development = "H:/WorkspaceOCI/CSIM/Rcode"
Root_Production = "Z:/GTRM_IRRLiq_QA/CSIM/Enhance2018/ModelImplementation/Rcode"
Root_Validation = "//ntfmao01/ERMShared/Model Governance/MVG Fileshare/MVG-Treasury Risk/Credit Spreads Internal Model/2017/ModelImplementationv3/Rcode"
UPDATE_PRODUCTION_CODE = TRUE
UPDATE_VALIDATION_CODE = FALSE

if (UPDATE_PRODUCTION_CODE) {
	### archive codes in production folder
	root_archive = file.path(Root_Production,"Archive")
	if (!dir.exists(root_archive)) {
		dir.create(root_archive)
	}
	
	root_archive_date = file.path(root_archive,format(Sys.time(), "%Y-%m-%d-%Hh%Mm%Ss"))
	if (!dir.exists(root_archive_date)) {
		dir.create(root_archive_date)
	}
	R_files = list.files(Root_Production,"*.R")
	file.copy(file.path(Root_Production,R_files), root_archive_date)
	file.remove(file.path(Root_Production,R_files))
	
	### copy updated codes to production folder
	list_of_files <- c("0.1_Config.R",
			"0.1_GenerateLatex.R",
			"0.2_PreprocessCSI_Universal.R",
			"0.3_PreprocessIV_MAOX.R",
			"0_Main.R",
			"1_ReadCSIandCompareToPriorVersion.R",
			"1_ReadData-Functions.R",
			"2_ClusterAnalysis-Functions.R",
			"2_ClusterAnalysis.R",
			"3_GeneratePC_Alternative.R",
			"4.0_ProcessIV.R",
			"4.1_Read_IV.R",
			"4.2_Read_DV.R",
			"4.4_ModelSelection_SFA.R",
			"4.5_ModelSelection_MFA.R",
			"4_ModelSelection-Functions.R",
			"5.1_Forecast.R",
			"5.2_Forecast_OutSampleTesting.R",
			"5.3_Forecast_CSI.R",
			"5.4_Compare_CSI.R",
			"5.5_Compare_MtM_BRK.R",
			"5_Forecast-Functions.R",
			"9.9_Generate_PDF.R",
			"X0_ConfigDiscounting.R",
			"X0.1_Supporting functions.R",
			"X1_BRS_CashFlow_formatter.R",
			"X2_Starting Spread calculator.R",
			"X3_Spread Projections.R",
			"X4_BookValueCalculator.R",
			"X5_DCF.R",
			"X6_Summary_for_Documentation.R",
			"X7_MtMImpactEvaluation.R")
	
	file.copy(file.path(Root_Development,list_of_files), Root_Production)
	
	tx = readLines(file.path(Root_Production,"0_Main.R"))
	tx  <- gsub(pattern = "PRODUCTION_ENVIRONMENT = FALSE", replace = "PRODUCTION_ENVIRONMENT = TRUE", x = tx)
	writeLines(tx, con=file.path(Root_Production,"0_Main.R"))
	### archive tex codes
}

if (UPDATE_VALIDATION_CODE) {
	### archive codes in production folder
	root_archive = file.path(Root_Validation,"Archive")
	if (!dir.exists(root_archive)) {
		dir.create(root_archive)
	}
	root_archive_date = file.path(root_archive,format(Sys.time(), "%Y-%m-%d-%Hh%Mm%Ss"))
	if (!dir.exists(root_archive_date)) {
		dir.create(root_archive_date)
	}
	R_files = list.files(Root_Validation,"*.R")
	file.copy(file.path(Root_Validation,R_files), root_archive_date)
	file.remove(file.path(Root_Validation,R_files))
	
	### copy libraries to validation folder
	root_library = file.path(Root_Validation,"RLibrary")
	if (!dir.exists(root_library)) {
		dir.create(root_library)
	}
	R_files = list.files("//ntfmao01/ERMShared/Treasury Risk/Charles/RLibrary/","*.R")
	file.copy(file.path("//ntfmao01/ERMShared/Treasury Risk/Charles/RLibrary/",R_files), root_library)
	
	### copy updated codes to production folder
	list_of_files <- c("0.1_Config.R",
			"0.1_GenerateLatex.R",
			"0.2_PreprocessCSI_Universal.R",
			"0.3_PreprocessIV_MAOX.R",
			"0_Main.R",
			"1_ReadCSIandCompareToPriorVersion.R",
			"1_ReadData-Functions.R",
			"2_ClusterAnalysis-Functions.R",
			"2_ClusterAnalysis.R",
			"3_GeneratePC_Alternative.R",
			"4.0_ProcessIV.R",
			"4.1_Read_IV.R",
			"4.2_Read_DV.R",
			"4.4_ModelSelection_SFA.R",
			"4.5_ModelSelection_MFA.R",
			"4_ModelSelection-Functions.R",
			"5.1_Forecast.R",
			"5.2_Forecast_OutSampleTesting.R",
			"5.3_Forecast_CSI.R",
			"5_Forecast-Functions.R",
			"9.9_Generate_PDF.R")
	file.copy(file.path(Root_Development,list_of_files), Root_Validation)
	
	tx = readLines(file.path(Root_Validation,"0_Main.R"))
	tx  <- gsub(pattern = "PRODUCTION_ENVIRONMENT = FALSE", replace = "PRODUCTION_ENVIRONMENT = TRUE", x = tx)
	tx  <- gsub(pattern = "VALIDATION = FALSE", replace = "VALIDATION = TRUE", x = tx)
	writeLines(tx, con=file.path(Root_Validation,"0_Main.R"))
	
#	tx = readLines(file.path(Root_Validation,"0.3_PreprocessIV_MAOX.R"))
#	tx  <- gsub(pattern = "e620927", replace = "employeeid", x = tx)
#	tx  <- gsub(pattern = "7eykp3d", replace = "password", x = tx)
#	writeLines(tx, con=file.path(Root_Validation,"0.3_PreprocessIV_MAOX.R"))
}

# Updated on 1n/19/2018
# 
# Author: e620927
###############################################################################

###############################################################################
###############################################################################
###############################################################################
###############################################################################
#PRODUCTION_ENVIRONMENT = FALSE 
PRODUCTION_ENVIRONMENT = TRUE	# if it is used for production

VALIDATION = FALSE
#VALIDATION = TRUE	# if the production is used for validation purpose.

Root_Development = "H:/WorkspaceOCI/CSIM/Rcode"
Root_Production = "Z:/GTRM_IRRLiq_QA/CSIM/Enhance2018/ModelImplementation/Rcode"
Root_Validation = "//ntfmao01/ERMShared/Model Governance/MVG Fileshare/MVG-Treasury Risk/Credit Spreads Internal Model/2017/ModelImplementationv3/Rcode"
PDFReportRoot_Validation = "Y:\\2017\\ModelImplementationv3\\PDFReport2018CCARProduction_NoCalib" # manually specify the root directory to the pdf reports
OutputRoot_Validation = "Y:\\2017\\ModelImplementationv3\\Output_2018CCARProduction_NoCalib" # manually specify the root directory to the model output files

if(PRODUCTION_ENVIRONMENT) {
	#install required libraries
#	remove.packages(c("ggplot2", "gridExtra", "RODBC", "egcm", "ggdendro","zoo","xlsx","xtable","reshape","plm","car","tseries","urca","lmtest","leaps","sandwich","doParallel","forecast","RJDBC","ggfortify"))
#	,"ggfortify"
	#	install.packages(c("ggplot2", "gridExtra", "RODBC", "egcm", "ggdendro","zoo","xlsx","xtable","reshape","plm","car","tseries","urca","lmtest","leaps","sandwich","doParallel","forecast","RJDBC"))
#	install.packages("plyr")
	
	setwd(Root_Production)
	Control_File = "Z:/GTRM_IRRLiq_QA/CSIM/Enhance2018/ModelImplementation/CSIM_Control.accdb"
} else {
	setwd(Root_Development)
	Control_File = "Z:/Charles/CSIM/CSIM_Control.accdb"
}

if(VALIDATION) {
	setwd("//ntfmao01/ERMShared/Model Governance/MVG Fileshare/MVG-Treasury Risk/Credit Spreads Internal Model/2017/ModelImplementationv3/Rcode")
	Control_File = "//ntfmao01/ERMShared/Model Governance/MVG Fileshare/MVG-Treasury Risk/Credit Spreads Internal Model/2017/ModelImplementationv3/CSIM_Control.accdb"
}
#  source(file.path(Root_Development,"10.0_Update_Production.R"))
###############################################################################
###############################################################################
###############################################################################
###############################################################################

################################Start Analysis####################################
if (VALIDATION) {
	source("./RLibrary/Library_DataFrame.R")
	source("./RLibrary/Library_DateAndString.R")
	source("./RLibrary/Library_AccessDatabase.R")
	source("./RLibrary/Library_IO.R")
	source("./RLibrary/Library_TimeSeries.R")
	source("./RLibrary/Library_Regression.R")
} else {
	source("//ntfmao01/ERMShared/Treasury Risk/Charles/RLibrary/Library_DataFrame.R")
	source("//ntfmao01/ERMShared/Treasury Risk/Charles/RLibrary/Library_DateAndString.R")
	source("//ntfmao01/ERMShared/Treasury Risk/Charles/RLibrary/Library_AccessDatabase.R")
	source("//ntfmao01/ERMShared/Treasury Risk/Charles/RLibrary/Library_IO.R")
	source("//ntfmao01/ERMShared/Treasury Risk/Charles/RLibrary/Library_TimeSeries.R")
	source("//ntfmao01/ERMShared/Treasury Risk/Charles/RLibrary/Library_Regression.R")
}

source("./1_ReadData-Functions.R")
source("./2_ClusterAnalysis-Functions.R")
source("./4_ModelSelection-Functions.R")
source("./5_Forecast-Functions.R")

OUTPUT_VERBOSE = TRUE

source("./0.1_Config.R")

###########################################################################################
# Credit Spread Modeling and Forecast
###########################################################################################
### Step0: generate Latex Template for PDF reporting
source("./0.1_GenerateLatex.R")
### Step1: data preparation
# process the downloaded credit spread indices from vendors
REPROCESS = FALSE # if TRUE, the CSI will be reprocessed and the database will be refreshed.
source("./0.2_PreprocessCSI_Universal.R")
# download independent variables from database
REPROCESS = FALSE # if TRUE, the independent variables will be reprocessed and the database will be refreshed.
source("./0.3_PreprocessIV_MAOX.R") # when new scenario data is available, sql needs to be updated (line 32-34).

### Step2: read CSI data from MS Access
OUTPUT_CSI_PLOT = FALSE;
#OUTPUT_CSI_PLOT = TRUE;
#Prior_Table_Name = "I_HIST_CSI_APP_PREV"
#Prior_Table_Name = "I_HIST_CSI_MONAVE"
Prior_Table_Name = "I_HIST_CSI_QTRAVE" #need to identify the name of table in the database of prior version
source("./1_ReadCSIandCompareToPriorVersion.R")

### Step3: Analyze CSI and classify them using hcluster algorithm
#just need to run once each time original raw data is updated
PARAM_CLUSTER = 0.85
source("./2_ClusterAnalysis.R")

### Step4: Review hcluster results and apply management overrides
#run PCA with final selection of clusters
#just need to run once each time original raw data is updated
source("./3_GeneratePC_Alternative.R")

### Step5: Read IV data from MS Access
#just need to run once each time original raw data is updated
Prior_IVTable_Name = "I_HIST_IV_PRIM" #need to identify the name of table in the database of prior version
source("./4.0_ProcessIV.R")

### Step6: Read CSI and IV and run verification if set TRUE
#IV_ANALYSIS <- TRUE
#DV_ANALYSIS <- TRUE
Prior_IVTable_Name = "I_HIST_IV_PRIM" #need to identify the name of table in the database of prior version
IV_ANALYSIS <- FALSE
DV_ANALYSIS <- FALSE
source("./4.1_Read_IV.R")

# read PC1 from all alternatives
FLAG_READ_PRIOR_VERSION = FALSE 
# FLAG_READ_PRIOR_VERSION = TRUE # TRUE if read dependent variables from prior version
source("./4.2_Read_DV.R")
#source("./4.3_ModelSelection_Correlation.R")

### Step7: Model Speicification with single variable and two variables
OutSamplePeriods <- 9; #exclude the most recent or the most acient 1 to X periods
RERUN_SFA_FLAG <- FALSE
REPLOT_SFA_FLAG <- FALSE
source("./4.4_ModelSelection_SFA.R")

OutSamplePeriods <- 9; #exclude the most recent or the most acient 1 to X periods
RERUN_MFA_FLAG <- FALSE
REPLOT_MFA_FLAG <- FALSE
source("./4.5_ModelSelection_MFA.R")

### Step8: Run Models with Final Model Specifications, out-sample-testing
source("./5.1_Forecast.R")
OutSamplePeriods <- 0:9;
source("./5.2_Forecast_OutSampleTesting.R")
# Run forecasts on index level
Do_Not_Recalibrate = FALSE
source("./5.3_Forecast_CSI.R")
# Compare index with prior version and with BRK's forecasts
source("./5.4_Compare_CSI.R")

# Compare MtM impact between CSIM and BRK on index level. The outputs are from BRK Platform
source("./5.5_Compare_MtM_BRK.R")

### Step9: Run Latex PDF packages
source("./9.9_Generate_PDF.R")

###########################################################################################
# Discount the Cash Flow and Assess the MtM Impact
###########################################################################################
### Step 10: configure the system before discounting
#re_import = TRUE
re_import = FALSE
replace_mapping = TRUE
source("./X0_ConfigDiscounting.R")

source("./X0.1_Supporting functions.R")

# need debug. currently using formatted version
#source("./X1_BRS_CashFlow_formatter.R")

export_to_db = "Yes"
re_calculate_starting_spread <- FALSE
source("./X2_Starting Spread calculator.R")

export_to_db <- "Yes" # "Yes" to write results in database tables
source("./X3_Spread Projections.R")

export_to_db <- "Yes" # "Yes" to write results in database tables
re_calculate_book_value <- FALSE
source("./X4_BookValueCalculator.R")

ScenarioNames <- ScenarioNames[3]
export_to_db = "Yes"
run_main_portfolio <- "Yes" # "Yes" to run the main IP discounting
run_reinvestments <- "No" # "Yes" to run reinvestments discounting
source("./X5_DCF.R")

# Choose what to export to Excel
export_CUSIP_Level <- "No" # "Yes" to export. 
export_Asset_Class_Level <- "Yes" # "Yes" to export at asset class level summarized
export_Index_Level <- "Yes" # "Yes" to export at index level summarized
File_Output_Key <- "_Benchmark2d_LiborOAS_GovCurve_"
source("./X6_Summary_for_Documentation.R")

# evaluate the impact from CSIM 1
#source("./X7_MtMImpactEvaluation.R")

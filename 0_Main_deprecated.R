# End-to-End analytical system to model credit spread indexes
# 
# Author: E620927
# Email: JCHuang@statestreet.com
###############################################################################
#install.packages(c("ggdendro", "RODBC","zoo", "xlsx", "ggplot2", "reshape", "plm", "car", 
#				"tseries","fUnitRoots", "urca", "lmtest", "leaps", "sandwich", "doParallel", "forecast"), 
#		repos="http://cran.r-project.org" )

##Need to Setup before Run
setwd("H:/WorkspaceOCI/CSIM")

################################Start Analysis####################################
library(ggplot2)
library(gridExtra)
library(grid)
source("//ntfmao01/ERMShared/Treasury Risk/Charles/RLibrary/Library_DataFrame.R")
source("//ntfmao01/ERMShared/Treasury Risk/Charles/RLibrary/Library_DateAndString.R")
source("//ntfmao01/ERMShared/Treasury Risk/Charles/RLibrary/Library_AccessDatabase.R")
source("//ntfmao01/ERMShared/Treasury Risk/Charles/RLibrary/Library_IO.R")
source("//ntfmao01/ERMShared/Treasury Risk/Charles/RLibrary/Library_TimeSeries.R")
source("//ntfmao01/ERMShared/Treasury Risk/Charles/RLibrary/Library_Regression.R")

#setwd("Z:/Charles/CSIM/Rcodes")
source("./1_ReadData-Functions.R")
source("./2_ClusterAnalysis-Functions.R")
source("./4_ModelSelection-Functions.R")
source("./5_Forecast-Functions.R")

OUTPUT_VERBOSE = TRUE
MONTHLY_VERSION = FALSE

Control_File = "//ntfmao01/ERMShared/Treasury Risk/Charles/CSIM/CSIM_Control.accdb"
source("./0.1_Config.R")
source("./0.1_GenerateLatex.R")

##Process CSI from different vendors. Need frequent adjustments
#need to run just once.
REPROCESS = FALSE
source("./0.2_PreprocessCSI_Universal.R")

##read data from MS Access: curve info and iv info
OUTPUT_CSI_PLOT = FALSE;
#OUTPUT_CSI_PLOT = TRUE;
source("./1_ReadCSIandCompareToPriorVersion.R")

##Credit Spread Shock Analysis
source("./9.1_ShockAnalysis.R")

##Analyze CSI and classify them using hcluster algorithm
#just need to run once each time original raw data is updated
PARAM_CLUSTER = 0.85
source("./2_ClusterAnalysis.R")

##Review hcluster results and apply management overrides
#run PCA with final selection of clusters
#just need to run once each time original raw data is updated
#source("./3_GeneratePC.R")
source("./3_GeneratePC_Alternative.R")

#Retrieve variables from CCAR PMO original files and reformat into MS ACCESS tables
#just need to run once each time original raw data is updated
source("./4.0_ProcessIV.R")

#IV_ANALYSIS <- TRUE
#DV_ANALYSIS <- TRUE

IV_ANALYSIS <- FALSE
DV_ANALYSIS <- FALSE
source("./4.1_Read_IV.R")

#read PC1 from all alternatives
source("./4.2_Read_DV.R")
#source("./4.3_ModelSelection_Correlation.R")

OutSamplePeriods <- 9; #exclude the most recent or the most acient 1 to X periods
RERUN_SFA_FLAG <- TRUE
REPLOT_SFA_FLAG <- FALSE
source("./4.4_ModelSelection_SFA.R")

OutSamplePeriods <- 9; #exclude the most recent or the most acient 1 to X periods
RERUN_MFA_FLAG <- TRUE
REPLOT_MFA_FLAG <- FALSE
source("./4.5_ModelSelection_MFA.R")

#### Run Models with Final Model Specifications, out-sample-testing
source("./5.1_Forecast.R")
#source("./5.1_Forecast_Monthly.R")
OutSamplePeriods <- 0:9;
source("./5.2_Forecast_OutSampleTesting.R")
#### Run forecasts on index level
source("./5.3_Forecast_CSI.R")

#PDFReportRoot
#setwd(PDFReportRoot)
#command = paste0("Z:/Charles/CSIM/PDFReportv08/ClusterAnalysis_Production.bat")
#command = "./ClusterAnalysis_Production.bat"
#system(command)
#runningtime = proc.time() - ptm_orig
#print(runningtime)
##
#setwd("..")

head(IV)
unique(IV["IV_ID"])
IV[IV["IV_ID"]=="IT_GOV_YLD_10Y",]








120*120/2+460
7660-120*24+2.5*24*24


f <- function (q) {
	return(60 + 3*(120-q) + (120-q)^2/2  + 40+3*q +2*q^2)
}

optimize(f,c(0,120),tol=0.0001)



















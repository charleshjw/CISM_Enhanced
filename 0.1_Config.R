# Assign global parameters and configure the run
# 
# Author: E620927
###############################################################################
require(xlsx)
#require LibraryAccessDatabase
options(digits=10)

dbc_control <- openAccess(Control_File)
versions <- sqlFetch(dbc_control,"Versions")
versions <- characterizeTable(versions)
current_version_info <- versions[versions$CurrentVersion==1,]
createVariables(current_version_info)
current_version = Version
list_files <- fetchTable(dbc_control,"List_Files",current_version)
odbcClose(dbc_control)

dbc <- openAccess(concat_file_path(current_version_info))

if (sum(versions$PriorVersion==1)==1) {
	prior_version_info <- versions[versions$PriorVersion==1,]
	dbc_prev <- openAccess(concat_file_path(prior_version_info))
	prior_version = as.character(prior_version_info$Version)
	prior_frequency = as.character(prior_version_info$Frequency)
	print(paste0("Prior Version is ",prior_version))
}

Output_root = as.character(OutputRoot)
if (!dir.exists(Output_root)) {
	dir.create(Output_root)
}

if (Frequency == "MONTHLY") {
	MONTHLY_VERSION = TRUE
} else {
	MONTHLY_VERSION = FALSE
}

History_END_DATE <- convertDateToNumeric(History_END_DATE)
Calibration_END_DATE <- convertDateToNumeric(Calibration_END_DATE)
Forecast_END_DATE <- convertDateToNumeric(Forecast_END_DATE)

if (MONTHLY_VERSION) {
	History_END_DATE <- as.numeric(as.yearmon(as.Date(as.yearqtr(History_END_DATE)+0.25)-1))
	Calibration_END_DATE <- as.numeric(as.yearmon(as.Date(as.yearqtr(Calibration_END_DATE)+0.25)-1))
	Forecast_END_DATE <- as.numeric(as.yearmon(as.Date(as.yearqtr(Forecast_END_DATE)+0.25)-1))
	
	####Define Names of Scenarios
	ScenarioNames <- current_version_info[,grepl("ScenarioNameMonthly",colnames(current_version_info))]
	ScenarioNames <- ScenarioNames[!is.na(ScenarioNames)]
	
	ScenarioNames_Prior <- prior_version_info[,grepl("ScenarioNameMonthly",colnames(prior_version_info))]
	ScenarioNames_Prior <- ScenarioNames_Prior[!is.na(ScenarioNames_Prior)]
	
	as.yearperiod <- as.yearmon
	
	I_CSI_TABLE = "I_HIST_CSI_MONAVE"
} else {
	####Define Names of Scenarios
	ScenarioNames <- current_version_info[,grepl("ScenarioName[^M]",colnames(current_version_info))]
	ScenarioNames <- ScenarioNames[!is.na(ScenarioNames)]
	
	ScenarioNames_Prior <- prior_version_info[,grepl("ScenarioName[^M]",colnames(prior_version_info))]
	ScenarioNames_Prior <- ScenarioNames_Prior[!is.na(ScenarioNames_Prior)]
	
	as.yearperiod <- as.yearqtr
	
	I_CSI_TABLE = "I_HIST_CSI_QTRAVE"
}

####Read global parameters for variable selection
pthreshold <- 0.05
Max_var <- 2;
Num_lags <- 1;
lags <- paste("L",0:1,sep="")
stationarity_test_options <- 1 #1 is simple with no lag, 2 with lag
max_model_to_test <- 200

###Process Excel and Predefined Mappings and Inputs. Save Excels to Destined Database
mapping <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","mapping")),sheetIndex=1)
mapping <- mapping[!is.na(mapping[,1]),]
mapping <- versionDataFrame(mapping,current_version)
mapping <- mapping[,!apply(mapping,2,function(data){all(is.na(data))})]

modelid_columns <- colnames(mapping)[grep("MOD_ID",colnames(mapping))]
for (i in 1:length(modelid_columns)) {
	mapping[,modelid_columns[i]] <- cleanString(mapping[,modelid_columns[i]])
}
table_name <- "I_MOD_CSI_AC";
try(sqlDrop(dbc,table_name))
saveTable(dbc,table_name,mapping,prohibitaddition=TRUE)

D_IV <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","D_IV")),sheetIndex=1)
D_IV <- versionDataFrame(D_IV,current_version)
table_name <- "I_IV";
saveTable(dbc,table_name,D_IV)

D_IV_SEC <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","D_IV_SEC")),sheetIndex=1)
D_IV_SEC <- versionDataFrame(D_IV_SEC,current_version)
table_name <- "I_IV_SEC";
saveTable(dbc,table_name,D_IV_SEC)

IV_transform <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","IV_transform")),sheetIndex=1)
IV_transform <- versionDataFrame(IV_transform,current_version)
table_name <- "I_IV_TRANSFORMS";
saveTable(dbc,table_name,IV_transform)

#TestStats <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","TestStats")),sheetIndex=1)
#TestStats <- versionDataFrame(TestStats,current_version)
#table_name <- "I_TESTSTATS";
#saveTable(dbc,table_name,TestStats)

#LOOPCRITERIA <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","LOOPCRITERIA")),sheetIndex=1)
#LOOPCRITERIA <- versionDataFrame(LOOPCRITERIA,current_version)
#table_name <- "I_LOOPCRITERIA";
#saveTable(dbc,table_name,LOOPCRITERIA)

MODELSPECIFICATION <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","MODELSPECIFICATION")),sheetIndex=1)
#a <- cleanString(MODELSPECIFICATION[,"CLUSTER"])
##b <- unique(mapping[,"MOD_ID"])
#b <- unique(as.vector(as.matrix(mapping[,grep("MOD_ID",colnames(mapping))])))
#
#if (!all(b%in%a)) {
#	print(b[!b%in%a])
#	print("WARNING: Mapping has more clusters than model specificaition defined")
#}
#
#if (!all(a%in%b)) {
#	print(a[!a%in%b])
#	MODELSPECIFICATION <- MODELSPECIFICATION[MODELSPECIFICATION[,"CLUSTER"]%in%a[a%in%b],]
#	print("Error: Model Speicification has more cluster than Mapping defined. Deleted")
#}

MODELSPECIFICATION <- versionDataFrame(MODELSPECIFICATION,current_version)
table_name <- "I_MOD_SPECIFICATION_FINAL";
saveTable(dbc,table_name,MODELSPECIFICATION)

MOD_IV <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","MOD_IV")),sheetIndex=1)
a <- cleanString(MODELSPECIFICATION[,"CLUSTER"])
b <- colnames(MOD_IV)[5:ncol(MOD_IV)]
if (!all(a%in%b)&all(b%in%a)) {
	print(a[!a%in%b])
	print(b[!b%in%a])
	stop("Error: Model Speicification and Mapping needs to attention")
}
MOD_IV <- versionDataFrame(MOD_IV,current_version)
table_name <- "I_MOD_IV";
try(sqlDrop(dbc,table_name))
saveTable(dbc,table_name,MOD_IV,prohibitaddition=FALSE)

D_CSI_SEC <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","D_CSI_SEC")),sheetIndex=1)
table_name <- "I_CSI_SEC";
try(sqlDrop(dbc,table_name))
saveTable(dbc,table_name,versionDataFrame(D_CSI_SEC,current_version),prohibitaddition=FALSE)

#try(sqlDrop(dbc,table_name))
#odbcCloseAll()
print(paste0("Current Version is ",current_version))
print(paste0("History of History end date: ", current_version_info$History_END_DATE))
print(paste0("History of Calibration end date: ", current_version_info$Calibration_END_DATE))
print(paste0("History of Forecast ends date: ", current_version_info$Forecast_END_DATE))


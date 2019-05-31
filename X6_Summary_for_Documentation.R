##########
#Summary#
##########
#read fair value table from DB?
Read_DB = FALSE

# Clear Environment and then Console
#rm(list = ls())
#cat("\014")
### Load libraries: If package missing load it with: #install.packages("lubridate")
library(lubridate)
library(PCICt)
library(RODBC)
library(reshape2)
library(xlsx)
library(plyr)

Output_folder <- file.path(Output_root,"Discounting")
if (!dir.exists(Output_folder)) {
	dir.create(Output_folder)
}

##### Control ########################################################################################################
### Path of Database and database name used to get input data ########################################################
#db_path <- "P:\\Remy\\CCAR\\2016_DFAST\\Production\\"
#db_name <- "CSIM_Discounting_and_Summary_Tools.accdb"
## The working directory is chosen as a subfolder to the database. It contains the functions file we need to source
#workdingDir <- "R_Files\\"
#setwd(paste0(db_path,workdingDir))
#source("Supporting functions.R")
# Select Fair Value versions to run, and which (unique) book value version to use
#FV_version_of_interest <- c("CCAR2016_Enhanced","CCAR2016_Enhanced_Multiplicative","CCAR2016_Enhanced_Unfloored")
#BV_version_of_interest <- "CCAR2016_Official"
FV_version_of_interest <- current_version
BV_version_of_interest <- current_version

######################################################################################################################

#### Data importing for all scenarios
# We get the data from the database: establish connection and load data for all scenarios first
# Current version running
running_version <- current_version
# List of scenarios we loop through
scenario_names <- ScenarioNames
# Dates
quarters_generic <- c("Q0","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9")
CCAR_horizons <-  format(subset(Dates_Master,Horizon %in% quarters_generic)$HorizonDate,"%m/%d/%Y")

# CUSIP Master Table (same for all scenaios)
CUSIP_information <- fetchTable(dbc,"CUSIP_Master_Table_Permanent",current_version)
CUSIP_information <- CUSIP_information[,c("CUSIPs","AssetClass","IndexMapping","Currency","ReferenceCurveType",
                                          "InternalCurveType","FixedFloating","FloatingReset","CF_Source",
                                          "CF_Currency","PurchaseDate","AFS_Ratio","FV_USD","AdjustmentMultiplier")]
						  
# Get CUSIPs names ordered
CUSIPs_list_ordered <- as.character(CUSIP_information$CUSIPs)
CUSIPs <- droplevels(CUSIP_information$CUSIPs)
# Time used for timestamps in output tables
#current_time <- format(Sys.time(),"%m/%d/%Y %I:%M:%S %p")

# Fair Values

if (Read_DB|!"fair_values_table" %in% ls()) {
	fair_values_table_all <- fetchTable(dbc,"Fair_Value_ALL",current_version)
	fair_values_table_rate <- fetchTable(dbc,"Fair_Value_Rate_Only",current_version)
	fair_values_table <- rbind(fair_values_table_all,fair_values_table_rate)
}
# Book value
book_values_table <- fetchTable(dbc,"Book_Value",current_version)
# FX Rates
fx_rates <- fetchTable(dbc,"FX_Spot_Rates",current_version)

# Convert in USD and adjust BRS issue
# Fair values
FV_USD <- subset(fair_values_table,Version %in% FV_version_of_interest)
FV_USD <- merge(FV_USD,CUSIP_information,by='CUSIPs')
FV_USD <- merge(FV_USD,fx_rates,by.x='CF_Currency',by.y='Currency_HFV')
FV_USD$Q0 <- FV_USD$FV_USD
FV_USD[,quarters_generic[2:10]] <- FV_USD[,quarters_generic[2:10]] * FV_USD$AdjustmentMultiplier * FV_USD$FXRate
FV_USD$Metric <- "FV"
colnames(FV_USD)
FV_USD <- FV_USD[,c("Metric","Component","Scenario","CUSIPs","CF_Currency","AFS_Ratio","AssetClass","IndexMapping","FixedFloating",quarters_generic)]
# Book Values
BV_USD <- subset(book_values_table,Version %in% BV_version_of_interest)
BV_USD <- merge(BV_USD,CUSIP_information,by='CUSIPs')
BV_USD <- merge(BV_USD,fx_rates,by.x='CF_Currency',by.y='Currency_HFV')
BV_USD$Q0 <- BV_USD$Q0 * BV_USD$FXRate
BV_USD[,quarters_generic[2:10]] <- BV_USD[,quarters_generic[2:10]] * BV_USD$AdjustmentMultiplier * BV_USD$FXRate
BV_USD$Component <- "BV"
BV_USD$Metric <- "BV"
BV_USD <- BV_USD[,c("Metric","Component","Scenario","CUSIPs","CF_Currency","AFS_Ratio","AssetClass","IndexMapping","FixedFloating",quarters_generic)]

### Compute MTM
MTM_USD <- FV_USD
MTM_USD <- merge(MTM_USD,BV_USD,by=c("CUSIPs","Scenario"))
MTM_USD[,quarters_generic] <- MTM_USD[,paste0(quarters_generic,".x")] - MTM_USD[,paste0(quarters_generic,".y")]
MTM_USD$Metric <- "MTM"
MTM_USD <- MTM_USD[,c("Metric","Component.x","Scenario","CUSIPs","CF_Currency.x","AFS_Ratio.x","AssetClass.x","IndexMapping.x","FixedFloating.x",quarters_generic)]
colnames(MTM_USD) <- c("Metric","Component","Scenario","CUSIPs","CF_Currency","AFS_Ratio","AssetClass","IndexMapping","FixedFloating",quarters_generic)
# AFS only
FV_USD_AFS <- FV_USD
BV_USD_AFS <- BV_USD
MTM_USD_AFS <- MTM_USD
FV_USD_AFS[,quarters_generic] <- FV_USD_AFS[,quarters_generic] * FV_USD_AFS$AFS_Ratio
FV_USD_AFS$Component <- paste0(FV_USD_AFS$Component,"_AFS_only")
BV_USD_AFS[,quarters_generic] <- BV_USD_AFS[,quarters_generic] * BV_USD_AFS$AFS_Ratio
BV_USD_AFS$Component <- paste0(BV_USD_AFS$Component,"_AFS_only")
MTM_USD_AFS[,quarters_generic] <- MTM_USD_AFS[,quarters_generic] * MTM_USD_AFS$AFS_Ratio
MTM_USD_AFS$Component <- paste0(MTM_USD_AFS$Component,"_AFS_only")

### Aggregate AFS only and AFS+HTM results
FV_USD <- rbind(FV_USD,FV_USD_AFS)
BV_USD <- rbind(BV_USD,BV_USD_AFS)
MTM_USD <- rbind(MTM_USD,MTM_USD_AFS)
output_table <- rbind(FV_USD,BV_USD,MTM_USD)

### Exporting results to Excel files
# Export CUSIP details
if (export_CUSIP_Level == "Yes") {
  output_filename <- file.path(Output_folder,paste0("MTM Summary - CUSIP Level",format(Sys.time(),"%Y %m %d %Hh%Mm"),".xlsx"))
  system.time(write.xlsx(output_table, output_filename))
  system.time(saveTable(dbc,"MTMSummary_CUSIP_Level",versionDataFrame(output_table,current_version)))
}
#deleteTable(dbc,"MTMSummary_CUSIP_Level")

# Export results at asset class level
asset_class_summary <- ddply(output_table,c("Metric","Component","Scenario","AssetClass"),summarise,
                           Q0=sum(Q0),Q1=sum(Q1),Q2=sum(Q2),Q3=sum(Q3),Q4=sum(Q4),Q5=sum(Q5),
                           Q6=sum(Q6),Q7=sum(Q7),Q8=sum(Q8),Q9=sum(Q9))
if (export_Asset_Class_Level == "Yes") {
   output_filename <- file.path(Output_folder,paste0("MTM Summary - Asset Class Level",format(Sys.time(),"%Y %m %d %Hh%Mm"),".xlsx"))
   system.time(write.xlsx(asset_class_summary, output_filename))
   system.time(saveTable(dbc,"MTMSummary_AssetClass_Level",versionDataFrame(asset_class_summary,current_version)))
}

# Export level summarized at index level
if (export_Index_Level == "Yes") {
  index_summary <- ddply(output_table,c("Metric","Component","Scenario","IndexMapping"),summarise,
                               Q0=sum(Q0),Q1=sum(Q1),Q2=sum(Q2),Q3=sum(Q3),Q4=sum(Q4),Q5=sum(Q5),
                               Q6=sum(Q6),Q7=sum(Q7),Q8=sum(Q8),Q9=sum(Q9))
   output_filename <- file.path(Output_folder,paste0("MTM Summary - Index Level",format(Sys.time(),"%Y %m %d %Hh%Mm"),".xlsx"))
   system.time(write.xlsx(index_summary, output_filename))
   system.time(saveTable(dbc,"MTMSummary_Index_Level",versionDataFrame(index_summary,current_version)))
}

## Asset Class Level Analysis
for (metric in c("MTM","FV")) {
#	metric <- "MTM"
	asset_class_summary_tmp <- tableFilter(asset_class_summary,"Metric",metric)
	
	asset_class_summary_tmp <- tableFilter(asset_class_summary_tmp,"Component","All_AFS_only")
	asset_class_summary_tmp <- tableFilter(asset_class_summary_tmp,"Scenario",transform_scenario_names("SevereAdverse",ScenarioNames))
	rownames(asset_class_summary_tmp) <- asset_class_summary_tmp$'AssetClass'
	asset_class_summary_tmp$AssetClass <- asset_class_summary_tmp$Metric <- asset_class_summary_tmp$Component <- asset_class_summary_tmp$Scenario <- NULL
	asset_class_summary_tmp["Total",] <- apply(asset_class_summary_tmp,2,sum)
	output_filename <- file.path(Output_folder,paste0("MTMSummary_",metric,File_Output_Key,format(Sys.time(),"%Y %m %d %Hh%Mm"),".xlsx"))
	write.xlsx(asset_class_summary_tmp,output_filename,row.names=TRUE)
}

metric <- "BV"
asset_class_summary_tmp <- tableFilter(asset_class_summary,"Metric",metric)
asset_class_summary_tmp <- tableFilter(asset_class_summary_tmp,"Component","BV_AFS_only")
asset_class_summary_tmp <- tableFilter(asset_class_summary_tmp,"Scenario",transform_scenario_names("SevereAdverse",ScenarioNames))
rownames(asset_class_summary_tmp) <- asset_class_summary_tmp$'AssetClass'
asset_class_summary_tmp$AssetClass <- asset_class_summary_tmp$Metric <- asset_class_summary_tmp$Component <- asset_class_summary_tmp$Scenario <- NULL
asset_class_summary_tmp["Total",] <- apply(asset_class_summary_tmp,2,sum)
output_filename <- file.path(Output_folder,paste0("MTMSummary_",metric,File_Output_Key,format(Sys.time(),"%Y %m %d %Hh%Mm"),".xlsx"))
write.xlsx(asset_class_summary_tmp,output_filename,row.names=TRUE)

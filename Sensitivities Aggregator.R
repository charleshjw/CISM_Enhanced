#########################
#Sensitivities Agregator#
########################
# This script takes the already calculated bond sensitivities (at the CUSIP level) computed by full revaluation
# (rates curve shocked, cash flows discounted with those shocked curves), aggregates them at the Credit Spread
# index level and exports those results to a csv file

# Clear Environment and then Console
rm(list = ls())
cat("\014")
### Load libraries: If package missing load it with: #install.packages("lubridate")
library(lubridate)
library(PCICt)
library(RODBC)
library(reshape2)
library(plyr)


##### Control ########################################################################################################
### Path of Database and database name used to get input data ########################################################
db_path <- "\\\\mfmaog02\\ERMShared\\Treasury Risk\\GTRM_IRRLiq_QA\\CSIM\\Discounting\\CCAR2017\\Production\\"
db_name <- "CSIM_Discounting_and_Summary_Tools.accdb"
# The working directory is chosen as a subfolder to the database. It contains the functions file we need to source
workdingDir <- "R_Files\\"
setwd(paste0(db_path,workdingDir))
source("Supporting functions.R")
######################################################################################################################


#### Data importing for all scenarios
# We get the data from the database: establish connection and load data for all scenarios first
db_conn <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",db_path,db_name))
# Current version running
versions_table <- sqlFetch(db_conn,"Versions")
running_version <- as.character(subset(versions_table,Running>0)$Overall)
# List of scenarios we loop through
scenario_names <- sqlFetch(db_conn,"Scenario_Names_qsel")$DB_Discounting
# File and folder information for file in Excel format
excel_files_info <- sqlFetch(db_conn,"Excel_Files_Location_qsel")
# Dates
dates_table <- sqlFetch(db_conn,"Dates_Master_Table_qsel")
quarters_generic <- c("Q0","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9")
CCAR_horizons <-  format(subset(dates_table,Horizon %in% quarters_generic)$HorizonDate,"%m/%d/%Y")
# CUSIP Master Table (same for all scenaios)
CUSIP_information <- sqlFetch(db_conn,"CUSIP_Master_Table_qsel")
AssetClass_Index_Map <- unique(CUSIP_information[,c("IndexMapping","AssetClass")],row.names = FALSE)
# Remove Alt-A ARM and Alt-A FRM from mapping
AssetClass_Index_Map<-AssetClass_Index_Map[!(AssetClass_Index_Map$AssetClass %in% c('Alt-A ARM','Alt-A FRM')),]
# Get CUSIPs names ordered
CUSIPs_list_ordered <- as.character(CUSIP_information$CUSIPs)
CUSIPs <- droplevels(CUSIP_information$CUSIPs)
# CUSIP spreads forecast and fixed rates yield cruves over 10 quarters, floating rates forwards for all CF dates
all_scenarios_sensitivities <- sqlFetch(db_conn,"CUSIP_Sensitivities_qsel")
## Non scenario specific information/parameters
# Time used for timestamps in output tables
current_time <- format(Sys.time(),"%m/%d/%Y %I:%M:%S %p")


### Current version sensitivities
sensitivities_all_tenors <- subset(all_scenarios_sensitivities,Version %in% running_version)
sensitivities <- sensitivities_all_tenors[,c("Scenario","AsOfDate","CUSIPs","TotalRatesDuration","SpreadDuration")]
colnames(sensitivities) <- c("Scenario","AsOfDate","CUSIPs","IR","CS")

### Add corresponding index spread to each row
sensitivities <- as.data.frame(merge(sensitivities,CUSIP_information[,c("CUSIPs","AssetClass","IndexMapping","AFS_Ratio")],by='CUSIPs',all.x=T))
sensitivities$IR <- sensitivities$IR*sensitivities$AFS_Ratio
sensitivities$CS <- sensitivities$CS*sensitivities$AFS_Ratio

summaryPerIndex <- ddply(sensitivities,c("Scenario","AsOfDate","IndexMapping"),summarize,IR.total=sum(IR),CS.total=sum(CS))
summaryPerIndex <- merge(summaryPerIndex,AssetClass_Index_Map,by='IndexMapping',all.x=T)

write.csv(summaryPerIndex,file=paste0("Sensitivities Asset Class Level.csv"))












## Clear Environment and then Console
#rm(list = ls())
#cat("\014")

#Load libraries
library(lubridate)
library(PCICt)
library(stringr)
library(plyr)
library(RODBC)
library(reshape2)


##### Control ########################################################################################################
#### Path of Database and database name used to get input data ########################################################
#db_path <- "\\\\mfmaog02\\ERMShared\\Treasury Risk\\GTRM_IRRLiq_QA\\CSIM\\Discounting\\CCAR2018\\Production\\"
#db_name <- "CSIM_Discounting_and_Summary_Tools.accdb"
## The working directory is chosen as a subfolder to the database. It contains the functions file we need to source
#workdingDir <- "R_Files\\"
#setwd(paste0(db_path,workdingDir))
#source("Supporting functions.R")
######################################################################################################################
CUSIP_of_interest <- "312965G59"

### Load data from the database
db_conn <- dbc
running_version <- current_version
scenario_names <- ScenarioNames
quarters_generic <- c("Q0","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9")
CCAR_horizons <-  format(subset(Dates_Master,Horizon %in% quarters_generic)$HorizonDate,"%m/%d/%Y")
quarters_specific_list <- as.character(subset(Dates_Master,Horizon %in% quarters_generic)$HorizonQuarter)

CUSIP_Master_info <- fetchTable(dbc,"CUSIP_Master_Table_Permanent",current_version)
rownames(CUSIP_Master_info) <- CUSIP_Master_info$CUSIPs

# CUSIP spreads forecast and fixed rates yield cruves over 10 quarters, floating rates forwards for all CF dates
if (!"all_fixed_rates" %in% ls()) {
	filename = concat_file_path(tableFilter(list_files,"VariableName","FixedRates"))
	all_fixed_rates <- read.csv(filename,header=T,row.names=NULL,sep=",",check.names = F)
}
if (!"all_floating_rates" %in% ls()) {
	filename = concat_file_path(tableFilter(list_files,"VariableName","FloatingRates"))
	all_floating_rates <- read.csv(filename,header=T,row.names=NULL,sep=",",check.names = F)
}

table_name = "Asset_Class_level_parameters"
Asset_Class_level_parameters <- fetchTable(dbc,table_name,current_version)
rownames(Asset_Class_level_parameters) <- Asset_Class_level_parameters$'AssetClass'
colnames(Asset_Class_level_parameters) <- c("VERSION","TimeStamp","AssetClass", "Additive_Multiplicative", "Index level floor", "CUSIP level floor")

table_name <- "Index_Spreads_To_Group_Map";
Spread_Indices_Map <- sqlFetch(dbc,table_name)
colnames(Spread_Indices_Map) <- gsub("_", " ",colnames(Spread_Indices_Map))
#head(Spread_Indices_Map)
rownames(Spread_Indices_Map) <- Spread_Indices_Map$'IndexMapping'

# IV data
IV_data_all_scenarios <- sqlFetch(db_conn,"Independent_Variables_qsel")
# DV data
all_scenarios_index_spreads <- sqlFetch(db_conn,"Index_Spreads_Forecasts_qsel")
# CUSIP spreads and discounting rates
all_scenarios_spreads <- sqlFetch(db_conn,"CUSIP_Spreads_Forecasts_qsel")
# Book Values and Spread Values, keep only useful columns
book_values <- sqlFetch(db_conn,"Book_Value")
book_values <- subset(book_values,Version %in% running_version)
fair_values <- sqlFetch(db_conn,"Fair_Value")
fair_values <- subset(fair_values,Version %in% running_version)
drop_columns <- c("Version","TimeStamp")
book_values <- book_values[,!(colnames(book_values) %in% drop_columns)]
fair_values <- fair_values[,!(colnames(fair_values) %in% drop_columns)]

### CUSIP specific information (CUSIP is referred to as x)
x_master_info <- subset(CUSIP_Master_info,CUSIPs %in% CUSIP_of_interest)
x_index <- as.character(x_master_info$IndexMapping)
x_AC_parameters <- Asset_Class_level_parameters[as.character(x_master_info$AssetClass),]
x_index_info <- Spread_Indices_Map[x_index,]



for (iScenarios in 1:length(scenario_names)){
	current_scenario <- as.character(scenario_names[iScenarios])
	cat("\n###################################################################################################################
					\n### Scenario: ###\n")
	print(current_scenario)
	
	# Get index spread's forecasts
	x_index_spreads <- subset(all_scenarios_index_spreads,Scenario == current_scenario & Index_Name == x_index)
	x_index_spreads[quarters_generic] <- formatC(as.numeric(x_index_spreads[quarters_generic]),format="d",digits=0)
	# Get underlying independent variables data
	x_IV1 <- subset(IV_data_all_scenarios,(Scenario == current_scenario |
						(Scenario == "Historical" & Quarter == quarters_specific_list[1])) & IV_ID == strsplit(x_var1,"\\|")[[1]][1])
	if (strsplit(x_var2,"\\|")[[1]][1] != "0") {
		x_IV2 <- subset(IV_data_all_scenarios,(Scenario == current_scenario |
							(Scenario == "Historical" & Quarter == quarters_specific_list[1])) & IV_ID == strsplit(x_var2,"\\|")[[1]][1])
	} else {
		x_IV2 <- "None"
	}
	if (strsplit(x_var3,"\\|")[[1]][1] != "0") {
		x_IV3 <- subset(IV_data_all_scenarios,(Scenario == current_scenario |
							(Scenario == "Historical" & Quarter == quarters_specific_list[1])) & IV_ID == strsplit(x_var3,"\\|")[[1]][1])
	} else {
		x_IV3 <- "None"
	}
	
	# Get CUSIP's rates
	if (x_master_info$FixedFloating == "Floating") {
		x_rates <- subset(all_floating_rates,Scenario == current_scenario & Rate == paste0(as.character(x_master_info$InternalCurveType),
						" ",as.character(x_master_info$FloatingReset)))[c(1,2,5,8,11,14,17,20,23,26,29,32)]
		nb_cols <- dim(x_rates)[2]
		cols_names <- colnames(x_rates)[(nb_cols-9):nb_cols]
		x_rates[cols_names] <- format(sapply(x_rates[cols_names],as.numeric),digits=2)
		x_rates <- x_rates[,colnames(x_rates) != "Curve"]
	} else {
		x_rates <-as.data.frame(subset(all_fixed_rates,Scenario == current_scenario & Rate == as.character(x_master_info$InternalCurveType)))
		nb_cols <- dim(x_rates)[2]
		cols_names <- colnames(x_rates)[(nb_cols-9):nb_cols]
		x_rates[cols_names] <- format(sapply(x_rates[cols_names],as.numeric),digits=2)
		x_rates <- x_rates[,colnames(x_rates) != "Curve"]
	}
	
	# Get CUSIP's spreads forecasts
	x_spread <- subset(all_scenarios_spreads,Scenario == current_scenario & CUSIPs == CUSIP_of_interest)
	x_spread[quarters_generic] <- formatC(as.numeric(x_spread[quarters_generic]),format="d",digits=0)
	
	# Get CUSIP's book values and fair values
	x_BV <- formatC(subset(book_values,Scenario == current_scenario & CUSIPs == CUSIP_of_interest)[quarters_generic],format="d",big.mark=",")
	x_FV_all <- formatC(subset(fair_values,Scenario == current_scenario & CUSIPs == CUSIP_of_interest & 
							Component == "All")[quarters_generic],format="d",big.mark=",")
	x_FV_ratesOnly <- formatC(subset(fair_values,Scenario == current_scenario & CUSIPs == CUSIP_of_interest & 
							Component == "RatesOnly")[quarters_generic],format="d",big.mark=",")
	# Compute CUSIP's MTM
	x_MTM_all <- formatC(as.numeric(gsub(",","",x_FV_all[quarters_generic]))-as.numeric(gsub(",","",x_BV[quarters_generic])),format="d",big.mark=",")
	x_MTM_rates <- formatC(as.numeric(gsub(",","",x_FV_ratesOnly[quarters_generic]))-as.numeric(gsub(",","",x_BV[quarters_generic])),format="d",big.mark=",")
	
	# Print all
	cat("\n### Master Info ###\n")
	print(x_master_info)
	cat("\n### Mapped to Index ###\n")
	print(x_index)
	cat("\n### Asset Class Parameters ###\n")
	print(x_AC_parameters)
	cat("\n### Mapped Index Information ###\n")
	print(x_index_info)
	cat("\n### Mapped Index's Cluster Information ###\n")
	print(x_cluster_info)
	cat("\n### IV 1 ###\n")
	print(x_var1)
	cat("\n### IV 1 Forecasts ###\n")
	print(x_IV1)
	cat("\n### IV 2 ###\n")
	print(x_var2)
	cat("\n### IV 2 Forecasts ###\n")
	print(x_IV2)
	cat("\n### IV 3 ###\n")
	print(x_var3)
	cat("\n### IV 3 Forecasts ###\n")
	print(x_IV3)
	cat("\n### Index Spread Forecasts ###\n")
	print(x_index_spreads)
	cat("\n### Rates Forecasts ###\n")
	print(x_rates)
	cat("\n### CUSIP Spread Forecasts ###\n")
	print(x_spread)
	cat("\n### MTM: Rates + Spreads impact ###\n")
	print(x_MTM_all)
	cat("\n### MTM: Rates only impact ###\n")
	print(x_MTM_rates)
	cat("\n### Book Values ###\n")
	print(x_BV)
	cat("\n### Fair Values: Rates + Spreads impact ###\n")
	print(x_FV_all)
	cat("\n### Fair Values: Rates only impact ###\n")
	print(x_FV_ratesOnly)
	
	
}





odbcCloseAll()

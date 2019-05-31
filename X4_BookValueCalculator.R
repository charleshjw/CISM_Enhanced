################################
# Book Value Calculator
################################

## Notes
# This file calculates the book value of each CUSIP in local currency (Cash Flows are supposed to come in local ccy,
# the HFV BV is translated into local using the Fx Rate) for the 9 CCAR horizons for each scenario, in local currency
# As input this file requires the CUSIP table, principal cash flows for each scenario, and the HFV file
# Par convergence for book value is supposed to be linear from the CCAR start date to the last principal payment date
# with the ratio at t0 taken as the ratio of "GAAP BV (FAS133)" field (HFV_BV_column_name in the code) in the HFV
# over field "Par" 


# Clear Environment and then Console
#rm(list = ls())
#cat("\014")

#Load libraries
library(stringr)
library(reshape2)
options(digits=15) 

##### Control ########################################################################################################
# Select whether to export results to the database
#export_to_db <- "Yes" # "Yes" to write results in database tables
### Path of Database and database name used to get input data ########################################################
#db_path <- "\\\\mfmaog02\\ERMShared\\Treasury Risk\\Ryan_Chen\\CSIM_Full\\DFAST2018\\Production\\"
#db_name <- "CSIM_Discounting_and_Summary_Tools.accdb"
# The working directory is chosen as a subfolder to the database. It contains the functions file we need to source
#workdingDir <- "R_Files\\"
#setwd(paste0(db_path,workdingDir))
#source("Supporting functions.R")
######################################################################################################################

table_name = "Book_Value"
if (dbHasTable(dbc,table_name)) {
	dbc_table <- fetchTable(dbc,table_name,current_version)
	if (current_version %in% unique(dbc_table$Version) & !re_calculate_book_value) {
		print("Book Values are already calculated. Skip.")
		rerun <- FALSE
	} else {
		rerun <- TRUE
	}
} else {
	rerun <- TRUE
}

if (rerun) {
	### Define names of the columns from the HFV file we will actually use
	HFV_CUSIP_column_name <- "CUSIP"
	HFV_Par_column_name <- "Par"
	HFV_BV_column_name <- "GAAP BV (FAS133)"
	HFV_Impairment_column_name <- "Impaired"
	HFV_system_source_column_name <- "System Source"
	FX_Rate_column_name <- "Fx Rate"
	
	### Load data from the database
	#db_conn <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",db_path,db_name))
	
	## Fetch data through queries: names of scenarios to loop through and information to get data from Excel files
	#scenario_names <- sqlFetch(db_conn,"Scenario_Names_qsel")$DB_Discounting
	scenario_names <- ScenarioNames
	
	#excel_files_info <- sqlFetch(db_conn,"Excel_Files_Location_qsel")
	#CUSIP_Master_info <- sqlFetch(db_conn,"CUSIP_Master_Table_qsel")
	CUSIP_Master_info <- fetchTable(dbc,"CUSIP_Master_Table_Permanent",current_version)
	rownames(CUSIP_Master_info) <- CUSIP_Master_info$CUSIPs
	
	quarters_generic <- c("Q0","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9")
	CCAR_horizons <- format(subset(Dates_Master,Horizon %in% quarters_generic)$HorizonDate,"%m/%d/%Y")
	# Remove trailing zeros from month in dates
	CCAR_horizons <- gsub("03/","3/",CCAR_horizons)
	CCAR_horizons <- gsub("06/","6/",CCAR_horizons)
	CCAR_horizons <- gsub("09/","9/",CCAR_horizons)
	CCAR_as_of_date <- CCAR_horizons[1]
	
	# HFV file: keep only the necessary rows (Intrader) and columns, rename columns, aggregate per CUSIP,
	# and add par_ratio used for the par convergence as (Book Value / Par)
	HFV_file <- fetchTable(dbc, "HFV_File_Permanent",HFV_File_Permanent_)
	HFV_file <- HFV_file[HFV_file[,HFV_system_source_column_name] == "Intrader",]
	HFV_file <- HFV_file[,c(HFV_CUSIP_column_name,HFV_Par_column_name,HFV_BV_column_name,HFV_Impairment_column_name,FX_Rate_column_name )]
	colnames(HFV_file) <- c("CUSIPs","Par","BV","Impaired","FX_Rate")
	HFV_file <- ddply(HFV_file,"CUSIPs",summarise,Par=sum(Par),BV=sum(BV),Impaired=Impaired[1],FX_Rate=FX_Rate[1],par_ratio=BV/Par)
	HFV_file <- HFV_file[complete.cases(HFV_file),] # Remove rows that have NA values
	rownames(HFV_file) <- HFV_file$CUSIPs
	HFV_file <- HFV_file[rownames(HFV_file) %in% CUSIP_Master_info$CUSIPs,]
	HFV_file <- HFV_file[droplevels(CUSIP_Master_info$CUSIPs),]
	#odbcCloseAll()
	
	# Create data frame that will contain the book values for all scenarios (so we only export data once)
	all_scenarios_book_values <- as.data.frame(matrix(0,nrow=0,ncol=(2+length(CCAR_horizons))))
	colnames(all_scenarios_book_values) <- c("Scenario","CUSIPs",quarters_generic)
	
	for (iScenarios in 1:length(scenario_names)) {
	#	iScenarios = 1
	  current_scenario <- as.character(scenario_names[iScenarios])  
	  filename <- concat_file_path(tableFilter(list_files,"VariableName",paste0("BRS_Cashflow_",current_scenario,"_Principal")))
	  BRS_principal <- as.matrix((read.csv(filename,header=T,row.names=1,sep=",",check.names = F)))
	  filename <- concat_file_path(tableFilter(list_files,"VariableName",paste0("QRM_Cashflow_",current_scenario,"_Principal")))
	  QRM_principal <- as.matrix((read.csv(filename,header=T,row.names=1,sep=",",check.names = F)))
	  
	  # Combine QRM and BRS cash flows, and get list of CF dates from it
	  principal_CFs <- CF_transformation(CUSIP_Master_info,QRM_principal,BRS_principal)
	  CF_dates <- colnames(principal_CFs)
	  
	  # Compute outstanding cash flows at each date (the custom function does that by row, but returns the result 
	  # with a matrix in transposed format from the cash flows: dates in rows, CUSIPs in columns)
	  principals_remaining <- t(apply(principal_CFs,1,FUN = function(x) {rev(cumsum(rev(x)))}))
	  # Cash flow dates need to be shifted by one month: as soon as a payment is made that amount is not outstanding
	  # anymore (shift created by using the cumsum function)
	  colnames(principals_remaining) <- c(CCAR_as_of_date,CF_dates[1:(length(CF_dates)-1)])
	  principals_remaining <- principals_remaining[rownames(CUSIP_Master_info),]
	  
	  # Get date of last principal payment for each CUSIP
	  last_principal_date <- CF_dates[apply(principals_remaining,1,FUN = function(x) {max(sum(x>0),1)})]
	  # Get principal left at each of the CCAR horizons
	  CCAR_horizons_principal_left <- principals_remaining[,CCAR_horizons]
	  
	  ## Compute the Par over BV ratio for the 9 CCAR horizons using linear convergence
	  par_convergence <- matrix(0,nrow=length(CUSIP_Master_info$CUSIPs),ncol=length(CCAR_horizons))
	  rownames(par_convergence) <- rownames(CUSIP_Master_info)
	  colnames(par_convergence) <- CCAR_horizons
	  # At t0 our ratio has been determined while formatting the HFV file
	  par_convergence[,CCAR_as_of_date] <- as.numeric(HFV_file[rownames(CUSIP_Master_info),"par_ratio"])
	  # Compute time left to maturity for each CUSIP at each horizon date:
	  # Compute time left to maturity at Q0, then substract time passed between Q0 and each CCAR horizon date
	  time_left_to_maturity <- as.numeric(as.Date(last_principal_date,"%m/%d/%Y")-as.Date(CCAR_horizons[1],"%m/%d/%Y"))
	  CCAR_horizons_from_Q0 <- as.numeric(as.Date(CCAR_horizons,"%m/%d/%Y")-as.Date(CCAR_horizons[1],"%m/%d/%Y"))
	  time_left_to_maturity <- do.call(cbind,replicate(length(CCAR_horizons),time_left_to_maturity,simplify=FALSE))
	  time_left_to_maturity <- sweep(time_left_to_maturity,2,CCAR_horizons_from_Q0)
	  # Set time left to maturity to 0 if bond matured
	  time_left_to_maturity[time_left_to_maturity<0] <- 0
	  # Linearly interpolate Par/BV ratio using t0 value and a value of 1 at maturity
	  par_convergence <- (time_left_to_maturity/time_left_to_maturity[,1])*(par_convergence[,1]-1)+1
	  # Set impaired CUSIPs ratio constant over time
	  impaired_CUSIPs_index <- which(HFV_file$Impaired == "Y")
	  par_convergence[impaired_CUSIPs_index,] <- par_convergence[impaired_CUSIPs_index,1]
	    
	  ## Compute BV as par_convergence ratio * principal left at each date
	  # At t0 we use the value from the HFV file
	  # format(round(x, 2), nsmall = 2) only shows k decimals in our numbers
	  book_value <- as.data.frame(format(round(par_convergence * CCAR_horizons_principal_left,2), nsmall = 2))
	  colnames(book_value) <- quarters_generic
	  book_value$CUSIPs <- rownames(CUSIP_Master_info)
	  book_value$Q0 <- HFV_file$BV/HFV_file$FX_Rate
	  book_value$Scenario <- current_scenario
	  
	  ## Append current scenario's results to aggregate results
	  all_scenarios_book_values <- rbind(all_scenarios_book_values,book_value[,colnames(all_scenarios_book_values)])
	}
	
	## Save all scenarios' book values in Access table
	if (export_to_db == "Yes") {
	  current_time <- format(Sys.time(),"%m/%d/%Y %I:%M:%S %p")
	  all_scenarios_book_values$Version <- running_version
	  all_scenarios_book_values$TimeStamp <- current_time
	  output_aggregate_table <- all_scenarios_book_values[c("Version","TimeStamp","Scenario","CUSIPs",quarters_generic)]
	  
	  table_name = "Book_Value"
	  saveTable(dbc,table_name,output_aggregate_table)
	}
	#	The variable output_aggregate_table now contains the book value for all cusip under all scenarios.
	#odbcCloseAll()
}


# CRITICAL TOOLS DOCUMENTATION	
# 
# File Name|BookValueCalculator.r
# 
# Overview/PURPOSE|Compute book values of each bond in each CCAR scenario for CCAR OCI discounting
# 
# Last updated date|8/22/2016
# Tester Name|
# Last testing date|
# 
# Filename and path|\\ntfmao01\ERMShared\Portfolio VaR\CSIM\Production\Input Data\BookValue\
# Current date|1/12/2017
# Update frequency|NA
# Owner|Remy Pasco, Jochen Steinbrecher
# Developer|Remy Pasco
# 
# Number of worksheets|NA
# 
# Contains formulas|Yes
# Contains macros|No
# Contains pivot tables|No
# Other programming|Yes
# 
# Technical specifications|R
# 
# 
# List of Inputs|Data from CSIM main database and CCAR cash flows
# 
# 
# 
# Distribution list|Not distributed directly. Final CCAR results only distributed to State Street CCAR team
# 
# Information Classification|Limited Access
# Complexity Level|Intermediate
# Impact Risk|Significant
# Overall risk rating|High
# 
# 
# Change log
# Date Changed|8/22/2016
# Approved By|Remy Pasco
# Implemented By|Remy Pasco
# Tested By|
# Brief Description of Change|Version 1.0; see purpose/overview


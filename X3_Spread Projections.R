################################
#Spreads projections calculator
################################

# Notes: this file calculates the spreads forecasts for both indices and CUSIPs over the 9 CCAR horizons
# To only compute the index or CUSIP spreads change forecast_spread_indices and forecast_CUSIP_spreads as needed
# The data is exported to the database in the tables Index_Spreads_Forecasts and CUSIP_Spreads_Forecasts if
# the variable export_to_db is set as "Yes"

##### Setup: Clear Environment and then Console
#rm(list = ls())
#cat("\014")
### Load libraries: If package missing load it with: #install.packages("lubridate")
library(lubridate)
library(PCICt)
library(stringr)
library(plyr)
library(RODBC)
library(reshape2)


##### Control ########################################################################################################
# Parameters to select what to run and whether we export results to the Access database
#export_to_db <- "Yes" # Yes to export
forecast_spread_indices <- "No" # "Yes" to forecast spread indices
forecast_CUSIP_spreads <- "Yes" # "Yes" to forecast CUSIP spreads
### Path of Database and database name used to get input data ########################################################
#db_path <-"\\\\mfmaog02\\ERMShared\\Treasury Risk\\Ryan_Chen\\CSIM_Full\\DFAST2018\\Production\\"
#db_name <- "CSIM_Discounting_and_Summary_Tools.accdb"
######################################################################################################################


################ Functions needed for spreads forecasts later in this file
### Function that compute spreads forecasts for indices
index_spread_calculator<-function(all_dates,CCAR_date_index,spread_indices,spread_indices_equations_coefficients,spread_indices_equations_variables,IV_transformations_scenario)
{
  ### This function returns the indices' changes for the 9 CCAR quarters of interest, as well as the indices' levels for the 10 quarters (start + 9 quarters)
  ### The result is a list with the first element being the changes matrix, the second the levels matrix for the given scenario
  
  ## Create and fill matrix with each index's spreads for CCAR horizons (start + 9 quarters) for each scenario
  ## by first computing the changes for each quarter and then filling the actual levels
  forecast_dates <- all_dates[(CCAR_date_index):(CCAR_date_index+9)]
  # Compute changes for this scenario
  scenario_index_spreads_changes <- matrix(0,nrow=(dim(spread_indices)[1]),ncol=9)
  rownames(scenario_index_spreads_changes) <- rownames(spread_indices)
  colnames(scenario_index_spreads_changes) <- forecast_dates[2:10]
  for (iIndex in 1:dim(spread_indices)[1]){
    currentIndexName <- rownames(scenario_index_spreads_changes)[iIndex]
    for (iDate in 1:9){
      coefficients <- spread_indices_equations_coefficients[currentIndexName,]
      IV_values <- c(0,0,0)
      for (iRegressionVariables in 1:3) { # Loop through each of the 3 variables of the regression's equation
        IV_name <- spread_indices_equations_variables[currentIndexName,iRegressionVariables,1]
        if (IV_name!="") {
          IV_transformation <- spread_indices_equations_variables[currentIndexName,iRegressionVariables,2]
          IV_lag <- strtoi(substr(spread_indices_equations_variables[currentIndexName,iRegressionVariables,3],2,2))
          IV_values[iRegressionVariables] <- IV_transformations_scenario[all_dates[CCAR_date_index+iDate-IV_lag],IV_name,IV_transformation]
        } else {
          IV_values[iRegressionVariables] <- 0
        }
      }
      scenario_index_spreads_changes[iIndex,iDate] <- sum(coefficients*IV_values)
    }
  }
  
  ## Go back to levels
  # Add a column of 0 to the changes matrix that corresponds to time 0
  scenario_index_spreads_changes <- cbind(matrix(0,nrow=(dim(scenario_index_spreads_changes)[1]),ncol=1),scenario_index_spreads_changes)
  # Set up matrix: rows are the indices, columns are the 10 dates
  scenario_index_spreads  <- matrix(0,nrow=(dim(spread_indices)[1]),ncol=10)
  rownames(scenario_index_spreads) <- rownames(spread_indices)
  colnames(scenario_index_spreads) <- forecast_dates
  # We need the starting spread at the CCAR as of date
  starting_spreads <- spread_indices[rownames(scenario_index_spreads),'Index start spread BPS']
  scenario_index_spreads[,1] <- starting_spreads # as.numeric(levels(starting_spreads))[starting_spreads]
  # Minimum spreads and flooring information is needed
  minimum_spreads <- spread_indices[rownames(scenario_index_spreads),'Index Historic Minimum']
  # minimum_spreads <- as.numeric(levels(minimum_spreads))[minimum_spreads]
  index_flooring <- as.character(spread_indices[rownames(scenario_index_spreads),'Index level floor'])
  index_flooring_bool <- matrix(0,nrow=length(minimum_spreads))
  index_flooring_bool[index_flooring=="Y"] <- 1
  index_flooring_bool[index_flooring=="N"] <- 0
  for (iDate in 1:9){
    newLevel <- scenario_index_spreads[,1]+rowSums(scenario_index_spreads_changes[rownames(scenario_index_spreads),1:(iDate+1)])
    belowMin <- (newLevel<minimum_spreads)
    scenario_index_spreads[,iDate+1] <- belowMin*index_flooring_bool*minimum_spreads+(1-belowMin*index_flooring_bool)*newLevel
  }
  
  indices_matrices <- list(scenario_index_spreads_changes[,2:10],scenario_index_spreads)
  return (indices_matrices)
}  

## Function that compute spreads forecasts for CUSIPs
cusip_spread_forecast_calculator<-function(forecast_dates,CUSIP_Master_info,scenario_index_spreads)
{
#	forecast_dates = quarters_specific_list
  # The next 2 parameters are minimum threshold above which an index spread must be over for all 10 quarters,
  # and above which a CUSIP's starting spread need to be above for the multiplicative approach to be applied
  # If we're not meeting these 2 conditions then we fall back to an additive approach
  index_threshold_mult_approach <- 0 # Chosen parameter used in the determination of additive vs multiplicative approach
  cusip_threshold_mult_approach <- 0 # Chosen parameter used in the determination of additive vs multiplicative approach
  scenario_cusip_spreads <- as.data.frame(matrix(0,nrow=dim(CUSIP_Master_info)[1],ncol=length(forecast_dates)))
  rownames(scenario_cusip_spreads) <- rownames(CUSIP_Master_info)
  colnames(scenario_cusip_spreads) <- forecast_dates
  scenario_cusip_spreads[,1] <- CUSIP_Master_info[rownames(scenario_cusip_spreads),'StartingSpread_bps']
  scenario_cusip_spreads$UsedApproach <- scenario_cusip_spreads$ChosenApproach <- ""
  
  for (iCusip in 1:dim(scenario_cusip_spreads)[1]) {
    currentCusip <- rownames(scenario_cusip_spreads)[iCusip]
    currentCusipIndex <- as.character(CUSIP_Master_info[currentCusip,'IndexMapping'])
    if (currentCusipIndex == 'NO_SPREAD') {
      scenario_cusip_spreads[iCusip,2:10] <- scenario_cusip_spreads[iCusip,1]
      scenario_cusip_spreads$ChosenApproach[iCusip] <- "NO_SPREAD"
      scenario_cusip_spreads$UsedApproach[iCusip] <- "NO_SPREAD"
    }
    else {
      # Check for additive vs mulitplicative approach: chosen approach is the input parameter, but used approach
      # is the one we actually use: it depends on a few parameters. Additive stays the same, but multiplicative requires some conditions
      # Conditions: CUSIP starting spread positive, and index levels starts and stays above the chosem threshold
      # index_threshold_mult_approach for all 9Q of that scenario
#			CUSIP_Master_info[currentCusip,]
      additive_multiplicative_approach <- as.character(CUSIP_Master_info[currentCusip,'Additive_Multiplicative'])
      scenario_cusip_spreads$ChosenApproach[iCusip] <- additive_multiplicative_approach
      if (additive_multiplicative_approach == 'Multiplicative') {
        if((scenario_cusip_spreads[iCusip,1] <= cusip_threshold_mult_approach) | (any(scenario_index_spreads[currentCusipIndex,] <= index_threshold_mult_approach))) {
          additive_multiplicative_approach <- 'Additive'
        }
      }
      scenario_cusip_spreads$UsedApproach[iCusip] <- additive_multiplicative_approach
      
      for (iDate in 2:length(forecast_dates)) {
        currentDate <- forecast_dates[iDate]
        # Additive or multiplicative based on effective approach
        if (additive_multiplicative_approach == 'Additive') {
          newLevel <- scenario_cusip_spreads[iCusip,1]+scenario_index_spreads[currentCusipIndex,currentDate]-scenario_index_spreads[currentCusipIndex,1]
        } else { # Multiplicative choice is chosen
          newLevel <- (scenario_cusip_spreads[iCusip,1])*(scenario_index_spreads[currentCusipIndex,currentDate])/(scenario_index_spreads[currentCusipIndex,1])
        }
        
        # Check whether we are flooring at the CUSIP level or not
        if (CUSIP_Master_info[currentCusip,'CUSIP level floor']=='Y') {
          scenario_cusip_spreads[iCusip,iDate] <- max(newLevel,CUSIP_Master_info[currentCusip,'minimum_spread'])
        } else {
          scenario_cusip_spreads[iCusip,iDate] <- newLevel
        }
      }
      
    }
  }
  # Put the 2 approach columns at the beginning of the data frame
  chosen_col_idx <- grep('ChosenApproach',names(scenario_cusip_spreads))
  used_col_idx <- grep('UsedApproach',names(scenario_cusip_spreads))
  scenario_cusip_spreads <- scenario_cusip_spreads[,c('ChosenApproach','UsedApproach',names(scenario_cusip_spreads)[-c(chosen_col_idx,used_col_idx)])]
  return (scenario_cusip_spreads)
}
################ End of Functions

#### Data importing for all scenarios, used for Indices and Spreads forecasts
# We get the data from the database: establish connection
#db_conn <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",db_path,db_name))
# Parameters at Asset Class level: Index level flooring, CUSIP level flooring, Additive vs Multiplicative approach
table_name = "Asset_Class_level_parameters"
Asset_Class_level_parameters <- fetchTable(dbc,table_name,current_version)
rownames(Asset_Class_level_parameters) <- Asset_Class_level_parameters$'AssetClass'
colnames(Asset_Class_level_parameters) <- c("VERSION","TimeStamp","AssetClass", "Additive_Multiplicative", "Index level floor", "CUSIP level floor")
# Spread Indices equations at cluster level
#table_name = "Index_Spreads_Group_Level_Equations"
#PC1_Spread_Indices_Equations <- fetchTable(dbc,table_name, current_version)
#rownames(PC1_Spread_Indices_Equations) <- PC1_Spread_Indices_Equations$'GroupName'
# Index spread to cluster map with load factor, starting Spread and historical minimum
table_name <- "Index_Spreads_To_Group_Map";
Spread_Indices_Map <- sqlFetch(dbc,table_name)
colnames(Spread_Indices_Map) <- gsub("_", " ",colnames(Spread_Indices_Map))
#head(Spread_Indices_Map)
rownames(Spread_Indices_Map) <- Spread_Indices_Map$'IndexMapping'
# Dates
quarters_generic <- c("Q0","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9")
quarters_specific_list <-  as.character(subset(Dates_Master,Horizon %in% quarters_generic)$HorizonDate)
#dates_table <- sqlFetch(db_conn,"Dates_Master_Table_qsel")
#CCAR_portfolio_date <- as.character(dates_table$HorizonQuarter[dates_table$Horizon == 'Q0'])
#quarters_generic <- c("Q0","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9")
#quarters_specific_list <- as.character(subset(dates_table,Horizon %in% quarters_generic)$HorizonQuarter)
# List of scenarios we loop through
scenario_names <- ScenarioNames
#scenario_names <- sqlFetch(db_conn,"Scenario_Names_qsel")$DB_Discounting
# Current time for timestamp in export
current_time <- format(Sys.time(),"%m/%d/%Y %I:%M:%S %p")
# Current version
running_version <- current_version

##### Index spreads forecasts
if (forecast_spread_indices == "Yes") {
  
  # Get IV data: restrict to scenario, reformat from column to table, drop quarter column (so we only have numbers)
  # and change format to matrix so we can operate on numbers (factors of data frame complicate things)
  IV_data_all_scenarios <- sqlFetch(db_conn,"Independent_Variables_qsel")
  
  ### Scenario independent: get each index's regression variables and coefficients in a clean way
  ## First get all index relevant data in the same dataframe
  spread_indices <- as.data.frame(merge(Spread_Indices_Map,PC1_Spread_Indices_Equations,by='GroupName'))
  spread_indices <- as.data.frame(merge(spread_indices,Asset_Class_level_parameters,by='AssetClass'))
  rownames(spread_indices) <- spread_indices$IndexMapping
  
  ## Create a matrix to contain the coefficients of each index's equations, and a matrix containing the variables with transformation and lag
  factor_loadings <- as.double(spread_indices[,'Load Factor'])
  spread_indices_equations_coefficients <- matrix(0,nrow=(dim(spread_indices)[1]),ncol=3)
  spread_indices_equations_coefficients <- factor_loadings * spread_indices[,c('Var 1 Coeff','Var 2 Coeff','Var 3 Coeff')]
  rownames(spread_indices_equations_coefficients) <- rownames(spread_indices)
  
  # Variable matrix: rows correspond to indices, columns correspond to variables 1/2/3, third dimension has the IV name, the transformation
  # (level, QoQ diff, QoQ percent change) and the lag (0,1,2 quarters). In summary: [,,1]: name, [,,2]: transformation, [,,3]: lag
  spread_indices_equations_variables <- as.matrix(spread_indices[,c('Var 1','Var 2','Var 3')])
  spread_indices_equations_variables <- cbind(spread_indices_equations_variables,spread_indices_equations_variables,spread_indices_equations_variables)
  dim(spread_indices_equations_variables) <- c(dim(spread_indices)[1],3,3)
  rownames(spread_indices_equations_variables) <- rownames(spread_indices)
  
  # Fill the matrix by going through each index's variable and splitting the string representing each of the 3 (at most) regression variables
  for (iIndex in 1:dim(spread_indices)[1]) {
    for (iVariable in 1:3) {
      # Get the variable with name, transformation and lag concatenated, eg: "TED_Spread|QoQ Difference|L0"
      # Split it given the separator used is '|' (need to escape it since it's a special character)
      rawEntry <- spread_indices_equations_variables[iIndex,iVariable,1]
      if (rawEntry!=0) {
        splitString <- strsplit(rawEntry,"\\|")[[1]]
        spread_indices_equations_variables[iIndex,iVariable,1] <- splitString[1]
        spread_indices_equations_variables[iIndex,iVariable,2] <- splitString[2]
        spread_indices_equations_variables[iIndex,iVariable,3] <- splitString[3]
      } else {
        spread_indices_equations_variables[iIndex,iVariable,1] <- ""
        spread_indices_equations_variables[iIndex,iVariable,2] <- ""
        spread_indices_equations_variables[iIndex,iVariable,3] <- ""
      }    
    }
  }
  
  # Loop through each scenario
  for (iScenarios in 1:length(scenario_names)) {
    current_scenario <- as.character(scenario_names[iScenarios])
    
    ### Get scenario specific IV history and forecasts. Then format matrix.
    IV_data_scenario <- subset(IV_data_all_scenarios, Scenario == 'Historical' | Scenario == current_scenario )
    IV_data_scenario <- dcast(IV_data_scenario,Quarter ~ IV_ID, value.var="IV_Value")
    rownames(IV_data_scenario) <- IV_data_scenario$Quarter
    IV_data_scenario <- as.matrix(IV_data_scenario[,(colnames(IV_data_scenario) != 'Quarter')])
    
    
    ##### IV transformations
    ### Compute IV transformations: levels, quarter on quarter difference, QoQ percent change
    ## Row names are the dates, column dates the variables, third dimension is the transformation
    # Rows: We only keep data that goes back a year before the CCAR date (4Q) and 9 quarters after it (last CCAR horizon)
    all_dates <- rownames(IV_data_scenario)
    CCAR_date_index <- which(all_dates==CCAR_portfolio_date)
    IV_transformations_dates <- all_dates[(CCAR_date_index-4):(CCAR_date_index+9)]
    nb_dates <- length(IV_transformations_dates)
    # Columns: We keep all the Independent Variables
    IV_list <- colnames(IV_data_scenario)
    # Third dimension: first levels, then quarter on quarter difference, then percentage difference
    variable_transformations <- c("LEVEL","FIRST_DIFF","PERCENT_CHG")
    ## Set up matrices: for differences and percentage we leave values as 0 for first date, and if we can't do a percent change (first level of 0)
    dummy_matrix <- matrix(0,nrow=nb_dates,ncol=length(IV_list))
    dummy_matrix <- as.double(dummy_matrix)
    IV_transformations_scenario <- cbind(dummy_matrix,dummy_matrix,dummy_matrix)
    dim(IV_transformations_scenario) <- c(nb_dates,length(IV_list),length(variable_transformations))
    dimnames(IV_transformations_scenario) <- list(IV_transformations_dates,IV_list,variable_transformations)
    # Levels: no transformation
    IV_transformations_scenario[,,1] <- IV_data_scenario[IV_transformations_dates,]
    # Quarter on Quarter difference: FIRST_DIFF
    IV_transformations_scenario[2:nb_dates,,2] <- diff(IV_data_scenario[IV_transformations_dates,], lag=1, differences=1)
    # Percentage change (as (quarter on quarter difference)/Level ): PERCENT_CHG
    IV_transformations_scenario[2:nb_dates,,3] <- IV_transformations_scenario[2:nb_dates,,2]/IV_data_scenario[IV_transformations_dates[1:(nb_dates-1)],]
    
    
    ##### Spread Index forecasts
    ## Compute spread changes using the IVs data computed above, and translate back to levels (all done in the function)
    ## scenario_index_spreads contains each index's spreads for CCAR horizons (start + 9 quarters) using the function index_spread_calculator
    scenario_index_spreads_data<-index_spread_calculator(all_dates,CCAR_date_index,spread_indices,spread_indices_equations_coefficients,spread_indices_equations_variables,IV_transformations_scenario)
    scenario_index_spreads_changes<-scenario_index_spreads_data[[1]]
    scenario_index_spreads<-scenario_index_spreads_data[[2]]
    
    
    ### Export data to database
    if (export_to_db == "Yes") {
      ## Data is appended to existing table Index_Spreads_Forecasts
      db_conn <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",db_path,db_name))
      # Replace actual dates with generic quarters and reformat output table
      output_table <- as.data.frame(scenario_index_spreads)
      colnames(output_table) <- quarters_generic
      output_table$Index_Name <- rownames(output_table)
      output_table$Scenario <- current_scenario
      output_table$Version <- running_version
      output_table$TimeStamp <- current_time
      output_table <- output_table[,c("Version","TimeStamp","Scenario","Index_Name",quarters_generic)]
      sqlSave(db_conn,output_table,tablename="Index_Spreads_Forecasts",append=TRUE,rownames=FALSE,safer=TRUE,fast=TRUE)
    }
  }
} # End of spread indices forecasts
#odbcCloseAll()

##### CUSIPs spreads forecasts
if (forecast_CUSIP_spreads == "Yes"){
  ## All scenarios index spreads forecasts
  table_name <- "Index_Spreads_Forecasts"
  all_scenarios_index_spreads <- fetchTable(dbc, table_name, current_version)
  all_scenarios_index_spreads$'VERSION' <- all_scenarios_index_spreads$'TimeStamp' <- NULL
#  head(all_scenarios_index_spreads)
	
  ## CUSIP Master Table and CUSIPs starting spreads
  CUSIP_Master_info <- fetchTable(dbc,"CUSIP_Master_Table_Permanent",current_version)
  rownames(CUSIP_Master_info) <- CUSIP_Master_info$CUSIPs
  CUSIP_starting_spreads <- fetchTable(dbc,"CUSIP_Starting_Spreads",current_version)
  match_starting_spreads_to_master_table <- function(CUSIP_Master_info,CUSIP_starting_spreads) {
	  CUSIP_Master_info$'key' = paste0(CUSIP_Master_info$'CUSIPs',CUSIP_Master_info$'CF_Source')
	  CUSIP_starting_spreads$'key' = paste0(CUSIP_starting_spreads$'CUSIPs',substr(CUSIP_starting_spreads$'SpreadSource',0,3))
	  return(merge(CUSIP_Master_info,CUSIP_starting_spreads,by="key",suffixes=c("",".y"))[,c("CUSIPs","StartingSpread_bps")])
  }
  CUSIP_starting_spreads <- match_starting_spreads_to_master_table(CUSIP_Master_info,CUSIP_starting_spreads)
  # Get all index relevant data  of spread indices
#  spread_indices <- as.data.frame(merge(Spread_Indices_Map,PC1_Spread_Indices_Equations,by='GroupName',all.x=TRUE))
	spread_indices <- as.data.frame(Spread_Indices_Map)
  spread_indices <- as.data.frame(merge(spread_indices,Asset_Class_level_parameters,by='AssetClass'))
  rownames(spread_indices) <- spread_indices$IndexMapping
  
#  a <- unique(CUSIP_Master_info$IndexMapping)
#  b <- unique(spread_indices$IndexMapping)
#  a[!a%in%b]
#  b[!b%in%a]
  
  ## Aggregate all CUSIP information for forecasts in the table CUSIP_Master_info: starting spreads,
  ## index information for flooring if necessary and additive vs mulitplicative
  CUSIP_Master_info <- as.data.frame(merge(CUSIP_starting_spreads,CUSIP_Master_info,by='CUSIPs'))
  
  index_columns_needed <- c('Index start spread BPS','Index Historic Minimum','Additive_Multiplicative','CUSIP level floor','IndexMapping')
  CUSIP_Master_info <- as.data.frame(merge(CUSIP_Master_info,spread_indices[,index_columns_needed],by='IndexMapping',all.x=T))
  CUSIP_minimums_component <- cbind(CUSIP_Master_info[,c('StartingSpread_bps','Index Historic Minimum')],0)
  CUSIP_Master_info$minimum_spread <- apply(CUSIP_minimums_component,1,min)
  rownames(CUSIP_Master_info) <- CUSIP_Master_info$CUSIPs
  CUSIPs_ordered <- rownames(CUSIP_Master_info) # Used to order data frame after different operations
  CUSIP_Master_info <- CUSIP_Master_info[CUSIPs_ordered,] # the merging operations modifies the order of the rows
  
#  head(CUSIP_Master_info)
	scenario_cusip_spreads_all <- data.frame()
  for (iScenarios in 1:length(scenario_names)) {
#	  iScenarios <- 1
    current_scenario <- as.character(scenario_names[iScenarios])
    print(paste0("Execute CUSIP Spread Forecast for Scenario ",current_scenario))
    # Format index spreads forecasts 
    scenario_index_spreads <- subset(all_scenarios_index_spreads,Scenario == current_scenario)
    rownames(scenario_index_spreads) <- scenario_index_spreads$Index_Name
    scenario_index_spreads$Index_Name <- scenario_index_spreads$Scenario <- NULL
    colnames(scenario_index_spreads) <- quarters_specific_list
	
	# build in controls to verify data accuracy
	a <- unique(CUSIP_Master_info$'IndexMapping')
	b <- unique(rownames(scenario_index_spreads))
	b <- c(b,"NO_SPREAD")
	non_exist_index <- a[!a%in%b]
	if (length(non_exist_index) > 0) {
		print(non_exist_index)
		stop("Some index spread forecast is not in the database. Check!")
	}
    ### Forecast each CUSIP's spread based on indices forecasts and CUSIP to index mapping
    ## Use function cusip_spread_forecast_calculator for spread forecasting for each scenario
    scenario_cusip_spreads <- cusip_spread_forecast_calculator(quarters_specific_list,CUSIP_Master_info,scenario_index_spreads)
	scenario_cusip_spreads$Scenario <- current_scenario
	scenario_cusip_spreads_all <- rbind(scenario_cusip_spreads_all,scenario_cusip_spreads)
  }
  
  if (export_to_db == "Yes") {
	  ## Data is appended to existing table CUSIP_Spreads_Forecasts
	  output_table <- as.data.frame(scenario_cusip_spreads_all)
	  names(output_table)[names(output_table) %in% quarters_specific_list] <- quarters_generic
	  output_table$CUSIPs <- rownames(output_table)
#	  output_table$Scenario <- current_scenario
	  output_table <- output_table[,c("Scenario","CUSIPs","ChosenApproach","UsedApproach",quarters_generic)]
	  table_name = "CUSIP_Spreads_Forecasts"
#	  dim(versionDataFrame(output_table,current_version))
	  saveTable(dbc,table_name,versionDataFrame(output_table,current_version))
  }
} # End of CUSIPs indices forecasts

#odbcCloseAll()



# CRITICAL TOOLS DOCUMENTATION	
# 
# File Name|Spread Projections.r
# 
# Overview/PURPOSE|Forecast spreads for indices and bonds at each CCAR horizon for each scenario for CCAR OCI discounting
# 
# Last updated date|10/31/2016
# Tester Name|
# Last testing date|
# 
# Filename and path|\\ntfmao01\ERMShared\Portfolio VaR\CSIM\Production\R_Files\
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
# List of Inputs|Data from CSIM main database
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
# Date Changed|10/31/2016
# Approved By|Remy Pasco
# Implemented By|Remy Pasco
# Tested By|
# Brief Description of Change|Version 1.0; see purpose/overview

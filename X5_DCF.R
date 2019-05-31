##########
#DCF Tool#
##########

# Clear Environment and then Console
#rm(list = ls())
#cat("\014")
### Load libraries: If package missing load it with: #install.packages("lubridate")
library(lubridate)
library(PCICt)
#library(RODBC)
library(reshape2)
library(openxlsx)

Output_folder <- file.path(Output_root,"Discounting")
if (!dir.exists(Output_folder)) {
	dir.create(Output_folder)
}

##### Control ########################################################################################################
# Select if the main portfolio should be run, reinvestments, and whether we export results to the database
#### Data importing for all scenarios
# We get the data from the database: establish connection and load data for all scenarios first
db_conn <- dbc
# Current version running
running_version <- current_version

# List of scenarios we loop through
scenario_names <- ScenarioNames

# Dates
quarters_generic <- c("Q0","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9")
CCAR_horizons <-  format(subset(Dates_Master,Horizon %in% quarters_generic)$HorizonDate,"%m/%d/%Y")

# CUSIP Master Table (same for all scenaios)
CUSIP_information <- fetchTable(dbc,"CUSIP_Master_Table_Permanent",current_version)
if ("selected_cusips"%in%ls()) {
	CUSIP_information <- CUSIP_information[CUSIP_information$CUSIPs%in%selected_cusips,]
	print("DCF: Found Selected CUSIPs. Only run DCF for those CUSIPs")
	rm(list = "selected_cusips")
}
CUSIP_information <- CUSIP_information[,c("CUSIPs","AssetClass","FixedFloating","Currency","InternalCurveType",
				"CF_Source","FloatingReset","PurchaseDate","PaymentFrequency")]
CUSIP_information$PurchaseDate <- format(as.Date(CUSIP_information$PurchaseDate,"%Y-%m-%d"),"%m/%d/%Y")
CUSIP_information$`InternalCurveType` <- as.character(CUSIP_information$`InternalCurveType`)
# Get CUSIPs names ordered
#run a subset of cusips
CUSIPs_list_ordered <- as.character(CUSIP_information$CUSIPs)
CUSIPs <- droplevels(CUSIP_information$CUSIPs)

# CUSIP spreads forecast and fixed rates yield cruves over 10 quarters, floating rates forwards for all CF dates
if (!"all_fixed_rates" %in% ls()) {
	filename = concat_file_path(tableFilter(list_files,"VariableName","FixedRates"))
	all_fixed_rates <- read.csv(filename,header=T,row.names=NULL,sep=",",check.names = F)
}
if (!"all_floating_rates" %in% ls()) {
	filename = concat_file_path(tableFilter(list_files,"VariableName","FloatingRates"))
	all_floating_rates <- read.csv(filename,header=T,row.names=NULL,sep=",",check.names = F)
}

all_scenarios_spreads <- fetchTable(dbc,"CUSIP_Spreads_Forecasts",current_version)

## Non scenario specific information/parameters
# Time used for timestamps in output tables
current_time <- format(Sys.time(),"%m/%d/%Y %I:%M:%S %p")
# Tool assumes that actual days in a year are used for discounting
days_in_year <- 360

# File and folder information for file in Excel format
#excel_files_info <- sqlFetch(db_conn,"Excel_Files_Location_qsel")

if (run_main_portfolio == "Yes") {
	Fair_Value_ALL <- data.frame()
	Fair_Value_Rate_Only <- data.frame()
	
	for (iScenarios in 1:length(scenario_names)) {
		current_scenario <- as.character(scenario_names[iScenarios])
		
		### Load scenario specific data
		## Cash flows from Excel files: Principal and Interest from BRS and QRM
		
		filename <- concat_file_path(tableFilter(list_files,"VariableName",paste0("BRS_Cashflow_",current_scenario,"_Principal")))
		BRS_scenario_principal <- as.matrix((read.csv(filename,header=T,row.names=1,sep=",",check.names = F)))
		filename <- concat_file_path(tableFilter(list_files,"VariableName",paste0("BRS_Cashflow_",current_scenario,"_Interest")))
		BRS_scenario_interest <- as.matrix((read.csv(filename,header=T,row.names=1,sep=",",check.names = F)))
		filename <- concat_file_path(tableFilter(list_files,"VariableName",paste0("QRM_Cashflow_",current_scenario,"_Principal")))
		QRM_scenario_principal <- as.matrix((read.csv(filename,header=T,row.names=1,sep=",",check.names = F)))
		filename <- concat_file_path(tableFilter(list_files,"VariableName",paste0("QRM_Cashflow_",current_scenario,"_Interest")))
		QRM_scenario_interest <- as.matrix((read.csv(filename,header=T,row.names=1,sep=",",check.names = F)))
		
		## CUSIP Spreads: set rownames equal to CUSIPs, then get rid of CUSIPs column,
		# change column names to be specific dates, and reorder rows to match other matrices
				
		scenario_spread <- subset(all_scenarios_spreads,Scenario == transform_scenario_names(current_scenario,unique(all_scenarios_spreads$'Scenario')))[,c("CUSIPs",quarters_generic)]
		rownames(scenario_spread) <- scenario_spread$CUSIPs
		scenario_spread$CUSIPs <- NULL
		colnames(scenario_spread) <- CCAR_horizons
		scenario_spread <- as.matrix(scenario_spread[CUSIPs_list_ordered,])
		
		spread_scenarios <- unique(all_floating_rates$'Scenario')
		## Benchmark rates for floating rate securities for each scenario. Set rownames as rate names
		scenario_benchmark <- subset(all_floating_rates,Scenario == transform_scenario_names(current_scenario,spread_scenarios))
		rownames(scenario_benchmark) <- scenario_benchmark$Rate
		scenario_benchmark$Rate <- scenario_benchmark$Scenario <- NULL
		
		## Yield curves for fixed rate securities
		scenario_fixed_curves <- subset(all_fixed_rates,Scenario == transform_scenario_names(current_scenario,spread_scenarios))
		scenario_fixed_curves$Scenario <- NULL
		
		### Format CFs: Create tables that combine QRM and BRS cash flows, and a table containing all cash flows
		scenario_principal <- CF_transformation(CUSIP_information,QRM_scenario_principal,BRS_scenario_principal)
		scenario_interest <- CF_transformation(CUSIP_information,QRM_scenario_interest,BRS_scenario_interest)
		scenario_CFs <- scenario_principal + scenario_interest
		# Get payment dates for all CFs 
		payment_dates <- colnames(scenario_CFs)
		
		### Discounting rate setup for floating securities (Discount rates for fixed rate securities are calculated
		# within the DCF_calculator function as they change for each horizon based on the corresponding yield curve)
		# Create discount rate table (one row per CUSIP, one column per cash flow date)
		scenario_rates <- as.data.frame(matrix(0,nrow=length(CUSIPs),ncol=length(payment_dates)))
		rownames(scenario_rates) <- CUSIPs
		colnames(scenario_rates) <- payment_dates

		## Dicount rates for floating CUSIPs: map each floating CUSIP to the corresponding benchmark rate
		# benchmark_curves contains the unique name for all benchmark rates to be used for discounting 
		benchmark_curves <- unique(CUSIP_information$`InternalCurveType`)
		for (j in (1:length(unique(CUSIP_information$`InternalCurveType`))))    
		{
			# For each benchmark curve and tenor, find the corresponding CUSIPs (floating rate securities)
			floater_1M <- which(CUSIP_information$`InternalCurveType`==as.character(benchmark_curves[j]) & CUSIP_information$`FloatingReset` =="1M")
			floater_3M <- which(CUSIP_information$`InternalCurveType`==as.character(benchmark_curves[j]) & CUSIP_information$`FloatingReset` =="3M")
			floater_6M <- which(CUSIP_information$`InternalCurveType`==as.character(benchmark_curves[j]) & CUSIP_information$`FloatingReset` =="6M")
			floater_12M <- which(CUSIP_information$`InternalCurveType`==as.character(benchmark_curves[j]) & CUSIP_information$`FloatingReset` =="12M")
			
			# Find the position of the curve in the table containing the benchmark curves
			rate_position_1M <- which(rownames(scenario_benchmark)==paste(benchmark_curves[j],"1M"))
			rate_position_3M <- which(rownames(scenario_benchmark)==paste(benchmark_curves[j],"3M"))
			rate_position_6M <- which(rownames(scenario_benchmark)==paste(benchmark_curves[j],"6M"))
			rate_position_12M <- which(rownames(scenario_benchmark)==paste(benchmark_curves[j],"12M"))
			
			# For each tenor update values of corresponding floating CUSIPs rates
			if(length(rate_position_1M) > 0) { scenario_rates[floater_1M,] <- scenario_benchmark[rate_position_1M,] }
			if(length(rate_position_3M) > 0) { scenario_rates[floater_3M,] <- scenario_benchmark[rate_position_3M,] }
			if(length(rate_position_6M) > 0) { scenario_rates[floater_6M,] <- scenario_benchmark[rate_position_6M,] }
			if(length(rate_position_12M)> 0) { scenario_rates[floater_12M,] <- scenario_benchmark[rate_position_12M,] }  
		}
		# Change type of tables contaning rate information to matrix for easier manipulation
		scenario_rates <- as.matrix(scenario_rates)
		
		### Spreads and rates tables setup for all CUSIPs
		# Spreads are fed as basis points, we need them in percentage for discounting
		scenario_spread <- scenario_spread/100
		# Rates only scenario: all spreads are set as equal to their t0 value
		rate_impact_scenario <- scenario_spread
		rate_impact_scenario[,1:dim(rate_impact_scenario)[2]] <- scenario_spread[,1]
		
		### Identify fixed rate CUSIPs and floating CUSIPs indices
		fixed_rate_CUSIPs <- which(CUSIP_information$`FixedFloating` == "Fixed")
		floating_CUSIPs <- which(CUSIP_information$`FixedFloating` == "Floating")
		
		### DCF calculations setup
		# Create tables containing DCF calculations. Tables for rates only DCF and the adjustments for clean price are identical
		scenario_DCF <- matrix(0,nrow=length(CUSIPs),ncol=length(CCAR_horizons))
		rownames(scenario_DCF) <- CUSIPs
		colnames(scenario_DCF) <- CCAR_horizons
		scenario_FV_adjustment <- rate_impact_scenario_DCF <- scenario_DCF
		
		### Discount cash flows using function DCF_calculator. Compute accrued adjustment (function clean_price_adjustments)
		# and get clean prices by adjusting for accrued interests
		# Rates and spreads together
#		filename <- file.path(Output_folder,"scenario_fixed_curves.xlsx")
#		write.xlsx(scenario_fixed_curves, filename, row.names = T, col.names = T)
		
		scenario_DCF <- DCF_calculator(scenario_DCF,CCAR_horizons,scenario_rates,payment_dates,scenario_spread,scenario_CFs,CUSIPs,floating_CUSIPs,fixed_rate_CUSIPs,CUSIP_information,scenario_fixed_curves,benchmark_curves,days_in_year)
		scenario_FV_adjustment <- clean_price_adjustments(scenario_FV_adjustment,scenario_interest,CCAR_horizons,CUSIPs,payment_dates,CUSIP_information)
		scenario_DCF <- scenario_DCF-scenario_FV_adjustment
		# Rates only
		rate_impact_scenario_DCF <- DCF_calculator(rate_impact_scenario_DCF,CCAR_horizons,scenario_rates,payment_dates,rate_impact_scenario,scenario_CFs,CUSIPs,floating_CUSIPs,fixed_rate_CUSIPs,CUSIP_information,scenario_fixed_curves,benchmark_curves,days_in_year)
		rate_impact_scenario_DCF <- rate_impact_scenario_DCF - scenario_FV_adjustment
		
		### Output results to database
		output_table <- as.data.frame(scenario_DCF)
		colnames(output_table) <- quarters_generic
		output_table[,c('VERSION','TimeStamp','Component','Scenario')] <- 
				rep(c(running_version,current_time,"All",current_scenario),each=length(CUSIPs))
		output_table$CUSIPs <- rownames(output_table)
		output_table <- output_table[,c("VERSION","TimeStamp","Component","Scenario","CUSIPs",quarters_generic)]
		Fair_Value_ALL <- rbind(Fair_Value_ALL,output_table)
		
		# Rates only
		output_table <- as.data.frame(rate_impact_scenario_DCF)
		colnames(output_table) <- quarters_generic
		output_table[,c('VERSION','TimeStamp','Component','Scenario')] <- 
				rep(c(running_version,current_time,"RatesOnly",current_scenario),each=length(CUSIPs))
		output_table$CUSIPs <- rownames(output_table)
		output_table <- output_table[,c("VERSION","TimeStamp","Component","Scenario","CUSIPs",quarters_generic)]
		Fair_Value_Rate_Only <- rbind(Fair_Value_Rate_Only,output_table)
	}
	if (export_to_db == "Yes") {
		table_name <- "Fair_Value_ALL";
		saveTable(dbc,table_name,Fair_Value_ALL)
		
		table_name <- "Fair_Value_Rate_Only";
		saveTable(dbc,table_name,Fair_Value_Rate_Only)
	}
#	odbcCloseAll()
} # End of main portfolio discounting

###############
#Reinvestments#
# Note: there is no adjustment for clean vs dirty price for reinvestments because they are defined
# as having monthly payments, so there is no accrued interest on CCAR dates
###############

#if (run_reinvestments == "Yes") {
#	#### Reinvestments data importing for all scenarios
#	db_conn <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",db_path,db_name))
#	# Purchase plans for all scenarios
#	all_scenarios_purchases <- sqlFetch(db_conn,"Reinvestments_Details_qsel")
#	
#	for (iScenarios in 1:length(scenario_names)) {
#		current_scenario <- as.character(scenario_names[iScenarios])
#		
#		## Data loading for purchases
#		scenario_purchases <- subset(all_scenarios_purchases,Scenario == current_scenario)
#		rownames(scenario_purchases) <- paste0(as.character(scenario_purchases$PurchaseDate)," ",
#				as.character(scenario_purchases$DummyCUSIP))
#		## Data loading for cash flows
#		scenario_CF_info <- excel_file_fetcher(excel_files_info,db_path,"CashFlows","StateStreet",current_scenario,"Reinvestments")
#		rownames(scenario_CF_info) <- paste0(as.character(scenario_CF_info$PurchaseDate)," ",
#				as.character(scenario_CF_info$DummyCUSIP))
#		scenario_CF_info <- scenario_CF_info[rownames(scenario_purchases),]
#		## Benchmark rates for floating rate securities for each scenario. Set rownames as rate names
#		scenario_benchmark <- subset(all_floating_rates,Scenario == current_scenario)
#		rownames(scenario_benchmark) <- scenario_benchmark$Rate
#		scenario_benchmark$Rate <- scenario_benchmark$Scenario <- NULL
#		## Yield curves for fixed rate securities
#		scenario_fixed_curves <- subset(all_fixed_rates,Scenario == current_scenario)
#		scenario_fixed_curves$Scenario <- NULL
#		
#		
#		## Initialize table containing the CF reinvestment amounts, and table containing discounting rates
#		# Get payment dates for all CFs: they are the same as the dates of the floating rates
#		payment_dates <- colnames(scenario_benchmark)
#		CF_dates_idx <- which(format(as.Date(colnames(scenario_CF_info),"%m/%d/%Y"),"%m/%d/%Y") %in% payment_dates)
#		scenario_reinv_CFs <- as.matrix(scenario_CF_info[,CF_dates_idx])
#		scenario_reinv_rates <- as.data.frame(scenario_reinv_CFs*0)
#		
#		
#		## The curves that are used by the reinvestments may be fewer than the ones existing in the tool, so this ensures 
#		# that the factor containing the curves for reinvestments contains all curves in the tool
#		benchmark_curves <- droplevels(unique(CUSIP_information$`InternalCurveType`))
#		scenario_CF_info$BenchmarkRate <- factor(scenario_CF_info$BenchmarkRate,levels(benchmark_curves))
#		
#		
#		## Dicounting rates for floating CUSIPs: map each floating CUSIP to the corresponding benchmark rate
#		# (fixed CUSIPs do not matter, their discounting rates are recomputed in the DCF function)
#		for (j in (1:length(unique(CUSIP_information$`InternalCurveType`))))    
#		{
#			# For each benchmark rate, find the corresponding CUSIPs (for floaters 3M is the assumed tenor for reinvestments)
#			rate_position <- which(rownames(scenario_benchmark)==paste(benchmark_curves[j],"3M"))
#			scenario_benchmark_Cusips<- which(as.character(scenario_CF_info$BenchmarkRate)==as.character(benchmark_curves[j]))
#			
#			# Update rates of CUSIPs that are discounted by the corresponding benchmark rate
#			if(length(rate_position > 0))
#			{ scenario_reinv_rates[scenario_benchmark_Cusips,] <- scenario_benchmark[rate_position,] }
#		}
#		## Change type of tables contaning rate information to matrix for easier manipulation
#		scenario_reinv_rates <- as.matrix(scenario_reinv_rates)
#		
#		
#		## Create and fill table with spreads only for reinvestments
#		scenario_reinv_spread <- matrix(0,nrow=length(scenario_purchases$DummyCUSIP),ncol = length(CCAR_horizons))
#		rownames(scenario_reinv_spread) <- rownames(scenario_purchases)
#		colnames(scenario_reinv_spread) <- CCAR_horizons
#		scenario_purchases_horizons_col <- which(colnames(scenario_purchases) %in% quarters_generic)
#		scenario_reinv_spread[,2:dim(scenario_reinv_spread)[2]] <- as.matrix(scenario_purchases[,scenario_purchases_horizons_col])
#		scenario_reinv_spread <- scenario_reinv_spread/100
#		
#		# Set up spreads for rates only scenario
#		rate_impact_reinv_scenario_spread <- scenario_reinv_spread * 0
#		rate_impact_reinv_scenario_spread[] <- scenario_purchases$purchaseSpread/100
#		
#		
#		## Identify fixed and floating rate CUSIPs separately
#		scenario_reinv_fixed_rate_CUSIPs <- which(scenario_purchases$CouponType == "Fixed")
#		scenario_reinv_floating_CUSIPs <- which(scenario_purchases$CouponType == "Float" | scenario_purchases$CouponType == "Floating")
#		
#		
#		## FV calculations: set up tables, CUSIP names (non unique), then ensure naming consistency across columns
#		scenario_reinv_DCF <- scenario_reinv_spread * 0
#		scenario_CUSIP <- droplevels(scenario_purchases$DummyCUSIP)
#		# Rates and Spreads impact
#		scenario_info_for_DCF <- scenario_purchases[,c("PurchaseDate","BenchmarkRate","CouponType")]
#		colnames(scenario_info_for_DCF) <- c("PurchaseDate","InternalCurveType","FixedFloating")
#		scenario_info_for_DCF$InternalCurveType <- factor(scenario_info_for_DCF$InternalCurveType,levels(benchmark_curves))
#		scenario_reinv_DCF <- DCF_calculator(scenario_reinv_DCF,CCAR_horizons,scenario_reinv_rates,payment_dates,
#				scenario_reinv_spread,scenario_reinv_CFs,scenario_CUSIP,
#				scenario_reinv_floating_CUSIPs,scenario_reinv_fixed_rate_CUSIPs,
#				scenario_info_for_DCF,scenario_fixed_curves,benchmark_curves,days_in_year)
#		# Rates only impact
#		rate_impact_reinvestments_scenario_DCF <- scenario_reinv_DCF * 0
#		rate_impact_reinvestments_scenario_DCF <- DCF_calculator(rate_impact_reinvestments_scenario_DCF,CCAR_horizons,
#				scenario_reinv_rates,payment_dates,
#				rate_impact_reinv_scenario_spread,scenario_reinv_CFs,
#				scenario_CUSIP,scenario_reinv_floating_CUSIPs,
#				scenario_reinv_fixed_rate_CUSIPs,scenario_info_for_DCF,
#				scenario_fixed_curves,benchmark_curves,days_in_year)
#		
#		
#		### Output results to database
#		db_conn <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",db_path,db_name))
#		
#		if (export_to_db == "Yes") {
#			# Rates and spreads impact
#			output_table <- as.data.frame(scenario_reinv_DCF)
#			colnames(output_table) <- quarters_generic
#			output_table[,c('Version','TimeStamp','Metric','Component','Scenario')] <- 
#					rep(c(running_version,current_time,"FairValue","All",current_scenario),each=length(scenario_CUSIP))
#			output_table[,c("PurchaseDate","DummyCUSIP")] <- cbind(as.character(scenario_info_for_DCF$PurchaseDate),as.character(scenario_CUSIP))
#			output_table <- output_table[,c("Version","TimeStamp","Metric","Component","Scenario","PurchaseDate","DummyCUSIP",quarters_generic)]  
#			sqlSave(db_conn, output_table, tablename = "Reinvestments_Fair_and_Book_Value",append = TRUE,rownames = FALSE,safer = TRUE,fast = TRUE)
#			
#			# Rates only
#			output_table <- as.data.frame(rate_impact_reinvestments_scenario_DCF)
#			colnames(output_table) <- quarters_generic
#			output_table[,c('Version','TimeStamp','Metric','Component','Scenario')] <- 
#					rep(c(running_version,current_time,"FairValue","RatesOnly",current_scenario),each=length(scenario_CUSIP))
#			output_table[,c("PurchaseDate","DummyCUSIP")] <- cbind(as.character(scenario_info_for_DCF$PurchaseDate),as.character(scenario_CUSIP))
#			output_table <- output_table[,c("Version","TimeStamp","Metric","Component","Scenario","PurchaseDate","DummyCUSIP",quarters_generic)]
#			sqlSave(db_conn, output_table, tablename = "Reinvestments_Fair_and_Book_Value",append = TRUE,rownames = FALSE,safer = TRUE,fast = TRUE) 
#		}
#	}
#} # End of reinvestments discounting

#odbcCloseAll()


# CRITICAL TOOLS DOCUMENTATION	
# 
# File Name|DCF Tool.r
# 
# Overview/PURPOSE|Compute fair values of bonds in each CCAR scenario
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
# List of Inputs|Data from CSIM main database, CCAR cash flows and CCAR rates
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

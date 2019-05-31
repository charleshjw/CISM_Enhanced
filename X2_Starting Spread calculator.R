############################
#Starting spread calculator# 
############################

# This file calculates the implied starting spreads for each CUSIP in the select cash flow source
# A subset of CUSIPs can be run by setting runSubsetOnly <- "Yes" and having the list of CUSIPs to run in the file "Spread Calculator - Subset CUSIPs.csv"
# The CUSIPs selected need to be in the cash flow used as input

# Please note that the function neccessary for running the spread calculator is at the beginning of the script. 
# Function "spread_calculator" estimates the absolute difference between the actual fair value of a CUSIP and the fair value implied from
# the provided starting spread for a single CUSIP
# The functions in the DCF tool supporting functions need to be run before the spread calculator: the file is sourced from this file

library(lubridate)

table_name = "CUSIP_Starting_Spreads"
if (dbHasTable(dbc,table_name)) {
	dbc_table <- fetchTable(dbc,table_name,current_version)
	if (current_version %in% unique(dbc_table$Version) & !re_calculate_starting_spread) {
		print("Starting Spreads are already calculated. Skip.")
		rerun <- FALSE
	} else {
		rerun <- TRUE
	}
} else {
	rerun <- TRUE
}

if (rerun) {
	### Load libraries: If package missing load it with: #install.packages("lubridate")
	
	##### Control ########################################################################################################
# Select whether we export results to the database
#export_to_db <- "Yes" # "Yes" to write results in database tables
	### Path of Database and database name used to get input data ########################################################
	## Select the source of cash flows used to compute starting spreads: QRM or BRS
	source_cf <- "QRM" # "BRS" or "QRM"
	######################################################################################################################
	##### To run the calculator on a subset of CUSIPs then select yes. Enter input file and its subfolder location
# with the list of CUSIPs in the first column with the first column named CUSIPs
	runSubsetOnly <- "No" #"Yes
	subset_file_subfolder <- ""
	if (runSubsetOnly == "Yes")
	{
		setwd(paste0(db_path,subset_file_subfolder))
		subsetCusipList_file <- c("Starting Spread Calculator - Subset CUSIPs.csv")
		subsetCusipList <- (read.csv(subsetCusipList_file,header=T,sep=",",row.names=1,check.names=F))
	}
	######################################################################################################################
	
	
	################ Accrued Interest adjustment function for single date and single CUSIP used later in the code
	single_accrued_interest <- function(interest_payments,paymentFrequency)
	{
		# Check there are remaining interest payments
		remaining_interest = sum(interest_payments)
		if (remaining_interest > 0){
			# Get dates of future interest payments
			payment_dates <- which(interest_payments>0)
			first_pay_date  <- payment_dates[1]
			first_payment <- interest_payments[first_pay_date]
			
			# If there is more than one payment we can back out a payment frequency, otherwise use frequency from input
			if (length(payment_dates) > 1){
				pay_frequency <- payment_dates[2] - first_pay_date
			} else {
				pay_frequency <- paymentFrequency
			}
			
			
			# Check paymentFrequency and the first payment date are compatible, otherwise set accrued interest to 0
			# If first payment is 1 year away or more, set accrued interest to 0
			if ((first_pay_date > pay_frequency) | (first_pay_date >= 12)) {
				# This either means that the first payment date is exactly pay_frequency months away from t0, so that a payment
				# occurred on t0, or that some interest payment is missing, in which case we default to no accrued interest
				accrued_interest <- 0
			} else {
				accrued_interest <- as.numeric(first_payment*((pay_frequency-first_pay_date)/pay_frequency))
			}
			
		} else {
			accrued_interest <- 0
		}
		
		return(as.numeric(accrued_interest))
	}
	################ Spread calculator function used later in the code
	spread_calculator <- function(q0_spread,test_cusip,cusip_FV,test_rate,test_type,days_in_year,payment_dates,date_diff_q0,days_per_month)
	{
		# Create rate + spread table
		test_rate <- test_rate + q0_spread/100
		test_rate <- as.matrix(test_rate)
		test_rate <- t(test_rate)
		
		# Create discount factors
		factor_period <- length(payment_dates)+1
		temp_factor <- matrix(1,nrow=1,ncol=factor_period)
		
		# Create table containing all the discount factors
		for (i in 1:length(test_cusip))
		{  
			if (test_type == "Floating")
			{
				temp_factor[i+1] <- temp_factor[i]*((1+test_rate[i]/100)^(days_per_month[i]/days_in_year))
			} else if (test_type == "Fixed") {
				temp_factor[i+1] <- (1+test_rate[i]/100)^(date_diff_q0[i]/days_in_year)
			}
		}
		
		# Estimate the fair value for a single CUSIP (stream of cash flows/discount factors)
		temp_factor <- temp_factor[2:length(temp_factor)]
		calculated_FV <- as.matrix((test_cusip/temp_factor))
		calculated_FV <- sum(calculated_FV)
		
		# Return difference between calculated and given fair value
		FV_diff <- (cusip_FV-calculated_FV)^2
		return (FV_diff) 
	}
	################ End of Functions
	
	#### Data importing from the database
#db_conn <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",db_path,db_name))
# Current version running
#versions_table <- sqlFetch(db_conn,"Versions")
#running_version <- as.character(subset(versions_table,Running>0)$Overall)
# Name of scenario used to do our computations
	scenario_used_name <- ScenarioNames
# File and folder information for file in Excel format
#excel_files_info <- sqlFetch(db_conn,"Excel_Files_Location_qsel")
# Dates
	quarters_generic <- c("Q0","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9")
	CCAR_horizons <- format(subset(Dates_Master,Horizon %in% quarters_generic)$HorizonDate,"%m/%d/%Y")
	
# FX Rates
	
	FX_rates <- fetchTable(dbc,"FX_Spot_Rates",current_version)
	rownames(FX_rates) <- FX_rates$Currency_HFV
# Fixed rates yield curves over 10 quarters, floating rates forwards for all CF dates
	filename = concat_file_path(tableFilter(list_files,"VariableName","FixedRates"))
	all_fixed_rates <- read.csv(filename,header=T,row.names=NULL,sep=",",check.names = F)
	filename = concat_file_path(tableFilter(list_files,"VariableName","FloatingRates"))
	all_floating_rates <- read.csv(filename,header=T,row.names=NULL,sep=",",check.names = F)
	
	## Non scenario specific information/parameters
# Time used for timestamps in output tables
	current_time <- format(Sys.time(),"%m/%d/%Y %I:%M:%S %p")
# Days in year (used in discount factors calculations)
	days_in_year <- 360
	
	### Get data specific to the scenario used
	## Cash flows from Excel files: Principal and Interest from BRS or QRM
	if (source_cf == "BRS") {
		
		filename <- concat_file_path(tableFilter(list_files,"VariableName",paste0("BRS_Cashflow_",transform_scenario_names("base",scenario_used_name),"_Principal")))
		CF_scenario_principal <- as.matrix((read.csv(filename,header=T,row.names=1,sep=",",check.names = F)))
		filename <- concat_file_path(tableFilter(list_files,"VariableName",paste0("BRS_Cashflow_",transform_scenario_names("base",scenario_used_name),"_Interest")))
		CF_scenario_interest <- as.matrix((read.csv(filename,header=T,row.names=1,sep=",",check.names = F)))
		other_source <- "QRM"
	} else if (source_cf == "QRM") {
		filename <- concat_file_path(tableFilter(list_files,"VariableName",paste0("QRM_Cashflow_",transform_scenario_names("base",scenario_used_name),"_Principal")))
		CF_scenario_principal <- as.matrix((read.csv(filename,header=T,row.names=1,sep=",",check.names = F)))
		filename <- concat_file_path(tableFilter(list_files,"VariableName",paste0("QRM_Cashflow_",transform_scenario_names("base",scenario_used_name),"_Interest")))
		CF_scenario_interest <- as.matrix((read.csv(filename,header=T,row.names=1,sep=",",check.names = F)))
		other_source <- "BRS"
	}

	spread_scenarios <- unique(all_floating_rates$Scenario)
	## Benchmark rates for floating rate securities. Set rownames as rate names
	scenario_benchmark <- subset(all_floating_rates,Scenario == transform_scenario_names("base",spread_scenarios))
	rownames(scenario_benchmark) <- scenario_benchmark$Rate
	scenario_benchmark$Rate <- scenario_benchmark$Scenario <- NULL
	
	## Yield curves for fixed rate securities
	scenario_fixed_curves <- subset(all_fixed_rates,Scenario == transform_scenario_names("base",spread_scenarios))
	scenario_fixed_curves$Scenario <- NULL
	
	## CUSIP Master Table, keep columns we need in this file only
	CUSIP_information_all <- fetchTable(dbc,"CUSIP_Master_Table_Permanent",current_version)
	CUSIP_information_all <- data.frame(CUSIP_information_all[,c("CUSIPs","AssetClass","FixedFloating","Currency","InternalCurveType",
							"CF_Source","FloatingReset","FV_USD","AdjustmentMultiplier","PaymentFrequency")],stringsAsFactors=FALSE)
	### CUSIPs for which the chosen CF source has cash flows
	CUSIPs_subset <- subset(CUSIP_information_all,CUSIPs %in% rownames(CF_scenario_principal))
	CUSIPs_subset$CF_Source <- source_cf
# Add a dummy row for the Other source, so we can use the CF_transformation table
	dummy_row <- subset(CUSIPs_subset,FixedFloating == "Fixed")[1,,drop=FALSE]
	dummy_row[,c("CUSIPs","CF_Source","FV_USD")] <- c("DUMMY_Cusip",other_source,"0")
	CUSIPs_subset <- rbind(CUSIPs_subset,data.frame(as.list(dummy_row)))
	
	### Initialize cash flow table (containing both principals and interest payments)
	## Other source: those CFs are not needed for the computations, only to go through the CFs manipulation function, so we set up a dummy
	Dummy_scenario_principal <- CF_scenario_principal[1,,drop=FALSE] * 0
	rownames(Dummy_scenario_principal) <- c("DUMMY_Cusip")
	Dummy_scenario_interest <- Dummy_scenario_principal
# Cash flow tables preparation
	if (source_cf == "BRS") {
		scenario_principal <- CF_transformation(CUSIPs_subset,Dummy_scenario_principal,CF_scenario_principal)
		scenario_interest <- CF_transformation(CUSIPs_subset,Dummy_scenario_interest,CF_scenario_interest)
	} else if (source_cf == "QRM") {
		scenario_principal <- CF_transformation(CUSIPs_subset,CF_scenario_principal,Dummy_scenario_principal)
		scenario_interest <- CF_transformation(CUSIPs_subset,CF_scenario_interest,Dummy_scenario_interest)
	}
	scenario_CFs <- scenario_principal + scenario_interest
	
	## Get interest payments in required format: CFs sometimes have N/As in the csv files instead of 0 values. These lines replace N/A with 0
	CF_scenario_interest <- as.data.frame((CF_scenario_interest))
	CF_scenario_interest[is.na(CF_scenario_interest)] <- 0
	CF_scenario_interest <- as.matrix((CF_scenario_interest))
	
	### Get rid of dummy CUSIP for cash flows
	CUSIPs_subset <- CUSIPs_subset[-which(CUSIPs_subset$CUSIPs == "DUMMY_Cusip"),]
	
	### Parameter setting: disable scientific notation
	options(scipen=10)
	
	### If we are only looking at a subset of CUSIPs we restrict the list of CUSIPs now
	if (runSubsetOnly == "Yes")
	{ CUSIPs_subset <- CUSIPs_subset[CUSIPs_subset$CUSIPs %in% rownames(subsetCusipList),] }
	
	### Preparations for spread calculation: first column is the spread, 2nd is the spread implied difference between FV and actual FV
	CUSIPs_subset_spreads <- matrix(9997,nrow=dim(CUSIPs_subset)[1],ncol=2)
	colnames(CUSIPs_subset_spreads) <- c("StartingSpread_bps","DifferenceImpliedFV_vs_Actual")
	
	## Get all payment dates
	payment_dates <- colnames(scenario_CFs)
	## Set up days per month for floating CUSIPs as it saves considerable computing time
	payment_dates_dateFormat <- as.Date(parse_date_time(payment_dates,orders="mdy"))
	days_per_month <- as.numeric(days_in_month(payment_dates_dateFormat))
	## Set date difference for discounting
	fixed_CF_dates <- as.matrix(as.Date(parse_date_time(payment_dates,orders="mdy")))
	## Discounting date is the Q0 date
	base_date <- CCAR_horizons[1]
	base_date <- as.numeric(as.Date(parse_date_time(base_date,orders="mdy")))
	date_diff_q0 <- fixed_CF_dates-base_date
	date_diff_q0 <- t(date_diff_q0) # Transpose table
	
# benchmark_curves contains the unique name for all benchmark rates to be used for discounting 
	benchmark_curves <- droplevels(unique(CUSIPs_subset$`InternalCurveType`))
# Calculate fixed rates
	CUSIPs_subset_fixed_curves<-as.data.frame(fixed_rates_all(scenario_fixed_curves,1, CCAR_horizons, payment_dates, benchmark_curves)) 
	rownames(CUSIPs_subset_fixed_curves) <- benchmark_curves 
	colnames(CUSIPs_subset_fixed_curves) <- payment_dates
	
	### Convert Fair Value in local currency to match CFs in local currency
	### Multiply by AdjustmentMultiplier to match Cash Flows with Fair Value at t0
	CUSIPs_subset$FV_local <- as.numeric(CUSIPs_subset$FV_USD)
	for (iCusip in 1:dim(CUSIPs_subset)[1])
	{
		FX_value <- FX_rates[rownames(FX_rates)==CUSIPs_subset[iCusip,'Currency'],'FXRate']
		adj <- as.numeric(CUSIPs_subset[iCusip,'AdjustmentMultiplier'])
		CUSIPs_subset[iCusip,'FV_local'] <- as.numeric(CUSIPs_subset[iCusip,'FV_USD'])/(FX_value*adj)
	}
	
	### Actual loop through each CUSIP to find the solving spread
	for (i in 1:dim(CUSIPs_subset_spreads)[1])
	{
		print(i)  # Keep track of where we are in the CUSIP list
		
		### Get CUSIP specific parameters
		CUSIP <- droplevels(CUSIPs_subset$CUSIPs[i])  # CUSIP name
		cusip_CF <- scenario_CFs[which(rownames(scenario_CFs)==CUSIP),] # CUSIP CF
		cusip_interest <- CF_scenario_interest[which(rownames(CF_scenario_interest)==CUSIP),]  # CUSIP interest payment
		cusip_FV <- (CUSIPs_subset$FV_local[i])  # Find the FV of the CUSIP (source: HFV)
		
		## Rate information
		cusip_rate_index <- droplevels(CUSIPs_subset$`InternalCurveType`[i]) # Discounting curve needed
		cusip_rate_floater <- droplevels(CUSIPs_subset$`FloatingReset`[i])
		cusip_type <- droplevels(CUSIPs_subset$`FixedFloating`[i])
		
		if (cusip_type == "Floating") {
			cusip_rate_index <- paste(cusip_rate_index,cusip_rate_floater)
			cusip_rate <- as.matrix(scenario_benchmark[which(rownames(scenario_benchmark)==cusip_rate_index),])
		} else if (cusip_type == "Fixed") {
			cusip_rate <- as.matrix(CUSIPs_subset_fixed_curves[which(rownames(CUSIPs_subset_fixed_curves)==cusip_rate_index),])
		}
		
		# Compute accrued interest before the optimization and adjust fair value
		paymentFrequency <- as.numeric(CUSIPs_subset$`PaymentFrequency`[i])
		accrued_adj <- single_accrued_interest(cusip_interest,paymentFrequency)
		cusip_FV_dirty <- cusip_FV + accrued_adj
		
		### If the optimization encounters an error the spread is set to 9999, and 9998 if there is a warning
		temp_starting_spread <- tryCatch({
					ff <- optim(par=c(100),fn=spread_calculator,test_cusip=cusip_CF,cusip_FV=cusip_FV_dirty,test_rate=cusip_rate,test_type=cusip_type,days_in_year=days_in_year,payment_dates=payment_dates,date_diff_q0=date_diff_q0,days_per_month=days_per_month,method = "Brent", lower = -2000, upper = 2000)
				}, warning = function(war) {
					ee <- list("")
					ee$par <- 9998
					ee$value <- 9998
					return(ee)
				}, error = function(err) {
					ee <- list("")
					ee$par <- 9999
					ee$value <- 9999
					return(ee)
				})
		# If we did not achieve convergence, try another slower optimization method
		if (temp_starting_spread$value > 10) { 
			temp_starting_spread <- tryCatch({
						ff <- optim(par=c(100),fn=spread_calculator,test_cusip=cusip_CF,cusip_FV=cusip_FV,test_rate=cusip_rate,test_type=cusip_type,days_in_year=days_in_year,payment_dates=payment_dates,date_diff_q0=date_diff_q0,days_per_month=days_per_month,method = "BFGS")
					}, warning = function(war) {
						ee <- list("")
						ee$par <- 9998
						ee$value <- 9998
						return(ee)
					}, error = function(err) {
						ee <- list("")
						ee$par <- 9999
						ee$value <- 9999
						return(ee)
					})
		}
		
		CUSIPs_subset_spreads[i,1] <- temp_starting_spread$par
		CUSIPs_subset_spreads[i,2] <- temp_starting_spread$value  
	}
	
	### Output results to database
	output_table <- as.data.frame(CUSIPs_subset_spreads)
	output_table$DifferenceImpliedFV_vs_Actual <- round(output_table$DifferenceImpliedFV_vs_Actual,3)
	output_table$CUSIPs <- CUSIPs_subset$CUSIPs
	output_table$SpreadSource <- paste0(source_cf,"_calculated")
	
	filename = concat_file_path(tableFilter(list_files,"VariableName","BRS_Starting_Spread"))
	BRS_Starting_Spread <- read.xlsx(filename,sheetName="Sheet1")
	output_table <- rbind(output_table,BRS_Starting_Spread[,colnames(output_table)])
	##quality control
#duplicated(output_table$CUSIPs)
	output_table <- VersionDataFrame(output_table,current_version)
#output_table <- output_table[,c("CUSIPs","StartingSpread_bps","SpreadSource","DifferenceImpliedFV_vs_Actual","VERSION","TimeStamp")]
	if (export_to_db == "Yes") {
		table_name = "CUSIP_Starting_Spreads"
		saveTable(dbc, table_name, output_table)
	}
#odbcCloseAll()
	
	### To test a specific CUSIP with a specific spread, enter both manually here and select Yes for runOneCUSIP
	CUSIP <- as.character("JP1051251F99")
	runOneCUSIP <- "No"  #"Yes"
	starting_spread <- 205.8254
	if(runOneCUSIP == "No")
	{
		i <- which(CUSIPs_subset$CUSIPs==CUSIP)
		#initialize parameters for different CUSIPs
		#CUSIP name
		CUSIP <- droplevels(CUSIPs_subset$CUSIPs[i])
		#CUSIP CF
		cusip_CF <- scenario_CFs[which(rownames(scenario_CFs)==CUSIP),]
		#CUSIP interest payment
		cusip_interest <- CF_scenario_interest[which(rownames(CF_scenario_interest)==CUSIP),]
		
		#Find the FV of the CUSIP (source: HFV)
		cusip_FV <- (CUSIPs_subset$FV_local[i])
		
		#Get CUSIP rate
		cusip_rate_index <- droplevels(CUSIPs_subset$`InternalCurveType`[i])
		cusip_rate_floater <- droplevels(CUSIPs_subset$`FloatingReset`[i])
		cusip_type <- droplevels(CUSIPs_subset$`FixedFloating`[i])
		if (cusip_type=="Floating")
		{
			cusip_rate_index <- paste(cusip_rate_index,cusip_rate_floater)
			cusip_rate <- as.matrix(scenario_benchmark[which(rownames(scenario_benchmark)==cusip_rate_index),])
		}
		if (cusip_type=="Fixed")
		{cusip_rate <- as.matrix(CUSIPs_subset_fixed_curves[which(rownames(CUSIPs_subset_fixed_curves)==cusip_rate_index),])}
		
		test_cusip=cusip_CF
		test_interest=cusip_interest
		cusip_FV=cusip_FV
		test_rate=cusip_rate
		test_type=cusip_type
		FV_spreadImpliedFV_diff<-spread_calculator(starting_spread,test_cusip,cusip_FV,test_rate,test_type,days_in_year,payment_dates,date_diff_q0,days_per_month)
		print("FV_spreadImpliedFV_diff:")
		FV_spreadImpliedFV_diff
	}
}

# CRITICAL TOOLS DOCUMENTATION	
# 
# File Name|Starting Spread calculator.r
# 
# Overview/PURPOSE|Compute credit spread at time zero for each bond for CCAR OCI discounting
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

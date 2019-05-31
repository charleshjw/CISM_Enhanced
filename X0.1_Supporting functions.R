###############################
#DCF Tool supporting functions#
###############################

#Functions that are called by the main body of the tool 
#This script needs to be executed before running the main "DCF Tool"

#Functions

#1. Function that calculated discount factors for all time periods
#2. Rate calculator for fixed rate securities 
#3. Main DCF calculator
#4. Adjustment for clean price 
#5. Data manipulation function
#6. Fetch Excel files function


#Function 1 : Discount factor calculator
#Overview:
#Different methods are used for floating and fixed rate securities
#A table containing the discount factors for all CUSIPs is returned
discount_factor<-function(factor,rates_table,date_diff,floating_CUSIPs,fixed_rate_CUSIPs,payment_dates,days_in_year)
{
#  write.xlsx(rates_table, "rates_table.xlsx", row.names = T, col.names = T)
  for (i in 1:ncol(rates_table))
  {
    #Discount factor calculation for floating rate securities
    
    #Calculation of days in month for each payment date
    temp_day <- as.Date(parse_date_time(payment_dates[i],orders="mdy"))
    temp_day <- as.numeric(days_in_month(temp_day))
    
    #Table containing all discount factors
    factor[floating_CUSIPs,i+1]<-factor[floating_CUSIPs,i]*((1+rates_table[floating_CUSIPs,i]/100)^(temp_day/days_in_year))
    
    #Discount factor calculation for fixed rate securities  
    factor[fixed_rate_CUSIPs,i+1] <- (1+rates_table[fixed_rate_CUSIPs,i]/100)^(date_diff[i]/days_in_year)
  }
  #return table containing discount factors for all CUSIPs
  return (factor)
}



#Function 2 : Calculation of fixed rates for all periods 
#Overview:
#Calculation of rates for fixed rate securities
#Inputs include the full yield curve for all benchmark rates and current horizon
#A table containing the interpolated rates for all benchmark rates is returned
fixed_rates_all <- function(all_fixed_curves, horizon_period, horizon_dates, payment_dates, benchmark_curves) 
{
  #Get the payment dates for the CFs in the system 
  fixed_CF_dates <- payment_dates
  fixed_CF_dates <- as.matrix(as.Date(parse_date_time(fixed_CF_dates,orders="mdy")))
  
  #Get the current horizon period for calculations
  base_date<-horizon_dates[horizon_period]
  base_date<-as.numeric(as.Date(parse_date_time(base_date,orders="mdy")))
  
  #Find the distance in days between the payment dates for all CFs and the current horizon period
  date_diff <- fixed_CF_dates-base_date
  
  #Initialize table that will hold monthly rates for all benchmark rates 
  all_fixed_rates <- as.data.frame(matrix(0, nrow = length(benchmark_curves), ncol=length(payment_dates)))
  
  #Each yield curve is interpolated between the availabe points to find all required tenors
  for (j in 1:length(benchmark_curves))
  {
    #Calculate number of available points on yield curve (e.g. 1M, 3M, 6M etc.)
    yield_curve_points <- which(all_fixed_curves$Rate==as.character(benchmark_curves[j]))
    #Calculate the corresponding term for each available point on curve (e.g. 30 days, 90 days etc.) 
    term <- as.matrix(all_fixed_curves$Term[yield_curve_points])
    #Subset larger table containing all fixed rates to the currently analyzed one
    yield_curve <- subset(all_fixed_curves,Rate == as.character(benchmark_curves[j]))
    #Delete unwanted columns to reach tale containing only the yield curve for specific horizon
    yield_curve$Rate <- yield_curve$Curve <- yield_curve$Term <- NULL
    #set as matrix for easier manipulations
    yield_curve <- as.matrix(yield_curve)
    
    #if the yield curve provided is missing data for the first month, then a dummy interest rate of 0 is added as the 1 month rate in order to 
    #avoid having 0 discoint rates till the first provided tenor. The dummy rate is used to linearly interpolate rates untill the first given point on the yield curve
    if (term[1] > 30) 
    {
      #set dummy term to 30 days
      initialize_term <- 30
      term <- rbind(initialize_term,term)
      
      #add the dummy tenor to the curve
      add_yield_curve <- matrix(0.0000000000001,ncol=dim(yield_curve)[2])
      yield_curve <- rbind(add_yield_curve,yield_curve)
      
    }
    
    
    #Initialize table containing monthly rates for current benchmark rate
    final_rate<-matrix(0,nrow=length(payment_dates))
    
    #specify end of loop as the length of the points on the yield curve
    end_of_loop <- length(term)
    
    for (i in 2:end_of_loop)
    {
      #period_dates holds the payment dates for CFs occuring between two points on the yield curve
      period_dates <- date_diff[date_diff<term[i] & date_diff>=term[i-1]]
      #x_test includes the 2 dates between which the rates need to be interpolated
      x_test <- matrix(c(term[i-1],term[i]))
      #y_test are the yield curve points to be interpolated
      y_test <- matrix(c(yield_curve[(i-1),horizon_period],yield_curve[i,horizon_period]))
      #interpolation for the rates between the term dates
      temp_rate <- approx(x_test,y_test,xout= period_dates,method="linear")$y
      #positions in table for the corresponding rates. suppresswarnings is used for when no rate is found between 2 periods 
      suppressWarnings(positions <- which(period_dates==date_diff))
      #fill table with discount rate 
      if (length(period_dates !=0)) {final_rate[which(date_diff==period_dates[1]):which(date_diff==period_dates[length(period_dates)])] <- temp_rate}
    }
    
    #transpose matrix 
    final_rate <- t(final_rate)
    #find the final point on the yield curve
    end_rate<-final_rate[max(which(final_rate!=0))]
    #keep the rate steady after the last available point on yield curve
    final_rate[max(which(final_rate!=0)):length(final_rate)] <- end_rate
    #add rates of current benchmark rate to 
    all_fixed_rates[j,] <- final_rate
    
  }
  
  #return table containing all fixed rates
  return (all_fixed_rates)
}


#Function 3 : Discounted Cafh Flows calculator 
#Overview:
#This function calculates the FV for all CUSIPs for all horizons for a single scenario
#Inputs include the horizon dates, all the benchmark rates for specific scenario, CFs for specific scenario
#Returns a table containing the FV for all CUSIPs for all horizons for a given scenario
DCF_calculator <- function(scenario_DCF,horizon_dates,scenario_rates,CF_dates,scenario_spread,scenario_CFs,CUSIPs,floating_CUSIPs,fixed_rate_CUSIPs,CUSIP_information,all_fixed_curves,benchmark_curves,days_in_year)
{
#	horizon_dates = CCAR_horizons
#	CF_dates = payment_dates
#	all_fixed_curves = scenario_fixed_curves

	#Specify purchase dates for all CUSIPs
  purchase_dates <- as.matrix(CUSIP_information$`PurchaseDate`)
  purchase_dates <- as.numeric(as.Date(parse_date_time(purchase_dates,orders="mdy")))
  
  #Loop calculating the FV for all CUSIPs
  for (i in 1:length(horizon_dates))
  {
    #Set current period
    period <- (-3*(i-1))
    if (i==1) {period<-0}
    #calculate rates for all fixed rate securities for given horizon 
    fixed_CF_rates<-as.data.frame(fixed_rates_all(all_fixed_curves,i, horizon_dates, CF_dates, benchmark_curves)) 
    
#    library(openxlsx)
#    write.xlsx(fixed_CF_rates, "fixed_CF_rates.xlsx", row.names = T, col.names = T)
    
    #Get the future payment dates for this scenario
    fixed_CF_dates <- as.matrix(as.Date(parse_date_time(CF_dates,orders="mdy")))
    
#    write.xlsx(fixed_CF_dates, "fixed_CF_dates.xlsx", row.names = T, col.names = T)
    
    base_date <- horizon_dates[i]
    base_date <- as.numeric(as.Date(parse_date_time(base_date,orders="mdy")))
    #calculate the difference from each payment date and the current horizon period (e.g. Jan 2015 will be 31 days away from horizon period 1-December 2014)
    date_diff <- fixed_CF_dates-base_date
    
    #Create temporary table that contains the benchmark rate for all CUSIPs 
    temp_rates <- as.data.frame(scenario_rates)
    
    #Initialize table that will containg benhmark rate + spread for all CUSIPs in portfolio
    discount_rates <- as.data.frame(temp_rates) 
    
    #Replace rates for fixed rate securities given 
    for(z in 1:length(benchmark_curves))
    {
      #find the position for each fixed rate security for the given benchmark curve
	fixed_rate_position <- which(CUSIP_information$`FixedFloating`=="Fixed" & CUSIP_information$`InternalCurveType` == as.character(benchmark_curves[z]))
			
      #replace the rates for the fixed rate securities with the corresponding rates calculated earlier in this function 
      if (length(fixed_rate_position) >0)
      {temp_rates[fixed_rate_position,]<-fixed_CF_rates[z,]}
    }
    
    
    
    #Add the scenario spread to the corresponding rate for all CUSIPs in portfolio 
    for (j in 1:length(CF_dates))
    {
      #calculate the discount rate for each CUSIP in the portfolio, as benchmark rate + Spread 
      discount_rates [,j] <- temp_rates[,j] + scenario_spread[,i] 
    }
    
    #Calculate temporary table containing information for specific horizon period 
    #Exclude CFs that have occured in previous periods
    temp_CFs <- scenario_CFs[,-1:(period)]
    #Exclude discount rates that have occured in previous periods
    temp_discount_rates <- discount_rates[,-1:(period)]
    #Exclude payment dates that have passed 
    date_diff2<-date_diff[(3*(i-1)+1):length(date_diff)]
    temp_dates<-CF_dates[(3*(i-1)+1):length(CF_dates)]
    #If we are alculating period 1, do not change CFs, rates and payment dates 
    if (i==1)
    {
      temp_CFs <- scenario_CFs
      temp_discount_rates <- discount_rates
      date_diff2 <- date_diff
      temp_dates <- CF_dates
    }
    
    #Initialize table containing discount factors for all CUSIPs. Length will be equal to the number of CFs + 1 (for month 0)
    factor_period <- length(CF_dates)+(period)+1
    temp_factors <- matrix(1,nrow=length(CUSIPs),ncol=factor_period)
    
    #Calculation of discount factors for all CUSIPS, with distinction between floating and fixed rate securities
    discount_factors <- discount_factor(temp_factors,temp_discount_rates,date_diff2,floating_CUSIPs,fixed_rate_CUSIPs,temp_dates,days_in_year)
    discount_factors <- discount_factors[,-1]
    
    
#    write.xlsx(discount_factors, "discount_factors.xlsx", row.names = T, col.names = T)
#    write.xlsx(temp_CFs, "temp_CFs.xlsx", row.names = T, col.names = T)
    
    #Fv for each CUSIP will be the sum of each future CF over the corresponding discount factor 
    scenario_DCF[,i] <- rowSums(temp_CFs/discount_factors)
    
    #Exclude securities that haven't been purchased as of this period-used for reinvestment assumptions
    exclude_CUSIPs <- which(purchase_dates>as.numeric(as.Date(parse_date_time(horizon_dates[i],orders="mdy"))))
    scenario_DCF[exclude_CUSIPs,i]<-0
  }
  #Return table containing the FV for all CUSIPs 
  return (scenario_DCF) 
}


#Function 4 : Adjustment for clean price  
#Overview:
#This function calculates the FV adjustment for all CUSIPs for all horizons for a single scenario
#Inputs include the matrix containing the interest payments for all CUSIPs, the payment dates and scenario dates
#Returns a table containing the FV adjustment (accrued interest to be subtracted from dirty FV) for all CUSIPs for all horizons for a given scenario.
clean_price_adjustments <- function(FV_correction,interest_payments,horizon_dates,CUSIPs,payment_dates,CUSIP_information)
{
#	FV_correction = scenario_FV_adjustment
#	interest_payments = scenario_interest
#	horizon_dates = CCAR_horizons 
  for (i in 2:length(horizon_dates))
    
  {
    # Get distance of current horizon date from t0, in months
    horizon_month <- which(as.Date(payment_dates,"%m/%d/%Y")==as.Date(horizon_dates[i],"%m/%d/%Y"))
    
    # Find CUSIPs potentially needing a clean price adjustment: those with no interest payment at the current date
    CUSIPs_remaining_interest <- as.matrix(which(interest_payments[,horizon_month]==0))
    
    # Calculate total remaining interest payments for all CUSIPs (occurring later than the current date)
    remaining_interest <- as.matrix(rowSums(interest_payments[,(horizon_month+1):dim(interest_payments)[2]]))
    rownames(remaining_interest) <- CUSIPs
    # Calculate interest payments occurring before the current date
    previous_interest <- as.matrix(rowSums(interest_payments[,1:(horizon_month-1)]))
    
    # CUSIPs with remaining interest payments
    non_zero_future_interest <- which(remaining_interest!=0)
    # Split CUSIPs between those with payments before the current date, and those without
    non_zero_past_interest <- which(previous_interest!=0)
    zero_past_interest <- which(previous_interest==0)
    
    # CUSIPs with no interest payment at current horizon, and with payments in the future
    CUSIPs_in_scope <- intersect(CUSIPs_remaining_interest,non_zero_future_interest)
    # Subset of CUSIPs_in_scope with payments in the past (use CF info to determine accrued interest)
    CUSIPs_CF_based <- intersect(CUSIPs_in_scope,non_zero_past_interest)
    # Subset of CUSIPs_in_scope with no payment in the past (use Master Table info to determine accrued interest)
    CUSIPs_HFV_based <- intersect(CUSIPs_in_scope,zero_past_interest)
    
    
    # Get list of payment dates occurring before and after the current horizon date
    dates_before_horizon <- which(as.Date(parse_date_time(payment_dates,orders="mdy"))<as.Date(parse_date_time(horizon_dates[i],orders="mdy")))
    dates_after_horizon <- which(as.Date(parse_date_time(payment_dates,orders="mdy"))>as.Date(parse_date_time(horizon_dates[i],orders="mdy")))
    
    if (length(CUSIPs_in_scope) > 0) {
	    for (j in 1:length(CUSIPs_in_scope))
	    {
	      current_CUSIP <- CUSIPs_in_scope[j]
	      
	      # Next date with an interest payment, in months from t0
	      next_payment <- min(intersect(which(interest_payments[current_CUSIP,]!=0),dates_after_horizon ))
	      
	      # Previous date with an interest payment, in months from t0
	      # Differentiate between bonds with a payment occurring in the cash flow file and those taking info from the Master Table
	      if (current_CUSIP %in% CUSIPs_CF_based) {
	        last_payment <- max(intersect(which(interest_payments[current_CUSIP,]!=0),dates_before_horizon ))
	      } else if (current_CUSIP %in% CUSIPs_HFV_based) {
	        payment_frequency_months <- as.numeric(CUSIP_information[current_CUSIP,"PaymentFrequency"])
	        
	        if (next_payment>payment_frequency_months) {
	          # The next coupon payment is occurring later than expected given the info in the HFV file.
	          # In that case the last_payment date is set as 0, the last coupon is expected to have occurred
	          # at t0. This is a best guess scenario given the data
	          last_payment <- 0
	        } else {
	          # In this case last_payment is negative
	          last_payment <- next_payment-payment_frequency_months
	        }
	      }
	      
	      # Calculate accrued interest
	      accrued_interest <- ((horizon_month-last_payment)/(next_payment-last_payment))*interest_payments[current_CUSIP,next_payment]
	      
	      # Enter adjustment to be made for the current CUSIP at the current horizon date
	      FV_correction[current_CUSIP,i] <- accrued_interest
	      
	    }    
	}
    
  }
  
  #return table containing all the neccessary FV adjustments for all CUSIPs for given scenario
  return(FV_correction)
  
}


#Function 5 : Data manipulation function
#Overview:
#This function calculates the combined table containing CFs from QRM and BRS
#Inputs include the table containing CUSIP information, the matrices containing BRS and QRM cash flows
#Returns a table containing the combined CFs for QRM and BRS

#QRM_cfs=QRM_scenario_principal
#BRS_cfs=BRS_scenario_principal

CF_transformation <- function(CUSIP_information,QRM_cfs,BRS_cfs) 
{
  # Replace NA's with 0 if there are some, then convert back to matrix format
  QRM_cfs <- as.data.frame(QRM_cfs)
  QRM_cfs[is.na(QRM_cfs)] <- 0
  QRM_cfs <- as.matrix(QRM_cfs)
  BRS_cfs <- as.data.frame(BRS_cfs)
  BRS_cfs[is.na(BRS_cfs)] <- 0
  BRS_cfs <- as.matrix(BRS_cfs)
  
  # Set all CUSIP names to uppercase to ensure matching across all data sources
  CUSIP_information$CUSIPs <- toupper(CUSIP_information$CUSIPs)
  
  # Get payment dates and CUSIP info
  CUSIPs <- CUSIP_information$CUSIPs
  
  #Ensure the CUSIP names in the BRS and QRM files only contain uppercase letters
  rownames(BRS_cfs) <- toupper(rownames((BRS_cfs)))
  rownames(QRM_cfs) <- toupper(rownames((QRM_cfs)))
  
  # Convert cash flow source to characters
  CF_source <- as.character(CUSIP_information$"CF_Source")
  # Identify the BRS and QRM CUSIPs in scope, as defined in the CUSIP main table
  BRS_in_scope <- subset(CUSIPs,CF_source == "BRS")
  QRM_in_scope <- subset(CUSIPs,CF_source == "QRM")
  BRS_CFs_rows_to_keep <- which(rownames(BRS_cfs) %in% BRS_in_scope)
  QRM_CFs_rows_to_keep <- which(rownames(QRM_cfs) %in% QRM_in_scope)
  
  # Only keep necessary CF rows and aggregate them
  BRS_cfs <- BRS_cfs[BRS_CFs_rows_to_keep,,drop=FALSE]
  QRM_cfs <- QRM_cfs[QRM_CFs_rows_to_keep,,drop=FALSE]
  all_CFs <- rbind(BRS_cfs,QRM_cfs)
  
  # Reorder the Cash Flows so they match the input CUSIP table
  scenario_cf <- all_CFs[CUSIPs,]
  
  return(scenario_cf)
}


# Function 6: Fetch Excel files
## Function to get an Excel file using the Excel_Files_Location table and the file's info
# It returns the file in a matrix format
# If new types of files are added to the Excel_Files table in Access this function needs to be adjusted
excel_file_fetcher <- function(excel_files_info,dbFolder,fileType,fileSource="",fileScenario="",fileSubType="")
{
  # The dbFolder string is necessary since the file's location is expressed as a subfolder to the DB folder
  # The fileSource, fileScenario, fileSubType are necessary for CF files. For other types of files these
  # arguments' values are ignored
  if (fileType == "CashFlows"){
    fileRow <- which((excel_files_info$FileType == fileType) & (excel_files_info$Source == fileSource) &
                       (excel_files_info$Scenario == fileScenario) & (excel_files_info$FileSubType == fileSubType))
    fileLocation <- as.character(excel_files_info$Subfolder[fileRow])
    filename <- as.character(excel_files_info$Filename[fileRow])
    
  } else {
    fileRow <- which(excel_files_info$FileType == fileType)
    fileLocation <- as.character(excel_files_info$Subfolder[fileRow])
    filename <- as.character(excel_files_info$Filename[fileRow])
  }
  setwd(paste0(dbFolder,fileLocation))
  
  if ((fileType == "CashFlows") & (fileSubType != "Reinvestments")){ # we check row names, they need to be unique
    fetchedFile <- as.matrix((read.csv(filename,header=T,row.names=1,sep=",",check.names = F)))
  } else {
    fetchedFile <- (read.csv(filename,header=T,row.names=NULL,sep=",",check.names = F))
  }
  
  return(fetchedFile)
}

transform_scenario_names <- function(from_scenario_name,to_scenarios_name_source) {
#			from_scenario_name <- current_scenario
#			to_scenarios_name_source <- unique(all_scenarios_spreads$'Scenario')
	
	if (grepl("base",from_scenario_name,ignore.case=TRUE)) {
		if (sum(grepl("base",to_scenarios_name_source,ignore.case=TRUE))==1) {
			return(as.character(to_scenarios_name_source[grep("base",to_scenarios_name_source,ignore.case=TRUE)]))
		} else {
			stop("Base Name is ambiguous.")
		}
	} else if (grepl("sev",from_scenario_name,ignore.case=TRUE)) {
		if (sum(grepl("sev",to_scenarios_name_source,ignore.case=TRUE))==1) {
			return(as.character(to_scenarios_name_source[grep("sev",to_scenarios_name_source,ignore.case=TRUE)]))
		} else {
			stop("Severe Adverse Name is ambiguous.")
		}
	} else if (grepl("adv",from_scenario_name,ignore.case=TRUE)&!grepl("sev",from_scenario_name,ignore.case=TRUE)) {
		tmp <- grepl("adv",to_scenarios_name_source,ignore.case=TRUE)&!grepl("sev",to_scenarios_name_source,ignore.case=TRUE)
		if (sum(tmp)==1) {
			return(as.character(to_scenarios_name_source[tmp]))
		} else {
			stop("Adverse Name is ambiguous.")
		}
	} else {
		stop("Scenario cannot be identified")
	}
}


# CRITICAL TOOLS DOCUMENTATION	
# 
# File Name|Supporting functions.r
# 
# Overview/PURPOSE|Help compute fair values of bonds in each CCAR scenario and starting spreads
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
# List of Inputs|Data from other CSIM R scripts
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
# 

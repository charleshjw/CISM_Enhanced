####################################
### Key Rate Durations Calculator
####################################
# This file computes the sensitivities of bonds in scope for CCAR by shocking the rates curve (or credit spread level)
# and recomputing the bond's fair values by discounting cash flows with the new rates
# Notes: this script needs floater margins to compute the IR duration of floaters. They are sourced from the HFV file
# and set to a minimum of zero (no negative)

# Clear Environment and then Console
#rm(list = ls())
#cat("\014")
### Load libraries: If package missing load it with: #install.packages("lubridate")
#library(lubridate)
library(PCICt)
library(plyr)
#library(stringr)
#library(RODBC)


##### Control ########################################################################################################
# Parameters to select what to run and whether we export results to the Access database
export_to_db <- "Yes"
##### Inputs
## List of tenors for which we compute KRDs (format Xm or Xy, should not be changed since DB table format matches it)
key_rate_tenors <- c("3m","6m","9m","1y","2y","3y","5y","7y","10y","20y","30y")
## Size of the shocks applied to the yield curve for KRDs, and for spread duration. Final result will be in $/1bp
base_shock <- 10
spread_shock <- 10
## Choice of shocks method for KRD calculation (results end up being very close)
# Method 1: shock each tenor by base_shock one at a time (plus neighboring tenors), method 2: shock everything up to tenor the same
method_number <- 1
### Path of Database and database name used to get input data ########################################################
#db_path <- dbc
#db_name <- "CSIM_Discounting_and_Summary_Tools.accdb"
## The working directory is chosen as a subfolder to the database. It contains the functions file we need to source
#workdingDir <- "R_Files\\"
#setwd(paste0(db_path,workdingDir))
#source("Supporting functions.R")
######################################################################################################################

################ Functions needed for sensitivities calculation
### This function calculates the FV for all CUSIPs for a one horizon date given a shock to the yield curve
# Inputs include the horizon dates, all the benchmark rates for specific scenario, CFs for specific scenario
# Returns a table containing the FV for all CUSIPs for all horizons for a given scenario
shocked_rates_FV_calculator <- function(scenario_DCF,horizon_dates,scenario_rates,CF_dates,scenario_spread,scenario_CFs,
                                        CUSIPs,floating_CUSIPs,fixed_rate_CUSIPs,CUSIP_information,all_fixed_curves,
                                        benchmark_curves,days_in_year,tenor_shock_sizes,calculation_date)
{
  #Specify purchase dates for all CUSIPs
  purchase_dates <- as.matrix(CUSIP_information$`PurchaseDate`)
  purchase_dates <- as.numeric(as.Date(parse_date_time(purchase_dates,orders="mdy")))
  
  # Set current period
  i <- which(as.Date(parse_date_time(horizon_dates,orders="mdy")) ==as.Date(parse_date_time(calculation_date,orders="mdy")))
  period <- (-3*(i-1))
  if (i==1) {period<-0}
  #calculate rates for all fixed rate securities for given horizon 
  fixed_CF_rates<-as.data.frame(fixed_rates_all(all_fixed_curves,i, horizon_dates, CF_dates, benchmark_curves)) 
  #Get the future payment dates for this scenario
  fixed_CF_dates <- as.matrix(as.Date(parse_date_time(CF_dates,orders="mdy")))
  base_date <- calculation_date
  base_date <- as.numeric(as.Date(parse_date_time(base_date,orders="mdy")))
  #calculate the difference from each payment date and the current horizon period (e.g. Jan 2015 will be 31 days away from horizon period 1-December 2014)
  date_diff <- fixed_CF_dates-base_date
  
  #Create temporary table that contains the benhmark rate for all CUSIPs 
  temp_rates <- as.data.frame(scenario_rates)
  
  #Initialize table that will containg benhmark rate + spread for all CUSIPs in portfolio
  discount_rates <- as.data.frame(temp_rates) 
  
  #Replace rates for fixed rate securities given 
  for(z in 1:length(benchmark_curves))
  {
    #find the position for each fixed rate security for the given benchmark curve
    fixed_rate_position <- which(CUSIP_information$`FixedFloating`=="Fixed" & CUSIP_information$`InternalCurveType` == benchmark_curves[z])
    
    #replace the rates for the fixed rate securities with the corresponding rates calculated earlier in this function 
    if (length(fixed_rate_position) >0)
    {temp_rates[fixed_rate_position,]<-fixed_CF_rates[z,]}
  }
  
  ## We now have all the rates set up so we can add the same tenor specific shocks to all curves (replicating the shock vector as many times as we have rows in temp_rates)
  temp_rates <- temp_rates + matrix(rep(tenor_shock_sizes,each=dim(temp_rates)[1]),nrow=dim(temp_rates)[1])/100
  
  
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
  #If we are calculating period 1, do not change CFs, rates and payment dates 
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
  #Fv for each CUSIP will be the sum of each future CF over the corresponding discount factor 
  scenario_DCF <- rowSums(temp_CFs/discount_factors)
  
  #Exclude securities that haven't been purchased as of this period-used for reinvestment assumptions
  exclude_CUSIPs <- which(purchase_dates>as.numeric(as.Date(parse_date_time(horizon_dates[i],orders="mdy"))))
  scenario_DCF[exclude_CUSIPs]<-0
  

  #Return table containing the FV for all CUSIPs 
  return (scenario_DCF) 
}

### Adjustment for clean price  
# This function calculates the FV adjustment for all CUSIPs for a given horizon date for a single scenario
# Inputs include the matrix containing the interest payments for all CUSIPs, the payment dates and scenario dates
# Returns a table containing the FV adjustment (accrued interest to be subtracted from dirty FV) for all CUSIPs for all horizons for a given scenario.
clean_price_adjustment_single_date <- function(FV_correction,interest_payments,single_horizon_date,CUSIPs,payment_dates)
{
  ### First check that the horizon date we're looking at is on or after the first cash flow date, otherwise return the vector of 0
  if (as.Date(parse_date_time(single_horizon_date,orders="mdy"))>=as.Date(parse_date_time(payment_dates[1],orders="mdy"))){  
    #get months from t0 for current discounting period
    horizon_month <- which(as.Date(payment_dates,"%m/%d/%Y")==as.Date(single_horizon_date,"%m/%d/%Y"))
    
    #find CUSIPs that have a zero interest payment for current horizon
    CUSIPs_remaining_interest <- as.matrix(which(interest_payments[,horizon_month]==0))
    
    #find remaining interest for all CUSIPs
    remaining_interest <- as.matrix(rowSums(interest_payments[,(horizon_month+1):dim(interest_payments)[2]]))
    rownames(remaining_interest) <- CUSIPs
    #find CUSIPs that have had at least one interest payment in the past 
    previous_interest <- as.matrix(rowSums(interest_payments[,1:(horizon_month-1)]))
    
    #CUSIPs that have non zero remaining interest
    non_zero_future_interest <- which(remaining_interest!=0)
    #CUSIPs that have non zero interest payments in the past (neccessary to calculate accrued interest)
    non_zero_past_interest <- which(previous_interest!=0)
    
    #CUSIPs that do not have an interest payment in current horizon and have remaining interest payments
    CUSIPs_in_scope <- intersect(CUSIPs_remaining_interest,non_zero_future_interest)
    #CUSIPs that do not have an interest payment in current horizon, have remaining interest payment and and have interest payments in the past
    CUSIPs_in_scope <- intersect(CUSIPs_in_scope,non_zero_past_interest)
    
    #We now have the CUSIPs that do not have an interest payment for the current horizon, at least one interest payment in the future and at least one interest payment in the past 
    
    
    #find payment dates before and after the horizon date
    dates_before_horizon <- which(as.Date(parse_date_time(payment_dates,orders="mdy"))<as.Date(parse_date_time(single_horizon_date,orders="mdy")))
    dates_after_horizon <- which(as.Date(parse_date_time(payment_dates,orders="mdy"))>as.Date(parse_date_time(single_horizon_date,orders="mdy")))
    
    
    for (j in 1:length(CUSIPs_in_scope))
    {
      
      #last period with an interest payment in months from t0
      last_payment <- max(intersect(which(interest_payments[CUSIPs_in_scope[j],]!=0),dates_before_horizon ))
      #next period with an interest payment in months from t0
      next_payment <- min(intersect(which(interest_payments[CUSIPs_in_scope[j],]!=0),dates_after_horizon ))
      
      #find accrued interest
      accrued_interest <- ((horizon_month-last_payment)/(next_payment-last_payment))*interest_payments[CUSIPs_in_scope[j],next_payment]
      
      #find correction in FV to be made at the end 
      FV_correction[CUSIPs_in_scope[j]] <- accrued_interest
      
    }
  }
  
  # Return table containing all the neccessary FV adjustments for all CUSIPs for given scenario and given horizon date
  return(FV_correction)
  
}

### This function calculates the shocks to be applied to each monthly rate
# Inputs: empty shock vector (monthly, for size), the index of the current key rate tenor, list of key rate tenors we're computing,
# the base shock size, a number of months offset (if we compute KRDs for a start date other than t0), and method number we are using
# Method 1: the current tenor is shocked while others are set at 0 (intermediate tenors are interpolated)
# Method 2: every tenor up to the key rate tenor we're looking at is shocked the same (2y tenor: everything up to 2y is shocked, everything after is constant)
key_rate_shocks_calculator <- function(tenor_shock_sizes,current_tenor,key_rate_tenors_in_months,base_shock,
                                       horizon_date_month_offset,method_number)
{
  # current_tenor is the index of the tenors in the vector key_rate_tenors_in_months
  nb_tenors <- length(key_rate_tenors_in_months)
  if (nb_tenors > 1) {
    if (method_number == 1) {
      if (current_tenor == 0) {
        # We do not shock the rates in that case: this will give us the reference FV
      } else if (current_tenor == 1) {
        # Shock everything up to the first tenor like the first tenor
        tenor_shock_sizes[(1+horizon_date_month_offset):(key_rate_tenors_in_months[1]+horizon_date_month_offset)] <- base_shock
        # Interpolation of shocks if next tenor is at least 2 months away
        x <- key_rate_tenors_in_months[current_tenor:(current_tenor+1)]
        if((x[2]-x[1]) > 1) {
          y <- c(base_shock,0)
          interpolated_months <- (x[1]+1):(x[2]-1)
          interpolation <- approx(x,y,interpolated_months)
          tenor_shock_sizes[interpolated_months+horizon_date_month_offset] <- interpolation$y
        }
      } else if (current_tenor == nb_tenors) {
        # Shock everything after the last tenor like the last tenor
        tenor_shock_sizes[(key_rate_tenors_in_months[current_tenor]+horizon_date_month_offset):length(tenor_shock_sizes)] <- base_shock
        # Interpolation of shocks if previous tenor is at least 2 months away
        x <- key_rate_tenors_in_months[(current_tenor-1):current_tenor]
        if((x[2]-x[1]) > 1) {
          y <- c(0,base_shock)
          interpolated_months <- (x[1]+1):(x[2]-1)
          interpolation <- approx(x,y,interpolated_months)
          tenor_shock_sizes[interpolated_months+horizon_date_month_offset] <- interpolation$y
        }
      } else {
        tenor_shock_sizes[key_rate_tenors_in_months[current_tenor]+horizon_date_month_offset] <- base_shock
        # Interpolation for shorter tenors
        x <- key_rate_tenors_in_months[(current_tenor-1):current_tenor]
        if((x[2]-x[1]) > 1) {
          y <- c(0,base_shock)
          interpolated_months <- (x[1]+1):(x[2]-1)
          interpolation <- approx(x,y,interpolated_months)
          tenor_shock_sizes[interpolated_months+horizon_date_month_offset] <- interpolation$y
        }
        # Interpolation for longer tenors
        x <- key_rate_tenors_in_months[current_tenor:(current_tenor+1)]
        if((x[2]-x[1]) > 1) {
          y <- c(base_shock,0)
          interpolated_months <- (x[1]+1):(x[2]-1)
          interpolation <- approx(x,y,interpolated_months)
          tenor_shock_sizes[interpolated_months+horizon_date_month_offset] <- interpolation$y
        }
      }
    } else if (method_number == 2) {
      if (current_tenor == 0) {
        # We do not shock the rates in that case: this will give us the reference FV
      } else {
        # Shock everything up to the current tenor the same
        tenor_shock_sizes[(1+horizon_date_month_offset):(key_rate_tenors_in_months[current_tenor]+horizon_date_month_offset)] <- base_shock
      }
    }
  } else {
    # If there is only one tenor then the whole curve is shocked at once
    tenor_shock_sizes[1:length(tenor_shock_sizes)] <- base_shock * current_tenor # Tenor 0: reference, tenor 1: only tenor
  }
  return(tenor_shock_sizes)
}

### Computes the floater spread margin for instruments for which we either don't have information or spread is 0
# It computes the margin only if we have cash flows going further than one quarter, otherwise computations will fail
# We first compute the effective coupon (interest payment/remaining principal balance) each month
# Then we can find the reset frequency of the coupon
# And then we compare that coupon with the underlying interest rate to find the margin: coupon - underlying rate
floater_spread_margin_calculator<- function(CUSIP,CUSIP_information,scenario_interest,scenario_principal,scenario_rates) 
{
  # Get Cusip information
  iCusip <- which(rownames(scenario_interest) == CUSIP)
  infoCusip <- CUSIP_information[iCusip,]
  interests <- scenario_interest[iCusip,]
  principals <- scenario_principal[iCusip,]
  principals_remaining <- rev(cumsum(rev(principals)))
  last_principal_month <- sum(principals_remaining>0)
  
  
  # Keep going if the CUSIP has any cash flows after the first quarter, otherwise floater spread is set to 0
  if (last_principal_month>3){
    payment_frequency <- which(interests!=0)[2]-which(interests!=0)[1]
    effective_coupon <- (12/payment_frequency)*100*(interests[1:last_principal_month])/(principals_remaining[1:last_principal_month])
    
    ## Determine the frequency at which the coupon is reset: find how long it takes for the effective rate to change 
    # (we ignore the first value, because if coupon resets every 6 months, pays monthly and we are at M>1 it would give a wrong result)
    first_ratio <- effective_coupon[1]
    second_ratio_loc <- 0
    third_ratio_loc <- 0
    coupon_reset_frequency <- 0
    for (iRatios in 2:last_principal_month){
      if ((abs(effective_coupon[iRatios]-first_ratio) > 0.01 ) & (second_ratio_loc == 0)) {
        second_ratio_loc <- iRatios
      } else if(second_ratio_loc>0){
        if ((abs(effective_coupon[iRatios]-effective_coupon[second_ratio_loc])>0.01) & (third_ratio_loc == 0)) {
          third_ratio_loc <- iRatios
          coupon_reset_frequency <- third_ratio_loc - second_ratio_loc
          break
        }
      }
    }
    
    if (coupon_reset_frequency == 0) {
      # It means something went wrong and the loop finished without finding the reset frequency:
      # Use the reset index as the reset frequency
      floating_reset <- as.character(infoCusip$'FloatingReset')
      coupon_reset_frequency <- strtoi(substr(floating_reset,0,nchar(floating_reset)-1))
    }
    
    # Get CUSIP's underlying rate on which the coupon is based, then shift series to account for reset period
    rates <- scenario_rates[iCusip,1:last_principal_month]
    time_matched_rates <- matrix(rates[1],ncol=length(rates))
    if (coupon_reset_frequency<last_principal_month){
      time_matched_rates[(coupon_reset_frequency+1):length(rates)] <- rates[1:(length(rates)-coupon_reset_frequency)]
    }
    
    
    if (as.character(infoCusip$'CF_Source') == "QRM") {
      max_index_effective_coupon <- max(length(effective_coupon[!is.na(effective_coupon)]),60)
    } else{
      max_index_effective_coupon <- length(effective_coupon[!is.na(effective_coupon)])
    }
    
    # Keep values up to the max index defined above and remove zeros
    effective_coupon <- effective_coupon[1:max_index_effective_coupon]
    non_zero_indices <- which(effective_coupon>0)
    implied_spread <- effective_coupon[non_zero_indices]-time_matched_rates[non_zero_indices]
    
    if (length(implied_spread)>6) {
      # Remove values in highest and lowest deciles if the series is long enough
      implied_spread_computation <- implied_spread[(implied_spread<quantile(implied_spread,0.9)) & (implied_spread>quantile(implied_spread,0.1))]
    } else {
      implied_spread_computation <- implied_spread
    }
    
    floater_spread <- max(mean(implied_spread_computation),0)*100 # in basis points
    
    #     print(infoCusip) # to be removed
    #     print("Principals remaining") # to be removed
    #     print(principals_remaining[1:last_principal_month]) # to be removed
    #     print("coupon_reset_frequency:")
    #     print(coupon_reset_frequency)
    #     indices <- which(!is.na(effective_coupon))
    #     ratio_vs_rate <- effective_coupon[1:last_principal_month]-time_matched_rates[1:last_principal_month]
    #     ymin <- min(time_matched_rates,effective_coupon[!is.na(effective_coupon)],ratio_vs_rate)
    #     ymax <- max(time_matched_rates,effective_coupon[!is.na(effective_coupon)],ratio_vs_rate)
    #     plot(effective_coupon[!is.na(effective_coupon)],ylim=c(ymin,ymax))
    #     lines(time_matched_rates[indices],col="red")
    #     lines(ratio_vs_rate,col="blue",lty=2)
    #     legend('topright',c("Effective Coupon","Rate","Coupon - Rate"),fill=c("black","red","blue"))
    
  } else {
    floater_spread <- 0
  }
  return(floater_spread)
}
################ End of Functions 

##### Convert key rate tenors we get as input into a number of months
nb_tenors <- length(key_rate_tenors)
key_rate_tenors_in_months <- matrix(0,ncol=nb_tenors)
for (i in 1:nb_tenors) {
  current_tenor <- key_rate_tenors[i]
  nb_characters <- nchar(current_tenor)
  unit <- substr(current_tenor,nb_characters,nb_characters) # whether tenor is in months or years ('m' or 'y')
  number_of <- as.numeric(substr(current_tenor,1,nb_characters-1)) # how many years or months in that tenor
  
  if (unit == "m" | unit == "M") {
    key_rate_tenors_in_months[i] <- number_of # tenor is already given in months
  } else if (unit == "y" | unit == "Y") {
    key_rate_tenors_in_months[i] <- number_of*12 # tenor given in years
  }
}


##### Get all data from DB, getting all scenarios data
db_conn <- dbc
# List of scenarios we loop through
scenario_names <- ScenarioNames

# File and folder information for file in Excel format
# Dates
quarters_generic <- c("Q0","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9")
CCAR_horizons <-  format(subset(Dates_Master,Horizon %in% quarters_generic)$HorizonDate,"%m/%d/%Y")
# CUSIP Master Table (same for all scenaios)
CUSIP_information <- fetchTable(dbc,"CUSIP_Master_Table_Permanent",current_version)
CUSIP_information <- CUSIP_information[,c("CUSIPs","AssetClass","FixedFloating","Currency","InternalCurveType",
                                          "CF_Source","FloatingReset","PurchaseDate","AdjustmentMultiplier")]
# Get CUSIPs names ordered
CUSIPs_list_ordered <- as.character(CUSIP_information$CUSIPs)
CUSIPs <- droplevels(CUSIP_information$CUSIPs)
# HFV file: we only keep the CUSIPs and the floaters' spread margin, and only Intrader instruments
HFV_file <- fetchTable(dbc, "HFV_File_Permanent",HFV_File_Permanent_)
HFV_file <- HFV_file[,c("CUSIP","FloaterMargin_DailyHFV","System Source")]
HFV_file <- ddply(subset(HFV_file,HFV_file$'System Source' == "Intrader"),"CUSIP",summarise,floater_margin_spread = FloaterMargin_DailyHFV[1])
rownames(HFV_file) <- HFV_file$CUSIP
HFV_file <- HFV_file[CUSIPs_list_ordered,]
CUSIP_information$floater_margin_spread <- HFV_file$floater_margin_spread
# FX Rates
FX_rates <- fetchTable(dbc,"FX_Spot_Rates",current_version)
rownames(FX_rates) <- FX_rates$Currency_HFV
# CUSIP spreads forecast and fixed rates yield cruves over 10 quarters, floating rates forwards for all CF dates
all_scenarios_spreads <- fetchTable(dbc,"CUSIP_Spreads_Forecasts",current_version)

# CUSIP spreads forecast and fixed rates yield cruves over 10 quarters, floating rates forwards for all CF dates
if (!"all_fixed_rates" %in% ls()) {
	filename = concat_file_path(tableFilter(list_files,"VariableName","FixedRates"))
	all_fixed_rates <- read.csv(filename,header=T,row.names=NULL,sep=",",check.names = F)
}
if (!"all_floating_rates" %in% ls()) {
	filename = concat_file_path(tableFilter(list_files,"VariableName","FloatingRates"))
	all_floating_rates <- read.csv(filename,header=T,row.names=NULL,sep=",",check.names = F)
}


## Non scenario specific information/parameters
# Current version running
running_version <- current_version
# Time used for timestamps in output tables
current_time <- format(Sys.time(),"%m/%d/%Y %I:%M:%S %p")
days_in_year <- 360 # Days in a year

for (iScenarios in 1:length(scenario_names)) {
  current_scenario <- as.character(scenario_names[iScenarios])
  print(current_scenario)
  
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
  scenario_spread <- subset(all_scenarios_spreads,Scenario == current_scenario)[,c("CUSIPs",quarters_generic)]
  rownames(scenario_spread) <- scenario_spread$CUSIPs
  scenario_spread$CUSIPs <- NULL
  colnames(scenario_spread) <- CCAR_horizons
  scenario_spread <- as.matrix(scenario_spread[CUSIPs_list_ordered,])
  scenario_spread <- scenario_spread/100  # from bps to percentage points
  
  ## Benchmark rates for floating rate securities for each scenario. Set rownames as rate names
  scenario_benchmark <- subset(all_floating_rates,Scenario == current_scenario)
  rownames(scenario_benchmark) <- scenario_benchmark$Rate
  scenario_benchmark$Rate <- scenario_benchmark$Scenario <- NULL
  
  ## Yield curves for fixed rate securities
  scenario_fixed_curves <- subset(all_fixed_rates,Scenario == current_scenario)
  scenario_fixed_curves$Scenario <- NULL

  ##### Setting up the cash flow tables and parameters
  # Get payment dates for all CFs 
  payment_dates <- colnames(BRS_scenario_interest)
  # Create tables that combine QRM and BRS cash flows, and combined
  scenario_principal <- CF_transformation(CUSIP_information,QRM_scenario_principal,BRS_scenario_principal)
  scenario_interest <- CF_transformation(CUSIP_information,QRM_scenario_interest,BRS_scenario_interest)
  scenario_CFs <- scenario_principal + scenario_interest

  
  ### Discount rate calculations for floating securities. 
  # Discount rates for fixed rate securities are calculated within the DCF_calculator function as they change for each horizon based on the corresponding yield curve
  #Initialize table. Number of rows is equal to the number of CUSIPs and number of columns is equal to the length of the cash flows 
  scenario_rates<-as.data.frame(matrix(0,nrow=length(CUSIPs),ncol=length(payment_dates)))
  rownames(scenario_rates) <- CUSIPs
  colnames(scenario_rates) <- payment_dates
  
  ## Fill dicount rates for floating CUSIPs: Map each floating CUSIP to the corresponding benchmark rate
  # benchmark_curves contains the unique name for all benchmark rates to be used for discounting 
  benchmark_curves <- droplevels(unique(CUSIP_information$`InternalCurveType`))
  for (j in (1:length(unique(CUSIP_information$`InternalCurveType`))))    
  {
    # For each benchmark curve and tenor, find the corresponding CUSIPs (floating rate securities)
    floater_1M <- which(CUSIP_information$`InternalCurveType`==benchmark_curves[j] & CUSIP_information$`FloatingReset` =="1M")
    floater_3M <- which(CUSIP_information$`InternalCurveType`==benchmark_curves[j] & CUSIP_information$`FloatingReset` =="3M")
    floater_6M <- which(CUSIP_information$`InternalCurveType`==benchmark_curves[j] & CUSIP_information$`FloatingReset` =="6M")
    floater_12M <- which(CUSIP_information$`InternalCurveType`==benchmark_curves[j] & CUSIP_information$`FloatingReset` =="12M")
    
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
  #Change type of tables containing rate information to matrix for easier manipulation
  scenario_rates <- as.matrix(scenario_rates)


  # Identify fixed rate and floating rate CUSIPs
  fixed_rate_CUSIPs <- which(CUSIP_information$`FixedFloating` == "Fixed")
  floating_CUSIPs <- which(CUSIP_information$`FixedFloating` == "Floating")


  ##### Loop through each of the CCAR horizons to compute sensitivities
  for (iCalcDates in 1:length(CCAR_horizons)) {
    calculation_date <- CCAR_horizons[iCalcDates]
    print(calculation_date)
    ### Calculation date has an impact on calculation and data needs to be shifted to take this into account
    if (as.numeric(as.Date(parse_date_time(calculation_date,orders="mdy")))<as.numeric(as.Date(parse_date_time(payment_dates[1],orders="mdy")))){
      calculation_date_month_index <- 0
    } else {
      calculation_date_month_index <- which(as.Date(parse_date_time(calculation_date,orders="mdy")) == as.numeric(as.Date(parse_date_time(payment_dates,orders="mdy"))))
    }
    
    
    ### Floating instruments
    # Replace the current cash flows to only take into account the next principal payment and the floater margin risk
    scenario_CFs_float_adjusted <- scenario_CFs
    for (iFloaters in 1:length(floating_CUSIPs)) {
      iCusip <- floating_CUSIPs[iFloaters]
      current_CUSIP <- CUSIP_information[iCusip,'CUSIPs']
      # Floater margin is floored to 0 (floater_spread_margin_calculator can be used to compute it if needed)
      floater_margin_spread <- min(CUSIP_information$floater_margin_spread[iCusip],0)
      #     if (floater_margin_spread < 1){
      #       floater_margin_spread <- floater_spread_margin_calculator(current_CUSIP,CUSIP_information,scenario_interest,scenario_principal,scenario_rates)
      #     }
      
      ## Compute the cash flows for duration computation using the floater spread margin, remaining principal balance, 
      # and the next principal payment. Divide by 12 to take into account a yearly spread margin while we have monthly payments
      principal_CFs <- scenario_principal[iCusip,]
      principals_remaining <- rev(cumsum(rev(principal_CFs)))
      # Margin spread cash flows
      new_CFs <- principals_remaining * 0.0001 * floater_margin_spread/12
      # Get first principal payment date and value occuring after the horizon date for which we're computing duration
      following_principal_month_index <- sum(!(which(principal_CFs>0)>calculation_date_month_index))+1
      new_CFs[following_principal_month_index] <- scenario_CFs[floating_CUSIPs[iFloaters],following_principal_month_index]
      scenario_CFs_float_adjusted[floating_CUSIPs[iFloaters],] <- new_CFs
    }
  
  
    ##### Key rate durations computation setup
    ## Create matrix that contains the fair value of each CUSIP for each shocked yield curve (and the initial curve)
    shocked_FV <- matrix(0,nrow=length(CUSIPs),ncol=nb_tenors+1)
    ## Create matrix that will contain the key rate durations in $/bp
    dollar_key_rate_duration <- matrix(0,nrow=length(CUSIPs),ncol=nb_tenors)
    
    ### Loop through each tenor to shock the rates and recompute fair value under the new conditions
    ### The location of the shocks needs to be offset to take into account the calculation date we're looking at (if different from t0)
    horizon_date_month_offset <- (which(as.Date(CCAR_horizons,"%m/%d/%Y")==as.Date(calculation_date,"%m/%d/%Y"))-1)*3
    for (i in 1:(nb_tenors+1)) {
      # Used in the shock computation function, 0 means no shock
      current_tenor <- i-1
      tenor_shock_sizes <- matrix(0,ncol=length(payment_dates))
      tenor_shock_sizes <- key_rate_shocks_calculator(tenor_shock_sizes,current_tenor,key_rate_tenors_in_months,base_shock,horizon_date_month_offset,method_number)  
      
      ##### DCF calculations
      # Create table containing DCF calculations and a table containing the adjustments for clean price 
      scenario_DCF <- scenario_FV_adjustment <- matrix(0,nrow=length(CUSIPs),ncol=1)
      # Functions 'shocked_rates_FV_calculator' and 'clean_price_adjustment_single_date' are defined at the top
      scenario_DCF <- shocked_rates_FV_calculator(scenario_DCF,CCAR_horizons,scenario_rates,payment_dates,scenario_spread,
                                                  scenario_CFs_float_adjusted,CUSIPs,floating_CUSIPs,fixed_rate_CUSIPs,CUSIP_information,
                                                  scenario_fixed_curves,benchmark_curves,days_in_year,tenor_shock_sizes,calculation_date)
      scenario_FV_adjustment <- clean_price_adjustment_single_date(scenario_FV_adjustment,scenario_interest,calculation_date,CUSIPs,payment_dates)
      # Clean FV is found by subtracting accrued interest for each CUSIP and for all horizons
      scenario_DCF <- scenario_DCF-scenario_FV_adjustment
      shocked_FV[,i] <- scenario_DCF
    }
    
    ## Now compute changes in fair value for each shocked curve, divided by shock size, with correct sign
    # Convert to dollars, scale for BRS CF issue and compute total IR duration
    if (nb_tenors > 1) {
      # Depending on the way rates are shocked the FV change is computed differently
      if (method_number == 1) {
        dollar_key_rate_duration <- (shocked_FV[,2:(nb_tenors+1)] - shocked_FV[,1])/(-base_shock)
      } else if (method_number == 2) {
        dollar_key_rate_duration <- (shocked_FV[,2:(nb_tenors+1)] - shocked_FV[,1:nb_tenors])/(-base_shock)
      }
      
      ## Convert numbers to dollar and scale using BRS CF ratio
      for (i in 1: length(CUSIPs)) {
        FX <- FX_rates$FXRate[rownames(FX_rates) == as.character(CUSIP_information$Currency[i])]
        dollar_key_rate_duration[i,] <- dollar_key_rate_duration[i,] * FX * CUSIP_information$AdjustmentMultiplier[i]
      }
      total_dollar_duration <- rowSums(dollar_key_rate_duration)
      dollar_key_rate_duration <- cbind(dollar_key_rate_duration,total_dollar_duration)
      colnames(dollar_key_rate_duration) <- c(key_rate_tenors,"TotalRatesDuration")
    } else { # Only one tenor: total rates duration
      dollar_key_rate_duration <- (shocked_FV[,2] - shocked_FV[,1])/(-base_shock)
      ## Convert numbers to dollar and scale using BRS CF ratio
      for (i in 1: length(CUSIPs)) {
        FX <- FX_rates$FXRate[rownames(FX_rates) == as.character(CUSIP_information$Currency[i])]
        dollar_key_rate_duration[i] <- dollar_key_rate_duration[i] * FX * CUSIP_information$AdjustmentMultiplier[i]
      }
      colnames(dollar_key_rate_duration) <- "TotalRatesDuration"
    }
    
    
    ##### Spread duration calculation
    ### Compute each CUSIP's fair value in the regular base case, and another time with shocked spreads
    ### We want key rate durations expressed in dollars for each tenor and for each CUSIP
    scenario_spread_shocked <- scenario_spread + spread_shock/100
    dollar_spread_duration <- matrix(0,nrow=length(CUSIPs),ncol=1)
    shocked_FV_spread <- matrix(0,nrow=length(CUSIPs),ncol=2)
    tenor_shock_sizes <- matrix(0,ncol=length(payment_dates))
    
    ## No shock computation of FV first, using the same functions as for the key rate duration calculations
    # No shock
    scenario_DCF <- scenario_FV_adjustment <- matrix(0,nrow=length(CUSIPs),ncol=1)
    scenario_DCF <- shocked_rates_FV_calculator(scenario_DCF,CCAR_horizons,scenario_rates,payment_dates,scenario_spread,
                                                scenario_CFs,CUSIPs,floating_CUSIPs,fixed_rate_CUSIPs,CUSIP_information,
                                                scenario_fixed_curves,benchmark_curves,days_in_year,tenor_shock_sizes,calculation_date)
    scenario_FV_adjustment <- clean_price_adjustment_single_date(scenario_FV_adjustment,scenario_interest,calculation_date,CUSIPs,payment_dates)
    scenario_DCF <- scenario_DCF-scenario_FV_adjustment
    shocked_FV_spread[,1] <- scenario_DCF
    # Shocked spreads
    scenario_DCF <- scenario_FV_adjustment <- matrix(0,nrow=length(CUSIPs),ncol=1)
    scenario_DCF <- shocked_rates_FV_calculator(scenario_DCF,CCAR_horizons,scenario_rates,payment_dates,scenario_spread_shocked,
                                                scenario_CFs,CUSIPs,floating_CUSIPs,fixed_rate_CUSIPs,CUSIP_information,
                                                scenario_fixed_curves,benchmark_curves,days_in_year,tenor_shock_sizes,calculation_date)
    scenario_FV_adjustment <- clean_price_adjustment_single_date(scenario_FV_adjustment,scenario_interest,calculation_date,CUSIPs,payment_dates)
    scenario_DCF <- scenario_DCF-scenario_FV_adjustment
    shocked_FV_spread[,2] <- scenario_DCF
    
    ## Compute changes in fair value
    dollar_spread_duration <- (shocked_FV_spread[,2] - shocked_FV_spread[,1])/(-spread_shock)
    
    ## Convert numbers to dollar and scale using BRS CF ratio
    for (i in 1: length(CUSIPs)) {
      FX <- FX_rates$FXRate[rownames(FX_rates) == as.character(CUSIP_information$Currency[i])]
      dollar_spread_duration[i] <- dollar_spread_duration[i] * FX * CUSIP_information$AdjustmentMultiplier[i]
    }
    
    
    ### Output results to database
    if (export_to_db == "Yes") {
      db_conn <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",db_path,db_name))
      # Get IR duration and Spread duration in the same table
      output_table <- as.data.frame(round(cbind(dollar_key_rate_duration,dollar_spread_duration),2))
      colnames(output_table) <- c(key_rate_tenors,"TotalRatesDuration","SpreadDuration")
      output_table$CUSIPs <- CUSIPs
      output_table[,c('Version','TimeStamp','Scenario','AsOfDate')] <- 
        rep(c(running_version,current_time,current_scenario,calculation_date),each=length(CUSIPs))
      output_table <- output_table[,c("Version","TimeStamp","Scenario","AsOfDate","CUSIPs",key_rate_tenors,"TotalRatesDuration","SpreadDuration")]  
      sqlSave(db_conn, output_table, tablename = "CUSIP_Sensitivities",append = TRUE,rownames = FALSE,safer = TRUE,fast = TRUE)
    }
    
  }
}

odbcCloseAll()


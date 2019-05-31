# Reformat IV data from table to only columns

#Initialization of tool
# Clear Environment and then Console
#rm(list = ls())
cat("\014")
### Load libraries: If package missing load it with: #install.packages("lubridate")
# This file could use the melt function in library(reshape2): scenario_data_reformatted <- melt(scenario_data_input,id.vars=c("Quarter"))
library(lubridate)
library(PCICt)
library(stringr)
library(plyr)
library(RODBC)
library(xlsx)
library(reshape2)

### The BRS CF input file should be formatted to be in column format already, only containing the CUSIPs we need
# The column names should match the format used here. The necessary columns are the following:
# Scenario, CUSIP, Interest, Principal, CF_Date with numbers expressed in local currency (or dollars, but not percent)
# CF_Date must have been transformed to end of month using the Excel formula eomonth() so it matches QRM CF dates

CCAR_portfolio_date <- format(as.Date(as.yearqtr(current_version_info$History_END_DATE)+0.25)-1,"%m/%d/%Y")
CCAR_run_name <- current_version # used in output cash flow name
# Get input data: CFs from BRS and CF dates
CFs_input_file <- concat_file_path(tableFilter(list_files,"VariableName","BRS_Cashflow"))
CFs_input <- as.data.frame((read.csv(CFs_input_file,header=T,row.names=NULL,sep=",",check.names = F)))
#CFS_dates_dummy <- as.data.frame((read.csv("//mfmaog02/ERMShared/Treasury Risk/GTRM_IRRLiq_QA/CSIM/Enhance2018/ModelImplementation/Inputs/DiscountingTables/CashFlow/CFs_dates_dummy - DFAST2018.csv",header=T,row.names=NULL,sep=",",check.names = F)))

# Get scenario names
BRS_scenario_names <- as.character(unique(CFs_input$'Scenario'))

output_scenario_names <- rep("",length(BRS_scenario_names))
for (i in 1:length(BRS_scenario_names)) {
#	i = 3
	if (grepl("base",BRS_scenario_names[i],ignore.case=TRUE)) {
		output_scenario_names[i] <- ScenarioNames[grep("base",ScenarioNames,ignore.case=TRUE)]
	} else if (grepl("sev",BRS_scenario_names[i],ignore.case=TRUE)) {
		output_scenario_names[i] <- ScenarioNames[grep("sev",ScenarioNames,ignore.case=TRUE)]
	} else if (grepl("adv",BRS_scenario_names[i],ignore.case=TRUE)&!grepl("sev",BRS_scenario_names[i],ignore.case=TRUE)) {
		output_scenario_names[i] <- ScenarioNames[grepl("adv",ScenarioNames,ignore.case=TRUE)&!grepl("sev",ScenarioNames,ignore.case=TRUE)]
	} else {
		stop("BRS Cash Flow Formatting Error: Output Scenario Name cannot be matched. Check scenario names")
	}
}	

# Get rid of rows corresponding to the CCAR portfolio date
valid_rows <- which(as.Date(parse_date_time(CFs_input$CF_Date,orders="mdy")) > as.Date(parse_date_time(CCAR_portfolio_date,orders="mdy")))
CFs_input <- CFs_input[valid_rows,]
# Set up dummy cash flows in column format: this will be used to have all cash flow dates in output more easily
# We match column names and order to align columns more easil
dummy_dates <- as.Date(as.character(unique(CFs_input$CF_Date)),format="%m/%d/%Y")
CFS_dates_dummy <- data.frame(matrix(rep(NA,1+length(dummy_dates)),nrow=1))
colnames(CFS_dates_dummy) <- c("Cusip",format(dummy_dates,"%m/%d/%Y"))
CFS_dates_dummy[1,"Cusip"] = "Dummy"

CFs_dates_dummy_interest <- melt(CFS_dates_dummy,id.vars = c("Cusip"),variable.name="CF_Date",value.name="Interest")
CFs_dates_dummy_interest<- rename(CFs_dates_dummy_interest, c("Cusip"="CUSIP"))[,c("CUSIP","Interest","CF_Date")]
CFs_dates_dummy_principal <- rename(CFs_dates_dummy_interest, c("Interest"="Principal"))
# Get column names ordered, so the output files match this
columns_ordered <- c("CUSIP",colnames(CFS_dates_dummy)[2:(dim(CFS_dates_dummy)[2])])

for (i in 1:length(BRS_scenario_names)) {
  # Isolate cash flows of a specific scenario
  scenario_CFs <- CFs_input[CFs_input$Scenario == BRS_scenario_names[i],]
  
  # Isolate interest and principal cash flows separately
  interest_CFs <- rbind(CFs_dates_dummy_interest,scenario_CFs[,c("CUSIP","Interest","CF_Date")])
  principal_CFs <- rbind(CFs_dates_dummy_principal,scenario_CFs[,c("CUSIP","Principal","CF_Date")])
  
  # Reformat cash flows from columns to a table
  reformatted_interest_CFS <- dcast(interest_CFs, CUSIP ~ CF_Date, value.var = "Interest",fun.aggregate = sum)
  reformatted_principal_CFS <- dcast(principal_CFs, CUSIP ~ CF_Date, value.var = "Principal",fun.aggregate = sum)
  
  # Delete dummy CUSIP row and make sure the order of the column matches that of the input dummy one
  # (same as QRM cash flows, needed for consistency, and it is chronological order)
  # And rename the CUSIP column as Cusip to match the format of QRM Cash Flow files
  non_dummy_rows <- which(reformatted_interest_CFS$CUSIP != "Dummy")
  reformatted_interest_CFS <- reformatted_interest_CFS[non_dummy_rows,columns_ordered]
  reformatted_interest_CFS <- rename(reformatted_interest_CFS, c("CUSIP"="Cusip"))
  non_dummy_rows <- which(reformatted_principal_CFS$CUSIP != "Dummy")
  reformatted_principal_CFS <- reformatted_principal_CFS[non_dummy_rows,columns_ordered]
  reformatted_principal_CFS <- rename(reformatted_principal_CFS, c("CUSIP"="Cusip"))
  
  # Export cash flows to csv files
  interest_file_name <- paste0(CCAR_run_name," - BRS ",output_scenario_names[i]," interest.csv")
  filename <- file.path(dirname(CFs_input_file),interest_file_name)
  write.csv(reformatted_interest_CFS,file=filename, row.names=FALSE)
  principal_file_name <- paste0(CCAR_run_name," - BRS ",output_scenario_names[i]," principals.csv")
  filename <- file.path(dirname(CFs_input_file),interest_file_name)
  write.csv(reformatted_principal_CFS,file=filename, row.names=FALSE)
}


# CRITICAL TOOLS DOCUMENTATION	
# 
# File Name|CFs_BRS_formatter.R
# 
# Overview/PURPOSE|Reformatting of BlackRock provided cash flows with format compatible with the rest of the model for CCAR OCI discounting
# 
# Last updated date|8/19/2016
# Tester Name|
# Last testing date|
# 
# Filename and path|\\ntfmao01\ERMShared\Portfolio VaR\CSIM\Production\Input Data\CFs\BRS\
# Current date|1/12/2017
# Update frequency|NA
# Owner|Remy Pasco, Jochen Steinbrecher
# Developer|Remy Pasco
# 
# Number of worksheets|NA
# 
# Contains formulas|No
# Contains macros|No
# Contains pivot tables|No
# Other programming|Yes
# 
# Technical specifications|R
# 
# 
# List of Inputs|Cash Flows for CCAR scenarios provided by BlackRock Solutions
# 
# 
# 
# Distribution list|Not distributed directly. Final CCAR results only distributed to State Street CCAR team
# 
# Information Classification|Limited Access
# Complexity Level|Light
# Impact Risk|Significant
# Overall risk rating|Medium
# 
# 
# Change log
# Date Changed|8/19/2016
# Approved By|Remy Pasco
# Implemented By|Remy Pasco
# Tested By|
# Brief Description of Change|Version 1.0; see purpose/overview



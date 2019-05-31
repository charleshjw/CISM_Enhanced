# Config to prepare for discounting
# 
# Author: e620927
###############################################################################

require(xlsx)
#require LibraryAccessDatabase
options(digits=10)

#dbc_control <- openAccess(Control_File)
#versions <- sqlFetch(dbc_control,"Versions")
#versions <- characterizeTable(versions)
#current_version_info <- versions[versions$CurrentVersion==1,]
#createVariables(current_version_info)
#current_version = Version
#list_files <- fetchTable(dbc_control,"List_Files",current_version)
#odbcClose(dbc_control)

dbc <- openAccess(concat_file_path(current_version_info))

# create master date table
date_q0 <- as.yearperiod(current_version_info$History_END_DATE)

dates <- date_q0 + seq(-1,+2.25,0.25)
Dates_Master <- data.frame("Horizon"=c("Q-4","Q-3","Q-2","Q-1","Q0","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9"),"HorizonDate"=as.Date(dates+0.25)-1,"HorizonQuarter"=gsub(" ","",as.character(dates)))
Dates_Master <- versionDataFrame(Dates_Master,current_version)

if (re_import) {
	#################################################
	###### import master table and reformat it ######
	#################################################
	# step 1: read it from excel file
	con_mapping <- odbcConnectExcel2007(concat_file_path(tableFilter(list_files,"VariableName","CUSIPMappingMasterTable")))
	master_table <- sqlFetch(con_mapping,"CUSIP Master Table")
	master_table <- versionDataFrame(master_table,current_version)
	master_table$'PurchaseDate' <- as.character(as.Date(master_table$'PurchaseDate'))
	master_table$IndexMapping <- as.character(master_table$IndexMapping)
	
	# step 2: convert CSI name to CSIM-type names
	csi_mapping <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","CUSIPMappingCSINameMapping")),sheetIndex=1)
#	csi_mapping <- read.xlsx("\\\\mfmaog02\\ERMShared\\Treasury Risk\\GTRM_IRRLiq_QA\\CSIM\\Enhance2018\\ModelImplementation\\Inputs\\DiscountingTables\\CSUIP Mapping Table\\Mapping_Index_20190129.xlsx",sheetIndex=1)
	
	csi_mapping <- characterizeTable(csi_mapping)
	csi_mapping_change <- csi_mapping[csi_mapping$CSIM_CSI_ID != csi_mapping$MasterTableCSI_ID,]
	for (i in 1:nrow(csi_mapping_change)) {
		master_table[master_table$IndexMapping == csi_mapping_change[i,'MasterTableCSI_ID'],'IndexMapping'] <- csi_mapping_change[i,'CSIM_CSI_ID']
	}
	
	# step 3: map the referrence curve
	M_CSI <- fetchTable(dbc,"I_MOD_CSI_AC",current_version)
	M_CSI <- unique(M_CSI[,c("CSI_ID","REF_CURVE")])
	colnames(M_CSI) <- c("IndexMapping","REF_CURVE")
	
	master_table <- merge(master_table,M_CSI,by="IndexMapping",all.x=TRUE)
	master_table[is.na(master_table$'REF_CURVE'),'REF_CURVE'] = "Treasury/Gov't curve"
	if (!all(master_table$'ReferenceCurveType' == master_table$'REF_CURVE')) {
		warning("ConfigDiscounting.R: cannot match reference curve with I_MOD_CSI_AC table.")
		if (replace_mapping) {
			master_table$'ReferenceCurveType' = master_table$'REF_CURVE'
		}
	} else {
		print("Success: the calculated reference curve can be matched with I_MOD_CSI_AC table")
	}
	master_table$REF_CURVE <- NULL

	# step 4: use currency and ReferenceCurvetype to map to InternalCurveType
	curve_mapping <- sqlFetch(con_mapping,"CurrencyAndCurveMap")
	curve_mapping <- curve_mapping[,c("Currency", "ReferenceCurveType", "IndexMapping", "InternalCurveType")]
	curve_mapping <- characterizeTable(curve_mapping)
	curve_mapping[is.na(curve_mapping)] <- ""
	table_name <- "DiscountCurveMapping";
	saveTable(dbc,table_name,versionDataFrame(curve_mapping ,current_version),prohibitaddition=TRUE)
	
	master_table[,"InternalCurveType2"] <- ""
	for (i in 1:nrow(master_table)) {
#		i = 2000
#		print(i)
		if (master_table[i,"IndexMapping"] == "NO_SPREAD") {
			master_table[i,"InternalCurveType2"] <- curve_mapping[curve_mapping$'Currency' == master_table[i,"Currency"]&curve_mapping$'IndexMapping' == master_table[i,"IndexMapping"],"InternalCurveType"]
		} else {
			master_table[i,"InternalCurveType2"] <- curve_mapping[curve_mapping$'Currency' == master_table[i,"Currency"]&curve_mapping$'ReferenceCurveType' == master_table[i,"ReferenceCurveType"],"InternalCurveType"]
		}
	}
	if (!all(master_table[,"InternalCurveType"] == master_table[,"InternalCurveType2"])) {
		warning("ConfigDiscounting.R: cannot match calculated internal curve type with existing column in CUSIP mapping master table.")
		if (replace_mapping) {
			master_table$'InternalCurveType' = master_table$'InternalCurveType2'
		}
	} else {
		print("Success: the calculated internal curve type can be matched with existing column in the CUSIP mapping master table.")
	}
	master_table$'InternalCurveType2' = NULL
	
#	unique(AssetClass[,c("ReferenceCurveType","IndexMapping")])
	
	# step 5: save reformated master table to the databse for future reference
	table_name <- "CUSIP_Master_Table_Permanent";
	saveTable(dbc,table_name,master_table,prohibitaddition=TRUE)
	
	### import FX Rates
	HFV_system_source_column_name <- "System Source"
	FX_Rate_column_name <- "Fx Rate"
	HFV_file <- fetchTable(dbc, "HFV_File_Permanent",HFV_File_Permanent_)
	HFV_file <- HFV_file[HFV_file[,HFV_system_source_column_name] == "Intrader",]
	unique_fx_rates <- unique(HFV_file[,c("As of Date","Currency","Fx Rate")])
	colnames(unique_fx_rates) <- c("AsOfDate","Currency_HFV","FXRate")
	table_name = "FX_Spot_Rates"
	saveTable(dbc, table_name,versionDataFrame(characterizeTable(unique_fx_rates),current_version),prohibitaddition=TRUE)
	
	Asset_Class_level_parameters <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","AssetClassSpecification")),sheetIndex=1)
	colnames(Asset_Class_level_parameters) <- gsub("[.]","",colnames(Asset_Class_level_parameters))
#	colnames(Asset_Class_level_parameters) <- c("AssetClass", "Additive_Multiplicative", "Index level floor", "CUSIP level floor")
	table_name = "Asset_Class_level_parameters"
#try(sqlDrop(dbc,table_name))
	saveTable(dbc, table_name,versionDataFrame(characterizeTable(Asset_Class_level_parameters),current_version),prohibitaddition=TRUE)
}

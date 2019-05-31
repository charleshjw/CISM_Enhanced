# 
# 
# Author: e620927
###############################################################################
library(RJDBC)
access_table_names <- "I_HIST_IV_PRIM"

if (!all(dbHasTable(dbc,access_table_names)) | REPROCESS) {
	#### get the list of independent variables
	D_IV <- fetchTable(dbc,"I_IV",current_version)
	
	tickers <- as.character(D_IV[,"MAOX_CODE"])
	tickers <- tickers[tickers!="N/A"]
	tickers <- unique(tickers)
	
	if (MONTHLY_VERSION){
		jdbcDriver =JDBC("oracle.jdbc.OracleDriver",classPath="c:\\software\\ojdbc6.jar")
		jdbcConnection <- dbConnect(jdbcDriver,"jdbc:oracle:thin:@gdcx12-scan:1521/CME.PROD.APP","e621994", "rc621994")
		
		data <- dbGetQuery(jdbcConnection, 
				"SELECT MNEMONIC, DESCRIPTION, SCENARIO, MACRO_ECONOMIC_FACTOR_VALUE, YEAR_NUM, MONTH_NUM, QUARTER_NUM, VENDOR_CODE
						FROM SCNRO_MGMT_DATA.VW_RELEASED_VIEW
						WHERE 
						YEAR_NUM>=2000 AND 
						YEAR_NUM<=2020 AND 
						FREQUENCY = 'Monthly' AND 
						EXERCISE = 'DFAST 2018'")
		
		dbDisconnect(jdbcConnection)
		
		data = data[data$MNEMONIC%in%tickers,]
		#reformat the scenario names
		data[data$SCENARIO=="Baseline","SCENARIO"]="Base-2018DFAST-Monthly"
		data[data$SCENARIO=="Adverse","SCENARIO"]="Adverse-2018DFAST-Monthly"
		data[data$SCENARIO=="Severely Adverse","SCENARIO"]="SevereAdverse-2018DFAST-Monthly"
		
		scenarios = unique(data$SCENARIO)
		mnes = unique(data$MNEMONIC)
		#check length of history
		for (i in 1:length(mnes)) {
#		i = 1
			tmp = data[data$MNEMONIC%in%mnes[i],]
			
			if (!setequal(unique(tmp$SCENARIO),scenarios)) {
				print(data$MNEMONIC[i])
				stop("Scenarios are not complete")
			}
			his_start = c()
			for (scenario in scenarios) {
#			scenario = scenarios[1]
				his_start =  c(his_start,min(tmp[tmp$SCENARIO == scenario,c("YEAR_NUM")]))
			}
			
			if (length(unique(his_start)) != 1) {
				print("Warning: History of independent variables are not the same across scenarios")
				print(data.frame(mnes[i],scenarios,his_start))
			}
		}
		
		#format data and save for modeling
		for(scenario in scenarios) {
			filename = concat_file_path(tableFilter(list_files,"VariableName",scenario))
			if(length(filename)>0){
				print(paste0("Extracting Scenario ", scenario))
				
				data_scenario = data[data$SCENARIO == scenario,]
				if(!all(tickers%in%unique(data_scenario$MNEMONIC))) {
					print("Error: Not all tickers can be found in the database")
					print(tickers[!tickers%in%unique(data_scenario$MNEMONIC)])
#					stop()
				}
				
				data_scenario = data.frame(paste0(data_scenario$YEAR_NUM,"M",data_scenario$MONTH_NUM),data_scenario$DESCRIPTION,data_scenario$MNEMONIC,data_scenario$MACRO_ECONOMIC_FACTOR_VALUE,data_scenario$VENDOR_CODE)
				colnames(data_scenario) = c("Date","Description","Mnemonic","Value","VENDOR")
				
				write.csv(data_scenario,filename)
			} else {
				print(paste0("Skip Scenario ", scenario))
			}
		}
	}

	#### download data from database
	### Quarterly Data ###
	jdbcDriver =JDBC("oracle.jdbc.OracleDriver",classPath="c:\\software\\ojdbc6.jar")
	jdbcConnection <- dbConnect(jdbcDriver,"jdbc:oracle:thin:@gdcx12-scan:1521/CME.PROD.APP","e621994", "rc621994")
	
#	test <- dbGetQuery(jdbcConnection, "
#							SELECT * 
#							FROM SCNRO_MGMT_DATA.VW_RELEASED_VIEW
#							WHERE rownum <= 10")
#			
#	write.xlsx(test,"C:/Users/E620927/Desktop/test2.xlsx",sheetName="Sheet1")
#	data_released <- dbGetQuery(jdbcConnection, "
#					SELECT EXERCISE, FREQUENCY, MNEMONIC, DESCRIPTION, SCENARIO, MACRO_ECONOMIC_FACTOR_VALUE, YEAR_NUM, MONTH_NUM, QUARTER_NUM
#					FROM SCNRO_MGMT_DATA.VW_RELEASED_VIEW)
	#WHERE FREQUENCY = 'Quarterly' AND (SCENARIO = 'BHC Baseline' OR SCENARIO = 'BHC Scenario 1' OR SCENARIO = 'BHC Scenario 2')")
	
#	data <- dbGetQuery(jdbcConnection, 
#			"SELECT MNEMONIC, DESCRIPTION, SCENARIO, MACRO_ECONOMIC_FACTOR_VALUE, YEAR_NUM, MONTH_NUM, QUARTER_NUM, VENDOR_CODE
#					FROM SCNRO_MGMT_DATA.VW_RELEASED_VIEW
#					WHERE 
#						YEAR_NUM>=2000 AND 
#						YEAR_NUM<=2020 AND 
#						FREQUENCY = 'Quarterly' AND 
#						(SCENARIO = 'BHC Baseline' OR SCENARIO = 'BHC Stress' OR SCENARIO = 'FRB Baseline' OR SCENARIO = 'FRB Adverse' OR SCENARIO = 'FRB Severely Adverse')")
#	#reformat the scenario names
#	data[data$SCENARIO=="BHC Baseline","SCENARIO"]="BHCBase"
#	data[data$SCENARIO=="BHC Stress","SCENARIO"]="BHCStress"
#	data[data$SCENARIO=="FRB Baseline","SCENARIO"]="FRBBaseline"
#	data[data$SCENARIO=="FRB Adverse","SCENARIO"]="FRBAdverse"
#	data[data$SCENARIO=="FRB Severely Adverse","SCENARIO"]="FRBSevAdv"
	
	data <- dbGetQuery(jdbcConnection, 
			"SELECT MNEMONIC, DESCRIPTION, SCENARIO, MACRO_ECONOMIC_FACTOR_VALUE, YEAR_NUM, MONTH_NUM, QUARTER_NUM, VENDOR_CODE
					FROM SCNRO_MGMT_DATA.VW_RELEASED_VIEW
					WHERE 
					YEAR_NUM>=2000 AND 
					YEAR_NUM<=2020 AND 
					FREQUENCY = 'Quarterly' AND 
					EXERCISE = 'DFAST 2018'")
		
#	unique(data$SCENARIO)
	dbDisconnect(jdbcConnection)
	#write.csv(sql_qtr.RELEASED_VIEW,paste0(Output_IV,"/released_view.csv"))
	
	#only need data after 2000
#	data = data[data$YEAR_NUM>=2000,]
	#head(data)
	#dim(data)
	#unique(data$EXERCISE)
	#unique(data$FREQUENCY)
	#unique(data$SCENARIO)
	#unique(data$DESCRIPTION)
	#unique(data$YEAR_NUM)
	#unique(data$QUARTER_NUM)
	#unique(data$MONTH_NUM)
	
#	data <- dbGetQuery(jdbcConnection, 
#			"SELECT MNEMONIC, DESCRIPTION, SCENARIO, MACRO_ECONOMIC_FACTOR_VALUE, YEAR_NUM, MONTH_NUM, QUARTER_NUM, VENDOR_CODE
#					FROM SCNRO_MGMT_DATA.VW_RELEASED_VIEW
#					WHERE 
#					YEAR_NUM>=2000 AND 
#					YEAR_NUM<=2001 AND 
#					FREQUENCY = 'Quarterly' AND 
#					(SCENARIO = 'BHC Baseline')")
	
	#write.csv(unique(data$MNEMONIC),paste0(Output_IV,"/unique_tickers.csv"))
#	write.xlsx(unique(data[c("MNEMONIC","DESCRIPTION")]),"C:/Users/E620927/Desktop/test.xlsx",sheetName="Sheet1")
	
#	data <- dbGetQuery(jdbcConnection, 
#			"SELECT MNEMONIC, DESCRIPTION, SCENARIO, MACRO_ECONOMIC_FACTOR_VALUE, YEAR_NUM, MONTH_NUM, QUARTER_NUM, VENDOR_CODE
#					FROM SCNRO_MGMT_DATA.VW_RELEASED_VIEW
#					WHERE 
#					YEAR_NUM>=2017 AND 
#					YEAR_NUM<=2018 AND 
#					FREQUENCY = 'Quarterly' AND 
#					(SCENARIO = 'BHC Baseline' OR SCENARIO = 'BHC Stress' OR SCENARIO = 'FRB Baseline' OR SCENARIO = 'FRB Adverse' OR SCENARIO = 'FRB Severely Adverse')")
#	data[data$MNEMONIC=="INTRATESGERRLG5",]
#	write.xlsx(data[data$MNEMONIC == "INTRATESITRLG",],paste0(Output_root,"/IV_INTRATESITRLG.xlsx"),sheetName="Sheet1")

	data = data[data$MNEMONIC%in%tickers,]
	#reformat the scenario names
	data[data$SCENARIO=="Baseline","SCENARIO"]="Base-2018DFAST"
	data[data$SCENARIO=="Adverse","SCENARIO"]="Adverse-2018DFAST"
	data[data$SCENARIO=="Severely Adverse","SCENARIO"]="SevereAdverse-2018DFAST"
	
	scenarios = unique(data$SCENARIO)
	mnes = unique(data$MNEMONIC)
	#check length of history
	for (i in 1:length(mnes)) {
#		i = 50
		tmp = data[data$MNEMONIC%in%mnes[i],]
		
		if (!setequal(unique(tmp$SCENARIO),scenarios)) {
			print(data$MNEMONIC[i])
			stop("Scenarios are not complete")
		}
		his_start = c()
		for (scenario in scenarios) {
#			scenario = scenarios[1]
			his_start =  c(his_start,min(tmp[tmp$SCENARIO == scenario,c("YEAR_NUM")]))
		}
		
		if (length(unique(his_start)) != 1) {
			print("Warning: History of independent variables are not the same across scenarios")
			print(data.frame(mnes[i],scenarios,his_start))
		}
	}
	
	#format data and save for modeling
	for(scenario in scenarios) {
		filename = concat_file_path(tableFilter(list_files,"VariableName",scenario))
		if(length(filename)>0){
			print(paste0("Extracting Scenario ", scenario))
			
			data_scenario = data[data$SCENARIO == scenario,]
			if(!all(tickers%in%unique(data_scenario$MNEMONIC))) {
				print("Error: Not all tickers can be found in the database")
				stop()
			}
			
			data_scenario = data.frame(paste0(data_scenario$YEAR_NUM,"Q",data_scenario$QUARTER_NUM),data_scenario$DESCRIPTION,data_scenario$MNEMONIC,data_scenario$MACRO_ECONOMIC_FACTOR_VALUE,data_scenario$VENDOR_CODE)
			colnames(data_scenario) = c("Date","Description","Mnemonic","Value","VENDOR")
			
			write.csv(data_scenario,filename)
		} else {
			print(paste0("Skip Scenario ", scenario))
		}
	}
}

if (!PRODUCTION_ENVIRONMENT) {
	## Ad hoc results
	## Download BRS variables
	jdbcDriver =JDBC("oracle.jdbc.OracleDriver",classPath="c:\\software\\ojdbc6.jar")
	jdbcConnection <- dbConnect(jdbcDriver,"jdbc:oracle:thin:@gdcx12-scan:1521/CME.PROD.APP","e620927", "7eykp3dA")
	data <- dbGetQuery(jdbcConnection, 
			"SELECT MNEMONIC, DESCRIPTION, SCENARIO, MACRO_ECONOMIC_FACTOR_VALUE, YEAR_NUM, MONTH_NUM, QUARTER_NUM, VENDOR_CODE
					FROM SCNRO_MGMT_DATA.VW_RELEASED_VIEW
					WHERE 
					YEAR_NUM>=2000 AND 
					YEAR_NUM<=2020 AND 
					FREQUENCY = 'Quarterly' AND 
					(SCENARIO = 'BHC Baseline' OR SCENARIO = 'BHC Stress' OR SCENARIO = 'FRB Baseline' OR SCENARIO = 'FRB Adverse' OR SCENARIO = 'FRB Severely Adverse')")
	tickers = c("LIBOR3MEOP","RFFEOP","SRBARUSC","RT10YEOP","RT30YEOP","STATE_STIG_CREDIT_EU","SRBARCBHY","VIX","STATE_STPAN_CREDIT_HYEU",
			"RT2YEOP","RT5YEOP","INTRATESIRERLG5","INTRATESITRLG5","INTRATESPTRLG5","INTRATESESRLG5",
			"INTRATESUKRSWAP1YREOP")
	all(tickers%in%unique(data[,"MNEMONIC"]))
	
	a = data[data$MNEMONIC=="LIBOR3MEOP",]
	b = data[data$MNEMONIC=="RFFEOP",]
	all(a$SCENARIO==b$SCENARIO&a$YEAR_NUM==b$YEAR_NUM&a$QUARTER_NUM==b$QUARTER_NUM)
	a[,"MACRO_ECONOMIC_FACTOR_VALUE"]=a[,"MACRO_ECONOMIC_FACTOR_VALUE"]-b[,"MACRO_ECONOMIC_FACTOR_VALUE"]
	a[,"MNEMONIC"] = "OIS_SPREAD"
	a[,"DESCRIPTION"] = "OIS Spread"
	
#unique(data[data$MNEMONIC%in%tickers,c("MNEMONIC","DESCRIPTION")])
	write.csv(rbind(data[data$MNEMONIC%in%tickers,],a),"C:/Users/E620927/Desktop/BRS_IV.csv")
	write.csv(unique(data[,c("MNEMONIC","DESCRIPTION")]),"C:/Users/E620927/Desktop/unique_tickers.csv")
}




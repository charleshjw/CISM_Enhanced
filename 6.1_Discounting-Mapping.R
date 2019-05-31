# TODO: Add comment
# 
# Author: E620927
###############################################################################

odbcCloseAll()

#dbc <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=./Output/CSIM_part2_v0.1.accdb")
#
##set up mapping key look up table
#MappingKeys <- sqlFetch(dbc,"MappingRules_relevant_attribute")
#mapping_keys <- list()
#unique_keys <- unique(MappingKeys$SecurityDescription)
#for (i in 1:length(unique_keys)) {
##	i<-20
#	if (sum(MappingKeys[MappingKeys$SecurityDescription == unique_keys[i],]==1)==0) {
#		mapping_keys[[i]] <- "SecurityDescription"
#	} else {
#		mapping_keys[[i]] <- c("SecurityDescription",colnames(MappingKeys)[MappingKeys[MappingKeys$SecurityDescription == unique_keys[i],]==1])
#	}
#}
#names(mapping_keys) <- as.character(unique_keys)
#
##set up detailed mapping rules
#MappingRules <- sqlFetch(dbc, "MappingRules")
##unique_rule <- unique(MappingRules$SecurityDescription)
#	all(unique_rule%in%unique_keys) #should be TRUE
##
##mapping_rules <- list()
##for (i in 1:length(unique_rule)) {
###	i<-1
##	mapping_rules[[i]] <- MappingRules[MappingRules$SecurityDescription == unique_rule[i],]
##	print(dim(mapping_rules[[i]]))
##}
##names(mapping_rules) <- as.character(unique_rule)
#
#data <- sqlFetch(dbc,"FRY14Q")
#WAL <- sqlFetch(dbc,"HFV_monthly")
#BBG <- sqlFetch(dbc,"BBG Parameters")
#hfv_daily <- sqlFetch(dbc,"HFV_daily")
#rating <- sqlFetch(dbc,"GTRM CCAR Team")
#
##unique(data[,"SecurityDescription"])
##data2 <- data[data[,"SecurityDescription"]=="Agency MBS",]
#mapCSI <- rep("",nrow(data))
#for (i in 1:nrow(data)) {
##for (i in 1:32) {
##	i <- 1;
#	portfolio <- data[i,]
#	if (portfolio[,"SecurityDescription"] == "Domestic Non-Agency RMBS (incl HEL ABS)") {
#		type <- rating[which(rating$IdentifierValue == portfolio$IdentifierValue),"OverrideSecurityDescription"]
#		portfolio[,"SecurityDescription"] <- as.character(type)
#	}
#	
#	keys <- mapping_keys[[as.character(portfolio$SecurityDescription)]]
#	
#	rules <- MappingRules
#	j <- 1;
#	while(nrow(rules) != 1 & j<=length(keys)) {
#		rules <- filterRules(portfolio, keys[j], rules)
#		j <- j+1;
#	}
#	if (nrow(rules) > 1) {
#		warning("ERROR: More than one rules appy")
#		next()
#	}
#	mapCSI[i] <- as.character(rules$SpreadIndex_ID)
#	print(i)
#	print(mapCSI[i])
#}
#
#MappingResults <- data.frame(ID=paste(data$IdentifierValue,data$TransactionID,sep="-"),CSI=mapCSI)

dbc <- odbcDriverConnect(paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",Output_root,"/CSIM_discounting_v0.1.accdb",sep=""))
CUSIP_ID <- sqlFetch(dbc,"I_CUSIP_ID")
HFV_file <- sqlFetch(dbc,"I_HFV_File")

head(CUSIP_ID)
head(HFV_file)


HFV_file[match(CUSIP_ID$CUSIPs,HFV_file$CUSIP),"Currency"]

summary(match(CUSIP_ID$CUSIPs,HFV_file$CUSIP))

CUSIP_ID$CUSIPs[!as.character(CUSIP_ID$CUSIPs)%in%as.character(HFV_file$CUSIP)]

HFV_file$CUSIP[6365:6369]
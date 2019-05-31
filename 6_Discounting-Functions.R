# TODO: Add comment
# 
# Author: E620927
###############################################################################


rate_to_number <- function(rate) {
	rate <- as.character(rate)
	if (rate == "AAA") {
		return(1)
	} else if (rate == "AAA-") {
		return(2)
	} else if (rate == "AA+") {
		return(3)
	} else if (rate == "AA") {
		return(4)
	} else if (rate == "AA-") {
		return(5)
	} else if (rate == "A+") {
		return(6)
	} else if (rate == "A") {
		return(7)
	} else if (rate == "A-") {
		return(8)
	} else if (rate == "BBB+") {
		return(9)
	} else if (rate == "BBB") {
		return(10)
	} else if (rate == "BBB-") {
		return(11)
	} else if (rate == "BB+") {
		return(12)
	} else if (rate == "BB") {
		return(13)
	} else if (rate == "BB-") {
		return(14)
	} else if (rate == "B+") {
		return(15)
	} else if (rate == "B") {
		return(16)
	} else if (rate == "B-") {
		return(17)
	} else if (rate == "CCC+") {
		return(18)
	} else if (rate == "CCC") {
		return(19)
	} else if (rate == "CCC-") {
		return(20)
	} else if (rate == "CC+") {
		return(21)
	} else if (rate == "CC") {
		return(22)
	} else if (rate == "CC-") {
		return(23)
	} else if (rate == "C+") {
		return(24)
	} else if (rate == "C") {
		return(25)
	} else if (rate == "C-") {
		return(26)
	} else if (rate == "D+") {
		return(27)
	} else if (rate == "D") {
		return(28)
	} else if (rate == "D-") {
		return(29)
	}
}

FFELPAA <- c("78442GHQ2","78442GLR5","19458LBD1","78442GNW2","784423AH8","78442GQL3","78442GRY4","78442GSL1","78442GSE7","64031QBL4","64031QBS9","64031QCU3")
FFELPToAA <- function(id) {
	id <- as.character(id)
	if (id %in% FFELPAA) {
		return("Y")
	} else {
		return("N")
	}
}


filterRules <- function(portfolio, key, rules) {
#	key <- keys[j]
	if (key == "SecurityDescription") {
		if (portfolio[,"SecurityDescription"] == "Domestic Non-Agency RMBS (incl HEL ABS)") {
			type <- rating[which(rating$IdentifierValue == portfolio$IdentifierValue),"OverrideSecurityDescription"]
			return(rules[which(rules[,"SecurityDescription"]==as.character(type)),])
		} else {
			return(rules[which(rules[,"SecurityDescription"]==as.character(portfolio[,"SecurityDescription"])),])
		}
	} else if (key == "SEC1") { 
		sec1 <- as.character(hfv_daily[which(hfv_daily$Ticket==portfolio$TransactionID),"SEC 1"])
		return(rules[which(rules[,"SEC1"] == sec1),])
	} else if (key == "SEC2") {
		sec2 <- as.character(hfv_daily[which(hfv_daily$Ticket==portfolio$TransactionID),"SEC 2"])
		return(rules[which(rules[,"SEC2"] == sec2),])
	} else if (key == "SEC3") {
		sec3 <- as.character(hfv_daily[which(hfv_daily$Ticket==portfolio$TransactionID),"SEC 3"])
		return(rules[which(rules[,"SEC3"] == sec3),])
	} else if (key == "Currency") {		
		return(rules[which(rules$Currency == as.character(portfolio$Currency)),])
	} else if (key == "Floater") { 
		floater <- as.character(hfv_daily[which(hfv_daily$Ticket==portfolio$TransactionID),"Floater"])
		return(rules[rules[,"Floater"]==floater,])
	} else if (key == "CollateralType") {
		colltype <- as.character(rating[which(rating$IdentifierValue == portfolio$IdentifierValue),"CollateralType"])
		return(rules[which(tolower(rules$CollateralType) == tolower(colltype)),])
	} else if (key == "Exception_SLABS") {
		ffelp_exception <- FFELPToAA(portfolio$IdentifierValue)
		return(rules[which(rules[,"Exception_SLABS"]==ffelp_exception),])
	} else if (key == "SEQ_flag") { 
		type <- as.character(BBG[which(BBG[,"IdentifierValue"] == as.character(portfolio[,"IdentifierValue"])),"MTG_TRANCHE_TYP"])
		if (grepl("SEQ", type)) {
			type <- "Y"
		} else {
			type <- "N"
		}
		return(rules[which(rules[,"SEQ_flag"] == type),])
	} else if (key == "Agcy_Issuer") { 
		issuer <- as.character(BBG[which(BBG[,"IdentifierValue"] == as.character(portfolio[,"IdentifierValue"])),"ISSUER"])
		return(rules[rules$Agcy_Issuer==issuer,])
	} else if (key == "CMBS_Class") {
		class <- as.character(BBG[which(BBG[,"IdentifierValue"] == as.character(portfolio[,"IdentifierValue"])),"Name"])
		class <- tail(strsplit(class," ")[[1]],1)
		return(rules[which(rules[,"CMBS_Class"] == class),])
	} else if (key == "Country") {
		return(rules[which(rules[,"Country"]==as.character(portfolio[,"Country"])),])
	} else if (key =="Rating_numeric") {
		rate <- rating[which(rating$IdentifierValue==as.character(portfolio$IdentifierValue)),"Rating"]
		rate <- rate_to_number(rate)
		return(rules[which(rate > rules$Rating_LowerBound&rate <= rules$Rating_UpperBound),])
	} else if (key == "WAL") {
		wal <- WAL[which(WAL$Ticket == as.character(portfolio$TransactionID)),"WAL"]
		return(rules[which(wal > rules$WAL_LowerBound&wal <= rules$WAL_UpperBound),])
	} else if (key == "Maturity_Orig_numeric") { 
		maturityAtOrigination <- WAL[which(WAL$Ticket == as.character(portfolio$TransactionID)),"Maturity at origination"]
		return(rules[maturityAtOrigination > rules$Maturity_Orig_LowerBound&maturityAtOrigination <= rules$Maturity_Orig_UpperBound,])
	} else{
		return(rules)
	}
}

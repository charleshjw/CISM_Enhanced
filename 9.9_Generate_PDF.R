# 
# 
# Author: e620927
###############################################################################

#### Run Latex PDF packages
if(PRODUCTION_ENVIRONMENT) {
	system(paste(PDFReportDir,"/CurveInfo_Production.bat",sep=""))
	
	system(paste(PDFReportDir,"/IV_Production.bat",sep=""))
	
	system(paste(PDFReportDir,"/PCA_Production.bat",sep=""))
	
	system(paste(PDFReportDir,"/ClusterAnalysis_Production.bat",sep=""))
	
	system(paste(PDFReportDir,"/Forecast_Production.bat",sep=""))
	if (VALIDATION == FALSE) {
		system(paste(PDFReportDir,"/ProductPackageShort_all.bat",sep=""))
		system(paste(PDFReportDir,"/ProductPackage_all.bat",sep=""))
	}
	
	system(paste(PDFReportDir,"/MtM_Compare_all.bat",sep=""))
	
} else {
	system(paste(PDFReportDir,"/IV_Production.bat",sep=""))
	system(paste(PDFReportDir,"/Forecast_Production.bat",sep=""))
}


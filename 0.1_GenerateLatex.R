# Generate Latex templates for presentation of model outputs
# 
# Author: e620927
###############################################################################

######################Output Latex Parameters for each Asset Class#####################
##Create Asset Class Tex Parameters
if (!dir.exists(PDFReportRoot)) {
	dir.create(PDFReportRoot)
}

if(grepl("Treasury Risk",PDFReportRoot)) {
	PDFReportDir = paste0("Z:",gsub(".*Treasury Risk","",PDFReportRoot))
} else {
	PDFReportDir = PDFReportRoot
}
if(VALIDATION) {
	PDFReportDir = PDFReportRoot_Validation
	OutputRoot = OutputRoot_Validation
}

# tex file: global parameters
fileConn <- file(paste(PDFReportDir,"/GlobalParameters.tex",sep=""),"w")
writeLines(paste0("\\newcommand\\Output{",gsub("[\\]","/",OutputRoot),"}"), fileConn)
writeLines("\\newcommand\\OutptCurveInfo{\\Output/CurveInfo}", fileConn)
writeLines("\\newcommand\\OutptMacroInfo{\\Output/MacroFactorVerification}", fileConn)
writeLines("\\newcommand\\OutptClusterAnalysis{\\Output/ClusterAnalysis}", fileConn)

writeLines("\\newcommand\\OutptPCA{\\Output/PCA_MOD_ID}", fileConn)
writeLines("\\newcommand\\OutptPCAtwo{\\Output/PCA_MOD_ID2}", fileConn)
writeLines("\\newcommand\\OutptPCAthree{\\Output/PCA_MOD_ID3}", fileConn)
writeLines("\\newcommand\\OutptPCAfour{\\Output/PCA_MOD_ID4}", fileConn)

writeLines("\\newcommand\\OutptAutoCorr{\\Output/AutoCorr}", fileConn)
writeLines("\\newcommand\\OutptFcst{\\Output/Forecast}", fileConn)
writeLines("\\newcommand\\OutptOutSample{\\Output/OutSample}", fileConn)
writeLines("\\newcommand\\OutptFcstCSI{\\Output/Forecast-CSI}", fileConn)
writeLines("\\newcommand\\OutptFcstCSICompare{\\Output/Forecast-CSI-Comparison}", fileConn)
writeLines("\\newcommand\\OutptMtMCompareBRK{\\Output/MtM-Comparison-BRK}", fileConn)
writeLines("\\newcommand\\OutptSFA{\\Output/SFA}", fileConn)
writeLines("\\newcommand\\OutptMFA{\\Output/MFA}", fileConn)
writeLines(paste0("\\newcommand\\MaxModel{1,...,",min(max_model_to_test,10),"}"), fileConn)
close(fileConn)

# tex file: parameters for each asset class
PDFReportDirAC <- paste0(PDFReportDir,"\\ACParameters")
if (!dir.exists(PDFReportDirAC)) {
	dir.create(PDFReportDirAC)
}

AC <- unique(mapping[,"AC_ID"])

for (i in 1:length(AC)) {
#	i <- 1
	fileConn <- file(paste(PDFReportDirAC,"/",AC[i],".tex",sep=""),"w")
	writeLines(paste0("\\newcommand\\CurrentVersion{",current_version,"}"), fileConn)
	writeLines(paste0("\\newcommand\\PriorVersion{",prior_version,"}"), fileConn)
	writeLines(paste0("\\newcommand\\Curves{",unique(paste(mapping[mapping[,"AC_ID"]==AC[i],"CSI_ID"],collapse=",")),"}"), fileConn)
	writeLines(paste0("\\newcommand\\ASSETCLASS{",AC[i],"}"), fileConn)
	writeLines(paste0("\\newcommand\\CLUSTERS{",paste(na.omit(unique(as.vector(as.matrix(mapping[mapping[,"AC_ID"]==AC[i],modelid_columns])))),collapse=","),"}"), fileConn)
	close(fileConn)
	
	mapping_ac <- mapping[mapping[,"AC_ID"]==AC[i],]
	mapping_ac <- mapping_ac[,!apply(mapping_ac,2, function(data){all(is.na(data))})]
	mapping_ac <- mapping_ac[,!colnames(mapping_ac)%in%c("VERSION","TimeStamp","ID")]
	
	filename <- file(paste(PDFReportDirAC,"/",AC[i],"_mapping.tex",sep=""),"w")
	writeMatrixToTex(filename,AC[i],mapping_ac)
}

##### Generate BAT files
#curve info
fileConn <- file(paste(PDFReportDir,"/CurveInfo_Production.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=CurveInfo_CSIMReport_%date:~10,4%_%date:~4,2%_%date:~7,2% CurveInfo_document.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=CurveInfo_CSIMReport_%date:~10,4%_%date:~4,2%_%date:~7,2% CurveInfo_document.tex", fileConn)
close(fileConn)

#independent variables
fileConn <- file(paste(PDFReportDir,"/IV_Production.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=IV_CSIMReport_%date:~10,4%_%date:~4,2%_%date:~7,2% IV_document.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=IV_CSIMReport_%date:~10,4%_%date:~4,2%_%date:~7,2% IV_document.tex", fileConn)
close(fileConn)

#PCA Analysis
fileConn <- file(paste(PDFReportDir,"/ClusterAnalysis_Production.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=ClusterAnalysis_CSIMReport_%date:~10,4%_%date:~4,2%_%date:~7,2% ClusterAnalysis_document.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=ClusterAnalysis_CSIMReport_%date:~10,4%_%date:~4,2%_%date:~7,2% ClusterAnalysis_document.tex", fileConn)
close(fileConn)

#cluster analysis
fileConn <- file(paste(PDFReportDir,"/PCA_Production.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=PCA_CSIMReport_%date:~10,4%_%date:~4,2%_%date:~7,2% PCA_document.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=PCA_CSIMReport_%date:~10,4%_%date:~4,2%_%date:~7,2% PCA_document.tex", fileConn)
close(fileConn)

#cluster analysis
fileConn <- file(paste(PDFReportDir,"/Forecast_Production.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=Forecast_CSIMReport_%date:~10,4%_%date:~4,2%_%date:~7,2% Forecast_document.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=Forecast_CSIMReport_%date:~10,4%_%date:~4,2%_%date:~7,2% Forecast_document.tex", fileConn)
close(fileConn)

#### MTM Comparison Analysis
fileConn <- file(paste(PDFReportDir,"/MtM_Compare_AgencyMBS.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_AgencyMBS_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_AgencyMBS.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_AgencyMBS_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_AgencyMBS.tex", fileConn)
close(fileConn)

fileConn <- file(paste(PDFReportDir,"/MtM_Compare_MuniOne.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_MuniOne_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_MuniOne.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_MuniOne_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_MuniOne.tex", fileConn)
close(fileConn)

fileConn <- file(paste(PDFReportDir,"/MtM_Compare_MuniTwo.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_MuniTwo_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_MuniTwo.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_MuniTwo_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_MuniTwo.tex", fileConn)
close(fileConn)

fileConn <- file(paste(PDFReportDir,"/MtM_Compare_Corporates.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_Corporates_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_Corporates.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_Corporates_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_Corporates.tex", fileConn)
close(fileConn)

fileConn <- file(paste(PDFReportDir,"/MtM_Compare_Gov.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_GovernmentBonds_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_Gov.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_GovernmentBonds_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_Gov.tex", fileConn)
close(fileConn)

fileConn <- file(paste(PDFReportDir,"/MtM_Compare_Others.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_Others_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_Others.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_Others_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_Others.tex", fileConn)
close(fileConn)

fileConn <- file(paste(PDFReportDir,"/MtM_Compare_all.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_AgencyMBS_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_AgencyMBS.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_AgencyMBS_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_AgencyMBS.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_MuniOne_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_MuniOne.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_MuniOne_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_MuniOne.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_MuniTwo_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_MuniTwo.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_MuniTwo_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_MuniTwo.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_Corporates_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_Corporates.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_Corporates_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_Corporates.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_GovernmentBonds_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_Gov.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_GovernmentBonds_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_Gov.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_Others_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_Others.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode -jobname=MtM_Comparison_Others_%date:~10,4%_%date:~4,2%_%date:~7,2% MtM_Comparison_Others.tex", fileConn)
close(fileConn)

# BAT for each asset class
fileConn <- file(paste(PDFReportDir,"/ProductPackage_all.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("echo \"Generate PDF report with LaTeX!\"", fileConn)
writeLines("for %%a in (", fileConn)
writeLines(paste(na.omit(unique(mapping$AC_ID)),collapse="\n\t"),fileConn)
writeLines(") do (\n	pdflatex -interaction=nonstopmode -jobname=ProductPackage_%%a_%date:~10,4%_%date:~4,2%_%date:~7,2% \"\\def\\ac{%%a} \\input{ProductPackage.tex}\"", fileConn)
writeLines("	pdflatex -interaction=nonstopmode -jobname=ProductPackage_%%a_%date:~10,4%_%date:~4,2%_%date:~7,2% \"\\def\\ac{%%a} \\input{ProductPackage.tex}\"\n)", fileConn)
close(fileConn)

ACs <- unique(mapping$AC_ID)
for (i in 1:length(ACs)) {
	fileConn <- file(paste(PDFReportDir,"/ProductPackage_",ACs[i],".bat",sep=""),"w")
	writeLines(paste0("set root=",PDFReportDir), fileConn)
	writeLines("cd /D %root%", fileConn)
	writeLines("echo \"Generate PDF report with LaTeX!\"", fileConn)
	writeLines("for %%a in (", fileConn)
	writeLines(paste0("\t",ACs[i]),fileConn)
	writeLines(") do (\n	pdflatex -interaction=nonstopmode -jobname=ProductPackage_%%a_%date:~10,4%_%date:~4,2%_%date:~7,2% \"\\def\\ac{%%a} \\input{ProductPackage.tex}\"", fileConn)
	writeLines("	pdflatex -interaction=nonstopmode -jobname=ProductPackage_%%a_%date:~10,4%_%date:~4,2%_%date:~7,2% \"\\def\\ac{%%a} \\input{ProductPackage.tex}\"\n)", fileConn)
	close(fileConn)
}

fileConn <- file(paste(PDFReportDir,"/ProductPackageShort_all.bat",sep=""),"w")
writeLines("echo \"Generate PDF report with LaTeX!\"", fileConn)
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("for %%a in (", fileConn)
writeLines(paste(na.omit(unique(mapping$AC_ID)),collapse="\n\t"),fileConn)
writeLines(") do (\n	pdflatex -interaction=nonstopmode -jobname=ProductPackageShortVersion_%%a_%date:~10,4%_%date:~4,2%_%date:~7,2% \"\\def\\ac{%%a} \\input{ProductPackageShortVersion.tex}\"", fileConn)
writeLines("	pdflatex -interaction=nonstopmode -jobname=ProductPackageShortVersion_%%a_%date:~10,4%_%date:~4,2%_%date:~7,2% \"\\def\\ac{%%a} \\input{ProductPackageShortVersion.tex}\"\n)", fileConn)
close(fileConn)

fileConn <- file(paste(PDFReportDir,"/Var_Selection_all.bat",sep=""),"w")
writeLines("echo \"Generate PDF report with LaTeX!\"", fileConn)
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("for %%x in (", fileConn)
writeLines(paste(na.omit(unique(as.vector(as.matrix(mapping[modelid_columns])))),collapse="\n\t"),fileConn)
writeLines(") do (\n	pdflatex -interaction=nonstopmode -jobname=Var_Selection_%%x_%date:~10,4%_%date:~4,2%_%date:~7,2% \"\\def\\CLUSTER{%%x} \\input{Var_Selection.tex}\"", fileConn)
writeLines("	pdflatex -interaction=nonstopmode -jobname=Var_Selection_%%x_%date:~10,4%_%date:~4,2%_%date:~7,2% \"\\def\\CLUSTER{%%x} \\input{Var_Selection.tex}\"\n)", fileConn)
close(fileConn)

fileConn <- file(paste(PDFReportDir,"/Var_Selection_Short_all.bat",sep=""),"w")
writeLines("echo \"Generate PDF report with LaTeX!\"", fileConn)
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("for %%x in (", fileConn)
writeLines(paste(na.omit(unique(as.vector(as.matrix(mapping[modelid_columns])))),collapse="\n\t"),fileConn)
writeLines(") do (\n	pdflatex -interaction=nonstopmode -jobname=Var_Selection_ShortVersion_%%x_%date:~10,4%_%date:~4,2%_%date:~7,2% \"\\def\\CLUSTER{%%x} \\input{Var_Selection_ShortVersion.tex}\"", fileConn)
writeLines("	pdflatex -interaction=nonstopmode -jobname=Var_Selection_ShortVersion_%%x_%date:~10,4%_%date:~4,2%_%date:~7,2% \"\\def\\CLUSTER{%%x} \\input{Var_Selection_ShortVersion.tex}\"\n)", fileConn)
close(fileConn)


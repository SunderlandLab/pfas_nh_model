library(stargazer)


# Tables ------------------------------------------------------------------
#Note: Rename HTML outputs because I made substantial edits in Word!

PFOAreg <- compounds_data[["PFOA"]][compounds_data[["PFOA"]]$final == 1, ]
PFOSreg <- compounds_data[["PFOS"]][compounds_data[["PFOS"]]$final == 1, ]
PFHXAreg <- compounds_data[["PFHXA"]][compounds_data[["PFHXA"]]$final == 1,]
PFHPAreg <- compounds_data[["PFHPA"]][compounds_data[["PFHPA"]]$final == 1, ]
PFPEAreg <- compounds_data[["PFPEA"]][compounds_data[["PFPEA"]]$final == 1, ]

max.len <- max(nrow(PFOAreg),nrow(PFOSreg),nrow(PFHXAreg),nrow(PFPEAreg),nrow(PFHPAreg))
PFOAvec <- c(PFOAreg$reg,rep(NA,max.len-nrow(PFOAreg)))
PFOSvec <- c(PFOSreg$reg,rep(NA,max.len-nrow(PFOSreg)))
PFHXAvec <- c(PFHXAreg$reg,rep(NA,max.len-nrow(PFHXAreg)))
PFPEAvec <- c(PFPEAreg$reg,rep(NA,max.len-nrow(PFPEAreg)))
PFHPAvec <- c(PFHPAreg$reg,rep(NA,max.len-nrow(PFHPAreg)))
summary_table <- data.frame(PFOAvec,PFHXAvec,PFHPAvec,PFPEAvec,PFOSvec)
names(summary_table) <- c("PFOA","PFHxA","PFPeA","PFHpA","PFOS")
#edit in word now! Creating additional columns for other pieces of info
#extra code for stuff
# CV: (not doing) sd(PFOSreg$reg)/mean(PFOSreg$reg)
# % Detect: need to rerun up until the "aggregate" section in recode.R, and then can use the following: 
#table(PFASwells1$PFPEAfinal), table(pfoapfhxa$PFOAfinal),etc.
#results as of 3/27: PFPeA 1324 0's and 615 1's, PFHpA 2276/1229, PFOS 3028/657, PFHxA 1316/943, PFOA 1096/2596 
stargazer(summary_table,title="Table I: Summary Statistics for Detectable Samples",summary.stat=c("n","mean","sd","min","p25","median","mean","p75","max"),out="table1_032719.html")
stargazer(compounds_logreg[["PFOA"]]$model,compounds_logreg[["PFHXA"]]$model,compounds_logreg[["PFPEA"]]$model,compounds_logreg[["PFHPA"]]$model,compounds_logreg[["PFOS"]]$model,
          title="Table II: Logistic Regression Model Coefficients", ci=TRUE,
          covariate.labels=c("Precipitation","Impact: Other Industries","Impact: Plastics and Rubbers","Impact: Airports and Waste",
                             "Impact: Printing","Impact: Semiconductors","Sand: % Very Coarse", "Sand: % Very Fine", "Silt: % Fine", "Clay: % Total", "Bulk Density",
                             "Available Water Capacity","Slope","Cation Exchange Capacity","Soil Organic Carbon","Depth to Bedrock","Water Table Depth: Annual Minimum","Horizon Thickness","Bedrock: Merrimack","Hydrologic Group: B","Drainage Class: Wet",
                             "Drainage Class: Poor","Hydrologic Group: C"),
          column.labels=c("PFOA","PFHxA","PFPeA","PFHpA","PFOS"),dep.var.labels.include = FALSE, dep.var.caption="",
          model.numbers=FALSE,keep.stat=c("n","aic"),out="logmodel_032619.html")

mean(compounds_logreg[["PFOA"]]$predicted_classes==compounds_logreg[["PFOA"]]$test_data$final)
mean(compounds_logreg[["PFHXA"]]$predicted_classes==compounds_logreg[["PFHXA"]]$test_data$final)
mean(compounds_logreg[["PFPEA"]]$predicted_classes==compounds_logreg[["PFPEA"]]$test_data$final)
mean(compounds_logreg[["PFHPA"]]$predicted_classes==compounds_logreg[["PFHPA"]]$test_data$final)
mean(compounds_logreg[["PFOS"]]$predicted_classes==compounds_logreg[["PFOS"]]$test_data$final)

#took this to R Studio, but some code for generating needed info:
#use sens_spec_tables and sens_spec_tablesf
#use MSE from application to TEST set (don't use built in one w generated model)
#1 - sum((y-predicted)^2)/sum((y-mean(y))^2) for R^2

stargazer(PFHXAmodel,PFHXAmodel,PFHXAmodel,PFHXAmodel,PFHXAmodel,PFHXAmodel,PFHXAmodel,PFHXAmodel,
          title="Table III: Model Performance and Diagnostics",
          covariate.labels=c("PFOA","PFHxA","PFPeA","PFHpA","PFOS"),dep.var.labels.include = FALSE, dep.var.caption="",
          column.labels=c("Logistic Regression","Classification Random Forest","Regression Random Forest"),
          column.separate = c(3,3,2),keep.stat=c("n"),out="forestmodel.html")


# Variable Importance Plots -----------------------------------------------

pdf("forestplots.pdf",useDingbats=FALSE)
varImpPlot(forest.PFOA)
varImpPlot(forest.PFHXA)
varImpPlot(forest.PFPEA)
varImpPlot(forest.PFHPA)
varImpPlot(forest.PFOS)
dev.off()


# Plot of all concentrations ----------------------------------------------
#gonna generate two versions...one where minimum concentration is sum of non-detect recode values
#and one where minimum concentration is 0??
#Straight sum:
totalPFAS <- PFASwells
totalPFAS$totalconc <- rowSums(totalPFAS[,2:6],na.rm=TRUE)
totalPFAS_arcmap <- merge(totalPFAS,unique1,by="StationID")

totalPFAS1 <- PFASwells
totalPFAS1$PFOAreg[totalPFAS1$PFOAreg == 1.414]<-0
totalPFAS1$PFHXAreg[totalPFAS1$PFHXAreg == 3.182]<-0
totalPFAS1$PFPEAreg[totalPFAS1$PFPEAreg == 3.182]<-0
totalPFAS1$PFHPAreg[totalPFAS1$PFHPAreg == 1.414]<-0
totalPFAS1$PFOSreg[totalPFAS1$PFOSreg == 2.828]<-0
totalPFAS1$totalconc <- rowSums(totalPFAS1[,2:6],na.rm=TRUE)
totalPFAS_arcmap1 <- merge(totalPFAS1,unique1,by="StationID")

write.csv(totalPFAS_arcmap,"totalPFAS_arcmap.csv")
write.csv(totalPFAS_arcmap1,"totalPFAS_arcmap1.csv")

# Plot of PFOS + PFOA concentrations ----------------------------------------------
#As of 4/18 work in progress!
pfos_unique <- compounds_data[["PFOS"]]
pfoa_unique <- compounds_data[["PFOA"]]
pfos_unique <- pfos_unique[,c(1:3)]
pfoa_unique <- pfoa_unique[,c(1:3)]
pfospfoa <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "StationID", all.x = TRUE),
                           list(unique1, pfoa_unique, pfos_unique))
colnames(pfospfoa) <- c("StationID","Longitude","Latitude","PFOA","PFOA_c","PFOS","PFOS_c")
pfospfoa$PFOSPFOA <- 0
#removing any rows with NA's in any of the columns
pfospfoa <- pfospfoa[-c(65, 89, 90, 91, 315, 1584,468, 225, 226, 2227,2223,2225,2226,2318,649,2222,2224,2319,2320,2321,2225,2226),]
#only 432 of these 
pfospfoa[pfospfoa$PFOA_c==1 & pfospfoa$PFOS_c == 1,"PFOSPFOA"] <- pfospfoa[pfospfoa$PFOA_c==1 & pfospfoa$PFOS_c == 1,"PFOA"] + pfospfoa[pfospfoa$PFOA_c==1 & pfospfoa$PFOS_c == 1,"PFOS"]
#24
pfospfoa[pfospfoa$PFOA_c==0 & pfospfoa$PFOS_c == 1,"PFOSPFOA"] <- pfospfoa[pfospfoa$PFOA_c==0 & pfospfoa$PFOS_c == 1,"PFOS"]
#1214
pfospfoa[pfospfoa$PFOA_c==1 & pfospfoa$PFOS_c == 0,"PFOSPFOA"] <- pfospfoa[pfospfoa$PFOA_c==1 & pfospfoa$PFOS_c == 0,"PFOA"]
#and then 693 non-detects

setwd("/Users/beverlyge/Research/NH_PFAS/")
write.csv(pfospfoa,"pfospfoa_map.csv")


# PFOS + PFOA map ---------------------------------------------------------

pfos_unique <- compounds_data[["PFOS"]]
pfoa_unique <- compounds_data[["PFOA"]]
pfos_unique <- pfos_unique[,c(1:3)]
pfoa_unique <- pfoa_unique[,c(1:3)]
pfospfoa <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "StationID", all.x = TRUE),
                   list(unique1, pfoa_unique, pfos_unique))
colnames(pfospfoa) <- c("StationID","Longitude","Latitude","PFOA","PFOA_c","PFOS","PFOS_c")
pfospfoa$PFOSPFOA <- 0
#removing any rows with NA's in any of the columns
pfospfoa <- pfospfoa[-c(65, 89, 90, 91, 315, 1584,468, 225, 226, 2227,2223,2225,2226,2318,649,2222,2224,2319,2320,2321,2225,2226),]
#only 432 of these 
pfospfoa[pfospfoa$PFOA_c==1 & pfospfoa$PFOS_c == 1,"PFOSPFOA"] <- pfospfoa[pfospfoa$PFOA_c==1 & pfospfoa$PFOS_c == 1,"PFOA"] + pfospfoa[pfospfoa$PFOA_c==1 & pfospfoa$PFOS_c == 1,"PFOS"]
#24
pfospfoa[pfospfoa$PFOA_c==0 & pfospfoa$PFOS_c == 1,"PFOSPFOA"] <- pfospfoa[pfospfoa$PFOA_c==0 & pfospfoa$PFOS_c == 1,"PFOS"]
#1214
pfospfoa[pfospfoa$PFOA_c==1 & pfospfoa$PFOS_c == 0,"PFOSPFOA"] <- pfospfoa[pfospfoa$PFOA_c==1 & pfospfoa$PFOS_c == 0,"PFOA"]
#and then 693 non-detects

setwd("/Users/beverlyge/Research/NH_PFAS/")
write.csv(pfospfoa,"pfospfoa_map.csv")


# Residual plots ----------------------------------------------------------

#not sure all of this is useful
PFOS_resids <- (PFOSreg_test$PFOSreg_log - outbag_predictions)^2
PFOSresid_map <- cbind(PFOSreg_test[,c(1:2)],PFOS_resids)
PFOSresid_arcmap <- merge(PFOSresid_map,unique[,-1],all.x=TRUE,by="StationID")
write.csv(PFOSresid_arcmap,"PFOSresid_arcmap.csv")

PFOS_resids1 <- PFOSreg_test$PFOSreg_log - outbag_predictions
#added outbag_predictions
PFOSresid_map1 <- cbind(PFOSreg_test[,c(1:2)],outbag_predictions, PFOS_resids1)
PFOS_residmean <- mean(PFOS_resids1)
PFOS_residstd <- sd(PFOS_resids1)

#Look at points with resids greater than or below 1 STD from mean of resids
PFOSreg1 <- cbind(PFOSreg_test,PFOS_resids1)
pfos_hiresids <- PFOSreg1[PFOSreg1$PFOS_resids1 >= PFOS_residmean + 2*PFOS_residstd,]
pfos_loresids <- PFOSreg1[PFOSreg1$PFOS_resids1 <= PFOS_residmean - 2*PFOS_residstd,]
pfos_xresids <- rbind(pfos_hiresids,pfos_loresids)
summary(pfos_xresids)
summary(PFOSreg_test)
PFOSresid_arcmap2 <- merge(pfos_xresids,unique[,-1],all.x=TRUE,by="StationID")
write.csv(PFOSresid_arcmap2,"PFOSresid_arcmap2.csv")

PFOSresid_arcmap1 <- merge(PFOSresid_map1,unique[,-1],all.x=TRUE,by="StationID")
write.csv(PFOSresid_arcmap1,"PFOSresid_arcmap1.csv")

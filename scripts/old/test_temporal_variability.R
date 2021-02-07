# # Test for temporal variability 
# # DoRun only after modeling data is completely cleaned
# 
# # Library -----------------------------------------------------------------
# 
# 
# # Load --------------------------------------------------------------------
# 
# PFASwells1 <- readRDS('modeling_data/PFASwells1.rds')
# pfoapfhxa <- readRDS('modeling_data/pfoapfhxa.rds')
# 
# 
# 
# # Test PFOA ---------------------------------------------------------------
# 
# pfoa_test <- na.omit(pfoapfhxa[pfoapfhxa$PFOAfinal == 1, c(1,2)])
# x <- pfoa_test[duplicated(pfoa_test$StationID) | duplicated(pfoa_test$StationID, fromLast = TRUE),]
# y <- aggregate(x$PFOAresults, by = list(StationID=x$StationID), FUN = var)
# z <- aggregate(x$PFOAresults, by = list(StationID=x$StationID), FUN = mean)
# pfoafinal <- data.frame(StationID = y$StationID, CV = y$x/z$x)
# pfoawells_hi <- pfoafinal[pfoafinal$CV >= quantile(pfoafinal$CV, .98, na.rm = T),]
# 
# mean(pfoafinal$CV, na.rm = TRUE) #=3.783449 UPDATED: =2.73
# median(pfoafinal$CV, na.rm = TRUE) #=0.3023792 UDPATED: = .14
# #198405024MW02
# #198405024SW04
# # appear for both PFOA and PFOS
# 
# 
# # Test PFOS ---------------------------------------------------------------
# 
# 
# pfos_test <- na.omit(PFASwells1[PFASwells1$PFOSfinal==1,c(1,4)])
# x<- pfos_test[duplicated(pfos_test$StationID) | duplicated(pfos_test$StationID,fromLast=TRUE),]
# y <- aggregate(x$PFOSresults,by=list(StationID=x$StationID),FUN=var)
# z <- aggregate(x$PFOSresults,by=list(StationID=x$StationID),FUN=mean)
# pfosfinal <- data.frame(StationID = y$StationID,CV=y$x/z$x)
# pfoswells_hi <- pfosfinal[pfosfinal$CV >= quantile(pfosfinal$CV,.98,na.rm=T),]
# mean(pfosfinal$CV,na.rm=TRUE) #=4.030 UPDATED: 2.50
# median(pfosfinal$CV,na.rm=TRUE) #median = 1.56 UPDATED: .21
# 
# 
# 
# # Other tests -------------------------------------------------------------
# 
# # actual_unique <- merge(PFASwells_original,unique[,-1],by=c("StationID"),all.x=TRUE)
# # blah <- unique(actual_unique[,c("StationID","Longitude","Latitude")])
# 
# #trying out spatially thinned dataset!
# #PFASwells <- merge(PFASwells_original,merged,by="StationID",all.y=TRUE)
# #PFASwells <- PFASwells[,-c(15)]
# 
# #500MB
# #subset, send code to Cindy
# #fiddling around with new approach
# 
# 
# 
# 

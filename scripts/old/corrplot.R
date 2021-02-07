# # Correlation Plot 
# # Maybe should check correlations again for original data after replacing NA's
# 
# # Library -----------------------------------------------------------------
# 
# library(corrplot)
# 
# # Load --------------------------------------------------------------------
# 
# unique_ivs <- readRDS('modeling_data/unique_ivs.rds')
# 
# 
# # Create correlation matrix -----------------------------------------------
# 
# len <- length(unique_ivs) - 5
# corrmatrix <- cor(unique_ivs[,2:len], use = "pairwise.complete.obs")
# 
# #-c(10,12:14,17,21,24:25,32,35,38)
# # len <- length(PFOSmodeldata) - 8
# # corrmatrix <- cor(PFHPAmodeldata[,4:len], use = "pairwise.complete.obs")
# # corrmatrix <- cor(PFPEAmodeldata[,-c(1:3,12,14:16,19,23,26:27,33:40)], use = "pairwise.complete.obs")
# 
# #corrgram(corrmatrix, order = NULL, lower.panel = panel.shade, upper.panel = NULL, text.panel = panel.txt, main = "Correlation Matrix for IVs")
# #corrplot(corrmatrix, type = "lower", method = "number", tl.cex = 1, number.cex = 0.5)
# cor.mtest <- function(mat, ...) {
#   mat <- as.matrix(mat)
#   n <- ncol(mat)
#   p.mat<- matrix(NA, n, n)
#   diag(p.mat) <- 0
#   for (i in 1:(n - 1)) {
#     for (j in (i + 1):n) {
#       tmp <- cor.test(mat[, i], mat[, j])
#       p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
#     }
#   }
#   colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
#   p.mat
# }
# # matrix of the p-value of the correlation
# p.mat <- cor.mtest(unique_ivs[,-c(1,23:27)])
# # p.mat <- cor.mtest(PFHXAmodeldata[,-c(1:3,12,14:16,19,23,26:27,33:40)])
# # p.mat <- cor.mtest(PFHPAmodeldata[,-c(1:3,33:40)])
# 
# 
# # Plot --------------------------------------------------------------------
# 
# corrplot(corrmatrix, type = "lower", method = "color", tl.cex = 1, tl.col = "black",number.cex = 0.5, diag = FALSE, addCoef.col = "gray",
#          p.mat = p.mat, sig.level = 0.05, insig = "blank", tl.srt = 45)
# 
# #postscript, dev.off() graphic machine R 

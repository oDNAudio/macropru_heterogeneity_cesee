rm(list=ls())

# packages
library(xtable)

out_folder <- "./output/"
dir.create(out_folder, showWarnings = FALSE)

plot_folder <- "./output/plots/"
dir.create(plot_folder, showWarnings = FALSE)

res_folder <- "./output/VAR_results/"

#####
# FEVD tables
horz <- c(5, 9, 13, 17, 21)

version <- "Results_baseline"

load(paste0(res_folder, version, "/Results_CAP_MPPI/irf_store.Rda"))

cred_med <- sprintf("%.2f", FEVD_common[3,horz,4,1] * 100)
cred_lo <- sprintf("%.2f", FEVD_common[2,horz,4,1] * 100)
cred_hi <- sprintf("%.2f", FEVD_common[4,horz,4,1] * 100)

cap_med <- sprintf("%.2f", FEVD_common[3,horz,6,1] * 100)
cap_lo <- sprintf("%.2f", FEVD_common[2,horz,6,1] * 100)
cap_hi <- sprintf("%.2f", FEVD_common[4,horz,6,1] * 100)


load(paste0(res_folder, version, "/Results_OI_MPPI/irf_store.Rda"))

oi_med <- sprintf("%.2f", FEVD_common[3,horz,6,1] * 100)
oi_lo <- sprintf("%.2f", FEVD_common[2,horz,6,1] * 100)
oi_hi <- sprintf("%.2f", FEVD_common[4,horz,6,1] * 100)

table_FEVD <- matrix(NA, 6, 6)

table_FEVD[1,1] <- "Credit growth"
table_FEVD[3,1] <- "CAP inflows"
table_FEVD[5,1] <- "OI inflows"

table_FEVD[1, 2:6] <- cred_med
table_FEVD[2, 2:6] <- paste0("[", cred_lo, "-", cred_hi, "]")
  
table_FEVD[3, 2:6] <- cap_med
table_FEVD[4, 2:6] <- paste0("[", cap_lo, "-", cap_hi, "]")

table_FEVD[5, 2:6] <- oi_med
table_FEVD[6, 2:6] <- paste0("[", oi_lo, "-", oi_hi, "]")

colnames(table_FEVD) <- c("Horizon", "1-year", "2-year", "3-year", "4-year", "5-year")

xtable_FEVD <- xtable(table_FEVD, align = c("l", "l", rep("c", 5)))

print(xtable_FEVD, include.rownames=FALSE)

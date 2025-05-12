# File for preparation of data and estimation of panel vector autoregressive models
# as outlined in Vashold (EM, 2025) and computation of impulse responses for a 
# tightening shock in the macroprudential environment (i.e. unit increase in MPPI) 
rm(list=ls())

# packages
library(mvtnorm)
library(coda)
library(MCMCpack)
library(abind)
library(magic)
library(MASS)
library(stringr)
library(GIGrvg)
library(bayesm)
library(matrixcalc)

# Load functions  ---------------------------------------------------------

source("./code/02_PVAR_functions.R")


# Settings ----------------------------------------------------------------

# Selector for runs with different settings below. When using a multi-thread 
# computing procedure this value should be read out from the task manager and 
# used to distribute the jobs across threads for parallel computing.
run <- 1

# number of burned and saved draws
nburn <- 5000 
nsave <- 5000 # number will be tripled and then every third will be retained

# horizon for IRFs, including impact period
nhor <- 21

# lags of endogenous and exogenous variables; Default: p = 4, q = 0
p <- 4
q <- 0

# selection of capital flow type, they are estimated separately
cap_slct <- "CAP" # Options: "CAP", "OI"

# selection of different MPP indexes
MPPI_slct <- "MPPI" # Options: "MPPI", "CAP_MPPI", "LQ_MPPI", "BB_MPPI"

# estimating large VAR with full variable set
big_var <- FALSE # Options: TRUE, FALSE



# different orderings:
#   - 0: original ordering
#   - 1: MPPI above IRST
#   - 2: MPPI below IRST
#   - 3: 1 with capflows above both policy vars
#   - 4: 2 with capflows above
ordering <- c(0, 1, 2, 3, 4) 

# splits according to MP activity: 
#   - 0: no split
#   - 1: already active pre-GFC
#   - 2: active post-GFC only
MPP_act <- c(0, 1, 2)

# splits according time period: 
#   - 0: no split
#   - 1: pre-GFC
#   - 2: post-GFC
prepost <- c(0, 1, 2)

# splits according to exchange rate regime: 
#   - 0: no split
#   - 1: fixed (or quasi-fixed) ER
#   - 2: floating (or quasi-floating) ER
exchange <- c(0, 1, 2)

# splits according to financial development: 
#   - 0: no split
#   - 1: more developed
#   - 2: less developed
fin_dev <- c(0, 1, 2)

# splits according to financial depth of insitutions: 
#   - 0: no split
#   - 1: deeper FIs
#   - 2: less deep FIs
fin_depth <- c(0, 1, 2)

# splits according to private indebtedness: 
#   - 0: no split
#   - 1: higher debt level (>= 50% of GDP)
#   - 2: lower debt level (< 50% of GDP)
loan_gdp <- c(0, 1, 2)

# splits according to FX loans ratio: 
#   - 0: no split
#   - 1: higher ratio (>= 30% of total loans)
#   - 2: lower ratio (< 30% of total loans)
loan_fx <- c(0, 1, 2)

# splits according to external indebtedness: 
#   - 0: no split
#   - 1: higher external debt (>= 80% of GDP)
#   - 2: lower external debt (< 80% of GDP)
ext_debt <- c(0, 1, 2)


# create grid of all possible combinations
combi <- expand.grid(p, q, cap_slct, MPPI_slct,
                     ordering, big_var,
                     prepost, MPP_act, 
                     exchange, fin_dev, fin_depth, loan_gdp, loan_fx, ext_debt)

# select the combination to estimate
combi.slct <- combi[run,]

# get settings from chosen specification
p <- combi.slct[[1]]
q <- combi.slct[[2]]
cap_slct <- combi.slct[[3]]
MPPI_slct <- combi.slct[[4]]
ordering <- combi.slct[[5]]
big_var <- combi.slct[[6]]
prepost <- combi.slct[[7]]
MPP_act <- combi.slct[[8]]
exchange <- combi.slct[[9]]
fin_dev <- combi.slct[[10]]
fin_depth <- combi.slct[[11]]
loan_gdp <- combi.slct[[12]]
loan_fx <- combi.slct[[13]]
ext_debt <- combi.slct[[14]]


# Preparing the data ------------------------------------------------------

load("./data/Xraw.Rda")

# selecting the right MPP index
pos_all_MPPI <- which(grepl("MPPI", names(Xraw)))
pos_slct_MPPI <- which(grepl(paste0("\\.", MPPI_slct), names(Xraw)))
pos_neg_slct_MPPI <- pos_all_MPPI[which(!pos_all_MPPI %in% pos_slct_MPPI)]
Xraw <- Xraw[, -pos_neg_slct_MPPI]

# selecting the capital flow variable
if(cap_slct == "CAP"){
  Xraw <- Xraw[, -grep("OILIAB|OIASSETS", names(Xraw))]
}else if(cap_slct == "OI"){
  Xraw <- Xraw[, -grep("CAPLIAB|CAPASSETS", names(Xraw))]
}

# de-selecting variables for small VAR
if(!big_var){
  Xraw <- Xraw[, -grep("LEQPRICE_SA|LREER_SA|ASSETS", names(Xraw))]
}
  
# selecting time period
if(prepost == 1) {
  Xraw <- Xraw[1:which(rownames(Xraw) == "2008.75"),]
} else if (prepost == 2) {
  Xraw <- Xraw[which(rownames(Xraw) == "2009"):nrow(Xraw), ] 
}

# selecting exogenous variables
Wraw <- as.matrix(Xraw[ , c("GLFAC", "GDP_DE", "MMR_DE")]) # check names
Xraw$GLFAC <- Xraw$AGRIP <- Xraw$GDP_DE <- Xraw$MMR_DE <- NULL

# selecting countries with similar MP dynamics
if(MPP_act == 1){
  Xraw <- Xraw[, grep("(BG|EE|HR|RO|SI)", substr(names(Xraw), 1, 2))]
}else if(MPP_act == 2){
  Xraw <- Xraw[, grep("(CZ|HU|LT|LV|PL|SK)", substr(names(Xraw), 1, 2))]
}

# selecting exchange rate regime
if(exchange == 1) {
  Xraw <- Xraw[, grep("(BG|EE|HR|LT|LV|SI|SK)", substr(names(Xraw), 1, 2))]
} else if(exchange == 2) {
  Xraw <- Xraw[, grep("(CZ|HU|PL|RO)", substr(names(Xraw), 1, 2))]
}

# selecting financial openness
if(fin_dev == 1) {
  Xraw <- Xraw[, grep("(CZ|EE|HU|PL|SI)", substr(names(Xraw), 1, 2))]
}else if(fin_dev == 2) {
  Xraw <- Xraw[, grep("(BG|HR|LT|LV|RO|SK)", substr(names(Xraw), 1, 2))]
}

# selecting on financial depth
if(fin_depth == 1) {
  Xraw <- Xraw[, grep("(CZ|EE|HR|HU|PL|SI|SK)", substr(names(Xraw), 1, 2))]
} else if(fin_depth == 2) {
  Xraw <- Xraw[, grep("(BG|LT|LV|RO)", substr(names(Xraw), 1, 2))]
}

# selecting on loan-to-GDP ratio
if(loan_gdp == 1) {
  Xraw <- Xraw[, grep("(EE|HR|LV|SI)", substr(names(Xraw), 1, 2))]
} else if(loan_gdp == 2) {
  Xraw <- Xraw[, grep("(BG|CZ|HU|LT|PL|RO|SK)", substr(names(Xraw), 1, 2))]
}

# selecting on FX loan ratio
if(loan_fx == 1) {
  Xraw <- Xraw[, grep("(BG|EE|HR|HU|LV|PL|RO)", substr(names(Xraw), 1, 2))]
} else if(loan_fx == 2) {
  Xraw <- Xraw[, grep("(CZ|LT|SI|SK)", substr(names(Xraw), 1, 2))]
}

# selecting on FX debt ratio
if(ext_debt == 1) {
  Xraw <- Xraw[, grep("(BG|EE|HR|HU|LV|SI)", substr(names(Xraw), 1, 2))]
} else if(ext_debt == 2) {
  Xraw <- Xraw[, grep("(CZ|LT|PL|RO|SK)", substr(names(Xraw), 1, 2))]
}

# getting names of countries
cN <- unique(substr(names(Xraw), 1, 2))
N <- length(cN)
var.names <- unique(substr(colnames(Xraw), 4, nchar(colnames(Xraw))))
M <- length(var.names)

# different orderings - only in small VAR
if(ordering != 0) {
  if(big_var) stop("Different orderings only in small-scale VAR.")
  if(ordering == 1) {
    new.order <- c("LGDPR_SA", "DP_QOQ", "LRCREDPRIV_SA", MPPI_slct, 
                   if(IRST) "IRST" else "IRLEND", paste0(cap_slct, "LIAB"))
  } else if (ordering == 2) {
    new.order <- c("LGDPR_SA", "DP_QOQ", "LRCREDPRIV_SA", 
                   if(IRST) "IRST" else "IRLEND", MPPI_slct, 
                   paste0(cap_slct, "LIAB"))
  } else if (ordering == 4) {
    new.order <- c("LGDPR_SA", "DP_QOQ", "LRCREDPRIV_SA", paste0(cap_slct, "LIAB"), 
                   if(IRST) "IRST" else "IRLEND", MPPI_slct)
  } else if (ordering == 3) {
    new.order <- c("LGDPR_SA", "DP_QOQ", "LRCREDPRIV_SA", paste0(cap_slct, "LIAB"),
                   MPPI_slct, if(IRST) "IRST" else "IRLEND")
  }
  grid <- expand.grid(new.order, cN)
  Xraw <- Xraw[, paste0(grid$Var2, ".", grid$Var1)]
}

# creating list of countries
Yraw <- list()
for(i in cN) {
  Yraw[[i]] <- na.omit(Xraw[, grep(i, substr(names(Xraw), 1, 2))])
  colnames(Yraw[[i]]) <- substr(names(Yraw[[i]]), 4, nchar(names(Yraw[[i]])))
  Yraw[[i]] <- as.matrix(Yraw[[i]])
}


# Estimating the PVAR -----------------------------------------------------

run <- PVAR(Yraw = Yraw, Wraw = Wraw, p = p, q = q, nsave = nsave, nburn = nburn, 
            cons = TRUE, store.draws = TRUE, store.post = TRUE, thin = 3)

# get main quantities needed for IRF computation
coeffs <- list(alpha = run$store$alpha,
               A = run$store$A,
               SIGMA = run$store$S,
               p = run$args$p)


# Compute IRFs and FEVDs --------------------------------------------------

shockvar <- MPPI_slct
irf_obj <- compute_irf(coeffs, nhor = nhor)
IRF_shock_MPPI  <- apply(irf_obj$IRF_store[irf_obj$IRF_ind==1,,shockvar,], c(2,3), 
                         quantile, c(0.05,.16,.50,.84,0.95),na.rm=TRUE)
IRFc_shock_MPPI <- apply(irf_obj$IRFc_store[irf_obj$IRF_ind==1,,shockvar,,], c(2,3,4), 
                         quantile, c(0.05,.16,.50,.84,0.95),na.rm=TRUE)

fevds <- compute_fevd(irf_obj)
FEVD_common <- apply(fevds$FEVD_common, c(2,3,4), 
                     quantile, c(0.05,.16,.50,.84,0.95),na.rm=TRUE)
FEVD_c <- apply(fevds$FEVD_c, c(2,3,4,5), 
                quantile, c(0.05,.16,.50,.84,0.95),na.rm=TRUE)


# Creating saving folder --------------------------------------------------

dirn <- "./output/"
dir.create(dirn, showWarnings = FALSE)
dirn <- paste0(dirn, "VAR_results/")
dir.create(dirn, showWarnings = FALSE)

if(MPP_act == 0 & prepost == 0 & ordering == 0 & 
   exchange == 0 & fin_dev == 0 & fin_depth == 0 & 
   loan_gdp == 0 & loan_fx == 0 & ext_debt == 0 & !big_var) {
  dirn <- paste0(dirn, "Results_baseline/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "Results_", cap_slct, "_", MPPI_slct, "/")
  dir.create(dirn, showWarnings = FALSE)
} else if (big_var) {
  dirn <- paste0(dirn, "Results_big_var/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "Results_", cap_slct, "_", MPPI_slct, "/")
  dir.create(dirn, showWarnings = FALSE)
} else if (MPP_act != 0) {
  dirn <- paste0(dirn, "Results_MPP_act/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "Results_", cap_slct, "_", MPPI_slct, "/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "MPP_act", MPP_act, "/")
  dir.create(dirn, showWarnings = FALSE)
} else if (prepost != 0) {
  dirn <- paste0(dirn, "Results_prepost/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "Results_", cap_slct, "_", MPPI_slct, "/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "prepost", prepost, "/")
  dir.create(dirn, showWarnings = FALSE)
} else if (ordering != 0) {
  dirn <- paste0(dirn, "Results_ordering/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "Results_", cap_slct, "_", MPPI_slct, "/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "ordering", ordering, "/")
  dir.create(dirn, showWarnings = FALSE)
} else if (exchange != 0) {
  dirn <- paste0(dirn, "Results_exchange/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "Results_", cap_slct, "_", MPPI_slct, "/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "exchange", exchange, "/")
  dir.create(dirn, showWarnings = FALSE)
} else if (fin_dev != 0) {
  dirn <- paste0(dirn, "Results_fin_dev/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "Results_", cap_slct, "_", MPPI_slct, "/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "fin_dev", fin_dev, "/")
  dir.create(dirn, showWarnings = FALSE)
} else if (fin_depth != 0) {
  dirn <- paste0(dirn, "Results_fin_depth/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "Results_", cap_slct, "_", MPPI_slct, "/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "fin_depth", fin_depth, "/")
  dir.create(dirn, showWarnings = FALSE)
} else if (loan_gdp != 0) {
  dirn <- paste0(dirn, "Results_loan_gdp/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "Results_", cap_slct, "_", MPPI_slct, "/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "loan_gdp", loan_gdp, "/")
  dir.create(dirn, showWarnings = FALSE)
} else if (loan_fx != 0) {
  dirn <- paste0(dirn, "Results_loan_fx/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "Results_", cap_slct, "_", MPPI_slct, "/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "loan_fx", loan_fx, "/")
  dir.create(dirn, showWarnings = FALSE)
} else if (ext_debt != 0) {
  dirn <- paste0(dirn, "Results_ext_debt/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "Results_", cap_slct, "_", MPPI_slct, "/")
  dir.create(dirn, showWarnings = FALSE)
  dirn <- paste0(dirn, "ext_debt", ext_debt, "/")
  dir.create(dirn, showWarnings = FALSE)
}


# Saving results ----------------------------------------------------------

# saving IRF results for later plotting
save(IRF_shock_MPPI, IRFc_shock_MPPI,
     FEVD_common, FEVD_c,
     file = paste0(dirn, "irf_store.Rda"))

# saving results for convergence checking etc
if(grepl("baseline", dirn)) {
  save(run, file = paste0(dirn, "run_store.Rda"))
}


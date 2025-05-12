rm(list=ls())

# packages
library(ggplot2)
library(cowplot)
library(reshape2)
library(xtable)
library(coda)
library(dplyr)
library(ggplot2)

out_folder <- "./output/"
dir.create(out_folder, showWarnings = FALSE)

plot_folder <- "./output/plots/"
dir.create(plot_folder, showWarnings = FALSE)
plot_folder <- paste0(plot_folder, "convergence_checks/")
dir.create(plot_folder, showWarnings = FALSE)

# take results from baseline run for CAP, results with OI are virtually the same
load("./output/VAR_results/Results_baseline/Results_CAP_MPPI/run_store.Rda")

#### Check the following parameters for convergence in different ways

## Geweke diagnostics + plots:
# lambda1: shrinkage parameter
# Lik: Log-likelihood
# alpha: common mean
# C0: average vcov-matrix

## Geweke diagnostics:
# A: country-specific autoregressive coefficients
# S: country-specific vcov-matrices 

png(paste0(plot_folder, "lambda1_conv.png"), width = 900, height = 600)
par(mfrow = c(1, 2))
plot(run$store$lambda1, type = "l", main = expression(paste("Trace Plot: ", lambda[1])), xlab = "", ylab = "")    
abline(h = mean(run$store$lambda1), col = "red", lty = 2)
abline(h = quantile(run$store$lambda1, p = c(0.05, 0.95)), col = "blue")
plot(density(run$store$lambda1), main = expression(paste("Density Plot: ", lambda[1])), xlab = "", ylab = "")
abline(v = mean(run$store$lambda1), col = "red", lty = 2)
abline(v = quantile(run$store$lambda1, p = c(0.05, 0.95)), col = "blue")
title(paste0("Geweke diagnostic z-score: ", round(geweke.diag(run$store$lambda1)$z, 2)), line = -40, outer = TRUE)
dev.off()

alpha_table <- as.data.frame.table(run$store$alpha) |> 
  transmute(index = rep(1:5000, 28*6), 
            lag.var = Var2, 
            lag.var = gsub("LGDPR_SA", "GDPg", lag.var),
            lag.var = gsub("DP_QOQ", "Inflation", lag.var),
            lag.var = gsub("LRCREDPRIV_SA", "Credit", lag.var),
            lag.var = gsub("CAPLIAB", "CapFlow", lag.var),
            lag.var = factor(lag.var,
                             levels = unique(lag.var)),
            dep.var = Var3, 
            dep.var = gsub("LGDPR_SA", "GDPg", dep.var),
            dep.var = gsub("DP_QOQ", "Inflation", dep.var),
            dep.var = gsub("LRCREDPRIV_SA", "Credit", dep.var),
            dep.var = gsub("CAPLIAB", "CapFlow", dep.var),
            dep.var = factor(dep.var,
                             levels = unique(dep.var)),
            value = Freq)

alpha_table <- alpha_table |> 
  group_by(lag.var, dep.var) |> 
  mutate(mean.var = mean(value), 
         q5 = quantile(value, p = 0.05),
         q95 = quantile(value, p = 0.95))

p_lag1 <- ggplot(data = alpha_table |> filter(grepl("lag1", lag.var)), aes(x = index, y = value)) +
  geom_line() + 
  ggh4x::facet_grid2(lag.var~dep.var, scales = "free_y", switch = "y", independent = "y") + 
  geom_hline(aes(yintercept = mean.var), col = "red") +
  geom_hline(aes(yintercept = q5), col = "blue") +
  geom_hline(aes(yintercept = q95), col = "blue") +
  theme_bw() + 
  theme(axis.title = element_blank(), 
        strip.text = element_text(size = 14))

p_lag2 <- ggplot(data = alpha_table |> filter(grepl("lag2", lag.var)), aes(x = index, y = value)) +
  geom_line() + 
  ggh4x::facet_grid2(lag.var~dep.var, scales = "free_y", switch = "y", independent = "y") + 
  geom_hline(aes(yintercept = mean.var), col = "red") +
  geom_hline(aes(yintercept = q5), col = "blue") +
  geom_hline(aes(yintercept = q95), col = "blue") +
  theme_bw()

p_lag3 <- ggplot(data = alpha_table |> filter(grepl("lag3", lag.var)), aes(x = index, y = value)) +
  geom_line() + 
  ggh4x::facet_grid2(lag.var~dep.var, scales = "free_y", switch = "y", independent = "y") + 
  geom_hline(aes(yintercept = mean.var), col = "red") +
  geom_hline(aes(yintercept = q5), col = "blue") +
  geom_hline(aes(yintercept = q95), col = "blue") +
  theme_bw()

p_lag4 <- ggplot(data = alpha_table |> filter(grepl("lag4", lag.var)), aes(x = index, y = value)) +
  geom_line() + 
  ggh4x::facet_grid2(lag.var~dep.var, scales = "free_y", switch = "y", independent = "y") + 
  geom_hline(aes(yintercept = mean.var), col = "red") +
  geom_hline(aes(yintercept = q5), col = "blue") +
  geom_hline(aes(yintercept = q95), col = "blue") +
  theme_bw()

p_w <- ggplot(data = alpha_table |> filter(grepl("GDP_DE|MMR_DE|GLFAC|cons", lag.var)), aes(x = index, y = value)) +
  geom_line() + 
  ggh4x::facet_grid2(lag.var~dep.var, scales = "free_y", switch = "y", independent = "y") + 
  geom_hline(aes(yintercept = mean.var), col = "red") +
  geom_hline(aes(yintercept = q5), col = "blue") +
  geom_hline(aes(yintercept = q95), col = "blue") +
  theme_bw()

png(paste0(plot_folder, "alpha_conv_lag1.png"), width = 1050, height = 600)
p_lag1
dev.off()

png(paste0(plot_folder, "alpha_conv_lag2.png"), width = 1050, height = 600)
p_lag2
dev.off()

png(paste0(plot_folder, "alpha_conv_lag3.png"), width = 1050, height = 600)
p_lag3
dev.off()

png(paste0(plot_folder, "alpha_conv_lag4.png"), width = 1050, height = 600)
p_lag4
dev.off()

png(paste0(plot_folder, "alpha_conv_w.png"), width = 1050, height = 600)
p_w
dev.off()

dimnames(run$store$C0)[[2]] <- dimnames(run$store$C0)[[3]] <- dimnames(run$store$alpha)[[3]]

c0_table <- as.data.frame.table(run$store$C0) |> 
  transmute(index = rep(1:5000, 6*6), 
            lag.var = Var2, 
            lag.var = gsub("LGDPR_SA", "GDPg", lag.var),
            lag.var = gsub("DP_QOQ", "Inflation", lag.var),
            lag.var = gsub("LRCREDPRIV_SA", "Credit", lag.var),
            lag.var = gsub("CAPLIAB", "CapFlow", lag.var),
            lag.var = factor(lag.var,
                             levels = unique(lag.var)),
            dep.var = Var3, 
            dep.var = gsub("LGDPR_SA", "GDPg", dep.var),
            dep.var = gsub("DP_QOQ", "Inflation", dep.var),
            dep.var = gsub("LRCREDPRIV_SA", "Credit", dep.var),
            dep.var = gsub("CAPLIAB", "CapFlow", dep.var),
            dep.var = factor(dep.var,
                             levels = unique(dep.var)),
            value = Freq)
c0_table <- c0_table |> 
  group_by(lag.var, dep.var) |> 
  mutate(mean.var = mean(value), 
         q5 = quantile(value, p = 0.05),
         q95 = quantile(value, p = 0.95))

p_c0 <- ggplot(data = c0_table, aes(x = index, y = value)) +
  geom_line() + 
  ggh4x::facet_grid2(lag.var~dep.var, scales = "free_y", switch = "y", independent = "y") + 
  geom_hline(aes(yintercept = mean.var), col = "red") +
  geom_hline(aes(yintercept = q5), col = "blue") +
  geom_hline(aes(yintercept = q95), col = "blue") +
  theme_bw() + 
  theme(axis.title = element_blank(), 
        strip.text = element_text(size = 14))


png(paste0(plot_folder, "c0_conv.png"), width = 1050, height = 600)
p_c0
dev.off()


#####
# Geweke convergence diagnostics
crit_val <- 1.96

# common mean autoregressive parameters
k <- dim(run$store$alpha)[2]
M <- dim(run$store$alpha)[3]
Z_scores_alpha <- c()
for(ii in 1:k){
  for(jj in 1:M){
    Z_scores_alpha[(jj-1)*k+ii] <- geweke.diag(run$store$alpha[,ii,jj])$z
  }
}
length(Z_scores_alpha)
sum(Z_scores_alpha > 1.96) 
# 3.5% of common mean autoregressive parameters have not converged
sum(Z_scores_alpha > 1.96)/length(Z_scores_alpha)


# country-specific autoregressive parameters
A_table <- as.data.frame.table(run$store$A) |> 
  transmute(id = rep(1:5000, 28*6*11), name = paste0(Var2, Var3, Var4), value = Freq) |> 
  tidyr::pivot_wider(names_from = name, values_from = value)
A_mcmc <- as.mcmc(A_table[,-1])
Z_scores_A <- geweke.diag(A_mcmc)$z
length(Z_scores_A)
sum(Z_scores_A > 1.96) 
# 4.2% of country-specific autoregressive parameters have not converged
sum(Z_scores_A > 1.96)/length(Z_scores_A)


# common mean variance parameters
C0_table <- as.data.frame.table(run$store$C0) |> 
  filter(!(Var2 == "MPPI" & Var3 %in% c("LGDPR_SA", "DP_QOQ", "LRCREDPRIV_SA", "IRST", "CAPLIAB")),
         !(Var2 == "LGDPR_SA" & Var3 %in% c("DP_QOQ", "LRCREDPRIV_SA", "IRST", "CAPLIAB")),
         !(Var2 == "DP_QOQ" & Var3 %in% c("LRCREDPRIV_SA", "IRST", "CAPLIAB")),
         !(Var2 == "LRCREDPRIV_SA" & Var3 %in% c("IRST", "CAPLIAB")),
         !(Var2 == "IRST" & Var3 %in% c("CAPLIAB"))) |> 
  transmute(id = rep(1:5000, ((6*7)/2)), name = paste0(Var2, Var3), value = Freq) |> 
  tidyr::pivot_wider(names_from = name, values_from = value)
C0_mcmc <- as.mcmc(C0_table[,-1])
Z_scores_C0 <- geweke.diag(C0_mcmc)$z
length(Z_scores_C0)
sum(Z_scores_C0 > 1.96) 
# 0% of common mean variance parameters have not converged
sum(Z_scores_C0 > 1.96)/length(Z_scores_C0)


# country-specific variance parameters
dimnames(run$store$S)[[2]] <- dimnames(run$store$S)[[3]] <- dimnames(run$store$alpha)[[3]]
dimnames(run$store$S)[[4]] <- dimnames(run$store$A)[[4]]
S_table <- as.data.frame.table(run$store$S) |> 
  filter(!(Var2 == "MPPI" & Var3 %in% c("LGDPR_SA", "DP_QOQ", "LRCREDPRIV_SA", "IRST", "CAPLIAB")),
         !(Var2 == "LGDPR_SA" & Var3 %in% c("DP_QOQ", "LRCREDPRIV_SA", "IRST", "CAPLIAB")),
         !(Var2 == "DP_QOQ" & Var3 %in% c("LRCREDPRIV_SA", "IRST", "CAPLIAB")),
         !(Var2 == "LRCREDPRIV_SA" & Var3 %in% c("IRST", "CAPLIAB")),
         !(Var2 == "IRST" & Var3 %in% c("CAPLIAB"))) |> 
  transmute(id = rep(1:5000, ((6*7)/2)*11), name = paste0(Var2, Var3, Var4), value = Freq) |> 
  tidyr::pivot_wider(names_from = name, values_from = value)
S_mcmc <- as.mcmc(S_table[,-1])
Z_scores_S <- geweke.diag(S_mcmc)$z
length(Z_scores_S)
sum(Z_scores_S > 1.96) 
# 2.6% of country-specific variance parameters have not converged
sum(Z_scores_S > 1.96)/length(Z_scores_S)

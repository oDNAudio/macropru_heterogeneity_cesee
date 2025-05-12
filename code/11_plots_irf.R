rm(list=ls())

# packages
library(ggplot2)
library(cowplot)
library(reshape2)
library(stringi)
library(xtable)

out_folder <- "./output/"
dir.create(out_folder, showWarnings = FALSE)

plot_folder <- "./output/plots/"
dir.create(plot_folder, showWarnings = FALSE)

res_folder <- "./output/VAR_results/"

horz <- 21
scaleFUN <- function(x) sprintf("%.2f", x)

#####
# Figure 2: MPP shock, all variables
load(paste0(res_folder, "Results_baseline/Results_CAP_MPPI/irf_store.Rda"))
irf_cap <- melt(IRF_shock_MPPI[2,,1:horz])
irf_cap$med <- melt(IRF_shock_MPPI[3,,1:horz])$value
irf_cap$hi <- melt(IRF_shock_MPPI[4,,1:horz])$value

load(paste0(res_folder, "Results_baseline/Results_OI_MPPI/irf_store.Rda"))
irf_oi <- melt(IRF_shock_MPPI[2,,1:horz])
irf_oi$med <- melt(IRF_shock_MPPI[3,,1:horz])$value
irf_oi$hi <- melt(IRF_shock_MPPI[4,,1:horz])$value

irf_comb <- rbind(irf_cap[which(irf_cap$Var1 != "MPPI"), ], irf_oi[which(irf_oi$Var1 == "OILIAB"),])
levels(irf_comb$Var1) <- c("MPPI", "GDP growth", "Price inflation", "Private sector credit growth",
                           "Short-term interest rate", "Total capital inflows", "Other investment inflows") 

p_all_1 <- ggplot(data = irf_comb |> 
                    filter(Var1 %in% c("GDP growth", "Price inflation", "Short-term interest rate")), 
                  aes(x = Var2 - 1)) +
  geom_ribbon(aes(ymin = value, ymax = hi), colour = "lightblue", fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = value), colour = "lightblue")+
  geom_line(aes(y = hi), colour = "lightblue")+
  geom_line(aes(y = med)) +
  labs(title = "", y = "", x = "") +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  scale_y_continuous(labels = scaleFUN) +
  facet_wrap(Var1~., ncol = 3) + 
  theme_bw() + 
  theme(legend.position = "top", 
        legend.key.width=unit(2,"cm"), 
        strip.background = element_blank(), 
        axis.text.x = element_text("", colour = "white"), 
        plot.margin = unit(c(-0.25,0.15,-0.75,-0.35), "cm"),
        strip.text = element_text(size = 16, face = "bold"), 
        axis.text.y = element_text(size = 14))
p_all_2 <- ggplot(data = irf_comb |> 
                    filter(Var1 %in% c("Private sector credit growth", "Total capital inflows", "Other investment inflows")),
                  aes(x = Var2 - 1)) +
  geom_ribbon(aes(ymin = value, ymax = hi), colour = "lightblue", fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = value), colour = "lightblue")+
  geom_line(aes(y = hi), colour = "lightblue")+
  geom_line(aes(y = med)) +
  labs(title = "", y = "", x = "") +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  scale_y_continuous(labels = scaleFUN) +
  facet_wrap(Var1~., ncol = 3) + theme_bw() + 
  theme(legend.position = "top",
        legend.key.width=unit(2,"cm"),
        strip.background = element_blank(), 
        plot.margin = unit(c(-0.75,0.15,-0.25,-0.35), "cm"), 
        strip.text = element_text(size = 16, face = "bold"), 
        axis.text = element_text(size = 14))

png(paste0(plot_folder, "Fig2-MPP_shock_allvars_responses.png"), 
          width = 5000, height = 2500, res = 300)
plot_grid(p_all_1, p_all_2, ncol = 1)
dev.off()


#####
# Figure 3: Country responses
load(paste0(res_folder, "Results_baseline/Results_CAP_MPPI/irf_store.Rda"))
irf_cap <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value


load(paste0(res_folder, "Results_baseline/Results_OI_MPPI/irf_store.Rda"))
irf_oi <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_comb <- rbind(irf_cap, irf_oi[which(irf_oi$Var1 == "OILIAB"), ])
irf_comb_c <- rbind(irf_cap_c, irf_oi_c[which(irf_oi_c$Var1 == "OILIAB"), ])
levels(irf_comb$Var1) <- c("Credit growth", "CAP inflows", "OI inflows")
levels(irf_comb_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows")

p_c_resp <- ggplot(data = irf_comb_c, aes(x = Var2 - 1)) +
  geom_hline(yintercept = 0.25, alpha = 0) +
  geom_ribbon(data = irf_comb, 
              aes(x = Var2 - 1, ymin = value, ymax = hi), colour = "grey", fill = "grey") +
  geom_ribbon(aes(ymin = value, ymax = hi), colour = "lightblue", fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = med)) +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  labs(title = "", y = "", x = "") +
  facet_grid(rows = vars(Var1), cols = vars(Var3), scales = "free_y", switch = "y") + 
  scale_y_continuous(position = "right") +
  theme_bw() + 
  theme(legend.position = "top", 
        legend.key.width=unit(2,"cm"),
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14))

png(paste0(plot_folder, "Fig3-MPP_shock_country_responses.png"), 
    width = 5000, height = 3000, res = 300)
p_c_resp
dev.off()



#####
# Figure 4: Exchange rate regimes & financial development

# Exchange rate regimes
load(paste0(res_folder, "Results_exchange/Results_CAP_MPPI/exchange1/irf_store.Rda"))
irf_cap_exchange1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_exchange1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_exchange1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_exchange1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_exchange1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_exchange1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_exchange/Results_OI_MPPI/exchange1/irf_store.Rda"))
irf_oi_exchange1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_exchange1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_exchange1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_exchange1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_exchange1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_exchange1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_exchange1 <- rbind(irf_cap_exchange1, irf_oi_exchange1[which(irf_oi_exchange1$Var1 == "OILIAB"),])
levels(irf_exchange1$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_exchange1$order <- "Fixed exchange rate"

irf_exchange1_c <- rbind(irf_cap_exchange1_c, irf_oi_exchange1_c[which(irf_oi_exchange1_c$Var1 == "OILIAB"),])
levels(irf_exchange1_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_exchange1_c$order <- "Fixed exchange rate"

load(paste0(res_folder, "Results_exchange/Results_CAP_MPPI/exchange2/irf_store.Rda"))
irf_cap_exchange2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_exchange2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_exchange2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_exchange2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_exchange2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_exchange2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_exchange/Results_OI_MPPI/exchange2/irf_store.Rda"))
irf_oi_exchange2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_exchange2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_exchange2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_exchange2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_exchange2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_exchange2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_exchange2 <- rbind(irf_cap_exchange2, irf_oi_exchange2[which(irf_oi_exchange2$Var1 == "OILIAB"),])
levels(irf_exchange2$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_exchange2$order <- "Flexible exchange rate"

irf_exchange2_c <- rbind(irf_cap_exchange2_c, irf_oi_exchange2_c[which(irf_oi_exchange2_c$Var1 == "OILIAB"),])
levels(irf_exchange2_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_exchange2_c$order <- "Flexible exchange rate"

# Financial development
load(paste0(res_folder, "Results_fin_dev/Results_CAP_MPPI/fin_dev1/irf_store.Rda"))
irf_cap_fin_dev1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_fin_dev1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_fin_dev1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_fin_dev1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_fin_dev1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_fin_dev1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_fin_dev/Results_OI_MPPI/fin_dev1/irf_store.Rda"))
irf_oi_fin_dev1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_fin_dev1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_fin_dev1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_fin_dev1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_fin_dev1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_fin_dev1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_fin_dev1 <- rbind(irf_cap_fin_dev1, irf_oi_fin_dev1[which(irf_oi_fin_dev1$Var1 == "OILIAB"),])
levels(irf_fin_dev1$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_fin_dev1$order <- "High financial development"

irf_fin_dev1_c <- rbind(irf_cap_fin_dev1_c, irf_oi_fin_dev1_c[which(irf_oi_fin_dev1_c$Var1 == "OILIAB"),])
levels(irf_fin_dev1_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_fin_dev1_c$order <- "High financial development"

load(paste0(res_folder, "Results_fin_dev/Results_CAP_MPPI/fin_dev2/irf_store.Rda"))
irf_cap_fin_dev2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_fin_dev2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_fin_dev2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_fin_dev2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_fin_dev2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_fin_dev2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_fin_dev/Results_OI_MPPI/fin_dev2/irf_store.Rda"))
irf_oi_fin_dev2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_fin_dev2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_fin_dev2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_fin_dev2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_fin_dev2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_fin_dev2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_fin_dev2 <- rbind(irf_cap_fin_dev2, irf_oi_fin_dev2[which(irf_oi_fin_dev2$Var1 == "OILIAB"),])
levels(irf_fin_dev2$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_fin_dev2$order <- "Low financial development"

irf_fin_dev2_c <- rbind(irf_cap_fin_dev2_c, irf_oi_fin_dev2_c[which(irf_oi_fin_dev2_c$Var1 == "OILIAB"),])
levels(irf_fin_dev2_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_fin_dev2_c$order <- "Low financial development"

irf_exchange_fin_dev <- rbind(irf_exchange1, irf_exchange2, irf_fin_dev1, irf_fin_dev2)
irf_exchange_fin_dev$order <- factor(irf_exchange_fin_dev$order, 
                                     levels = unique(irf_exchange_fin_dev$order))

irf_exchange_fin_dev_c <- rbind(irf_exchange1_c, irf_exchange2_c, 
                                irf_fin_dev1_c, irf_fin_dev2_c)
irf_exchange_fin_dev_c$order <- factor(irf_exchange_fin_dev_c$order, 
                                       levels = unique(irf_exchange_fin_dev_c$order))

p_exchange_FD <- ggplot(data = irf_exchange_fin_dev, aes(x = Var2-1)) +
  geom_hline(yintercept = 0.25, alpha = 0) +
  geom_line(data = irf_exchange_fin_dev_c, aes(x = Var2-1, y = med, group = Var3), lty = 2, size = 0.25) +
  geom_ribbon(aes(ymin = value, ymax = hi), colour = "lightblue", fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = med)) +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  labs(title = "", y = "", x = "") +
  facet_grid(rows = vars(Var1), cols = vars(order), scales = "free_y", switch = "y") + 
  scale_y_continuous(position = "right") +
  theme_bw() + 
  theme(legend.position = "top", 
        legend.key.width=unit(2,"cm"), 
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 14))

png(paste0(plot_folder, "Fig4-MPP_shock_exchange_FD.png"), 
    width = 5000, height = 2500, res = 300)
p_exchange_FD
dev.off()



#####
# Figure 5: External debt & FX loans

# External debt
load(paste0(res_folder, "Results_ext_debt/Results_CAP_MPPI/ext_debt1/irf_store.Rda"))
irf_cap_ext_debt1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_ext_debt1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_ext_debt1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_ext_debt1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_ext_debt1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_ext_debt1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_ext_debt/Results_OI_MPPI/ext_debt1/irf_store.Rda"))
irf_oi_ext_debt1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_ext_debt1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_ext_debt1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_ext_debt1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_ext_debt1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_ext_debt1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_ext_debt1 <- rbind(irf_cap_ext_debt1, irf_oi_ext_debt1[which(irf_oi_ext_debt1$Var1 == "OILIAB"),])
levels(irf_ext_debt1$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_ext_debt1$order <- "High external debt"

irf_ext_debt1_c <- rbind(irf_cap_ext_debt1_c, irf_oi_ext_debt1_c[which(irf_oi_ext_debt1_c$Var1 == "OILIAB"),])
levels(irf_ext_debt1_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_ext_debt1_c$order <- "High external debt"

load(paste0(res_folder, "Results_ext_debt/Results_CAP_MPPI/ext_debt2/irf_store.Rda"))
irf_cap_ext_debt2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_ext_debt2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_ext_debt2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_ext_debt2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_ext_debt2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_ext_debt2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_ext_debt/Results_OI_MPPI/ext_debt2/irf_store.Rda"))
irf_oi_ext_debt2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_ext_debt2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_ext_debt2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_ext_debt2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_ext_debt2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_ext_debt2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_ext_debt2 <- rbind(irf_cap_ext_debt2, irf_oi_ext_debt2[which(irf_oi_ext_debt2$Var1 == "OILIAB"),])
levels(irf_ext_debt2$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_ext_debt2$order <- "Low external debt"

irf_ext_debt2_c <- rbind(irf_cap_ext_debt2_c, irf_oi_ext_debt2_c[which(irf_oi_ext_debt2_c$Var1 == "OILIAB"),])
levels(irf_ext_debt2_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_ext_debt2_c$order <- "Low external debt"


# FX loans
load(paste0(res_folder, "Results_loan_fx/Results_CAP_MPPI/loan_fx1/irf_store.Rda"))
irf_cap_loan_fx1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_loan_fx1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_loan_fx1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_loan_fx1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_loan_fx1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_loan_fx1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_loan_fx/Results_OI_MPPI/loan_fx1/irf_store.Rda"))
irf_oi_loan_fx1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_loan_fx1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_loan_fx1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_loan_fx1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_loan_fx1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_loan_fx1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_loan_fx1 <- rbind(irf_cap_loan_fx1, irf_oi_loan_fx1[which(irf_oi_loan_fx1$Var1 == "OILIAB"),])
levels(irf_loan_fx1$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_loan_fx1$order <- "High FX loans share"

irf_loan_fx1_c <- rbind(irf_cap_loan_fx1_c, irf_oi_loan_fx1_c[which(irf_oi_loan_fx1_c$Var1 == "OILIAB"),])
levels(irf_loan_fx1_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_loan_fx1_c$order <- "High FX loans share"

load(paste0(res_folder, "Results_loan_fx/Results_CAP_MPPI/loan_fx2/irf_store.Rda"))
irf_cap_loan_fx2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_loan_fx2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_loan_fx2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_loan_fx2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_loan_fx2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_loan_fx2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_loan_fx/Results_OI_MPPI/loan_fx2/irf_store.Rda"))
irf_oi_loan_fx2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_loan_fx2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_loan_fx2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_loan_fx2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_loan_fx2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_loan_fx2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_loan_fx2 <- rbind(irf_cap_loan_fx2, irf_oi_loan_fx2[which(irf_oi_loan_fx2$Var1 == "OILIAB"),])
levels(irf_loan_fx2$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_loan_fx2$order <- "Low FX loans share"

irf_loan_fx2_c <- rbind(irf_cap_loan_fx2_c, irf_oi_loan_fx2_c[which(irf_oi_loan_fx2_c$Var1 == "OILIAB"),])
levels(irf_loan_fx2_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_loan_fx2_c$order <- "Low FX loans share"


irf_ext_debt_loan_fx <- rbind(irf_ext_debt1, irf_ext_debt2, irf_loan_fx1, irf_loan_fx2)
irf_ext_debt_loan_fx$order <- factor(irf_ext_debt_loan_fx$order, 
                                     levels = unique(irf_ext_debt_loan_fx$order))

irf_ext_debt_loan_fx_c <- rbind(irf_ext_debt1_c, irf_ext_debt2_c, irf_loan_fx1_c, irf_loan_fx2_c)
irf_ext_debt_loan_fx_c$order <- factor(irf_ext_debt_loan_fx_c$order, 
                                       levels = unique(irf_ext_debt_loan_fx_c$order))

p_ext_debt_loan_fx <- ggplot(data = irf_ext_debt_loan_fx, aes(x = Var2-1)) +
  geom_hline(yintercept = 0.25, alpha = 0) +
  geom_line(data = irf_ext_debt_loan_fx_c, aes(x = Var2-1, y = med, group = Var3), lty = 2, size = 0.25) +
  geom_ribbon(aes(ymin = value, ymax = hi), colour = "lightblue", fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = med)) +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  labs(title = "", y = "", x = "") +
  facet_grid(rows = vars(Var1), cols = vars(order), scales = "free_y", switch = "y") + 
  scale_y_continuous(position = "right") +
  theme_bw() +
  theme(legend.position = "top", 
        legend.key.width=unit(2,"cm"),
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14))


png(paste0(plot_folder, "Fig5-MPP_shock_extdebt_FXloans.png"), 
    width = 5000, height = 2500, res = 300)
p_ext_debt_loan_fx
dev.off()




#####
# Figure E1: Large VAR results
load(paste0(res_folder, "Results_big_var/Results_CAP_MPPI/irf_store.Rda"))
irf_cap <- melt(IRF_shock_MPPI[2,,1:horz])
irf_cap$med <- melt(IRF_shock_MPPI[3,,1:horz])$value
irf_cap$hi <- melt(IRF_shock_MPPI[4,,1:horz])$value

load(paste0(res_folder, "Results_big_var/Results_OI_MPPI/irf_store.Rda"))
irf_oi <- melt(IRF_shock_MPPI[2,,1:horz])
irf_oi$med <- melt(IRF_shock_MPPI[3,,1:horz])$value
irf_oi$hi <- melt(IRF_shock_MPPI[4,,1:horz])$value

irf_comb <- rbind(irf_cap[which(irf_cap$Var1 != "MPPI"), ], irf_oi[which(irf_oi$Var1 %in% c("OILIAB", "OIASSETS")),])
levels(irf_comb$Var1) <- c("MPPI", "GDP growth", "Price inflation", "Private sector credit growth",
                           "Short-term interest rate", "Equity price growth", "REER changes", 
                           "Total capital inflows", "Total capital outflows", 
                           "Other investment inflows", "Other investment outflows") 

p_big_var <- ggplot(data = irf_comb, aes(x = Var2-1)) +
  geom_ribbon(aes(ymin = value, ymax = hi), colour = "lightblue", fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = value), colour = "lightblue")+
  geom_line(aes(y = hi), colour = "lightblue")+
  geom_line(aes(y = med)) +
  labs(title = "", y = "", x = "") +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  facet_wrap(Var1~., ncol = 3, scales = "free") + 
  theme_bw() + 
  theme(legend.position = "top", 
        legend.key.width=unit(2,"cm"),
        strip.background = element_blank(), 
        plot.margin = unit(c(-0.25,0.15,-0.25,-0.35), "cm"),
        strip.text = element_text(size = 16, face = "bold"), 
        axis.text.y = element_text(size = 12))

png(paste0(plot_folder, "FigE1-MPP_shock_largeVAR.png"), 
    width = 5000, height = 2500, res = 300)
p_big_var
dev.off()


#####
# Figure E2: Different orderings of variables

# Above IRST, above capital flows
load(paste0(res_folder, "Results_ordering/Results_CAP_MPPI/ordering1/irf_store.Rda"))
irf_cap_ordering1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_ordering1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_ordering1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_ordering1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_ordering1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_ordering1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_ordering/Results_OI_MPPI/ordering1/irf_store.Rda"))
irf_oi_ordering1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_ordering1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_ordering1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_ordering1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_ordering1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_ordering1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_ordering1 <- rbind(irf_cap_ordering1, irf_oi_ordering1[which(irf_oi_ordering1$Var1 == "OILIAB"),])
levels(irf_ordering1$Var1) <- c("Credit growth", "CAP inflows", "OI inflows")  
irf_ordering1$order <- "Above IRST, \n above capital flows"
irf_ordering1_c <- rbind(irf_cap_ordering1_c, irf_oi_ordering1_c[which(irf_oi_ordering1_c$Var1 == "OILIAB"),])
levels(irf_ordering1_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_ordering1_c$order <- "Above IRST, \n above capital flows"

# Below IRST, above capital flows
load(paste0(res_folder, "Results_ordering/Results_CAP_MPPI/ordering2/irf_store.Rda"))
irf_cap_ordering2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_ordering2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_ordering2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_ordering2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_ordering2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_ordering2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_ordering/Results_OI_MPPI/ordering2/irf_store.Rda"))
irf_oi_ordering2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_ordering2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_ordering2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_ordering2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_ordering2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_ordering2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_ordering2 <- rbind(irf_cap_ordering2, irf_oi_ordering2[which(irf_oi_ordering2$Var1 == "OILIAB"),])
levels(irf_ordering2$Var1) <- c("Credit growth", "CAP inflows", "OI inflows")  
irf_ordering2$order <- "Below IRST, \n above capital flows"
irf_ordering2_c <- rbind(irf_cap_ordering2_c, irf_oi_ordering2_c[which(irf_oi_ordering2_c$Var1 == "OILIAB"),])
levels(irf_ordering2_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_ordering2_c$order <- "Below IRST, \n above capital flows"

# Above IRST, below capital flows
load(paste0(res_folder, "Results_ordering/Results_CAP_MPPI/ordering3/irf_store.Rda"))
irf_cap_ordering3 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_ordering3$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_ordering3$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_ordering3_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_ordering3_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_ordering3_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_ordering/Results_OI_MPPI/ordering3/irf_store.Rda"))
irf_oi_ordering3 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_ordering3$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_ordering3$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_ordering3_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_ordering3_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_ordering3_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_ordering3 <- rbind(irf_cap_ordering3, irf_oi_ordering3[which(irf_oi_ordering3$Var1 == "OILIAB"),])
levels(irf_ordering3$Var1) <- c("Credit growth", "CAP inflows", "OI inflows")  
irf_ordering3$order <- "Above IRST, \n below capital flows"
irf_ordering3_c <- rbind(irf_cap_ordering3_c, irf_oi_ordering3_c[which(irf_oi_ordering3_c$Var1 == "OILIAB"),])
levels(irf_ordering3_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_ordering3_c$order <- "Above IRST, \n below capital flows"

# Below IRST, below cpaital flows
load(paste0(res_folder, "Results_ordering/Results_CAP_MPPI/ordering4/irf_store.Rda"))
irf_cap_ordering4 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_ordering4$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_ordering4$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_ordering4_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_ordering4_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_ordering4_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_ordering/Results_OI_MPPI/ordering4/irf_store.Rda"))
irf_oi_ordering4 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_ordering4$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_ordering4$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_ordering4_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_ordering4_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_ordering4_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_ordering4 <- rbind(irf_cap_ordering4, irf_oi_ordering4[which(irf_oi_ordering4$Var1 == "OILIAB"),])
levels(irf_ordering4$Var1) <- c("Credit growth", "CAP inflows", "OI inflows")  
irf_ordering4$order <- "Below IRST, \n below capital flows"
irf_ordering4_c <- rbind(irf_cap_ordering4_c, irf_oi_ordering4_c[which(irf_oi_ordering4_c$Var1 == "OILIAB"),])
levels(irf_ordering4_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_ordering4_c$order <- "Below IRST, \n below capital flows"



irf_ordering <- rbind(irf_ordering1, irf_ordering2, irf_ordering3, irf_ordering4)
irf_ordering$order <- factor(irf_ordering$order, levels = unique(irf_ordering$order))

irf_ordering_c <- rbind(irf_ordering1_c, irf_ordering2_c, irf_ordering3_c, irf_ordering4_c)
irf_ordering_c$order <- factor(irf_ordering_c$order, levels = unique(irf_ordering_c$order))


p_order <- ggplot(data = irf_ordering, aes(x = Var2-1)) +
  geom_hline(yintercept = 0.25, alpha = 0) +
  geom_line(data = irf_ordering_c, aes(x = Var2-1, y = med, group = Var3), lty = 2, size = 0.25) +
  geom_ribbon(aes(ymin = value, ymax = hi), colour = "lightblue", fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = med)) +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  labs(title = "", y = "", x = "") +
  facet_grid(rows = vars(Var1), cols = vars(order), scales = "free_y", switch = "y") + 
  scale_y_continuous(position = "right") +
  theme_bw() +
  theme(legend.position = "top", 
        legend.key.width=unit(2,"cm"), 
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 14))

png(paste0(plot_folder, "FigE2-MPP_shock_ordering.png"), 
    width = 5000, height = 2500, res = 300)
p_order
dev.off()



#####
# Figure F1: Subindices of MPP index

# Capital-based subindex
load(paste0(res_folder, "Results_baseline/Results_CAP_CAP_MPPI/irf_store.Rda"))
irf_cap_cap_mppi <- melt(IRF_shock_MPPI[2,,1:horz])
irf_cap_cap_mppi$med <- melt(IRF_shock_MPPI[3,,1:horz])$value
irf_cap_cap_mppi$hi <- melt(IRF_shock_MPPI[4,,1:horz])$value

load(paste0(res_folder, "Results_baseline/Results_OI_CAP_MPPI/irf_store.Rda"))
irf_oi_cap_mppi <- melt(IRF_shock_MPPI[2,,1:horz])
irf_oi_cap_mppi$med <- melt(IRF_shock_MPPI[3,,1:horz])$value
irf_oi_cap_mppi$hi <- melt(IRF_shock_MPPI[4,,1:horz])$value

irf_cap_mppi <- rbind(irf_cap_cap_mppi[which(irf_cap_cap_mppi$Var1 != "CAP_MPPI"), ],
                      irf_oi_cap_mppi[which(irf_oi_cap_mppi$Var1 == "OILIAB"),])
irf_cap_mppi <- irf_cap_mppi |> 
  filter(Var1 %in% c("LRCREDPRIV_SA", "CAPLIAB", "OILIAB")) |> 
  mutate(shock = "CAP-MPPI")

levels(irf_cap_mppi$Var1) <- c("CAP_MPPI", "GDP growth", "Price inflation", "Private sector credit growth",
                               "Short-term interest rate", "Total capital inflows", "Other investment inflows")

p_MPPI_CAP <- ggplot(data = irf_cap_mppi, aes(x = Var2 - 1)) +
  geom_ribbon(aes(ymin = value, ymax = hi), colour = "lightblue", fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = value), colour = "lightblue")+
  geom_line(aes(y = hi), colour = "lightblue")+
  geom_line(aes(y = med)) +
  labs(title = "", y = "CAP-MPPI", x = "") +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  scale_y_continuous(labels = scaleFUN) +
  facet_wrap(.~Var1, ncol = 3) + 
  theme_bw() + 
  theme(legend.position = "left", legend.key.width=unit(2,"cm"), strip.background = element_blank(), 
        axis.text.x = element_text("", colour = "white"),
        plot.margin = unit(c(-0.25,0.15,-0.75,0), "cm"),
        strip.text = element_text(size = 16, face = "bold"), 
        axis.title.y = element_text(size = 16, face = "bold"), 
        axis.text.y = element_text(size = 14))

# Liquidity-based subindex
load(paste0(res_folder, "Results_baseline/Results_CAP_LQ_MPPI/irf_store.Rda"))
irf_cap_lq_mppi <- melt(IRF_shock_MPPI[2,,1:horz])
irf_cap_lq_mppi$med <- melt(IRF_shock_MPPI[3,,1:horz])$value
irf_cap_lq_mppi$hi <- melt(IRF_shock_MPPI[4,,1:horz])$value

load(paste0(res_folder, "Results_baseline/Results_OI_LQ_MPPI/irf_store.Rda"))
irf_oi_lq_mppi <- melt(IRF_shock_MPPI[2,,1:horz])
irf_oi_lq_mppi$med <- melt(IRF_shock_MPPI[3,,1:horz])$value
irf_oi_lq_mppi$hi <- melt(IRF_shock_MPPI[4,,1:horz])$value


irf_lq_mppi <- rbind(irf_cap_lq_mppi[which(irf_cap_lq_mppi$Var1 != "LQ_MPPI"), ],
                      irf_oi_lq_mppi[which(irf_oi_lq_mppi$Var1 == "OILIAB"),])
irf_lq_mppi <- irf_lq_mppi |> 
  filter(Var1 %in% c("LRCREDPRIV_SA", "CAPLIAB", "OILIAB")) |> 
  mutate(shock = "LQ-MPPI")

levels(irf_lq_mppi$Var1) <- c("LQ_MPPI", "GDP growth", "Price inflation", "Private sector credit growth",
                          "Short-term interest rate", "Total capital inflows", "Other investment inflows")

p_MPPI_LQ <- ggplot(data = irf_lq_mppi, aes(x = Var2 - 1)) +
  geom_ribbon(aes(ymin = value, ymax = hi), colour = "lightblue", fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = value), colour = "lightblue")+
  geom_line(aes(y = hi), colour = "lightblue")+
  geom_line(aes(y = med)) +
  labs(title = "", y = "LQ-MPPI", x = "") +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  scale_y_continuous(labels = scaleFUN) +
  facet_wrap(.~Var1, ncol = 3) + theme_bw() + 
  theme(legend.position = "left", legend.key.width=unit(2,"cm"), strip.background = element_blank(), 
        axis.text.x = element_text("", colour = "white"),
        plot.margin = unit(c(-0.25,0.15,-0.75,0), "cm"),
        strip.text = element_blank(), 
        axis.title.y = element_text(size = 16, face = "bold"), 
        axis.text.y = element_text(size = 14))

# Borrower-based subindex
load(paste0(res_folder, "Results_baseline/Results_CAP_BB_MPPI/irf_store.Rda"))
irf_cap_bb_mppi <- melt(IRF_shock_MPPI[2,,1:horz])
irf_cap_bb_mppi$med <- melt(IRF_shock_MPPI[3,,1:horz])$value
irf_cap_bb_mppi$hi <- melt(IRF_shock_MPPI[4,,1:horz])$value

load(paste0(res_folder, "Results_baseline/Results_OI_BB_MPPI/irf_store.Rda"))
irf_oi_bb_mppi <- melt(IRF_shock_MPPI[2,,1:horz])
irf_oi_bb_mppi$med <- melt(IRF_shock_MPPI[3,,1:horz])$value
irf_oi_bb_mppi$hi <- melt(IRF_shock_MPPI[4,,1:horz])$value


irf_bb_mppi <- rbind(irf_cap_bb_mppi[which(irf_cap_bb_mppi$Var1 != "BB_MPPI"), ],
                     irf_oi_bb_mppi[which(irf_oi_bb_mppi$Var1 == "OILIAB"),])
irf_bb_mppi <- irf_bb_mppi |> 
  filter(Var1 %in% c("LRCREDPRIV_SA", "CAPLIAB", "OILIAB")) |> 
  mutate(shock = "BB-MPPI")

levels(irf_bb_mppi$Var1) <- c("BB_MPPI", "GDP growth", "Price inflation", "Private sector credit growth",
                              "Short-term interest rate", "Total capital inflows", "Other investment inflows")

p_MPPI_BB <- ggplot(data = irf_bb_mppi, aes(x = Var2-1)) +
  geom_ribbon(aes(ymin = value, ymax = hi), colour = "lightblue", fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = value), colour = "lightblue")+
  geom_line(aes(y = hi), colour = "lightblue")+
  geom_line(aes(y = med)) +
  labs(title = "", y = "BB-MPPI", x = "") +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  scale_y_continuous(labels = scaleFUN) +
  facet_wrap(.~Var1, ncol = 3) + theme_bw() + 
  theme(legend.position = "left", legend.key.width=unit(2,"cm"), strip.background = element_blank(), 
        axis.text.x = element_text("", colour = "white"),
        plot.margin = unit(c(-0.25,0.15,-0.75,0), "cm"),
        strip.text = element_blank(), 
        axis.title.y = element_text(size = 16, face = "bold"), 
        axis.text.y = element_text(size = 14))


png(paste0(plot_folder, "FigF1-MPP_shock_MPPI-subindices.png"), 
    width = 5000, height = 3000, res = 300)
plot_grid(p_MPPI_CAP, p_MPPI_LQ, p_MPPI_BB, ncol = 1, rel_heights = c(1.1, 1, 1))
dev.off()


#####
# Figure F2: Private debt & depth of financial institutions

# Private debt
load(paste0(res_folder, "Results_loan_gdp/Results_CAP_MPPI/loan_gdp1/irf_store.Rda"))
irf_cap_loan_gdp1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_loan_gdp1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_loan_gdp1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_loan_gdp1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_loan_gdp1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_loan_gdp1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_loan_gdp/Results_OI_MPPI/loan_gdp1/irf_store.Rda"))
irf_oi_loan_gdp1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_loan_gdp1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_loan_gdp1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_loan_gdp1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_loan_gdp1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_loan_gdp1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_loan_gdp1 <- rbind(irf_cap_loan_gdp1, irf_oi_loan_gdp1[which(irf_oi_loan_gdp1$Var1 == "OILIAB"),])
levels(irf_loan_gdp1$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_loan_gdp1$order <- "High private debt"

irf_loan_gdp1_c <- rbind(irf_cap_loan_gdp1_c, irf_oi_loan_gdp1_c[which(irf_oi_loan_gdp1_c$Var1 == "OILIAB"),])
levels(irf_loan_gdp1_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_loan_gdp1_c$order <- "High private debt"

load(paste0(res_folder, "Results_loan_gdp/Results_CAP_MPPI/loan_gdp2/irf_store.Rda"))
irf_cap_loan_gdp2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_loan_gdp2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_loan_gdp2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_loan_gdp2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_loan_gdp2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_loan_gdp2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_loan_gdp/Results_OI_MPPI/loan_gdp2/irf_store.Rda"))
irf_oi_loan_gdp2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_loan_gdp2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_loan_gdp2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_loan_gdp2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_loan_gdp2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_loan_gdp2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_loan_gdp2 <- rbind(irf_cap_loan_gdp2, irf_oi_loan_gdp2[which(irf_oi_loan_gdp2$Var1 == "OILIAB"),])
levels(irf_loan_gdp2$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_loan_gdp2$order <- "Low private debt"

irf_loan_gdp2_c <- rbind(irf_cap_loan_gdp2_c, irf_oi_loan_gdp2_c[which(irf_oi_loan_gdp2_c$Var1 == "OILIAB"),])
levels(irf_loan_gdp2_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_loan_gdp2_c$order <- "Low private debt"


# Depth of financial institutions
load(paste0(res_folder, "Results_fin_depth/Results_CAP_MPPI/fin_depth1/irf_store.Rda"))
irf_cap_fin_depth1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_fin_depth1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_fin_depth1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_fin_depth1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_fin_depth1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_fin_depth1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_fin_depth/Results_OI_MPPI/fin_depth1/irf_store.Rda"))
irf_oi_fin_depth1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_fin_depth1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_fin_depth1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_fin_depth1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_fin_depth1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_fin_depth1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_fin_depth1 <- rbind(irf_cap_fin_depth1, irf_oi_fin_depth1[which(irf_oi_fin_depth1$Var1 == "OILIAB"),])
levels(irf_fin_depth1$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_fin_depth1$order <- "High financial depth"

irf_fin_depth1_c <- rbind(irf_cap_fin_depth1_c, irf_oi_fin_depth1_c[which(irf_oi_fin_depth1_c$Var1 == "OILIAB"),])
levels(irf_fin_depth1_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_fin_depth1_c$order <- "High financial depth"

load(paste0(res_folder, "Results_fin_depth/Results_CAP_MPPI/fin_depth2/irf_store.Rda"))
irf_cap_fin_depth2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_fin_depth2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_fin_depth2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_fin_depth2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_fin_depth2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_fin_depth2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_fin_depth/Results_OI_MPPI/fin_depth2/irf_store.Rda"))
irf_oi_fin_depth2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_fin_depth2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_fin_depth2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_fin_depth2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_fin_depth2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_fin_depth2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_fin_depth2 <- rbind(irf_cap_fin_depth2, irf_oi_fin_depth2[which(irf_oi_fin_depth2$Var1 == "OILIAB"),])
levels(irf_fin_depth2$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_fin_depth2$order <- "Low financial depth"

irf_fin_depth2_c <- rbind(irf_cap_fin_depth2_c, irf_oi_fin_depth2_c[which(irf_oi_fin_depth2_c$Var1 == "OILIAB"),])
levels(irf_fin_depth2_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_fin_depth2_c$order <- "Low financial depth"


irf_loan_gdp_fin_depth <- rbind(irf_loan_gdp1, irf_loan_gdp2, irf_fin_depth1, irf_fin_depth2)
irf_loan_gdp_fin_depth$order <- factor(irf_loan_gdp_fin_depth$order, 
                                       levels = unique(irf_loan_gdp_fin_depth$order))

irf_loan_gdp_fin_depth_c <- rbind(irf_loan_gdp1_c, irf_loan_gdp2_c, irf_fin_depth1_c, irf_fin_depth2_c)
irf_loan_gdp_fin_depth_c$order <- factor(irf_loan_gdp_fin_depth_c$order, 
                                         levels = unique(irf_loan_gdp_fin_depth_c$order))


p_debt_depth <- ggplot(data = irf_loan_gdp_fin_depth, aes(x = Var2-1)) +
  geom_hline(yintercept = 0.25, alpha = 0) +
  geom_line(data = irf_loan_gdp_fin_depth_c, aes(x = Var2-1, y = med, group = Var3), lty = 2, size = 0.25) +
  geom_ribbon(aes(ymin = value, ymax = hi), colour = "lightblue", fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = med)) +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  labs(title = "", y = "", x = "") +
  facet_grid(rows = vars(Var1), cols = vars(order), scales = "free_y",  switch = "y") + 
  scale_y_continuous(position = "right") +
  theme_bw() + 
  theme(legend.position = "top",
        legend.key.width=unit(2,"cm"),
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 14))


png(paste0(plot_folder, "FigF2-MPP_shock_debt_depth.png"), 
    width = 5000, height = 2500, res = 300)
p_debt_depth
dev.off()



#####
# Figure F3: MP activity & GFC split

# MP activity
load(paste0(res_folder, "Results_MPP_act/Results_CAP_MPPI/MPP_act1/irf_store.Rda"))
irf_cap_mpp_act1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_mpp_act1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_mpp_act1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_mpp_act1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_mpp_act1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_mpp_act1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_MPP_act/Results_OI_MPPI/MPP_act1/irf_store.Rda"))
irf_oi_mpp_act1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_mpp_act1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_mpp_act1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_mpp_act1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_mpp_act1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_mpp_act1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_mpp_act1 <- rbind(irf_cap_mpp_act1, irf_oi_mpp_act1[which(irf_oi_mpp_act1$Var1 == "OILIAB"),])
levels(irf_mpp_act1$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_mpp_act1$order <- "MPP active pre-crisis"

irf_mpp_act1_c <- rbind(irf_cap_mpp_act1_c, irf_oi_mpp_act1_c[which(irf_oi_mpp_act1_c$Var1 == "OILIAB"),])
levels(irf_mpp_act1_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_mpp_act1_c$order <- "MPP active pre-crisis"

load(paste0(res_folder, "Results_MPP_act/Results_CAP_MPPI/MPP_act2/irf_store.Rda"))
irf_cap_mpp_act2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_mpp_act2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_mpp_act2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_mpp_act2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_mpp_act2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_mpp_act2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_MPP_act/Results_OI_MPPI/MPP_act2/irf_store.Rda"))
irf_oi_mpp_act2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_mpp_act2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_mpp_act2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_mpp_act2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_mpp_act2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_mpp_act2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_mpp_act2 <- rbind(irf_cap_mpp_act2, irf_oi_mpp_act2[which(irf_oi_mpp_act2$Var1 == "OILIAB"),])
levels(irf_mpp_act2$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_mpp_act2$order <- "MPP active post-crisis"

irf_mpp_act2_c <- rbind(irf_cap_mpp_act2_c, irf_oi_mpp_act2_c[which(irf_oi_mpp_act2_c$Var1 == "OILIAB"),])
levels(irf_mpp_act2_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_mpp_act2_c$order <- "MPP active post-crisis"


irf_mpp_act <- rbind(irf_mpp_act1, irf_mpp_act2)
irf_mpp_act$order <- factor(irf_mpp_act$order, levels = unique(irf_mpp_act$order))

irf_mpp_act_c <- rbind(irf_mpp_act1_c, irf_mpp_act2_c)
irf_mpp_act_c$order <- factor(irf_mpp_act_c$order, levels = unique(irf_mpp_act_c$order))

p_MPPI <- ggplot(data = irf_mpp_act, aes(x = Var2-1)) +
  geom_hline(yintercept = 0.25, alpha = 0) +
  geom_line(data = irf_mpp_act_c, aes(x = Var2-1, y = med, group = Var3), lty = 2, size = 0.25) +
  geom_ribbon(aes(ymin = value, ymax = hi), colour = "lightblue", fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = med)) +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  labs(title = "", y = "", x = "") +
  facet_grid(rows = vars(Var1), cols = vars(order), scales = "free_y",  switch = "y") +
  scale_y_continuous(position = "right") +
  theme_bw() + 
  theme(legend.position = "top", 
        legend.key.width=unit(2,"cm"),
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 14))


# GFC split
load(paste0(res_folder, "Results_prepost/Results_CAP_MPPI/prepost1/irf_store.Rda"))
irf_cap_prepost1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_prepost1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_prepost1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_prepost1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_prepost1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_prepost1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_prepost/Results_OI_MPPI/prepost1/irf_store.Rda"))
irf_oi_prepost1 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_prepost1$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_prepost1$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_prepost1_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_prepost1_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_prepost1_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_prepost1 <- rbind(irf_cap_prepost1, irf_oi_prepost1[which(irf_oi_prepost1$Var1 == "OILIAB"),])
levels(irf_prepost1$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_prepost1$order <- "Pre-crisis period"

irf_prepost1_c <- rbind(irf_cap_prepost1_c, irf_oi_prepost1_c[which(irf_oi_prepost1_c$Var1 == "OILIAB"),])
levels(irf_prepost1_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_prepost1_c$order <- "Pre-crisis period"

load(paste0(res_folder, "Results_prepost/Results_CAP_MPPI/prepost2/irf_store.Rda"))
irf_cap_prepost2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])
irf_cap_prepost2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_prepost2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz])$value
irf_cap_prepost2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])
irf_cap_prepost2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value
irf_cap_prepost2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "CAPLIAB"),1:horz,])$value

load(paste0(res_folder, "Results_prepost/Results_OI_MPPI/prepost2/irf_store.Rda"))
irf_oi_prepost2 <- melt(IRF_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz])
irf_oi_prepost2$med <- melt(IRF_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_prepost2$hi <- melt(IRF_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz])$value
irf_oi_prepost2_c <- melt(IRFc_shock_MPPI[2,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])
irf_oi_prepost2_c$med <- melt(IRFc_shock_MPPI[3,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value
irf_oi_prepost2_c$hi <- melt(IRFc_shock_MPPI[4,c("LRCREDPRIV_SA", "OILIAB"),1:horz,])$value

irf_prepost2 <- rbind(irf_cap_prepost2, irf_oi_prepost2[which(irf_oi_prepost2$Var1 == "OILIAB"),])
levels(irf_prepost2$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_prepost2$order <- "Post-crisis period"

irf_prepost2_c <- rbind(irf_cap_prepost2_c, irf_oi_prepost2_c[which(irf_oi_prepost2_c$Var1 == "OILIAB"),])
levels(irf_prepost2_c$Var1) <- c("Credit growth", "CAP inflows", "OI inflows") 
irf_prepost2_c$order <- "Post-crisis period"

irf_prepost <- rbind(irf_prepost1, irf_prepost2)
irf_prepost$order <- factor(irf_prepost$order, levels = unique(irf_prepost$order))

irf_prepost_c <- rbind(irf_prepost1_c, irf_prepost2_c)
irf_prepost_c$order <- factor(irf_prepost_c$order, levels = unique(irf_prepost_c$order))

p_crisis <- ggplot(data = irf_prepost, aes(x = Var2-1)) +
  geom_hline(yintercept = 0.25, alpha = 0) +
  geom_line(data = irf_prepost_c, aes(x = Var2-1, y = med, group = Var3), lty = 2, size = 0.25) +
  geom_ribbon(aes(ymin = value, ymax = hi), colour = "lightblue", fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = med)) +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  labs(title = "", y = "", x = "") +
  facet_grid(rows = vars(Var1), cols = vars(order), scales = "free_y",  switch = "y") + 
  scale_y_continuous(position = "right") +
  theme_bw() + 
  theme(legend.position = "top", 
        legend.key.width=unit(2,"cm"), 
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 14),
        strip.text.y = element_text(colour = "white"))


png(paste0(plot_folder, "FigF3-MPP_shock_MPP_crisis.png"), 
    width = 5000, height = 2500, res = 300)
cowplot::plot_grid(p_MPPI + theme(plot.margin = unit(c(0, -0.40, 0, 0), "cm")), 
                   p_crisis + theme(plot.margin = unit(c(0, 0, 0, -0.4), "cm")), 
                   ncol = 2)
dev.off()



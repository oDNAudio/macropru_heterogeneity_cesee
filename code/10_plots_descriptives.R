rm(list=ls())

# packages
library(tidyverse)
library(cowplot)
library(reshape2)

data_folder <- "./data/"
dir.create(data_folder, showWarnings = F)

out_folder <- "./output/"
dir.create(out_folder, showWarnings = F)

plot_folder <- "./output/plots/"
dir.create(plot_folder, showWarnings = F)

countries <- c("CESEE-11", "BG", "CZ", "EE", "HU", "HR", "LT", "LV", "PL", "RO", "SI", "SK")

fix_data <- function(x){
  
  rownames(x) <- x$Time
  x <- x[ , which(grepl("*Index*", names(x)))]
  return(x)
  
}


#####
# Figure 1: Overview of MPPI

load(paste0(data_folder, "MPPI_data_all.Rda"))

MPPI_diff_plot <- fix_data(MPPI_diff_df)
MPPI_diff_plot <- MPPI_diff_plot[ , which(grepl("MaPru_Index|Prudential_Index", names(MPPI_diff_plot)))]

cesee_plot <- MPPI_diff_plot[,1:2]
names(cesee_plot) <- paste0("CE", substr(names(cesee_plot), 3, nchar(names(cesee_plot))))

for(j in 1:2){
  cesee_plot[,j] <- rowMeans(MPPI_diff_plot[ , seq(j, j + 20, 2)])
}

MPPI_diff_plot <- cbind(cesee_plot, MPPI_diff_plot)

MPPI_diff_plot <- melt(MPPI_diff_plot)
MPPI_diff_plot$ID <- substr(MPPI_diff_plot$variable, 1, 2)
MPPI_diff_plot$Date <- seq(ISOdate(1997,1,1), ISOdate(2018,12,31), by = "quarter")
MPPI_diff_plot$variable <- as.character(MPPI_diff_plot$variable)
MPPI_diff_plot$variable <- substr(MPPI_diff_plot$variable, 4, nchar(as.character(MPPI_diff_plot$variable)))

MPPI_diff_plot[which(MPPI_diff_plot$ID == "CE") , "ID"] <- "CESEE-11"
MPPI_diff_plot$ID <- factor(MPPI_diff_plot$ID, countries)
levels(MPPI_diff_plot$ID) <- c(" CESEE-11", "Bulgaria", "Czech Republic", "Estonia", "Hungary", "Croatia",
                               "Lithuania", "Latvia", "Poland", "Romania", "Slovenia", "Slovakia")
MPPI_diff_plot$ID <- as.character(MPPI_diff_plot$ID)

png(paste0(plot_folder, "Fig1-MPPI_ovw.png"), width = 5000, height = 2500, res = 300)
ggplot(data = MPPI_diff_plot[which(MPPI_diff_plot$variable == "Prudential_Index"),], aes(x = Date, y = value)) + 
  facet_wrap(ID~., nrow = 3) + 
  labs(y = "") + 
  geom_hline(yintercept = 0, lty = 2, colour= "red") +
  geom_line(size = 1, colour = "navyblue") +
  theme_bw() +
  ylab("") + xlab("") +
  theme(legend.justification = "left", legend.position = "bottom", 
        legend.box = "horizontal", legend.box.just = "left",
        legend.title = element_blank(), 
        legend.text = element_text(size = 14), 
        legend.spacing.x = unit(0.05, "cm"), 
        legend.spacing.y = unit(0.05, "cm"),
        strip.text.x = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        strip.background = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14)) +
  guides(fill = guide_legend(order = 0, nrow = 5), colour = guide_legend(order = 1)) 
dev.off()


#####
# Figure C1: Bar chart of MPPI components

load(paste0(data_folder, "MPPI_data_all.Rda"))

MPPI_diff_bar <- fix_data(MPPI_diff_df)

MPPI_diff_PPI <- MPPI_diff_bar[ , which(grepl("Prudential_Index", names(MPPI_diff_bar)))]

MPPI_diff_bar <- MPPI_diff_bar[ , -which(grepl("MaPru_Index|LB_Index|Prudential_Index|Liquidity-based_Index|Capital-based_Index", names(MPPI_diff_bar)))]
names(MPPI_diff_bar) <- str_replace(names(MPPI_diff_bar), "Borrower-based", "BB")

cesee_mean_diff <- MPPI_diff_bar[, 1:10]
names(cesee_mean_diff) <- paste0("CE", 
                                 substr(names(cesee_mean_diff), 
                                        3, 
                                        nchar(names(cesee_mean_diff))))
for(j in 1:10){
  cesee_mean_diff[, j] <- rowMeans(MPPI_diff_bar[ , seq(j, j + 100, 10)])
}

MPPI_diff_PPI$CE.Prudential_Index <- rowMeans(MPPI_diff_PPI)

MPPI_diff_bar <- cbind(cesee_mean_diff, MPPI_diff_bar)

MPPI_diff_bar <- melt(MPPI_diff_bar)
MPPI_diff_bar$ID <- substr(MPPI_diff_bar$variable, 1, 2)
MPPI_diff_bar$Date <- seq(ISOdate(1997,1,1), ISOdate(2018,12,31), by = "quarter")
MPPI_diff_bar$variable <- as.character(MPPI_diff_bar$variable)
MPPI_diff_bar$variable <- substr(MPPI_diff_bar$variable, 4, nchar(as.character(MPPI_diff_bar$variable)))

MPPI_diff_bar[which(MPPI_diff_bar$ID == "CE") , "ID"] <- "CESEE-11"

MPPI_diff_bar$ID <- factor(MPPI_diff_bar$ID, countries)
levels(MPPI_diff_bar$ID) <- c(" CESEE-11", "Bulgaria", "Czech Republic", "Estonia", "Hungary", "Croatia",
                               "Lithuania", "Latvia", "Poland", "Romania", "Slovenia", "Slovakia")
MPPI_diff_bar$ID <- as.character(MPPI_diff_bar$ID)


MPPI_diff_PPI <- melt(MPPI_diff_PPI)
MPPI_diff_PPI$ID <- substr(MPPI_diff_PPI$variable, 1, 2)
MPPI_diff_PPI$Date <- seq(ISOdate(1997,1,1), ISOdate(2018,12,31), by = "quarter")
MPPI_diff_PPI$variable <- as.character(MPPI_diff_PPI$variable)
MPPI_diff_PPI$variable <- substr(MPPI_diff_PPI$variable, 4, nchar(as.character(MPPI_diff_PPI$variable)))

MPPI_diff_PPI[which(MPPI_diff_PPI$ID == "CE") , "ID"] <- "CESEE-11"
MPPI_diff_PPI$ID <- factor(MPPI_diff_PPI$ID, countries)
levels(MPPI_diff_PPI$ID) <- c(" CESEE-11", "Bulgaria", "Czech Republic", "Estonia", "Hungary", "Croatia",
                              "Lithuania", "Latvia", "Poland", "Romania", "Slovenia", "Slovakia")
MPPI_diff_PPI$ID <- as.character(MPPI_diff_PPI$ID)

cols_indices <- c("#ed7d31", "#ffc000", "#f8cbad", "#aeaaaa", "#bf8f00", 
                  "#ffff00", "#ff5050", "#92d050", "#b482da", "#5b9bd5")
names(cols_indices) <- unique(MPPI_diff_bar$variable)
breaks <- unique(MPPI_diff_bar$variable)

png(paste0(plot_folder, "FigC1-MPPI_bar.png"), width = 4000, height = 5000, res = 300)
ggplot(data = MPPI_diff_bar, aes(x = Date, y = value)) + 
  geom_col(aes(fill = variable)) + 
  facet_wrap(ID~., nrow = 4) + 
  labs(y = "") + 
  scale_fill_manual(breaks = breaks,
                    labels = c("Capital requirements", "Reserve requirements", 
                               "Buffer requirements", "Risk weights", "Liquidity requirements", 
                               "Single client exposure limits", "Sector & market segment exposure limits",
                               "Intragroup exposure limits", "Foreign currency mismatch limits", "Borrower-based measures"),
                    values = cols_indices) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_line(data = MPPI_diff_PPI, aes(x = Date, y = value, colour = "MPPI"), size = 1) + scale_color_manual(values = c(MPPI = "black")) +
  theme_bw() +
  ylab("") + xlab("") +
  theme(legend.justification = "left", legend.position = "bottom", 
        legend.box = "horizontal", legend.box.just = "left",
        legend.title = element_blank(), 
        legend.text = element_text(size = 14), 
        legend.spacing.x = unit(0.05, "cm"), 
        legend.spacing.y = unit(0.05, "cm"),
        strip.text.x = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        strip.background = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14)) +
  guides(fill = guide_legend(order = 0, nrow = 5), colour = guide_legend(order = 1)) +
  ylim(min = -12, max = 27) 
dev.off()

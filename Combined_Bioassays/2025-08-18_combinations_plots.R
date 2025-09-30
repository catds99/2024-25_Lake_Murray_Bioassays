# 2025-08-18
# Combined bioassay plots


library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)
library(ggpubr)
library(broom)
library(agricolae)
library(grid)


############################################################ RFU @ 72 h boxplot

############# import data:

Carb_RFU = read_excel("Combined_Bioassays/2025_combinations_master.xlsx", sheet = "Carb_RFU")

Dic_RFU = read_excel("Combined_Bioassays/2025_combinations_master.xlsx", sheet = "Dic_RFU")

PFOS_RFU = read_excel("Combined_Bioassays/2025_combinations_master.xlsx", sheet = "PFOS_RFU")

Q_RFU = read_excel("Combined_Bioassays/2025_combinations_master.xlsx", sheet = "Q_RFU")

combined_RFU = read_excel("Combined_Bioassays/2025-09-23_data_for_combined_figures.xlsx", sheet = "RFU_72h")


############# rearrange and reformat data:

Carb_RFU$Group <- factor(Carb_RFU$Group,
                             levels = c("control", "solvent control", "Carblow", "Carbhigh", "Carb+Diclow", "Carb+Dichigh", "Carb+PFOSlow", "Carb+PFOShigh", "Carb+6ppdqlow", "Carb+6ppdqhigh", "nutrients only", "nutrients + solvent", "Carblownutrients", "Carbhighnutrients", "Carb+Diclownutrients", "Carb+Dichighnutrients", "Carb+PFOSlownutrients", "Carb+PFOShighnutrients", "Carb+6ppdqlownutrients", "Carb+6ppdqhighnutrients"),
                             labels = c("Control", "Solvent", "C low", "C high", "C+D low", "C+D high", "C+P low", "C+P high", "C+Q low", "C+Q high", "Nutrients", "Solvent N", "C low N", "C high N", "C+D low N", "C+D high N", "C+P low N", "C+P high N", "C+Q low N", "C+Q high N"))

Dic_RFU$Group <- factor(Dic_RFU$Group,
                            levels = c("control", "solvent control", "Diclow", "Dichigh", "Dic+Carblow", "Dic+Carbhigh", "Dic+PFOSlow", "Dic+PFOShigh", "Dic+6ppdqlow", "Dic+6ppdqhigh", "nutrients only", "nutrients + solvent", "Diclownutrients", "Dichighnutrients", "Dic+Carblownutrients", "Dic+Carbhighnutrients", "Dic+PFOSlownutrients", "Dic+PFOShighnutrients", "Dic+6ppdqlownutrients", "Dic+6ppdqhighnutrients"),
                            labels = c("Control", "Solvent", "D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "Nutrients", "Solvent N", "D low N", "D high N", "D+C low N", "D+C high N", "D+P low N", "D+P high N", "D+Q low N", "D+Q high N"))


PFOS_RFU$Group <- factor(PFOS_RFU$Group,
                         levels = c("control", "solvent control", "PFOSlow", "PFOShigh", "PFOS+Carblow", "PFOS+Carbhigh", "PFOS+Diclow", "PFOS+Dichigh", "PFOS+6ppdqlow", "PFOS+6ppdqhigh", "nutrients only", "nutrients + solvent", "PFOSlownutrients", "PFOShighnutrients", "PFOS+Carblownutrients", "PFOS+Carbhighnutrients", "PFOS+Diclownutrients", "PFOS+Dichighnutrients", "PFOS+6ppdqlownutrients", "PFOS+6ppdqhighnutrients"),
                         labels = c("Control", "Solvent", "P low", "P high", "P+C low", "P+C high", "P+D low", "P+D high", "P+Q low", "P+Q high", "Nutrients", "Solvent N", "P low N", "P high N", "P+C low N", "P+C high N", "P+D low N", "P+D high N", "P+Q low N", "P+Q high N"))

Q_RFU$Group <- factor(Q_RFU$Group,
                         levels = c("control", "solvent control", "6ppdqlow", "6ppdqhigh", "6ppdq+Carblow", "6ppdq+Carbhigh", "6ppdq+Diclow", "6ppdq+Dichigh", "6ppdq+PFOSlow", "6ppdq+PFOShigh", "nutrients only", "nutrients + solvent", "6ppdqlownutrients", "6ppdqhighnutrients", "6ppdq+Carblownutrients", "6ppdq+Carbhighnutrients", "6ppdq+Diclownutrients", "6ppdq+Dichighnutrients", "6ppdq+PFOSlownutrients", "6ppdq+PFOShighnutrients"),
                         labels = c("Control", "Solvent", "Q low", "Q high", "Q+C low", "Q+C high", "Q+D low", "Q+D high", "Q+P low", "Q+P high", "Nutrients", "Solvent N", "Q low N", "Q high N", "Q+C low N", "Q+C high N", "Q+D low N", "Q+D high N", "Q+P low N", "Q+P high N"))

combined_RFU$Group <- factor(combined_RFU$Group,
                      levels = c("control", "solvent control", "Carblow", "Carbhigh", "Carb+Diclow", "Carb+Dichigh", "Carb+PFOSlow", "Carb+PFOShigh", "Carb+6ppdqlow", "Carb+6ppdqhigh", "nutrients only", "nutrients + solvent", "Carblownutrients", "Carbhighnutrients", "Carb+Diclownutrients", "Carb+Dichighnutrients", "Carb+PFOSlownutrients", "Carb+PFOShighnutrients", "Carb+6ppdqlownutrients", "Carb+6ppdqhighnutrients", "control", "solvent control", "Diclow", "Dichigh", "Dic+Carblow", "Dic+Carbhigh", "Dic+PFOSlow", "Dic+PFOShigh", "Dic+6ppdqlow", "Dic+6ppdqhigh", "nutrients only", "nutrients + solvent", "Diclownutrients", "Dichighnutrients", "Dic+Carblownutrients", "Dic+Carbhighnutrients", "Dic+PFOSlownutrients", "Dic+PFOShighnutrients", "Dic+6ppdqlownutrients", "Dic+6ppdqhighnutrients", "control", "solvent control", "PFOSlow", "PFOShigh", "PFOS+Carblow", "PFOS+Carbhigh", "PFOS+Diclow", "PFOS+Dichigh", "PFOS+6ppdqlow", "PFOS+6ppdqhigh", "nutrients only", "nutrients + solvent", "PFOSlownutrients", "PFOShighnutrients", "PFOS+Carblownutrients", "PFOS+Carbhighnutrients", "PFOS+Diclownutrients", "PFOS+Dichighnutrients", "PFOS+6ppdqlownutrients", "PFOS+6ppdqhighnutrients", "control", "solvent control", "6ppdqlow", "6ppdqhigh", "6ppdq+Carblow", "6ppdq+Carbhigh", "6ppdq+Diclow", "6ppdq+Dichigh", "6ppdq+PFOSlow", "6ppdq+PFOShigh", "nutrients only", "nutrients + solvent", "6ppdqlownutrients", "6ppdqhighnutrients", "6ppdq+Carblownutrients", "6ppdq+Carbhighnutrients", "6ppdq+Diclownutrients", "6ppdq+Dichighnutrients", "6ppdq+PFOSlownutrients", "6ppdq+PFOShighnutrients"),
                      labels = c("Control", "Solvent", "C low", "C high", "C+D low", "C+D high", "C+P low", "C+P high", "C+Q low", "C+Q high", "Nutrients", "Solvent N", "C low N", "C high N", "C+D low N", "C+D high N", "C+P low N", "C+P high N", "C+Q low N", "C+Q high N", "Control", "Solvent", "D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "Nutrients", "Solvent N", "D low N", "D high N", "D+C low N", "D+C high N", "D+P low N", "D+P high N", "D+Q low N", "D+Q high N", "Control", "Solvent", "Q low", "Q high", "Q+C low", "Q+C high", "Q+D low", "Q+D high", "Q+P low", "Q+P high", "Nutrients", "Solvent N", "Q low N", "Q high N", "Q+C low N", "Q+C high N", "Q+D low N", "Q+D high N", "Q+P low N", "Q+P high N", "Control", "Solvent", "Q low", "Q high", "Q+C low", "Q+C high", "Q+D low", "Q+D high", "Q+P low", "Q+P high", "Nutrients", "Solvent N", "Q low N", "Q high N", "Q+C low N", "Q+C high N", "Q+D low N", "Q+D high N", "Q+P low N", "Q+P high N"))



############# figures:

summary(Dic_RFU)

Carb_box = ggplot() +
  geom_boxplot(aes(x = Group, 
                   y = `72h`),
               fill = "lightblue4",
               data = Carb_RFU) +
  xlab("Treatment") +
  ylab("Chl a Fluorescence (RFU)") +
  scale_y_continuous(limits = c(1, 12)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(Carb_box, filename = "Combined_Bioassays/Figures/end_RFU/Carb_box.png",
       device = "png", height = 7, width = 11)

Dic_box = ggplot() +
  geom_boxplot(aes(x = Group, 
                   y = `72h`),
               fill = "lightblue4",
               data = Dic_RFU) +
  xlab("Treatment") +
  ylab("Chl a Fluorescence (RFU)") +
  scale_y_continuous(limits = c(1, 12)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(Dic_box, filename = "Combined_Bioassays/Figures/end_RFU/Dic_box.png",
       device = "png", height = 7, width = 11)

PFOS_box = ggplot() +
  geom_boxplot(aes(x = Group, 
                   y = `72h`),
               fill = "lightblue4",
               data = PFOS_RFU) +
  xlab("Treatment") +
  ylab("Chl a Fluorescence (RFU)") +
  scale_y_continuous(limits = c(1, 12)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_box, filename = "Combined_Bioassays/Figures/end_RFU/PFOS_box.png",
       device = "png", height = 7, width = 11)

Q_box = ggplot() +
  geom_boxplot(aes(x = Group, 
                   y = `72h`),
               fill = "lightblue4",
               data = Q_RFU) +
  xlab("Treatment") +
  ylab("Chl a Fluorescence (RFU)") +
  scale_y_continuous(limits = c(1, 12)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(Q_box, filename = "Combined_Bioassays/Figures/end_RFU/Q_box.png",
       device = "png", height = 7, width = 11)

summary(Q_RFU)


combined_RFU_72h = ggarrange(Carb_box + rremove("ylab") + rremove("xlab"), Dic_box + rremove("ylab") + rremove("xlab"), PFOS_box + rremove("ylab") + rremove("xlab"), Q_box + rremove("ylab") + rremove("xlab"),
                            labels = c("A", "B", "C", "D"),
                            label.x = 0.09,
                            label.y = 1,
                            nrow = 2,
                            ncol = 2)



combined_RFU_72h  = annotate_figure(combined_RFU_72h, left = textGrob("Chl a Fluorescence (RFU)", rot = 90, vjust = 1, gp = gpar( cex = 1.3, fontsize = 18)),
                                   bottom = textGrob("Treatment", gp = gpar(cex = 1.3, fontsize = 18)))



############################################################ percent difference barchart

############# import data:

pd = read_excel("Combined_Bioassays/2025_combinations_master.xlsx", sheet = "all_pd")
glimpse(pd)

############# rearrange and reformat data:


pd$Carbamazepine <- factor(pd$Carbamazepine,
                              levels = c("Carblow", "Carbhigh", "Carb+Diclow", "Carb+Dichigh", "Carb+PFOSlow", "Carb+PFOShigh", "Carb+6ppdqlow", "Carb+6ppdqhigh", "Carblownutrients", "Carbhighnutrients", "Carb+Diclownutrients", "Carb+Dichighnutrients", "Carb+PFOSlownutrients", "Carb+PFOShighnutrients", "Carb+6ppdqlownutrients", "Carb+6ppdqhighnutrients"),
                              labels = c("C low", "C high", "C+D low", "C+D high", "C+P low", "C+P high", "C+Q low", "C+Q high", "C low N", "C high N", "C+D low N", "C+D high N", "C+P low N", "C+P high N", "C+Q low N", "C+Q high N"))

pd$Diclofenac <- factor(pd$Diclofenac,
                             levels = c("Diclow", "Dichigh", "Dic+Carblow", "Dic+Carbhigh", "Dic+PFOSlow", "Dic+PFOShigh", "Dic+6ppdqlow", "Dic+6ppdqhigh", "Diclownutrients", "Dichighnutrients", "Dic+Carblownutrients", "Dic+Carbhighnutrients", "Dic+PFOSlownutrients", "Dic+PFOShighnutrients", "Dic+6ppdqlownutrients", "Dic+6ppdqhighnutrients"),
                             labels = c("D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "D low N", "D high N", "D+C low N", "D+C high N", "D+P low N", "D+P high N", "D+Q low N", "D+Q high N"))

pd$PFOS <- factor(pd$PFOS,
                               levels = c("PFOSlow", "PFOShigh", "PFOS+Carblow", "PFOS+Carbhigh", "PFOS+Diclow", "PFOS+Dichigh", "PFOS+6ppdqlow", "PFOS+6ppdqhigh",  "PFOSlownutrients", "PFOShighnutrients", "PFOS+Carblownutrients", "PFOS+Carbhighnutrients", "PFOS+Diclownutrients", "PFOS+Dichighnutrients", "PFOS+6ppdqlownutrients", "PFOS+6ppdqhighnutrients"),
                               labels = c("P low", "P high", "P+C low", "P+C high", "P+D low", "P+D high", "P+Q low", "P+Q high", "P low N", "P high N", "P+C low N", "P+C high N", "P+D low N", "P+D high N", "P+Q low N", "P+Q high N"))

pd$Quinone <- factor(pd$Quinone,
                      levels = c("6ppdqlow", "6ppdqhigh", "6ppdq+Carblow", "6ppdq+Carbhigh", "6ppdq+Diclow", "6ppdq+Dichigh", "6ppdq+PFOSlow", "6ppdq+PFOShigh", "6ppdqlownutrients", "6ppdqhighnutrients", "6ppdq+Carblownutrients", "6ppdq+Carbhighnutrients", "6ppdq+Diclownutrients", "6ppdq+Dichighnutrients", "6ppdq+PFOSlownutrients", "6ppdq+PFOShighnutrients"),
                      labels = c("Q low", "Q high", "Q+C low", "Q+C high", "Q+D low", "Q+D high", "Q+P low", "Q+P high", "Q low N", "Q high N", "Q+C low N", "Q+C high N", "Q+D low N", "Q+D high N", "Q+P low N", "Q+P high N"))


############# figures:

pd_carb = ggplot() +
  geom_col(aes(x = Carbamazepine, 
               y = avg_pd_C),
           fill = "lightblue4",
           data = pd) +
  geom_errorbar(data = pd, aes(x = Carbamazepine, 
                               y = avg_pd_C,
                               ymin = avg_pd_C-sd_pd_C,
                               ymax = avg_pd_C+sd_pd_C)) +
  xlab("Treatment") +
  ylab("Percent Difference from Control") +
  theme_classic(base_size = 15) +
  scale_y_continuous(limits = c(-65, 145)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(pd_carb, filename = "Combined_Bioassays/Figures/percent_difference_control/pd_carb.png",
       device = "png", height = 7, width = 11)

pd_dic = ggplot() +
  geom_col(aes(x = Diclofenac, 
               y = avg_pd_D),
           fill = "lightblue4",
           data = pd) +
  geom_errorbar(data = pd, aes(x = Diclofenac, 
                               y = avg_pd_D,
                               ymin = avg_pd_D-sd_pd_D,
                               ymax = avg_pd_D+sd_pd_D)) +
  xlab("Treatment") +
  ylab("Percent Difference from Control") +
  theme_classic(base_size = 15) +
  scale_y_continuous(limits = c(-65, 145)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(pd_dic, filename = "Combined_Bioassays/Figures/percent_difference_control/pd_dic.png",
       device = "png", height = 7, width = 11)

pd_PFOS = ggplot() +
  geom_col(aes(x = PFOS, 
               y = avg_pd_P),
           fill = "lightblue4",
           data = pd) +
  geom_errorbar(data = pd, aes(x = PFOS, 
                                    y = avg_pd_P,
                                    ymin = avg_pd_P-sd_pd_P,
                                    ymax = avg_pd_P+sd_pd_P)) +
  xlab("Treatment") +
  ylab("Percent Difference from Control") +
  theme_classic(base_size = 15) +
  scale_y_continuous(limits = c(-65, 145)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(pd_PFOS, filename = "Combined_Bioassays/Figures/percent_difference_control/pd_PFOS.png",
       device = "png", height = 7, width = 11)

pd_Q = ggplot() +
  geom_col(aes(x = Quinone, 
               y = avg_pd_Q),
           fill = "lightblue4",
           data = pd) +
  geom_errorbar(data = pd, aes(x = Quinone, 
                               y = avg_pd_Q,
                               ymin = avg_pd_Q-sd_pd_Q,
                               ymax = avg_pd_Q+sd_pd_Q)) +
  xlab("Treatment") +
  ylab("Percent Difference from Control") +
  theme_classic(base_size = 15) +
  scale_y_continuous(limits = c(-65, 145)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(pd_Q, filename = "Combined_Bioassays/Figures/percent_difference_control/pd_Q.png",
       device = "png", height = 7, width = 11)



############################################################ delta RFU barchart

############# import data:

delta = read_excel("Combined_Bioassays/2025_combinations_master.xlsx", sheet = "all_delta")

glimpse(delta)
summary(delta)

############# rearrange and reformat data:


delta$Carbamazepine <- factor(delta$Carbamazepine,
                         levels = c("Control", "Solvent", "Carblow", "Carbhigh", "Carb+Diclow", "Carb+Dichigh", "Carb+PFOSlow", "Carb+PFOShigh", "Carb+6ppdqlow", "Carb+6ppdqhigh", "nutrients only", "nutrients + solvent", "Carblownutrients", "Carbhighnutrients", "Carb+Diclownutrients", "Carb+Dichighnutrients", "Carb+PFOSlownutrients", "Carb+PFOShighnutrients", "Carb+6ppdqlownutrients", "Carb+6ppdqhighnutrients"),
                         labels = c("Control", "Solvent", "C low", "C high", "C+D low", "C+D high", "C+P low", "C+P high", "C+Q low", "C+Q high", "Nutrients", "Solvent N", "C low N", "C high N", "C+D low N", "C+D high N", "C+P low N", "C+P high N", "C+Q low N", "C+Q high N"))

delta$Diclofenac <- factor(delta$Diclofenac,
                        levels = c("Control", "Solvent", "Diclow", "Dichigh", "Dic+Carblow", "Dic+Carbhigh", "Dic+PFOSlow", "Dic+PFOShigh", "Dic+6ppdqlow", "Dic+6ppdqhigh", "nutrients only", "nutrients + solvent", "Diclownutrients", "Dichighnutrients", "Dic+Carblownutrients", "Dic+Carbhighnutrients", "Dic+PFOSlownutrients", "Dic+PFOShighnutrients", "Dic+6ppdqlownutrients", "Dic+6ppdqhighnutrients"),
                        labels = c("Control", "Solvent", "D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "Nutrients", "Solvent N", "D low N", "D high N", "D+C low N", "D+C high N", "D+P low N", "D+P high N", "D+Q low N", "D+Q high N"))


delta$PFOS <- factor(delta$PFOS,
                         levels = c("Control", "Solvent", "PFOSlow", "PFOShigh", "PFOS+Carblow", "PFOS+Carbhigh", "PFOS+Diclow", "PFOS+Dichigh", "PFOS+6ppdqlow", "PFOS+6ppdqhigh", "nutrients only", "nutrients + solvent", "PFOSlownutrients", "PFOShighnutrients", "PFOS+Carblownutrients", "PFOS+Carbhighnutrients", "PFOS+Diclownutrients", "PFOS+Dichighnutrients", "PFOS+6ppdqlownutrients", "PFOS+6ppdqhighnutrients"),
                         labels = c("Control", "Solvent", "P low", "P high", "P+C low", "P+C high", "P+D low", "P+D high", "P+Q low", "P+Q high", "Nutrients", "Solvent N", "P low N", "P high N", "P+C low N", "P+C high N", "P+D low N", "P+D high N", "P+Q low N", "P+Q high N"))

delta$Quinone <- factor(delta$Quinone,
                      levels = c("Control", "Solvent", "6ppdqlow", "6ppdqhigh", "6ppdq+Carblow", "6ppdq+Carbhigh", "6ppdq+Diclow", "6ppdq+Dichigh", "6ppdq+PFOSlow", "6ppdq+PFOShigh", "nutrients only", "nutrients + solvent", "6ppdqlownutrients", "6ppdqhighnutrients", "6ppdq+Carblownutrients", "6ppdq+Carbhighnutrients", "6ppdq+Diclownutrients", "6ppdq+Dichighnutrients", "6ppdq+PFOSlownutrients", "6ppdq+PFOShighnutrients"),
                      labels = c("Control", "Solvent", "Q low", "Q high", "Q+C low", "Q+C high", "Q+D low", "Q+D high", "Q+P low", "Q+P high", "Nutrients", "Solvent N", "Q low N", "Q high N", "Q+C low N", "Q+C high N", "Q+D low N", "Q+D high N", "Q+P low N", "Q+P high N"))

############# Figures:


delta_carb = ggplot() +
  geom_col(aes(x = Carbamazepine, 
               y = avg_delta_C),
           fill = "lightblue4",
           data = delta) +
  geom_errorbar(data = delta, aes(x = Carbamazepine, 
                               y = avg_delta_C,
                               ymin = avg_delta_C-sd_delta_C,
                               ymax = avg_delta_C+sd_delta_C)) +
  xlab("Treatment") +
  ylab("Delta RFU (72h - T0)") +
  theme_classic(base_size = 20) +
  scale_y_continuous(limits = c(-3.5, 8.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(delta_carb, filename = "Combined_Bioassays/Figures/delta_RFU/delta_carb.png",
       device = "png", height = 7, width = 11)

delta_dic = ggplot() +
  geom_col(aes(x = Diclofenac, 
               y = avg_delta_D),
           fill = "lightblue4",
           data = delta) +
  geom_errorbar(data = delta, aes(x = Diclofenac, 
                                  y = avg_delta_D,
                                  ymin = avg_delta_D-sd_delta_D,
                                  ymax = avg_delta_D+sd_delta_D)) +
  xlab("Treatment") +
  ylab("Delta RFU (72h - T0)") +
  theme_classic(base_size = 20) +
  scale_y_continuous(limits = c(-3.5, 8.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(delta_dic, filename = "Combined_Bioassays/Figures/delta_RFU/delta_dic.png",
       device = "png", height = 7, width = 11)


delta_PFOS = ggplot() +
  geom_col(aes(x = PFOS, 
               y = avg_delta_P),
           fill = "lightblue4",
           data = delta) +
  geom_errorbar(data = delta, aes(x = PFOS, 
                                  y = avg_delta_P,
                                  ymin = avg_delta_P-sd_delta_P,
                                  ymax = avg_delta_P+sd_delta_P)) +
  xlab("Treatment") +
  ylab("Delta RFU (72h - T0)") +
  theme_classic(base_size = 20) +
  scale_y_continuous(limits = c(-3.5, 8.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(delta_PFOS, filename = "Combined_Bioassays/Figures/delta_RFU/delta_PFOS.png",
       device = "png", height = 7, width = 11)


delta_Q = ggplot() +
  geom_col(aes(x = Quinone, 
               y = avg_delta_Q),
           fill = "lightblue4",
           data = delta) +
  geom_errorbar(data = delta, aes(x = Quinone, 
                                  y = avg_delta_Q,
                                  ymin = avg_delta_Q-sd_delta_Q,
                                  ymax = avg_delta_Q+sd_delta_Q)) +
  xlab("Treatment") +
  ylab("Delta RFU (72h - T0)") +
  theme_classic(base_size = 20) +
  scale_y_continuous(limits = c(-3.5, 8.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(delta_Q, filename = "Combined_Bioassays/Figures/delta_RFU/delta_Q.png",
       device = "png", height = 7, width = 11)


combined_delta = ggarrange(delta_carb + rremove("ylab") + rremove("xlab"), delta_dic + rremove("ylab") + rremove("xlab"), delta_PFOS + rremove("ylab") + rremove("xlab"), delta_Q + rremove("ylab") + rremove("xlab"),
                             labels = c("A", "B", "C", "D"),
                             label.x = 0.05,
                             label.y = 1,
                             nrow = 2,
                             ncol = 2)



combined_delta  = annotate_figure(combined_delta, left = textGrob(label = expression(Delta * "Chl a Fluorescence (RFU)"), rot = 90, vjust = 1, gp = gpar( cex = 1.3, fontsize = 18)),
                                    bottom = textGrob("Treatment", gp = gpar(cex = 1.3, fontsize = 18)))


############################################################ percent difference barchart based on delta RFU

############# import data:

pd_delta = read_excel("Combined_Bioassays/2025_combinations_master.xlsx", sheet = "all_pd_delta")
glimpse(pd)

############# rearrange and reformat data:


pd_delta$Carbamazepine <- factor(pd_delta$Carbamazepine,
                           levels = c("Carblow", "Carbhigh", "Carb+Diclow", "Carb+Dichigh", "Carb+PFOSlow", "Carb+PFOShigh", "Carb+6ppdqlow", "Carb+6ppdqhigh", "Carblownutrients", "Carbhighnutrients", "Carb+Diclownutrients", "Carb+Dichighnutrients", "Carb+PFOSlownutrients", "Carb+PFOShighnutrients", "Carb+6ppdqlownutrients", "Carb+6ppdqhighnutrients"),
                           labels = c("C low", "C high", "C+D low", "C+D high", "C+P low", "C+P high", "C+Q low", "C+Q high", "C low N", "C high N", "C+D low N", "C+D high N", "C+P low N", "C+P high N", "C+Q low N", "C+Q high N"))

pd_delta$Diclofenac <- factor(pd_delta$Diclofenac,
                        levels = c("Diclow", "Dichigh", "Dic+Carblow", "Dic+Carbhigh", "Dic+PFOSlow", "Dic+PFOShigh", "Dic+6ppdqlow", "Dic+6ppdqhigh", "Diclownutrients", "Dichighnutrients", "Dic+Carblownutrients", "Dic+Carbhighnutrients", "Dic+PFOSlownutrients", "Dic+PFOShighnutrients", "Dic+6ppdqlownutrients", "Dic+6ppdqhighnutrients"),
                        labels = c("D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "D low N", "D high N", "D+C low N", "D+C high N", "D+P low N", "D+P high N", "D+Q low N", "D+Q high N"))

pd_delta$PFOS <- factor(pd_delta$PFOS,
                  levels = c("PFOSlow", "PFOShigh", "PFOS+Carblow", "PFOS+Carbhigh", "PFOS+Diclow", "PFOS+Dichigh", "PFOS+6ppdqlow", "PFOS+6ppdqhigh",  "PFOSlownutrients", "PFOShighnutrients", "PFOS+Carblownutrients", "PFOS+Carbhighnutrients", "PFOS+Diclownutrients", "PFOS+Dichighnutrients", "PFOS+6ppdqlownutrients", "PFOS+6ppdqhighnutrients"),
                  labels = c("P low", "P high", "P+C low", "P+C high", "P+D low", "P+D high", "P+Q low", "P+Q high", "P low N", "P high N", "P+C low N", "P+C high N", "P+D low N", "P+D high N", "P+Q low N", "P+Q high N"))

pd_delta$Quinone <- factor(pd_delta$Quinone,
                     levels = c("6ppdqlow", "6ppdqhigh", "6ppdq+Carblow", "6ppdq+Carbhigh", "6ppdq+Diclow", "6ppdq+Dichigh", "6ppdq+PFOSlow", "6ppdq+PFOShigh", "6ppdqlownutrients", "6ppdqhighnutrients", "6ppdq+Carblownutrients", "6ppdq+Carbhighnutrients", "6ppdq+Diclownutrients", "6ppdq+Dichighnutrients", "6ppdq+PFOSlownutrients", "6ppdq+PFOShighnutrients"),
                     labels = c("Q low", "Q high", "Q+C low", "Q+C high", "Q+D low", "Q+D high", "Q+P low", "Q+P high", "Q low N", "Q high N", "Q+C low N", "Q+C high N", "Q+D low N", "Q+D high N", "Q+P low N", "Q+P high N"))


############# figures:

pd_delta_carb = ggplot() +
  geom_col(aes(x = Carbamazepine, 
               y = avg_pd_C),
           fill = "lightblue4",
           data = pd_delta) +
  geom_errorbar(data = pd_delta, aes(x = Carbamazepine, 
                               y = avg_pd_C,
                               ymin = avg_pd_C-sd_pd_C,
                               ymax = avg_pd_C+sd_pd_C)) +
  xlab("Treatment") +
  ylab("Percent Difference from Control") +
  theme_classic(base_size = 15) +
  scale_y_continuous(limits = c(-400, 300)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(pd_delta_carb, filename = "Combined_Bioassays/Figures/percent_difference_control_delta/pd_delta_carb.png",
       device = "png", height = 7, width = 11)

pd_delta_dic = ggplot() +
  geom_col(aes(x = Diclofenac, 
               y = avg_pd_D),
           fill = "lightblue4",
           data = pd_delta) +
  geom_errorbar(data = pd_delta, aes(x = Diclofenac, 
                               y = avg_pd_D,
                               ymin = avg_pd_D-sd_pd_D,
                               ymax = avg_pd_D+sd_pd_D)) +
  xlab("Treatment") +
  ylab("Percent Difference from Control") +
  theme_classic(base_size = 15) +
  scale_y_continuous(limits = c(-400, 300)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(pd_delta_dic, filename = "Combined_Bioassays/Figures/percent_difference_control_delta/pd_delta_dic.png",
       device = "png", height = 7, width = 11)

pd_delta_PFOS = ggplot() +
  geom_col(aes(x = PFOS, 
               y = avg_pd_P),
           fill = "lightblue4",
           data = pd_delta) +
  geom_errorbar(data = pd_delta, aes(x = PFOS, 
                               y = avg_pd_P,
                               ymin = avg_pd_P-sd_pd_P,
                               ymax = avg_pd_P+sd_pd_P)) +
  xlab("Treatment") +
  ylab("Percent Difference from Control") +
  theme_classic(base_size = 15) +
  scale_y_continuous(limits = c(-1000, 1000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(pd_delta_PFOS, filename = "Combined_Bioassays/Figures/percent_difference_control_delta/pd_delta_PFOS.png",
       device = "png", height = 7, width = 11)

pd_delta_Q = ggplot() +
  geom_col(aes(x = Quinone, 
               y = avg_pd_Q),
           fill = "lightblue4",
           data = pd_delta) +
  geom_errorbar(data = pd_delta, aes(x = Quinone, 
                               y = avg_pd_Q,
                               ymin = avg_pd_Q-sd_pd_Q,
                               ymax = avg_pd_Q+sd_pd_Q)) +
  xlab("Treatment") +
  ylab("Percent Difference from Control") +
  theme_classic(base_size = 15) +
  scale_y_continuous(limits = c(-3000, 1000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(pd_delta_Q, filename = "Combined_Bioassays/Figures/percent_difference_control_delta/pd_delta_Q.png",
       device = "png", height = 7, width = 11)

############################################################ change in RFU line graph


############# import data:

Carb_RFU = read_excel("Combined_Bioassays/2025_combinations_master.xlsx", sheet = "Carb_RFU")

Dic_RFU = read_excel("Combined_Bioassays/2025_combinations_master.xlsx", sheet = "Dic_RFU")

PFOS_RFU = read_excel("Combined_Bioassays/2025_combinations_master.xlsx", sheet = "PFOS_RFU")

Q_RFU = read_excel("Combined_Bioassays/2025_combinations_master.xlsx", sheet = "Q_RFU")


############# mean and st dev:

Carb_change = Carb_RFU %>%
  group_by(Group) %>%
  summarize(mean_T0 = mean(T0, na.rm = TRUE),
            mean_24h = mean(`24h (closer to 27)`, na.rm = TRUE),
            mean_48h = mean(`48h`, na.rm = TRUE),
            mean_72h = mean(`72h`, na.rm = TRUE),
            sd_T0 = sd(T0, na.rm = TRUE),
            sd_24h = sd(`24h (closer to 27)`, na.rm = TRUE),
            sd_48h = sd(`48h`, na.rm = TRUE),
            sd_72h = sd(`72h`, na.rm = TRUE))

Dic_change = Dic_RFU %>%
  group_by(Group) %>%
  summarize(mean_T0 = mean(T0, na.rm = TRUE),
            mean_24h = mean(`24h`, na.rm = TRUE),
            mean_48h = mean(`48h`, na.rm = TRUE),
            mean_72h = mean(`72h`, na.rm = TRUE),
            sd_T0 = sd(T0, na.rm = TRUE),
            sd_24h = sd(`24h`, na.rm = TRUE),
            sd_48h = sd(`48h`, na.rm = TRUE),
            sd_72h = sd(`72h`, na.rm = TRUE))

PFOS_change = PFOS_RFU %>%
  group_by(Group) %>%
  summarize(mean_T0 = mean(T0, na.rm = TRUE),
            mean_24h = mean(`24h`, na.rm = TRUE),
            mean_48h = mean(`48h`, na.rm = TRUE),
            mean_72h = mean(`72h`, na.rm = TRUE),
            sd_T0 = sd(T0, na.rm = TRUE),
            sd_24h = sd(`24h`, na.rm = TRUE),
            sd_48h = sd(`48h`, na.rm = TRUE),
            sd_72h = sd(`72h`, na.rm = TRUE))

Q_change = Q_RFU %>%
  group_by(Group) %>%
  summarize(mean_T0 = mean(T0, na.rm = TRUE),
            mean_24h = mean(`24h`, na.rm = TRUE),
            mean_48h = mean(`48h`, na.rm = TRUE),
            mean_72h = mean(`72h`, na.rm = TRUE),
            sd_T0 = sd(T0, na.rm = TRUE),
            sd_24h = sd(`24h`, na.rm = TRUE),
            sd_48h = sd(`48h`, na.rm = TRUE),
            sd_72h = sd(`72h`, na.rm = TRUE))

############# rearrange and reformat data:

Dic_change_2 <- Dic_change %>%
  pivot_longer(
    cols = -Group,
    names_to = c(".value", "Time"),
    names_pattern = "(mean|sd)_(.*)"
  )

Dic_change_2$Time <- factor(Dic_change_2$Time, levels = c("T0", "24h", "48h", "72h"))

Dic_change_2$Group <- factor(Dic_change_2$Group,
                             levels = c("control", "solvent control", "Diclow", "Dichigh", "Dic+Carblow", "Dic+Carbhigh", "Dic+PFOSlow", "Dic+PFOShigh", "Dic+6ppdqlow", "Dic+6ppdqhigh", "nutrients only", "nutrients + solvent", "Diclownutrients", "Dichighnutrients", "Dic+Carblownutrients", "Dic+Carbhighnutrients", "Dic+PFOSlownutrients", "Dic+PFOShighnutrients", "Dic+6ppdqlownutrients", "Dic+6ppdqhighnutrients"),
                             labels = c("Control", "Solvent", "D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "Nutrients", "Solvent N", "D low N", "D high N", "D+C low N", "D+C high N", "D+P low N", "D+P high N", "D+Q low N", "D+Q high N"))


Carb_change_2 <- Carb_change %>%
  pivot_longer(
    cols = -Group,
    names_to = c(".value", "Time"),
    names_pattern = "(mean|sd)_(.*)"
  )

Carb_change_2$Time <- factor(Carb_change_2$Time, levels = c("T0", "24h", "48h", "72h"))

Carb_change_2$Group <- factor(Carb_change_2$Group,
                              levels = c("control", "solvent control", "Carblow", "Carbhigh", "Carb+Diclow", "Carb+Dichigh", "Carb+PFOSlow", "Carb+PFOShigh", "Carb+6ppdqlow", "Carb+6ppdqhigh", "nutrients only", "nutrients + solvent", "Carblownutrients", "Carbhighnutrients", "Carb+Diclownutrients", "Carb+Dichighnutrients", "Carb+PFOSlownutrients", "Carb+PFOShighnutrients", "Carb+6ppdqlownutrients", "Carb+6ppdqhighnutrients"),
                              labels = c("Control", "Solvent Control", "C low", "C high", "C+D low", "C+D high", "C+P low", "C+P high", "C+Q low", "C+Q high", "Nutrients", "Solvent N", "C low N", "C high N", "C+D low N", "C+D high N", "C+P low N", "C+P high N", "C+Q low N", "C+Q high N"))



PFOS_change_2 <- PFOS_change %>%
  pivot_longer(
    cols = -Group,
    names_to = c(".value", "Time"),
    names_pattern = "(mean|sd)_(.*)"
  )

PFOS_change_2$Time <- factor(PFOS_change_2$Time, levels = c("T0", "24h", "48h", "72h"))

PFOS_change_2$Group <- factor(PFOS_change_2$Group,
                              levels = c("control", "solvent control", "PFOSlow", "PFOShigh", "PFOS+Carblow", "PFOS+Carbhigh", "PFOS+Diclow", "PFOS+Dichigh", "PFOS+6ppdqlow", "PFOS+6ppdqhigh", "nutrients only", "nutrients + solvent", "PFOSlownutrients", "PFOShighnutrients", "PFOS+Carblownutrients", "PFOS+Carbhighnutrients", "PFOS+Diclownutrients", "PFOS+Dichighnutrients", "PFOS+6ppdqlownutrients", "PFOS+6ppdqhighnutrients"),
                              labels = c("Control", "Solvent", "P low", "P high", "P+C low", "P+C high", "P+D low", "P+D high", "P+Q low", "P+Q high", "Nutrients", "Solvent N", "P low N", "P high N", "P+C low N", "P+C high N", "P+D low N", "P+D high N", "P+Q low N", "P+Q high N"))

Q_change_2 <- Q_change %>%
  pivot_longer(
    cols = -Group,
    names_to = c(".value", "Time"),
    names_pattern = "(mean|sd)_(.*)"
  )

Q_change_2$Time <- factor(Q_change_2$Time, levels = c("T0", "24h", "48h", "72h"))

Q_change_2$Group <- factor(Q_change_2$Group,
                              levels = c("control", "solvent control", "6ppdqlow", "6ppdqhigh", "6ppdq+Carblow", "6ppdq+Carbhigh", "6ppdq+Diclow", "6ppdq+Dichigh", "6ppdq+PFOSlow", "6ppdq+PFOShigh", "nutrients only", "nutrients + solvent", "6ppdqlownutrients", "6ppdqhighnutrients", "6ppdq+Carblownutrients", "6ppdq+Carbhighnutrients", "6ppdq+Diclownutrients", "6ppdq+Dichighnutrients", "6ppdq+PFOSlownutrients", "6ppdq+PFOShighnutrients"),
                              labels = c("Control", "Solvent", "Q low", "Q high", "Q+C low", "Q+C high", "Q+D low", "Q+D high", "Q+P low", "Q+P high", "Nutrients", "Solvent N", "Q low N", "Q high N", "Q+C low N", "Q+C high N", "Q+D low N", "Q+D high N", "Q+P low N", "Q+P high N"))



############# figures:

###Diclofenac

Dic_change_over_time = ggplot(Dic_change_2, aes(x = Time, y = mean, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  labs(
    y = "Mean RFU ± SD",
    x = "Time"
  ) +
  guides(color = guide_legend(ncol = 2)) +
  theme(legend.position = "bottom") +
  theme_classic(base_size = 20)
ggsave(Dic_change_over_time, filename = "Combined_Bioassays/Figures/Change_over_time/Dic_change_over_time.png",
       device = "png", height = 7, width = 11)

###Carbamazepine

Carb_change_over_time = ggplot(Carb_change_2, aes(x = Time, y = mean, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  labs(
    y = "Mean RFU ± SD",
    x = "Time"
  ) +
  guides(color = guide_legend(ncol = 2)) +
  theme(legend.position = "bottom") +
  theme_classic(base_size = 20)
ggsave(Carb_change_over_time, filename = "Combined_Bioassays/Figures/Change_over_time/Carb_change_over_time.png",
       device = "png", height = 7, width = 11)


###PFOS

PFOS_change_over_time = ggplot(PFOS_change_2, aes(x = Time, y = mean, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  labs(
    y = "Mean RFU ± SD",
    x = "Time"
  ) +
  guides(color = guide_legend(ncol = 2)) +
  theme(legend.position = "bottom") +
  theme_classic(base_size = 20)
ggsave(PFOS_change_over_time, filename = "Combined_Bioassays/Figures/Change_over_time/PFOS_change_over_time.png",
       device = "png", height = 7, width = 11)


###6ppdq

Q_change_over_time = ggplot(Q_change_2, aes(x = Time, y = mean, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  labs(
    y = "Mean RFU ± SD",
    x = "Time"
  ) +
  guides(color = guide_legend(ncol = 2)) +
  theme(legend.position = "bottom") +
  theme_classic(base_size = 20)
ggsave(Q_change_over_time, filename = "Combined_Bioassays/Figures/Change_over_time/Q_change_over_time.png",
       device = "png", height = 7, width = 11)



combined_time = ggarrange(Carb_change_over_time + rremove("ylab") + rremove("xlab"), Dic_change_over_time + rremove("ylab") + rremove("xlab"), PFOS_change_over_time + rremove("ylab") + rremove("xlab"), Q_change_over_time + rremove("ylab") + rremove("xlab"),
                           labels = c("A", "B", "C", "D"),
                           label.x = 0.04,
                           label.y = 0.99,
                           nrow = 2,
                           ncol = 2)



combined_time  = annotate_figure(combined_time, left = textGrob("Chl a Fluorescence (RFU)", rot = 90, vjust = 1, gp = gpar( cex = 1.3, fontsize = 18)),
                                  bottom = textGrob("Time (h)", gp = gpar(cex = 1.3, fontsize = 18)))






############################################################ FvFm @ 72 h boxplot


############# import data:

all_pam = read_excel("Combined_Bioassays/2025_combinations_master.xlsx", sheet = "all_PAM")

glimpse(all_pam)

############# rearrange and reformat data:


all_pam$Carbamazepine <- factor(all_pam$Carbamazepine,
                                levels = c("control", "solvent control", "Carblow", "Carbhigh", "Carb+Diclow", "Carb+Dichigh", "Carb+PFOSlow", "Carb+PFOShigh", "Carb+6ppdqlow", "Carb+6ppdqhigh", "nutrient control", "nutrient control (with solvent)", "Carblownutrients", "Carbhighnutrients", "Carb+Diclownutrients", "Carb+Dichighnutrients", "Carb+PFOSlownutrients", "Carb+PFOShighnutrients", "Carb+6ppdqlownutrients", "Carb+6ppdqhighnutrients"),
                                labels = c("Control", "Solvent Control", "C low", "C high", "C+D low", "C+D high", "C+P low", "C+P high", "C+Q low", "C+Q high", "Nutrients", "Solvent N", "C low N", "C high N", "C+D low N", "C+D high N", "C+P low N", "C+P high N", "C+Q low N", "C+Q high N"))

all_pam$Diclofenac <- factor(all_pam$Diclofenac,
                             levels = c("control", "solvent control", "Diclow", "Dichigh", "Dic+Carblow", "Dic+Carbhigh", "Dic+PFOSlow", "Dic+PFOShigh", "Dic+6ppdqlow", "Dic+6ppdqhigh", "nutrient control", "nutrient control (with solvent)", "Diclownutrients", "Dichighnutrients", "Dic+Carblownutrients", "Dic+Carbhighnutrients", "Dic+PFOSlownutrients", "Dic+PFOShighnutrients", "Dic+6ppdqlownutrients", "Dic+6ppdqhighnutrients"),
                             labels = c("Control", "Solvent", "D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "Nutrients", "Solvent N", "D low N", "D high N", "D+C low N", "D+C high N", "D+P low N", "D+P high N", "D+Q low N", "D+Q high N"))


all_pam$PFOS <- factor(all_pam$PFOS,
                       levels = c("control", "solvent control", "PFOSlow", "PFOShigh", "PFOS+Carblow", "PFOS+Carbhigh", "PFOS+Diclow", "PFOS+Dichigh", "PFOS+6ppdqlow", "PFOS+6ppdqhigh", "nutrient control", "nutrient control (with solvent)", "PFOSlownutrients", "PFOShighnutrients", "PFOS+Carblownutrients", "PFOS+Carbhighnutrients", "PFOS+Diclownutrients", "PFOS+Dichighnutrients", "PFOS+6ppdqlownutrients", "PFOS+6ppdqhighnutrients"),
                       labels = c("Control", "Solvent", "P low", "P high", "P+C low", "P+C high", "P+D low", "P+D high", "P+Q low", "P+Q high", "Nutrients", "Solvent N", "P low N", "P high N", "P+C low N", "P+C high N", "P+D low N", "P+D high N", "P+Q low N", "P+Q high N"))

all_pam$Quinone <- factor(all_pam$Quinone,
                           levels = c("control", "solvent control", "6ppdqlow", "6ppdqhigh", "6ppdq+Carblow", "6ppdq+Carbhigh", "6ppdq+Diclow", "6ppdq+Dichigh", "6ppdq+PFOSlow", "6ppdq+PFOShigh", "nutrients only", "nutrients + solvent", "6ppdqlownutrients", "6ppdqhighnutrients", "6ppdq+Carblownutrients", "6ppdq+Carbhighnutrients", "6ppdq+Diclownutrients", "6ppdq+Dichighnutrients", "6ppdq+PFOSlownutrients", "6ppdq+PFOShighnutrients"),
                           labels = c("Control", "Solvent", "Q low", "Q high", "Q+C low", "Q+C high", "Q+D low", "Q+D high", "Q+P low", "Q+P high", "Nutrients", "Solvent N", "Q low N", "Q high N", "Q+C low N", "Q+C high N", "Q+D low N", "Q+D high N", "Q+P low N", "Q+P high N"))

############# figures:

carb_pam = ggplot() +
  geom_boxplot(aes(x = Carbamazepine, 
                   y = FvFm_C),
               fill = "lightblue4",
               data = all_pam) +
  xlab("Treatment") +
  ylab("Fv/Fm") +
  scale_y_continuous(breaks = seq(0.25, 0.7, by = 0.05), limits = c(0.25,0.7)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(carb_pam, filename = "Combined_Bioassays/Figures/FvFm/carb_pam.png",
       device = "png", height = 7, width = 11)


dic_pam = ggplot() +
  geom_boxplot(aes(x = Diclofenac, 
                   y = FvFm_D),
               fill = "lightblue4",
               data = all_pam) +
  xlab("Treatment") +
  ylab("Fv/Fm") +
  scale_y_continuous(breaks = seq(0.25, 0.7, by = 0.05), limits = c(0.25,0.7)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(dic_pam, filename = "Combined_Bioassays/Figures/FvFm/dic_pam.png",
       device = "png", height = 7, width = 11)


PFOS_pam = ggplot() +
  geom_boxplot(aes(x = PFOS, 
                   y = FvFm_P),
               fill = "lightblue4",
               data = all_pam) +
  xlab("Treatment") +
  ylab("Fv/Fm") +
  scale_y_continuous(breaks = seq(0.25, 0.7, by = 0.05), limits = c(0.25,0.7)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "Combined_Bioassays/Figures/FvFm/PFOS_pam.png",
       device = "png", height = 7, width = 11)

Q_pam = ggplot() +
  geom_boxplot(aes(x = Quinone, 
                   y = FvFm_Q),
               fill = "lightblue4",
               data = all_pam) +
  xlab("Treatment") +
  ylab("Fv/Fm") +
  scale_y_continuous(breaks = seq(0.25, 0.7, by = 0.05), limits = c(0.25,0.7)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(Q_pam, filename = "Combined_Bioassays/Figures/FvFm/Q_pam.png",
       device = "png", height = 7, width = 11)


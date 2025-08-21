# 2025-07-28 
# fv/fm plots for carb and dic bioassays

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

#import data:

Carb_RFU = read_excel("2025_combinations_master.xlsx", sheet = "Carb_RFU")

Dic_RFU = read_excel("2025_combinations_master.xlsx", sheet = "Dic_RFU")

PFOS_RFU = read_excel("2025_combinations_master.xlsx", sheet = "PFOS_RFU")


glimpse(Carb_RFU)
glimpse(Dic_RFU)

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

Dic_change_over_time = ggplot(Dic_change_2, aes(x = Time, y = mean, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  labs(
    title = "Change Over Time by Group, Diclofenac",
    y = "Mean RFU ± SD",
    x = "Time"
  ) +
  theme(legend.position = "bottom") +
  theme_classic(base_size = 15)
ggsave(Dic_change_over_time, filename = "Dic_change_over_time.png",
       device = "png", height = 7, width = 11)






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

Carb_change_over_time = ggplot(Carb_change_2, aes(x = Time, y = mean, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  labs(
    title = "Change Over Time by Group, Carbamazepine",
    y = "Mean RFU ± SD",
    x = "Time"
  ) +
  theme(legend.position = "bottom") +
  theme_classic(base_size = 15)
ggsave(Carb_change_over_time, filename = "Carb_change_over_time.png",
       device = "png", height = 7, width = 11)





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

PFOS_change_over_time = ggplot(PFOS_change_2, aes(x = Time, y = mean, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  labs(
    title = "Change Over Time by Group, PFOS",
    y = "Mean RFU ± SD",
    x = "Time"
  ) +
  theme(legend.position = "bottom") +
  theme_classic(base_size = 15)
ggsave(PFOS_change_over_time, filename = "PFOS_change_over_time.png",
       device = "png", height = 7, width = 11)

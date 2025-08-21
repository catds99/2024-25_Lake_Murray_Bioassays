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

all_pam = read_excel("2025_combinations_master.xlsx", sheet = "all_PAM")

glimpse(all_pam)



all_pam$Carbamazepine <- factor(all_pam$Carbamazepine,
                               levels = c("control", "solvent control", "Carblow", "Carbhigh", "Carb+Diclow", "Carb+Dichigh", "Carb+PFOSlow", "Carb+PFOShigh", "Carb+6ppdqlow", "Carb+6ppdqhigh", "nutrient control", "nutrient control (with solvent)", "Carblownutrients", "Carbhighnutrients", "Carb+Diclownutrients", "Carb+Dichighnutrients", "Carb+PFOSlownutrients", "Carb+PFOShighnutrients", "Carb+6ppdqlownutrients", "Carb+6ppdqhighnutrients"),
                               labels = c("Control", "Solvent Control", "C low", "C high", "C+D low", "C+D high", "C+P low", "C+P high", "C+Q low", "C+Q high", "Nutrients", "Solvent N", "C low N", "C high N", "C+D low N", "C+D high N", "C+P low N", "C+P high N", "C+Q low N", "C+Q high N"))

all_pam$Diclofenac <- factor(all_pam$Diclofenac,
                                levels = c("control", "solvent control", "Diclow", "Dichigh", "Dic+Carblow", "Dic+Carbhigh", "Dic+PFOSlow", "Dic+PFOShigh", "Dic+6ppdqlow", "Dic+6ppdqhigh", "nutrient control", "nutrient control (with solvent)", "Diclownutrients", "Dichighnutrients", "Dic+Carblownutrients", "Dic+Carbhighnutrients", "Dic+PFOSlownutrients", "Dic+PFOShighnutrients", "Dic+6ppdqlownutrients", "Dic+6ppdqhighnutrients"),
                                labels = c("Control", "Solvent", "D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "Nutrients", "Solvent N", "D low N", "D high N", "D+C low N", "D+C high N", "D+P low N", "D+P high N", "D+Q low N", "D+Q high N"))


all_pam$PFOS <- factor(all_pam$PFOS,
                             levels = c("control", "solvent control", "PFOSlow", "PFOShigh", "PFOS+Carblow", "PFOS+Carbhigh", "PFOS+Diclow", "PFOS+Dichigh", "PFOS+6ppdqlow", "PFOS+6ppdqhigh", "nutrient control", "nutrient control (with solvent)", "PFOSlownutrients", "PFOShighnutrients", "PFOS+Carblownutrients", "PFOS+Carbhighnutrients", "PFOS+Diclownutrients", "PFOS+Dichighnutrients", "PFOS+6ppdqlownutrients", "PFOS+6ppdqhighnutrients"),
                             labels = c("Control", "Solvent", "P low", "P high", "P+C low", "P+C high", "P+D low", "P+D high", "P+Q low", "P+Q high", "Nutrients", "Solvent N", "P low N", "P high N", "P+C low N", "P+C high N", "P+D low N", "P+D high N", "P+Q low N", "P+Q high N"))



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
ggsave(carb_pam, filename = "carb_pam.png",
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
ggsave(dic_pam, filename = "dic_pam.png",
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
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)

summary(all_pam)

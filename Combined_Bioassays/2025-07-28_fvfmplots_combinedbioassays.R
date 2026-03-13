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

all_FvFm = read_excel("2025_combinations_master.xlsx", sheet = "all_FvFm")

glimpse(all_FvFm)


all_pam_C = all_pam %>%
  filter(Bioassay == "Carb")

glimpse (all_pam_C)

all_pam_C$Group


all_pam_C$Group <- factor(all_pam_C$Group,
                               levels = c("control", "solvent control", "Carblow", "Carbhigh", "Carb+Diclow", "Carb+Dichigh", "Carb+PFOSlow", "Carb+PFOShigh", "Carb+6ppdqlow", "Carb+6ppdqhigh", "nutrient control", "nutrient control (with solvent)", "Carblownutrients", "Carbhighnutrients", "Carb+Diclownutrients", "Carb+Dichighnutrients", "Carb+PFOSlownutrients", "Carb+PFOShighnutrients", "Carb+6ppdqlownutrients", "Carb+6ppdqhighnutrients"),
                               labels = c("Control", "Solvent Control", "C low", "C high", "C+D low", "C+D high", "C+P low", "C+P high", "C+Q low", "C+Q high", "Nutrients", "Solvent N", "C low N", "C high N", "C+D low N", "C+D high N", "C+P low N", "C+P high N", "C+Q low N", "C+Q high N"))

all_FvFm$Carbamazepine <- factor(all_FvFm$Carbamazepine,
                                levels = c("control", "solvent control", "Carblow", "Carbhigh", "Carb+Diclow", "Carb+Dichigh", "Carb+PFOSlow", "Carb+PFOShigh", "Carb+6ppdqlow", "Carb+6ppdqhigh", "nutrient control", "nutrient control (with solvent)", "Carblownutrients", "Carbhighnutrients", "Carb+Diclownutrients", "Carb+Dichighnutrients", "Carb+PFOSlownutrients", "Carb+PFOShighnutrients", "Carb+6ppdqlownutrients", "Carb+6ppdqhighnutrients"),
                                labels = c("Control", "Solvent Control", "C low", "C high", "C+D low", "C+D high", "C+P low", "C+P high", "C+Q low", "C+Q high", "Nutrients", "Solvent N", "C low N", "C high N", "C+D low N", "C+D high N", "C+P low N", "C+P high N", "C+Q low N", "C+Q high N"))


all_pam_D = all_pam %>%
  filter(Bioassay == "Dic")

glimpse (all_pam_D)

all_pam_D$Group


all_pam_D$Group <- factor(all_pam_D$Group,
                                levels = c("control", "solvent control", "Diclow", "Dichigh", "Dic+Carblow", "Dic+Carbhigh", "Dic+PFOSlow", "Dic+PFOShigh", "Dic+6ppdqlow", "Dic+6ppdqhigh", "nutrient control", "nutrient control (with solvent)", "Diclownutrients", "Dichighnutrients", "Dic+Carblownutrients", "Dic+Carbhighnutrients", "Dic+PFOSlownutrients", "Dic+PFOShighnutrients", "Dic+6ppdqlownutrients", "Dic+6ppdqhighnutrients"),
                                labels = c("Control", "Solvent", "D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "Nutrients", "Solvent N", "D low N", "D high N", "D+C low N", "D+C high N", "D+P low N", "D+P high N", "D+Q low N", "D+Q high N"))

all_FvFm$Diclofenac <- factor(all_FvFm$Diclofenac,
                             levels = c("control", "solvent control", "Diclow", "Dichigh", "Dic+Carblow", "Dic+Carbhigh", "Dic+PFOSlow", "Dic+PFOShigh", "Dic+6ppdqlow", "Dic+6ppdqhigh", "nutrient control", "nutrient control (with solvent)", "Diclownutrients", "Dichighnutrients", "Dic+Carblownutrients", "Dic+Carbhighnutrients", "Dic+PFOSlownutrients", "Dic+PFOShighnutrients", "Dic+6ppdqlownutrients", "Dic+6ppdqhighnutrients"),
                             labels = c("Control", "Solvent", "D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "Nutrients", "Solvent N", "D low N", "D high N", "D+C low N", "D+C high N", "D+P low N", "D+P high N", "D+Q low N", "D+Q high N"))


all_pam_P = all_pam %>%
  filter(Bioassay == "PFOS")

glimpse (all_pam_P)

all_pam_P$Group



all_pam_P$Group <- factor(all_pam_P$Group,
                             levels = c("control", "solvent control", "PFOSlow", "PFOShigh", "PFOS+Carblow", "PFOS+Carbhigh", "PFOS+Diclow", "PFOS+Dichigh", "PFOS+6ppdqlow", "PFOS+6ppdqhigh", "nutrient control", "nutrient control (with solvent)", "PFOSlownutrients", "PFOShighnutrients", "PFOS+Carblownutrients", "PFOS+Carbhighnutrients", "PFOS+Diclownutrients", "PFOS+Dichighnutrients", "PFOS+6ppdqlownutrients", "PFOS+6ppdqhighnutrients"),
                             labels = c("Control", "Solvent", "P low", "P high", "P+C low", "P+C high", "P+D low", "P+D high", "P+Q low", "P+Q high", "Nutrients", "Solvent N", "P low N", "P high N", "P+C low N", "P+C high N", "P+D low N", "P+D high N", "P+Q low N", "P+Q high N"))

all_FvFm$PFOS <- factor(all_FvFm$PFOS,
                       levels = c("control", "solvent control", "PFOSlow", "PFOShigh", "PFOS+Carblow", "PFOS+Carbhigh", "PFOS+Diclow", "PFOS+Dichigh", "PFOS+6ppdqlow", "PFOS+6ppdqhigh", "nutrient control", "nutrient control (with solvent)", "PFOSlownutrients", "PFOShighnutrients", "PFOS+Carblownutrients", "PFOS+Carbhighnutrients", "PFOS+Diclownutrients", "PFOS+Dichighnutrients", "PFOS+6ppdqlownutrients", "PFOS+6ppdqhighnutrients"),
                       labels = c("Control", "Solvent", "P low", "P high", "P+C low", "P+C high", "P+D low", "P+D high", "P+Q low", "P+Q high", "Nutrients", "Solvent N", "P low N", "P high N", "P+C low N", "P+C high N", "P+D low N", "P+D high N", "P+Q low N", "P+Q high N"))


all_FvFm$Quinone <- factor(all_FvFm$Quinone,
                      levels = c("control", "solvent control", "6ppdqlow", "6ppdqhigh", "6ppdq+Carblow", "6ppdq+Carbhigh", "6ppdq+Diclow", "6ppdq+Dichigh", "6ppdq+PFOSlow", "6ppdq+PFOShigh", "nutrients only", "nutrients + solvent", "6ppdqlownutrients", "6ppdqhighnutrients", "6ppdq+Carblownutrients", "6ppdq+Carbhighnutrients", "6ppdq+Diclownutrients", "6ppdq+Dichighnutrients", "6ppdq+PFOSlownutrients", "6ppdq+PFOShighnutrients"),
                      labels = c("Control", "Solvent", "Q low", "Q high", "Q+C low", "Q+C high", "Q+D low", "Q+D high", "Q+P low", "Q+P high", "Nutrients", "Solvent N", "Q low N", "Q high N", "Q+C low N", "Q+C high N", "Q+D low N", "Q+D high N", "Q+P low N", "Q+P high N"))

all_pam_Q = all_pam %>%
  filter(Bioassay == "Quin")

glimpse (all_pam_Q)

all_pam_Q$Group

all_pam_Q$Group <- factor(all_pam_Q$Group,
                           levels = c("control", "solvent control", "6ppdqlow", "6ppdqhigh", "6ppdq+Carblow", "6ppdq+Carbhigh", "6ppdq+Diclow", "6ppdq+Dichigh", "6ppdq+PFOSlow", "6ppdq+PFOShigh", "nutrient control", "nutrient control (with solvent)", "6ppdqlownutrients", "6ppdqhighnutrients", "6ppdq+Carblownutrients", "6ppdq+Carbhighnutrients", "6ppdq+Diclownutrients", "6ppdq+Dichighnutrients", "6ppdq+PFOSlownutrients", "6ppdq+PFOShighnutrients"),
                           labels = c("Control", "Solvent", "Q low", "Q high", "Q+C low", "Q+C high", "Q+D low", "Q+D high", "Q+P low", "Q+P high", "Nutrients", "Solvent N", "Q low N", "Q high N", "Q+C low N", "Q+C high N", "Q+D low N", "Q+D high N", "Q+P low N", "Q+P high N"))


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


carb_fvfm = ggplot() +
  geom_boxplot(aes(x = Carbamazepine, 
                   y = FvFm_C),
               fill = "lightblue4",
               data = all_FvFm) +
  xlab("Treatment") +
  ylab("Fv/Fm") +
  ggtitle("Carbamazepine Fv/Fm") +
  scale_y_continuous(breaks = seq(0.25, 0.7, by = 0.05), limits = c(0.25,0.7)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(carb_pam, filename = "carb_pam.png",
       device = "png", height = 7, width = 11)

############## Alpha

Carb_alpha = ggplot() +
  geom_point(aes(x = Group, 
                 y = Alpha...11),
             fill = "lightblue4",
             data = all_pam_C) +
  ggtitle("Carbamazepine: Alpha, Platt et al 1980") +
  xlab("Group") +
  ylab("Alpha") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)

Carb_alpha_2 = ggplot() +
  geom_point(aes(x = Group, 
                 y = Alpha...16),
             fill = "lightblue4",
             data = all_pam_C) +
  ggtitle("Carbamazepine: Alpha, Jassby and Platt 1980") +
  xlab("Group") +
  ylab("Alpha") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############## ETR max


Carb_etr = ggplot() +
  geom_point(aes(x = Group, 
                 y = ETRmax),
             fill = "lightblue4",
             data = all_pam_C) +
  ggtitle("Carbamazepine: ETRmax, Platt et al 1980") +
  xlab("Group") +
  ylab("ETRmax") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)


Carb_etr_2 = ggplot() +
  geom_point(aes(x = Group, 
                 y = ETRMax),
             fill = "lightblue4",
             data = all_pam_C) +
  ggtitle("Carbamazepine: ETRmax, Jassby and Platt 1980") +
  xlab("Group") +
  ylab("ETRmax") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############## Ik


Carb_Ik = ggplot() +
  geom_point(aes(x = Group, 
                 y = Ik...13),
             fill = "lightblue4",
             data = all_pam_C) +
  ggtitle("Carbamazepine: Ik, Platt et al 1980") +
  xlab("Group") +
  ylab("Ik") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)


Carb_Ik_2 = ggplot() +
  geom_point(aes(x = Group, 
                 y = Ik...18),
             fill = "lightblue4",
             data = all_pam_C) +
  ggtitle("Carbamazepine: Ik, Jassby and Platt 1980") +
  xlab("Group") +
  ylab("Ik") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############## beta

Carb_beta = ggplot() +
  geom_point(aes(x = Group, 
                 y = beta),
             fill = "lightblue4",
             data = all_pam_C) +
  ggtitle("Carbamazepine: beta, Platt et al 1980") +
  xlab("Group") +
  ylab("beta") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)

############## ETR m pot


Carb_ETRmPot = ggplot() +
  geom_point(aes(x = Group, 
                 y = Ik...13),
             fill = "lightblue4",
             data = all_pam_C) +
  ggtitle("Carbamazepine: ETRmPot, Platt et al 1980") +
  xlab("Group") +
  ylab("ETRmPot") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)















################################################################ Dic

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

dic_fvfm = ggplot() +
  geom_boxplot(aes(x = Diclofenac, 
                   y = FvFm_D),
               fill = "lightblue4",
               data = all_FvFm) +
  xlab("Treatment") +
  ylab("Fv/Fm") +
  ggtitle("Diclofenac Fv/Fm") +
  scale_y_continuous(breaks = seq(0.25, 0.7, by = 0.05), limits = c(0.25,0.7)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(dic_pam, filename = "dic_pam.png",
       device = "png", height = 7, width = 11)



############## Alpha

Dic_alpha = ggplot() +
  geom_point(aes(x = Group, 
                 y = Alpha...11),
             fill = "lightblue4",
             data = all_pam_D) +
  ggtitle("Diclofenac: Alpha, Platt et al 1980") +
  xlab("Group") +
  ylab("Alpha") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)

Dic_alpha_2 = ggplot() +
  geom_point(aes(x = Group, 
                 y = Alpha...16),
             fill = "lightblue4",
             data = all_pam_D) +
  ggtitle("Diclofenac: Alpha, Jassby and Platt 1980") +
  xlab("Group") +
  ylab("Alpha") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############## ETR max


Dic_etr = ggplot() +
  geom_point(aes(x = Group, 
                 y = ETRmax),
             fill = "lightblue4",
             data = all_pam_D) +
  ggtitle("Diclofenac: ETRmax, Platt et al 1980") +
  xlab("Group") +
  ylab("ETRmax") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)


Dic_etr_2 = ggplot() +
  geom_point(aes(x = Group, 
                 y = ETRMax),
             fill = "lightblue4",
             data = all_pam_D) +
  ggtitle("Diclofenac: ETRmax, Jassby and Platt 1980") +
  xlab("Group") +
  ylab("ETRmax") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############## Ik


Dic_Ik = ggplot() +
  geom_point(aes(x = Group, 
                 y = Ik...13),
             fill = "lightblue4",
             data = all_pam_D) +
  ggtitle("Diclofenac: Ik, Platt et al 1980") +
  xlab("Group") +
  ylab("Ik") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)


Dic_Ik_2 = ggplot() +
  geom_point(aes(x = Group, 
                 y = Ik...18),
             fill = "lightblue4",
             data = all_pam_D) +
  ggtitle("Diclofenac: Ik, Jassby and Platt 1980") +
  xlab("Group") +
  ylab("Ik") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############## beta

Dic_beta = ggplot() +
  geom_point(aes(x = Group, 
                 y = beta),
             fill = "lightblue4",
             data = all_pam_D) +
  ggtitle("Diclofenac: beta, Platt et al 1980") +
  xlab("Group") +
  ylab("beta") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)

############## ETR m pot


Dic_ETRmPot = ggplot() +
  geom_point(aes(x = Group, 
                 y = Ik...13),
             fill = "lightblue4",
             data = all_pam_D) +
  ggtitle("Diclofenac: ETRmPot, Platt et al 1980") +
  xlab("Group") +
  ylab("ETRmPot") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)
















##################################################################### PFOS

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

PFOS_FvFm = ggplot() +
  geom_boxplot(aes(x = PFOS, 
                   y = FvFm_P),
               fill = "lightblue4",
               data = all_FvFm) +
  ggtitle("PFOS Fv/Fm") +
  xlab("Treatment") +
  ylab("Fv/Fm") +
  scale_y_continuous(breaks = seq(0.25, 0.7, by = 0.05), limits = c(0.25,0.7)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)



############## Alpha

PFOS_alpha = ggplot() +
  geom_point(aes(x = Group, 
                 y = Alpha...11),
             fill = "lightblue4",
             data = all_pam_P) +
  ggtitle("PFOS: Alpha, Platt et al 1980") +
  xlab("Group") +
  ylab("Alpha") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)

PFOS_alpha_2 = ggplot() +
  geom_point(aes(x = Group, 
                 y = Alpha...16),
             fill = "lightblue4",
             data = all_pam_P) +
  ggtitle("PFOS: Alpha, Jassby and Platt 1980") +
  xlab("Group") +
  ylab("Alpha") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############## ETR max


PFOS_etr = ggplot() +
  geom_point(aes(x = Group, 
                 y = ETRmax),
             fill = "lightblue4",
             data = all_pam_P) +
  ggtitle("PFOS: ETRmax, Platt et al 1980") +
  xlab("Group") +
  ylab("ETRmax") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)


PFOS_etr_2 = ggplot() +
  geom_point(aes(x = Group, 
                 y = ETRMax),
             fill = "lightblue4",
             data = all_pam_P) +
  ggtitle("PFOS: ETRmax, Jassby and Platt 1980") +
  xlab("Group") +
  ylab("ETRmax") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############## Ik


PFOS_Ik = ggplot() +
  geom_point(aes(x = Group, 
                 y = Ik...13),
             fill = "lightblue4",
             data = all_pam_P) +
  ggtitle("PFOS: Ik, Platt et al 1980") +
  xlab("Group") +
  ylab("Ik") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)


PFOS_Ik_2 = ggplot() +
  geom_point(aes(x = Group, 
                 y = Ik...18),
             fill = "lightblue4",
             data = all_pam_P) +
  ggtitle("PFOS: Ik, Jassby and Platt 1980") +
  xlab("Group") +
  ylab("Ik") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############## beta

PFOS_beta = ggplot() +
  geom_point(aes(x = Group, 
                 y = beta),
             fill = "lightblue4",
             data = all_pam_P) +
  ggtitle("PFOS: beta, Platt et al 1980") +
  xlab("Group") +
  ylab("beta") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)

############## ETR m pot


PFOS_ETRmPot = ggplot() +
  geom_point(aes(x = Group, 
                 y = Ik...13),
             fill = "lightblue4",
             data = all_pam_P) +
  ggtitle("PFOS: ETRmPot, Platt et al 1980") +
  xlab("Group") +
  ylab("ETRmPot") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)











########################################################## 6ppdq


############## Fv/Fm

Quin_FvFm = ggplot() +
  geom_boxplot(aes(x = Quinone, 
                   y = FvFm_Q),
               fill = "lightblue4",
               data = all_FvFm) +
  xlab("Treatment") +
  ylab("Fv/Fm") +
  ggtitle("6ppdq Fv/Fm") + 
  scale_y_continuous(breaks = seq(0.25, 0.7, by = 0.05), limits = c(0.25,0.7)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)



############## Alpha


Quin_alpha = ggplot() +
  geom_point(aes(x = Group, 
                   y = Alpha...11),
               fill = "lightblue4",
               data = all_pam_Q) +
  ggtitle("6ppdq: Alpha, Platt et al 1980") +
  xlab("Group") +
  ylab("Alpha") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)


Quin_alpha_2 = ggplot() +
  geom_point(aes(x = Group, 
                 y = Alpha...16),
             fill = "lightblue4",
             data = all_pam_Q) +
  ggtitle("6ppdq: Alpha, Jassby and Platt 1980") +
  xlab("Group") +
  ylab("Alpha") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############## ETR max


Quin_etr = ggplot() +
  geom_point(aes(x = Group, 
                 y = ETRmax),
             fill = "lightblue4",
             data = all_pam_Q) +
  ggtitle("6ppdq: ETRmax, Platt et al 1980") +
  xlab("Group") +
  ylab("ETRmax") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)


Quin_etr_2 = ggplot() +
  geom_point(aes(x = Group, 
                 y = ETRMax),
             fill = "lightblue4",
             data = all_pam_Q) +
  ggtitle("6ppdq: ETRmax, Jassby and Platt 1980") +
  xlab("Group") +
  ylab("ETRmax") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

############## Ik


Quin_Ik = ggplot() +
  geom_point(aes(x = Group, 
                 y = Ik...13),
             fill = "lightblue4",
             data = all_pam_Q) +
  ggtitle("6ppdq: Ik, Platt et al 1980") +
  xlab("Group") +
  ylab("Ik") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)


Quin_Ik_2 = ggplot() +
  geom_point(aes(x = Group, 
                 y = Ik...18),
             fill = "lightblue4",
             data = all_pam_Q) +
  ggtitle("6ppdq: Ik, Jassby and Platt 1980") +
  xlab("Group") +
  ylab("Ik") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############## beta

Quin_beta = ggplot() +
  geom_point(aes(x = Group, 
                 y = beta),
             fill = "lightblue4",
             data = all_pam_Q) +
  ggtitle("6ppdq: beta, Platt et al 1980") +
  xlab("Group") +
  ylab("beta") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)

############## ETR m pot


Quin_ETRmPot = ggplot() +
  geom_point(aes(x = Group, 
                 y = Ik...13),
             fill = "lightblue4",
             data = all_pam_Q) +
  ggtitle("6ppdq: ETRmPot, Platt et al 1980") +
  xlab("Group") +
  ylab("ETRmPot") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_pam, filename = "PFOS_pam.png",
       device = "png", height = 7, width = 11)




summary(all_pam)

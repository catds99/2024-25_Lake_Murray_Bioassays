#2024-09-09
#CDS
#Total Chl a data from qppd-q Bioassay 1 (2024/08)

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

Q_B1_TCA = read_excel("6ppd-q/Data/HPLC_6.xlsx", sheet = 1)

summary(Q_B1_TCA)
glimpse(Q_B1_TCA)

############################################## Biomass

#################### Boxplot to visualize

Q_B1_TCA$Sample <- factor(Q_B1_TCA$Sample,
                             levels = c("T0", "Control", "Methanol", "ten", "twentyfive", "fifty", "seventyfive", "hundred", "hundredtwentyfive"),
                             labels = c("Time Zero", "Control", "Methanol Control", "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))

QB1_TCA_boxplot = ggplot() +
  geom_boxplot(aes(x = Sample, 
                   y = Total_Chl_a),
               fill = "darkgrey",
               data = Q_B1_TCA) +
  xlab("Treatment") +
  ylab(expression(paste("Total Chl ", italic(a)))) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(QB1_TCA_boxplot, filename = "6ppd-q/Figures/QB1_TCA_boxplot.png",
       device = "png", height = 7, width = 11)

#################### means

treatment_group_mean_tca = Q_B1_TCA %>%
  group_by(Sample) %>%
  summarise(mean = mean(Total_Chl_a))
print(treatment_group_mean_tca)

# Time Zero         5.88
#2 Control          12.7 
#3 Methanol Control 18.0 
#4 10% Max          13.4 
#5 25% Max          11.0 
#6 50% Max          10.3 
#7 75% Max          12.3 
#8 100% Max         11.6 
#9 125% Max          9.55


tca.aov = aov(Total_Chl_a ~ Sample, data = Q_B1_TCA)

summary(tca.aov)

REGW_tca = REGW.test(y = tca.aov, "Sample", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW_tca)

#10% Max > T0
#100% Max < Methanol Control
#125% Max < Methanol 
#25% Max < Methanol 
#50% Max < Methanol 
#75% Max < Methanol 
#75% Max > T0
#Methanol Control > Control
#Methanol Control > Time Zero
#Control > Time Zero

############################################## Percent Change from control

#################### boxplot to visualize

PFOS_B1_TCA$Sample <- factor(PFOS_B1_TCA$Sample,
                             levels = c("T0", "Control", "Methanol", "ten", "twentyfive", "fifty", "seventyfive", "hundred", "hundredtwentyfive"),
                             labels = c("Time Zero", "Control", "Methanol Control", "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))

PB1_PC_boxplot = ggplot() +
  geom_boxplot(aes(x = Sample, 
                   y = percent_change),
               fill = "darkgrey",
               data = PFOS_B1_TCA) +
  xlab("Treatment") +
  ylab(expression(paste("Percent Change in Total Chl ", italic(a)))) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PB1_PC_boxplot, filename = "PFOS/Figures/PB1_PC_boxplot.png",
       device = "png", height = 7, width = 11)

#################### means

treatment_group_mean_pc = PFOS_B1_TCA %>%
  group_by(Sample) %>%
  summarise(mean = mean(percent_change))
print(treatment_group_mean_pc)

#1 Time Zero        -63.1
#2 Control            0  
#3 Methanol Control  20.0
#4 10% Max          -23.4
#5 25% Max          -21.4
#6 50% Max          -18.6
#7 75% Max          -21.3
#8 100% Max         -18.6
#9 125% Max         -30.6

#################### barchart

Carb_B1_TCA$Sample <- factor(Carb_B1_TCA$Sample,
                             levels = c("T0", "Control", "Acetone", "ten", "twentyfive", "fifty", "seventyfive", "hundred", "hundredtwentyfive"),
                             labels = c("Time Zero", "Control", "Acetone Control", "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))

PB1_PC_plot = ggplot() +
  geom_col(aes(x = Sample, 
               y = mean),
           fill = "darkgrey",
           data = treatment_group_mean_pc) +
  xlab("Treatment") +
  ylab(expression(paste("Percent Change in Total Chl ", italic(a)))) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PB1_PC_plot, filename = "PFOS/Figures/PB1_PC_plot.png",
       device = "png", height = 7, width = 11)

############### one-way anova percent change

tca.aov.2 = aov(percent_change ~ Sample, data = PFOS_B1_TCA)

summary(tca.aov.2)

REGW_tca_pc = REGW.test(y = tca.aov.2, "Sample", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW_tca_pc)

#Methanol Control > Time Zero
#Control > Time Zero

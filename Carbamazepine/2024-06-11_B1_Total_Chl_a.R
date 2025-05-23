#2024-06-11
#CDS
#Total Chl a data from Carbamazepine Bioassay 1 (2024/05/22-25)

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

Carb_B1_TCA = read_excel("Carbamazepine/Data/HPLC_C.xlsx", sheet = 1)

summary(Carb_B1_TCA)
glimpse(Carb_B1_TCA)

############################################## Biomass

#################### Boxplot to visualize

# set order of levels and rename:

Carb_B1_TCA$Sample <- factor(Carb_B1_TCA$Sample,
                             levels = c("T0", "Control", "Acetone", "ten", "twentyfive", "fifty", "seventyfive", "hundred", "hundredtwentyfive"),
                             labels = c("Time Zero", "Control", "Acetone Control", "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))

# boxplot:

B1_TCA_boxplot = ggplot() +
  geom_boxplot(aes(x = Sample, 
                   y = Total_Chl_a),
               fill = "darkgrey",
               data = Carb_B1_TCA) +
  xlab("Treatment") +
  ylab(expression(paste("Total Chl ", italic(a)))) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(B1_TCA_boxplot, filename = "Carbamazepine/Figures/B1_TCA_boxplot.png",
       device = "png", height = 7, width = 11)

#################### means

treatment_group_mean_tca = Carb_B1_TCA %>%
  group_by(Sample) %>%
  summarise(mean = mean(Total_Chl_a))
print(treatment_group_mean_tca)

#1 Acetone           14.5 
#2 Control           12.9 
#3 T0                 6.55
#4 fifty             17.7 
#5 hundred           17.1 
#6 hundredtwentyfive 16.7 
#7 seventyfive       17.0 
#8 ten               15.6 
#9 twentyfive        18.3  

#################### ANOVA

tca.aov = aov(Total_Chl_a ~ Sample, data = Carb_B1_TCA)

summary(tca.aov)

REGW_tca = REGW.test(y = tca.aov, "Sample", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW_tca)

#Acetone Control > Time Zero
#50% Max > Control
#25% Max > Control
#Control > Time Zero
#50% Max > T0
#100% Max > T0
#125% Max > T0
#75% Max > T0
#10% Max > T0
#25% Max > T0

############################################## Percent Change from control

#################### boxplot to visualize

Carb_B1_TCA$Sample <- factor(Carb_B1_TCA$Sample,
                             levels = c("T0", "Control", "Acetone", "ten", "twentyfive", "fifty", "seventyfive", "hundred", "hundredtwentyfive"),
                             labels = c("Time Zero", "Control", "Acetone Control", "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))

B1_PC_boxplot = ggplot() +
  geom_boxplot(aes(x = Sample, 
                   y = percent_change),
               fill = "darkgrey",
               data = Carb_B1_TCA) +
  xlab("Treatment") +
  ylab(expression(paste("Percent Change in Total Chl ", italic(a)))) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(B1_PC_boxplot, filename = "Carbamazepine/Figures/B1_PC_boxplot.png",
       device = "png", height = 7, width = 11)

#################### means

treatment_group_mean_pc = Carb_B1_TCA %>%
  group_by(Sample) %>%
  summarise(mean = mean(percent_change))
print(treatment_group_mean_pc)

#################### barchart

Carb_B1_TCA$Sample <- factor(Carb_B1_TCA$Sample,
                             levels = c("T0", "Control", "Acetone", "ten", "twentyfive", "fifty", "seventyfive", "hundred", "hundredtwentyfive"),
                             labels = c("Time Zero", "Control", "Acetone Control", "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))

B1_PC_plot = ggplot() +
  geom_col(aes(x = Sample, 
               y = mean),
               fill = "darkgrey",
               data = treatment_group_mean_pc) +
  xlab("Treatment") +
  ylab(expression(paste("Percent Change in Total Chl ", italic(a)))) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(B1_PC_plot, filename = "Carbamazepine/Figures/B1_PC_plot.png",
       device = "png", height = 7, width = 11)

############### one-way anova percent change

tca.aov.2 = aov(percent_change ~ Sample, data = Carb_B1_TCA)

summary(tca.aov.2)

REGW_tca_pc = REGW.test(y = tca.aov.2, "Sample", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW_tca_pc)
#10% max > Time Zero
#100% max > Time Zero
#125% max > Time Zero
#25% max > Control
#25% max > Time Zero
#50% max > Time Zero
#75% max > Time Zero
#Acetone Control > Time Zero
#Control > Time Zero


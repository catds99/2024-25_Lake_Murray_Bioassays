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

Carb_B1_TCA = read_excel("Carbamazepine/Data/HPLC.xlsx", sheet = 1)

summary(Carb_B1_TCA)
glimpse(Carb_B1_TCA)

############################################## Biomass

#################### Boxplot to visualize

Carb_B1_TCA$Sample <- factor(Carb_B1_TCA$Sample,
                                levels = c("T0", "Control", "Acetone", "10", "25", "50", "75", "100", "125"),
                                labels = c("Time Zero", "Control", "Acetone Control", "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))

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

############################################## Percent Change from control

#################### boxplot to visualize

Carb_B1_TCA$Sample <- factor(Carb_B1_TCA$Sample,
                             levels = c("T0", "Control", "Acetone", "10", "25", "50", "75", "100", "125"),
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
                             levels = c("T0", "Control", "Acetone", "10", "25", "50", "75", "100", "125"),
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
#50% max > Control
#50% max > Time Zero
#75% max > Time Zero
#Acetone Control > Time Zero
#Control > Time Zero


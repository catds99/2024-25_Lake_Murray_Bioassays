#2024-08-26
#CDS
#PAM data from 6ppd-q Bioassay 1 (2024/08/21-24)

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

Q_B1_PAM = read_excel("6ppd-q/Data/PAM_6.xlsx", sheet = 1)

summary(Q_B1_PAM)
glimpse(Q_B1_PAM)

#################### Boxplot to visualize

Q_B1_PAM$Treatment <- factor(Q_B1_PAM$Treatment,
                               levels = c("T0", "Control", "Methanol", "ten", "twentyfive", "fifty", "seventyfive", "hundred", "hundredtwentyfive"),
                               labels = c("Time Zero", "Control", "Methanol Control", "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))

Q_B1_PAM_boxplot = ggplot() +
  geom_boxplot(aes(x = Treatment, 
                   y = FvFm),
               fill = "darkgrey",
               data = Q_B1_PAM) +
  xlab("Treatment") +
  ylab(expression(F[v]/F[m])) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(Q_B1_PAM_boxplot, filename = "6ppd-q/Figures/Q_B1_PAM_boxplot.png",
       device = "png", height = 7, width = 11)

#################### ANOVA

#### One-way ANOVA, parametric

B1.aov = aov(FvFm ~ Treatment, data = Q_B1_PAM)

summary(B1.aov)

# F = 46,89, p = <2e-16, df = 8, 36


REGW = REGW.test(y = B1.aov, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW)
#Methanol Control, 10% max, 25% Max, 50% Max, 75% Max, 100% Max, 125% Max > Control
#Methanol Control > 100% Max
#Control, Methanol Control, 10% max, 25% Max, 50% Max, 75% Max, 100% Max, 125% Max  > Time Zero




install.packages("onewaytests")
library(onewaytests)


welch_test <- welch.test(FvFm ~ Treatment, data = Dic_B1_PAM)
print(welch_test)

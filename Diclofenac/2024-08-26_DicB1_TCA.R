#2024-06-17
#CDS
#PAM data from Diclofenac Bioassay 1 (2024/06/12-15)

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

Dic_B1_PAM = read_excel("Diclofenac/Data/PAM_D.xlsx", sheet = 1)

summary(Dic_B1_PAM)
glimpse(Dic_B1_PAM)

#################### Boxplot to visualize

Dic_B1_PAM$Treatment <- factor(Dic_B1_PAM$Treatment,
                               levels = c("T0", "Control", "Acetone", "ten", "twentyfive", "fifty", "seventyfive", "hundred", "hundredtwentyfive"),
                               labels = c("Time Zero", "Control", "Acetone Control", "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))

B1_PAM_boxplot = ggplot() +
  geom_boxplot(aes(x = Treatment, 
                   y = FvFm),
               fill = "darkgrey",
               data = Dic_B1_PAM) +
  xlab("Treatment") +
  ylab(expression(F[v]/F[m])) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(B1_PAM_boxplot, filename = "Diclofenac/Figures/B1_PAM_boxplot.png",
       device = "png", height = 7, width = 11)

#################### ANOVA

#### One-way ANOVA, parametric

B1.aov = aov(FvFm ~ Treatment, data = Dic_B1_PAM)

summary(B1.aov)

# F = 377.3, p = <2e-16, df = 8, 36


REGW = REGW.test(y = B1.aov, "Treatment", alpha = 0.05, group = TRUE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW)
#10% max > Time Zero
#100% max > Time Zero
#125% max > Time Zero
#25% max > Time Zero
#50% max > Time Zero
#75% max > Time Zero
#Acetone Control > Time Zero
#Control > Time Zero
#100% max > 10% max, acetone control, control
#125% max > control, acetone control
#75% max > control, acetone control

#                  FvFm groups
#100% Max        0.6720      a
#125% Max        0.6702     ab
#75% Max         0.6694     ab
#25% Max         0.6628    abc
#50% Max         0.6618    abc
#10% Max         0.6576     bc
#Acetone Control 0.6544      c
#Control         0.6502      c
#Time Zero       0.4804      d


install.packages("onewaytests")
library(onewaytests)


welch_test <- welch.test(FvFm ~ Treatment, data = Dic_B1_PAM)
print(welch_test)

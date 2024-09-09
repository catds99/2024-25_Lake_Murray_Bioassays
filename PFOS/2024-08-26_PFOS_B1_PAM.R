#2024-08-26
#CDS
#PAM data from PFOS Bioassay 1 (2024/0)

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

PFOS_B1_PAM = read_excel("PFOS/Data/PAM_P.xlsx", sheet = 1)

summary(PFOS_B1_PAM)
glimpse(PFOS_B1_PAM)

#################### Boxplot to visualize

PFOS_B1_PAM$Treatment <- factor(PFOS_B1_PAM$Treatment,
                             levels = c("T0", "Control", "Methanol", "ten", "twentyfive", "fifty", "seventyfive", "hundred", "hundredtwentyfive"),
                             labels = c("Time Zero", "Control", "Methanol Control", "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))

PFOS_B1_PAM_boxplot = ggplot() +
  geom_boxplot(aes(x = Treatment, 
                   y = FvFm),
               fill = "darkgrey",
               data = PFOS_B1_PAM) +
  xlab("Treatment") +
  ylab(expression(F[v]/F[m])) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(PFOS_B1_PAM_boxplot, filename = "PFOS/Figures/PFOS_B1_PAM_boxplot.png",
       device = "png", height = 7, width = 11)

#################### ANOVA

#### One-way ANOVA, parametric

B1.aov = aov(FvFm ~ Treatment, data = PFOS_B1_PAM)

summary(B1.aov)

# F = 101, p = <2e-16, df = 8, 36


REGW = REGW.test(y = B1.aov, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW)
#Methanol Control, 10% max, 25% Max, 50% Max, 75% Max, 100% Max, 125% Max > Control
#Control, Methanol Control, 10% max, 25% Max, 50% Max, 75% Max, 100% Max, 125% Max  > Time Zero
#25% Max > 10% Max




install.packages("onewaytests")
library(onewaytests)


welch_test <- welch.test(FvFm ~ Treatment, data = Dic_B1_PAM)
print(welch_test)

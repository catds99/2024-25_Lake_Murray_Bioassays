#2024-05-28
#CDS
#PAM data from Carbamazepine Bioassay 1 (2024/05/22-25)

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)
library(ggpubr)
library(broom)

#import data:

Carb_B1_PAM = read_excel("Carbamazepine/Data/PAM.xlsx", sheet = 1)

summary(Carb_B1_PAM)
glimpse(Carb_B1_PAM)

#################### Boxplot to visualize

Carb_B1_PAM$Treatment <- factor(Carb_B1_PAM$Treatment,
                         levels = c("T_0", "Control", "A_Control", "ten", "twentyfive", "fifty", "seventyfive", "hundred", "hundredtwentyfive"),
                         labels = c("Time Zero", "Control", "Acetone Control", "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))

B1_PAM_boxplot = ggplot() +
  geom_boxplot(aes(x = Treatment, 
                   y = FvFm),
               fill = "darkgrey",
               data = Carb_B1_PAM) +
  xlab("Treatment") +
  ylab(expression(F[v]/F[m])) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(B1_PAM_boxplot, filename = "Carbamazepine/Figures/B1_PAM_boxplot.png",
       device = "png", height = 7, width = 11)

#################### ANOVA

#### One-way ANOVA, parametric

B1.aov = aov(FvFm ~ Treatment, data = Carb_B1_PAM)

summary(B1.aov)

# No difference between treatment groups, F = 0.65, p = 0.712, df = 7

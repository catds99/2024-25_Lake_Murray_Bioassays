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

getwd()

#import data:

Carb_B1_PAM = read_excel("Carbamazepine/Data/PAM_C.xlsx", sheet = 1)

summary(Carb_B1_PAM)
glimpse(Carb_B1_PAM)

#################### Boxplot to visualize

Carb_B1_PAM$Treatment <- factor(Carb_B1_PAM$Treatment,
                                levels = c("T0", "Control", "Acetone", "ten", "twentyfive", "fifty", "seventyfive", "hundred", "hundredtwentyfive"),
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


REGW = REGW.test(y = B1.aov, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
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

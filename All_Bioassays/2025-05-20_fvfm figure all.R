#2025-05-20 
#Fv/Fm figure

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)

#import data:

master = read_excel("All_Bioassays/FvFm_all.xlsx", sheet = 1)

glimpse(master)

master$Treatment <- factor(master$Treatment,
                           levels = c("T0", "Control", "Solvent", "ten", "twentyfive", "fifty", "seventyfive", "hundred", "hundredtwentyfive"),
                           labels = c("Time Zero", "Control", "Solvent Control", "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))

master$Bioassay <- factor(master$Bioassay,
                          levels = c("Carbamazepine", "Diclofenac", "PFOS", "Quinone"),
                          labels = c("Carbamazepine", "Diclofenac", "PFOS", "6ppd-Quinone"))


#################### Create the graph:


fvfm_boxplot_square = ggplot(data = summary, aes(x = Treatment, y = FvFm), fill = "lightcyan3") +
  geom_boxplot(fill = "lightcyan3") +
  facet_wrap(~Bioassay, ncol = 2) +
  xlab("Treatment") +
  ylab(expression(F[V]/F[M])) +
  theme_classic(base_size = 35) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1.07))


ggsave(fvfm_boxplot_square, filename = "All_Bioassays/Figures/fvfm_boxplot_square.png",
       device = "png", height = 18, width = 24)

#2024-10-03
#combined boxplots for poster

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)

#import data:

master = read_excel("Mastersheet.xlsx", sheet = 1)

glimpse(master)

master$Treatment <- factor(master$Treatment,
                     levels = c("T0", "Control", "Solvent", "ten", "twentyfive", "fifty", "seventyfive", "hundred", "hundredtwentyfive"),
                     labels = c("Time Zero", "Control", "Solvent Control", "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))

master$Bioassay <- factor(master$Bioassay,
                           levels = c("Carbamazepine", "Diclofenac", "PFOS", "Quinone"),
                           labels = c("Carbamazepine", "Diclofenac", "PFOS", "6ppd-Quinone"))

summary = master %>%
  group_by(Bioassay, Treatment)

#################### Create the graph:

tca_boxplot = ggplot(data = summary, aes(x = Treatment, y = TCA)) +
  geom_boxplot(fill = "darkred") +
  facet_wrap(~Bioassay, ncol = 1) +
  xlab("Treatment") +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) +
  theme_classic(base_size = 35) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1.07))


ggsave(tca_boxplot, filename = "tca_boxplot.png",
       device = "png", height = 25, width = 10)


fvfm_boxplot = ggplot(data = summary, aes(x = Treatment, y = FvFm), fill = "darkred") +
  geom_boxplot(fill = "darkred") +
  facet_wrap(~Bioassay, ncol = 1) +
  xlab("Treatment") +
  ylab(expression(F[V]/F[M])) +
  theme_classic(base_size = 35) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1.07))


ggsave(fvfm_boxplot, filename = "fvfm_boxplot.png",
       device = "png", height = 25, width = 10)

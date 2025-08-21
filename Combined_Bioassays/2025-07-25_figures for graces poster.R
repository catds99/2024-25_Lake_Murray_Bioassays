

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

bargraphs = read_excel("Bioassay poster graphs.xlsx", sheet = "Sheet1")

glimpse(bargraphs)

carb_bar = bargraphs %>%
  filter(Contaminant == "Carb")

dic_bar = bargraphs %>%
  filter(Contaminant == "Dic")

glimpse(carb_bar)

carb_bar$Treatment <- factor(carb_bar$Treatment,
                             levels = c("Control", "Solvent", "C low", "C high", "C+D low", "C+D high", "C+P low", "C+P high", "C+Q low", "C+Q high", "Nutrient control", "Solvent + Nutrient", "C low +nut", "C high +nut", "C+D low +nut", "C+D high +nut", "C+P low +nut", "C+P high +nut", "C+Q low +nut", "C+Q high +nut"),
                             labels = c("Control", "Solvent", "C low", "C high", "C+D low", "C+D high", "C+P low", "C+P high", "C+Q low", "C+Q high", "Nutrients", "Solvent N", "C low N", "C high N", "C+D low N", "C+D high N", "C+P low N", "C+P high N", "C+Q low N", "C+Q high N"))

dic_bar$Treatment <- factor(dic_bar$Treatment,
                             levels = c("control", "Solvent", "D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "nutrient", "Solvent + Nutrient", "D low +nut", "D high +nut", "D+C low +nut", "D+C high +nut", "D+P low +nut", "D+P high +nut", "D+Q low +nut", "D+Q high +nut"),
                             labels = c("Control", "Solvent", "D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "Nutrients", "Solvent N", "D low N", "D high N", "D+C low N", "D+C high N", "D+P low N", "D+P high N", "D+Q low N", "D+Q high N"))

RFU_carb = ggplot() +
  geom_col(aes(x = Treatment, 
                   y = avg_delta_RFU),
               fill = "lightblue4",
               data = carb_bar) +
  geom_errorbar(data = carb_bar, aes(x = Treatment, 
                                     y = avg_delta_RFU,
                                     ymin = avg_delta_RFU-std_dev_RFU,
                                ymax = avg_delta_RFU+std_dev_RFU)) +
  xlab("Treatment") +
  ylab("Change in RFU") +
  scale_y_continuous(limits = c(-2, 8)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(RFU_carb, filename = "RUF_carb.png",
       device = "png", height = 7, width = 11)

RFU_dic = ggplot() +
  geom_col(aes(x = Treatment, 
               y = avg_delta_RFU),
           fill = "lightblue4",
           data = dic_bar) +
  geom_errorbar(data = dic_bar, aes(x = Treatment, 
                                     y = avg_delta_RFU,
                                     ymin = avg_delta_RFU-std_dev_RFU,
                                     ymax = avg_delta_RFU+std_dev_RFU)) +
  xlab("Treatment") +
  ylab("Change in RFU") +
  scale_y_continuous(limits = c(-2, 8)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(RFU_dic, filename = "RFU_dic.png",
       device = "png", height = 7, width = 11)




carb_bar_pd = carb_bar[,c("Contaminant","Treatment","avg_pd", "std_dev_pd")]  # returns a data.frame
glimpse(carb_bar_pd)
carb_bar_pd = carb_bar_pd[complete.cases(carb_bar_pd), ]
carb_bar_pd$avg = (carb_bar_pd$avg_pd)*100
carb_bar_pd$sd = (carb_bar_pd$std_dev_pd)*100


dic_bar_pd = dic_bar[,c("Contaminant","Treatment","avg_pd", "std_dev_pd")]  # returns a data.frame
glimpse(dic_bar_pd)
dic_bar_pd = dic_bar_pd[complete.cases(dic_bar_pd), ]
dic_bar_pd$avg = (dic_bar_pd$avg_pd)*100
dic_bar_pd$sd = (dic_bar_pd$std_dev_pd)*100

pd_carb = ggplot() +
  geom_col(aes(x = Treatment, 
               y = avg),
           fill = "lightblue4",
           data = carb_bar_pd) +
  geom_errorbar(data = carb_bar_pd, aes(x = Treatment, 
                                     y = avg,
                                     ymin = avg-sd,
                                     ymax = avg+sd)) +
  xlab("Treatment") +
  ylab("Percent Difference from Control") +
  scale_y_continuous(limits = c(-250, 325)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(pd_carb, filename = "pd_carb.png",
       device = "png", height = 7, width = 11)

pd_dic = ggplot() +
  geom_col(aes(x = Treatment, 
               y = avg),
           fill = "lightblue4",
           data = dic_bar_pd) +
  geom_errorbar(data = dic_bar_pd, aes(x = Treatment, 
                                    y = avg,
                                    ymin = avg-sd,
                                    ymax = avg+sd)) +
  xlab("Treatment") +
  ylab("Percent Difference from Control") +
  scale_y_continuous(limits = c(-250, 325)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(pd_dic, filename = "pd_dic.png",
       device = "png", height = 7, width = 11)




















boxplots = read_excel("Bioassay poster graphs.xlsx", sheet = "Sheet2")

glimpse(boxplots)

carb_boxplot = boxplots %>%
  filter(Contaminant == "Carb")
carb_boxplot$Treament <- factor(carb_boxplot$Treament,
                             levels = c("control", "Solvent", "D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "Nutrient control", "Solvent + Nutrient", "D low +nut", "D high +nut", "D+C low +nut", "D+C high +nut", "D+P low +nut", "D+P high +nut", "D+Q low +nut", "D+Q high +nut"),
                             labels = c("Control", "Solvent", "D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "Nutrients", "Solvent N", "D low N", "D high N", "D+C low N", "D+C high N", "D+P low N", "D+P high N", "D+Q low N", "D+Q high N"))


carb_box = ggplot() +
  geom_boxplot(aes(x = Treament, 
                   y = `72h`),
               fill = "lightblue4",
               data = carb_boxplot) +
  xlab("Treatment") +
  ylab("Chl a Fluorescence (RFU)") +
  scale_y_continuous(limits = c(1, 12)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(carb_box, filename = "carb_box.png",
       device = "png", height = 7, width = 11)



dic_boxplot = boxplots %>%
  filter(Contaminant == "Dic")
dic_boxplot$Treament <- factor(dic_boxplot$Treament,
                               levels = c("Control", "Solvent", "D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "Nutrient", "Solvent + Nutrient", "D low +nut", "D high +nut", "D+C low +nut", "D+C high +nut", "D+P low +nut", "D+P high +nut", "D+Q low +nut", "D+Q high +nut"),
                               labels = c("Control", "Solvent", "D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "Nutrients", "Solvent N", "D low N", "D high N", "D+C low N", "D+C high N", "D+P low N", "D+P high N", "D+Q low N", "D+Q high N"))


dic_box = ggplot() +
  geom_boxplot(aes(x = Treament, 
                   y = `72h`),
               fill = "lightblue4",
               data = dic_boxplot) +
  xlab("Treatment") +
  ylab("Chl a Fluorescence (RFU)") +
  scale_y_continuous(limits = c(1, 12)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(dic_box, filename = "dic_box.png",
       device = "png", height = 7, width = 11)

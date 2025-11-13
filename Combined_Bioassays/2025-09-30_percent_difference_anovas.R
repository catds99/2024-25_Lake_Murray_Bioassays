# 2025-09-30
# statistics


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
library(nortest)


############################################################ RFU @ 72 h boxplot

############# import data:

data_pd = read_excel("Combined_Bioassays/2025-08-29_data.xlsx", sheet = 2)

glimpse(data_pd)

################################## Bioassay 1: Carbamazepine

carb_no_nut = data_pd %>%
  filter(Bioassay == "1") %>%
  filter(Nutrients == "no") 

carb_no_nut %>%
  group_by(Group) %>%
  get_summary_stats(pd_control, pd_solvent, type = "mean_sd")

carb_nut = data_pd %>%
  filter(Bioassay == "1") %>%
  filter(Nutrients == "yes") 

carb_nut %>%
  group_by(Group) %>%
  get_summary_stats(pd_control, pd_solvent, type = "mean_sd")


######## Normality checks


carb_no_nut_long <- carb_no_nut %>%
  pivot_longer(
    cols = c(pd_control, pd_solvent, delta_pd_control, delta_pd_solvent),
    names_to = "control type",
    values_to = "pd"
  )

carb_no_nut_shapiro = carb_no_nut_long %>%
  group_by(Group, `control type`) %>%
  shapiro_test(pd)

print(carb_no_nut_shapiro, n=40) #all normally distributed (p > 0.05)

carb_nut_long <- carb_nut %>%
  pivot_longer(
    cols = c(pd_control, pd_solvent, delta_pd_control, delta_pd_solvent),
    names_to = "control type",
    values_to = "pd"
  )

carb_nut_shapiro = carb_nut_long %>%
  group_by(Group, `control type`) %>%
  shapiro_test(pd)

print(carb_nut_shapiro, n=40) #mostly normally distributed (p > 0.05), exceptions: c+q_low (solvent and control), and c+q high (delta pd control)


######## Hom. of variance checks

carb_no_nut_long_solvent = carb_no_nut_long %>%
  filter(`control type` == "pd_solvent")

carb_no_nut_long_solvent %>% levene_test(pd ~ Group) # p > 0.05 = passes 

carb_nut_long_solvent = carb_nut_long %>%
  filter(`control type` == "pd_solvent")

carb_nut_long_solvent %>% levene_test(pd ~ Group) # p > 0.05 = passes 


######## ANOVA


carb_no_nut_ANOVA <- aov(pd ~ Group, data = carb_no_nut_long_solvent)
summary(carb_no_nut_ANOVA)

TukeyHSD(carb_no_nut_ANOVA, "Group")
carb_no_nut_REGW = REGW.test(carb_no_nut_ANOVA, "Group", DFerror, MSerror, alpha = 0.05, group=FALSE)
print(carb_no_nut_REGW)


#### carb_no_nut_long_72 %>% kruskal_test(RFU ~ Group)

#### wilcox_carb_no_nut = carb_no_nut_long_72 %>% wilcox_test(RFU ~ Group, p.adjust.method = "bonferroni")
#### print(wilcox_carb_no_nut, n=45)


carb_nut_ANOVA <- aov(pd ~ Group, data = carb_nut_long_solvent)
summary(carb_nut_ANOVA)

#### TukeyHSD(carb_nut_ANOVA, "Group")


#### carb_nut_long_72 %>% kruskal_test(RFU ~ Group)














################################## Bioassay 2: Diclofenac

dic_no_nut = data_pd %>%
  filter(Bioassay == "2") %>%
  filter(Nutrients == "no") 

dic_no_nut %>%
  group_by(Group) %>%
  get_summary_stats(pd_control, pd_solvent, type = "mean_sd")

dic_nut = data_pd %>%
  filter(Bioassay == "2") %>%
  filter(Nutrients == "yes") 

dic_nut %>%
  group_by(Group) %>%
  get_summary_stats(pd_control, pd_solvent, type = "mean_sd")


######## Normality checks


dic_no_nut_long <- dic_no_nut %>%
  pivot_longer(
    cols = c(pd_control, pd_solvent, delta_pd_control, delta_pd_solvent),
    names_to = "control type",
    values_to = "pd"
  )

dic_no_nut_shapiro = dic_no_nut_long %>%
  group_by(Group, `control type`) %>%
  shapiro_test(pd)

print(dic_no_nut_shapiro, n=40) #all normally distributed (p > 0.05)

dic_nut_long <- dic_nut %>%
  pivot_longer(
    cols = c(pd_control, pd_solvent, delta_pd_control, delta_pd_solvent),
    names_to = "control type",
    values_to = "pd"
  )

dic_nut_shapiro = dic_nut_long %>%
  group_by(Group, `control type`) %>%
  shapiro_test(pd)

print(dic_nut_shapiro, n=40) #all normally distributed (p > 0.05)


######## Hom. of variance checks

dic_no_nut_long_solvent = dic_no_nut_long %>%
  filter(`control type` == "pd_solvent")

dic_no_nut_long_solvent %>% levene_test(pd ~ Group) # p > 0.05 = passes 

dic_nut_long_solvent = dic_nut_long %>%
  filter(`control type` == "pd_solvent")

dic_nut_long_solvent %>% levene_test(pd ~ Group) # p > 0.05 = passes 


######## ANOVA


dic_no_nut_ANOVA <- aov(pd ~ Group, data = dic_no_nut_long_solvent)
summary(dic_no_nut_ANOVA)

TukeyHSD(dic_no_nut_ANOVA, "Group")
dic_no_nut_REGW = REGW.test(dic_no_nut_ANOVA, "Group", DFerror, MSerror, alpha = 0.05, group=TRUE)
print(dic_no_nut_REGW)


#### carb_no_nut_long_72 %>% kruskal_test(RFU ~ Group)

#### wilcox_carb_no_nut = carb_no_nut_long_72 %>% wilcox_test(RFU ~ Group, p.adjust.method = "bonferroni")
#### print(wilcox_carb_no_nut, n=45)


carb_nut_ANOVA <- aov(pd ~ Group, data = carb_nut_long_solvent)
summary(carb_nut_ANOVA)

#### TukeyHSD(carb_nut_ANOVA, "Group")


#### carb_nut_long_72 %>% kruskal_test(RFU ~ Group)








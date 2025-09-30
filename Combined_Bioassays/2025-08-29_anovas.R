# 2025-08-29
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

data = read_excel("Combined_Bioassays/2025-08-29_data.xlsx", sheet = 1)

glimpse(data)

data_pd = read_excel("Combined_Bioassays/2025-08-29_data.xlsx", sheet = 2)

################################## Bioassay 1: Carbamazepine

carb_no_nut = data %>%
  filter(Bioassay == "1") %>%
  filter(Nutrients == "no") 

carb_no_nut %>%
  group_by(Group) %>%
  get_summary_stats(`72h`, type = "mean_sd")

carb_nut = data %>%
  filter(Bioassay == "1") %>%
  filter(Nutrients == "yes") 

carb_nut %>%
  group_by(Group) %>%
  get_summary_stats(`72h`, type = "mean_sd")


######## Normality checks


carb_no_nut_long <- carb_no_nut %>%
  pivot_longer(
    cols = c(T0, `24h`, `48h`, `72h`),
    names_to = "Time",
    values_to = "RFU"
  )

carb_no_nut_shapiro = carb_no_nut_long %>%
  group_by(Group, Time) %>%
  shapiro_test(RFU)

print(carb_no_nut_shapiro, n=40) #all normally distributed (p > 0.05)

carb_nut_long <- carb_nut %>%
  pivot_longer(
    cols = c(T0, `24h`, `48h`, `72h`),
    names_to = "Time",
    values_to = "RFU"
  )

carb_nut_shapiro = carb_nut_long %>%
  group_by(Group, Time) %>%
  shapiro_test(RFU)

print(carb_nut_shapiro, n=40) #mostly normally distributed (p > 0.05), exceptions: c+q_low 72h, c_high 48h, solvent 48h


######## Hom. of variance checks

carb_no_nut_long_72 = carb_no_nut_long %>%
  filter(Time == "72h")

carb_no_nut_long_72 %>% levene_test(RFU ~ Group) # p > 0.05 = passes 

carb_nut_long_72 = carb_nut_long %>%
  filter(Time == "72h")

carb_nut_long_72 %>% levene_test(RFU ~ Group) # p > 0.05 = passes 


######## ANOVA


carb_no_nut_ANOVA <- aov(RFU ~ Group, data = carb_no_nut_long_72)
summary(carb_no_nut_ANOVA)

TukeyHSD(carb_no_nut_ANOVA, "Group")
carb_no_nut_REGW = REGW.test(carb_no_nut_ANOVA, "Group", DFerror, MSerror, alpha = 0.05, group=FALSE)
print(carb_no_nut_REGW)


carb_no_nut_long_72 %>% kruskal_test(RFU ~ Group)

wilcox_carb_no_nut = carb_no_nut_long_72 %>% wilcox_test(RFU ~ Group, p.adjust.method = "bonferroni")
print(wilcox_carb_no_nut, n=45)


carb_nut_ANOVA <- aov(RFU ~ Group, data = carb_nut_long_72)
summary(carb_nut_ANOVA)

TukeyHSD(carb_nut_ANOVA, "Group")


carb_nut_long_72 %>% kruskal_test(RFU ~ Group)















################################## Bioassay 2: Diclofenac

dic_no_nut = data %>%
  filter(Bioassay == "2") %>%
  filter(Nutrients == "no") 

dic_no_nut %>%
  group_by(Group) %>%
  get_summary_stats(`72h`, type = "mean_sd")

dic_nut = data %>%
  filter(Bioassay == "2") %>%
  filter(Nutrients == "yes") 

dic_nut %>%
  group_by(Group) %>%
  get_summary_stats(`72h`, type = "mean_sd")


######## Normality checks


dic_no_nut_long <- dic_no_nut %>%
  pivot_longer(
    cols = c(T0, `24h`, `48h`, `72h`),
    names_to = "Time",
    values_to = "RFU"
  )

dic_no_nut_shapiro = dic_no_nut_long %>%
  group_by(Group, Time) %>%
  shapiro_test(RFU)

print(dic_no_nut_shapiro, n=40) #mostly normally distributed (p > 0.05), D high p = 0.0443

dic_nut_long <- dic_nut %>%
  pivot_longer(
    cols = c(T0, `24h`, `48h`, `72h`),
    names_to = "Time",
    values_to = "RFU"
  )

dic_nut_shapiro = dic_nut_long %>%
  group_by(Group, Time) %>%
  shapiro_test(RFU)

print(dic_nut_shapiro, n=40) #mostly normally distributed (p > 0.05), exceptions: c+q_low 72h, c_high 48h, solvent 48h


######## Hom. of variance checks

dic_no_nut_long_72 = dic_no_nut_long %>%
  filter(Time == "72h")

dic_no_nut_long_72 %>% levene_test(RFU ~ Group) # p > 0.05 = passes 

dic_nut_long_72 = dic_nut_long %>%
  filter(Time == "72h")

dic_nut_long_72 %>% levene_test(RFU ~ Group) # p > 0.05 = passes 


######## ANOVA


dic_no_nut_ANOVA <- aov(RFU ~ Group, data = dic_no_nut_long_72)
summary(dic_no_nut_ANOVA)

TukeyHSD(dic_no_nut_ANOVA, "Group")
dic_no_nut_REGW = REGW.test(dic_no_nut_ANOVA, "Group", DFerror, MSerror, alpha = 0.05, group=FALSE)
print(dic_no_nut_REGW)


dic_no_nut_long_72 %>% kruskal_test(RFU ~ Group)

wilcox_dic_no_nut = dic_no_nut_long_72 %>% wilcox_test(RFU ~ Group, p.adjust.method = "bonferroni")
print(wilcox_dic_no_nut, n=45)


dic_nut_ANOVA <- aov(RFU ~ Group, data = dic_nut_long_72)
summary(dic_nut_ANOVA)

TukeyHSD(dic_nut_ANOVA, "Group")
dic_nut_REGW = REGW.test(dic_nut_ANOVA, "Group", DFerror, MSerror, alpha = 0.05, group=FALSE)
print(dic_nut_REGW)

dic_nut_long_72 %>% kruskal_test(RFU ~ Group)

wilcox_dic_nut = dic_nut_long_72 %>% wilcox_test(RFU ~ Group, p.adjust.method = "bonferroni")
print(wilcox_dic_nut, n=45)






















################################## Bioassay 3: PFOS

pfos_no_nut = data %>%
  filter(Bioassay == "3") %>%
  filter(Nutrients == "no") 

pfos_no_nut %>%
  group_by(Group) %>%
  get_summary_stats(`72h`, type = "mean_sd")

pfos_nut = data %>%
  filter(Bioassay == "3") %>%
  filter(Nutrients == "yes") 

pfos_nut %>%
  group_by(Group) %>%
  get_summary_stats(`72h`, type = "mean_sd")


######## Normality checks


pfos_no_nut_long <- pfos_no_nut %>%
  pivot_longer(
    cols = c(T0, `24h`, `48h`, `72h`),
    names_to = "Time",
    values_to = "RFU"
  )

pfos_no_nut_shapiro = pfos_no_nut_long %>%
  group_by(Group, Time) %>%
  shapiro_test(RFU)

print(pfos_no_nut_shapiro, n=40) #mostly normally distributed (p > 0.05) except for a few 48 h groups

pfos_nut_long <- pfos_nut %>%
  pivot_longer(
    cols = c(T0, `24h`, `48h`, `72h`),
    names_to = "Time",
    values_to = "RFU"
  )

pfos_nut_shapiro = pfos_nut_long %>%
  group_by(Group, Time) %>%
  shapiro_test(RFU)

print(pfos_nut_shapiro, n=40) #mostly normally distributed (p > 0.05), except for a few 24 h groups


######## Hom. of variance checks

pfos_no_nut_long_72 = pfos_no_nut_long %>%
  filter(Time == "72h")

pfos_no_nut_long_72 %>% levene_test(RFU ~ Group) # p > 0.05 = passes 

pfos_nut_long_72 = pfos_nut_long %>%
  filter(Time == "72h")

pfos_nut_long_72 %>% levene_test(RFU ~ Group) # p > 0.05 = passes 


######## ANOVA


pfos_no_nut_ANOVA <- aov(RFU ~ Group, data = pfos_no_nut_long_72)
summary(pfos_no_nut_ANOVA)

TukeyHSD(pfos_no_nut_ANOVA, "Group")
pfos_no_nut_REGW = REGW.test(pfos_no_nut_ANOVA, "Group", DFerror, MSerror, alpha = 0.05, group=FALSE)
print(pfos_no_nut_REGW)


pfos_no_nut_long_72 %>% kruskal_test(RFU ~ Group)

wilcox_pfos_no_nut = pfos_no_nut_long_72 %>% wilcox_test(RFU ~ Group, p.adjust.method = "bonferroni")
print(wilcox_pfos_no_nut, n=45)


pfos_nut_ANOVA <- aov(RFU ~ Group, data = pfos_nut_long_72)
summary(pfos_nut_ANOVA)

TukeyHSD(pfos_nut_ANOVA, "Group")
pfos_nut_REGW = REGW.test(pfos_nut_ANOVA, "Group", DFerror, MSerror, alpha = 0.05, group=FALSE)
print(pfos_nut_REGW)

pfos_nut_long_72 %>% kruskal_test(RFU ~ Group)

wilcox_pfos_nut = pfos_nut_long_72 %>% wilcox_test(RFU ~ Group, p.adjust.method = "bonferroni")
print(wilcox_pfos_nut, n=45)



















################################## Bioassay 4: 6ppdq

quin_no_nut = data %>%
  filter(Bioassay == "4") %>%
  filter(Nutrients == "no") 

quin_no_nut %>%
  group_by(Group) %>%
  get_summary_stats(`72h`, type = "mean_sd")

quin_nut = data %>%
  filter(Bioassay == "4") %>%
  filter(Nutrients == "yes") 

quin_nut %>%
  group_by(Group) %>%
  get_summary_stats(`72h`, type = "mean_sd")


######## Normality checks


quin_no_nut_long <- quin_no_nut %>%
  pivot_longer(
    cols = c(T0, `24h`, `48h`, `72h`),
    names_to = "Time",
    values_to = "RFU"
  )

quin_no_nut_shapiro = quin_no_nut_long %>%
  group_by(Group, Time) %>%
  shapiro_test(RFU)

print(quin_no_nut_shapiro, n=40) #mostly normally distributed (p > 0.05) except for Q+P high 72h, solvent 72h, and a few other 24/48h groups

quin_nut_long <- quin_nut %>%
  pivot_longer(
    cols = c(T0, `24h`, `48h`, `72h`),
    names_to = "Time",
    values_to = "RFU"
  )

quin_nut_shapiro = pfos_nut_long %>%
  group_by(Group, Time) %>%
  shapiro_test(RFU)

print(quin_nut_shapiro, n=40) #mostly normally distributed (p > 0.05), except for a few 24 h groups


######## Hom. of variance checks

quin_no_nut_long_72 = quin_no_nut_long %>%
  filter(Time == "72h")

quin_no_nut_long_72 %>% levene_test(RFU ~ Group) # p > 0.05 = passes 

quin_nut_long_72 = quin_nut_long %>%
  filter(Time == "72h")

quin_nut_long_72 %>% levene_test(RFU ~ Group) # p > 0.05 = passes 


######## ANOVA


quin_no_nut_ANOVA <- aov(RFU ~ Group, data = quin_no_nut_long_72)
summary(quin_no_nut_ANOVA)

TukeyHSD(quin_no_nut_ANOVA, "Group")
quin_no_nut_REGW = REGW.test(quin_no_nut_ANOVA, "Group", DFerror, MSerror, alpha = 0.05, group=FALSE)
print(quin_no_nut_REGW)

quin_no_nut_long_72 %>% kruskal_test(RFU ~ Group)

wilcox_quin_no_nut = quin_no_nut_long_72 %>% wilcox_test(RFU ~ Group, p.adjust.method = "bonferroni")
print(wilcox_quin_no_nut, n=45)


quin_nut_ANOVA <- aov(RFU ~ Group, data = quin_nut_long_72)
summary(quin_nut_ANOVA)

TukeyHSD(quin_nut_ANOVA, "Group")
quin_nut_REGW = REGW.test(quin_nut_ANOVA, "Group", DFerror, MSerror, alpha = 0.05, group=FALSE)
print(quin_nut_REGW)

quin_nut_long_72 %>% kruskal_test(RFU ~ Group)

wilcox_quin_nut = quin_nut_long_72 %>% wilcox_test(RFU ~ Group, p.adjust.method = "bonferroni")
print(wilcox_quin_nut, n=45)

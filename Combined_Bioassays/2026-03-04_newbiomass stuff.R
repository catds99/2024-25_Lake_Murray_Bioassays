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
library(grid)


### read in data:

abs_data = read_excel("Combined_Bioassays/2026-03-04_master_data_values_only.xlsx", sheet = "abs")
pd_data = read_excel("Combined_Bioassays/2026-03-04_master_data_values_only.xlsx", sheet = "pd")
glimpse(pd_data)
glimpse(abs_data)

### reformat data:

abs_data$Bioassay <- factor(abs_data$Bioassay,
                             levels = c("Carbamazepine", "Diclofenac", "PFOS", "Quinone"),
                             labels = c("Carbamazepine", "Diclofenac", "PFOS", "6ppd-q"))


abs_data$Treatment <- factor(abs_data$Treatment,
                         levels = c("c", "sc", "n", "sn", "c_low", "c_high", "cd_low", "cd_high", "cp_low", "cp_high", "cq_low", "cq_high",  "c_low_n", "c_high_n", "cd_low_n", "cd_high_n", "cp_low_n", "cp_high_n", "cq_low_n", "cq_high_n", "d_low", "d_high", "dc_low", "dc_high", "dp_low", "dp_high", "dq_low", "dq_high", "d_low_n", "d_high_n", "dc_low_n", "dc_high_n", "dp_low_n", "dp_high_n", "dq_low_n", "dq_high_n", "p_low", "p_high", "pc_low", "pc_high", "pd_low", "pd_high", "pq_low", "pq_high", "p_low_n", "p_high_n", "pc_low_n", "pc_high_n", "pd_low_n", "pd_high_n", "pq_low_n", "pq_high_n", "q_low", "q_high", "qc_low", "qc_high", "qd_low", "qd_high", "qp_low", "qp_high", "q_low_n", "q_high_n", "qc_low_n", "qc_high_n", "qd_low_n", "qd_high_n", "qp_low_n", "qp_high_n"),
                         labels = c("Control", "Solvent Control", "Nutrient Control", "Nutrient-Solvent Control", "C low", "C high", "C+D low", "C+D high", "C+P low", "C+P high", "C+Q low", "C+Q high", "C low N", "C high N", "C+D low N", "C+D high N", "C+P low N", "C+P high N", "C+Q low N", "C+Q high N", "D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "D low N", "D high N", "D+C low N", "D+C high N", "D+P low N", "D+P high N", "D+Q low N", "D+Q high N", "P low", "P high", "P+C low", "P+C high", "P+D low", "P+D high", "P+Q low", "P+Q high", "P low N", "P high N", "P+C low N", "P+C high N", "P+D low N", "P+D high N", "P+Q low N", "P+Q high N", "Q low", "Q high", "Q+C low", "Q+C high", "Q+D low", "Q+D high", "Q+P low", "Q+P high", "Q low N", "Q high N", "Q+C low N", "Q+C high N", "Q+D low N", "Q+D high N", "Q+P low N", "Q+P high N"))



pd_data$Bioassay <- factor(pd_data$Bioassay,
                            levels = c("Carbamazepine", "Diclofenac", "PFOS", "Quinone"),
                            labels = c("Carbamazepine", "Diclofenac", "PFOS", "6ppd-q"))


pd_data$Treatment <- factor(pd_data$Treatment,
                             levels = c("c_low", "c_high", "cd_low", "cd_high", "cp_low", "cp_high", "cq_low", "cq_high",  "c_low_n", "c_high_n", "cd_low_n", "cd_high_n", "cp_low_n", "cp_high_n", "cq_low_n", "cq_high_n", "d_low", "d_high", "dc_low", "dc_high", "dp_low", "dp_high", "dq_low", "dq_high", "d_low_n", "d_high_n", "dc_low_n", "dc_high_n", "dp_low_n", "dp_high_n", "dq_low_n", "dq_high_n", "p_low", "p_high", "pc_low", "pc_high", "pd_low", "pd_high", "pq_low", "pq_high", "p_low_n", "p_high_n", "pc_low_n", "pc_high_n", "pd_low_n", "pd_high_n", "pq_low_n", "pq_high_n", "q_low", "q_high", "qc_low", "qc_high", "qd_low", "qd_high", "qp_low", "qp_high", "q_low_n", "q_high_n", "qc_low_n", "qc_high_n", "qd_low_n", "qd_high_n", "qp_low_n", "qp_high_n"),
                             labels = c("C low", "C high", "C+D low", "C+D high", "C+P low", "C+P high", "C+Q low", "C+Q high", "C low N", "C high N", "C+D low N", "C+D high N", "C+P low N", "C+P high N", "C+Q low N", "C+Q high N", "D low", "D high", "D+C low", "D+C high", "D+P low", "D+P high", "D+Q low", "D+Q high", "D low N", "D high N", "D+C low N", "D+C high N", "D+P low N", "D+P high N", "D+Q low N", "D+Q high N", "P low", "P high", "P+C low", "P+C high", "P+D low", "P+D high", "P+Q low", "P+Q high", "P low N", "P high N", "P+C low N", "P+C high N", "P+D low N", "P+D high N", "P+Q low N", "P+Q high N", "Q low", "Q high", "Q+C low", "Q+C high", "Q+D low", "Q+D high", "Q+P low", "Q+P high", "Q low N", "Q high N", "Q+C low N", "Q+C high N", "Q+D low N", "Q+D high N", "Q+P low N", "Q+P high N"))



#################################################### figures

### boxplot based on biomass:

biomass_box = ggplot() +
  geom_boxplot(aes(x = Treatment, 
                   y = B_72,
                   fill = Nutrients),
               data = abs_data) +
  scale_fill_manual(values = c("lightblue", "darkgreen"))+
  facet_wrap(~Bioassay, scales = "free")+
  xlab("Treatment") +
  ylab(expression("Total Biomass (μg Chl " *italic(a)* " L"^-1*")")) +
  scale_y_continuous(limits = c(1, 12)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### boxplot based on RFU:

RFU_box = ggplot() +
  geom_boxplot(aes(x = Treatment, 
                   y = RFU_72,
                   fill = Nutrients),
               data = abs_data) +
  scale_fill_manual(values = c("lightblue", "darkgreen"))+
  facet_wrap(~Bioassay, scales = "free")+
  xlab("Treatment") +
  ylab(expression("Chl " *italic(a)* " Fluorescence (RFU)")) +
  scale_y_continuous(limits = c(1, 12)) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




### bar graph based on change in biomass over 72 h:

B_delta_mean <- abs_data %>%
  group_by(Bioassay, Treatment, Nutrients) %>%
  summarize(mean_delta_B = mean(B_delta),
            sd_delta_B = sd(B_delta))


delta_biomass = ggplot(aes(x = Treatment, 
                           y = mean_delta_B,
                           fill = Nutrients),
                       data = B_delta_mean) +
  geom_col(aes(x = Treatment, 
               y = mean_delta_B,
               fill = Nutrients),
           data = B_delta_mean) +
  geom_errorbar(aes(ymin = mean_delta_B - sd_delta_B, 
                    ymax = mean_delta_B + sd_delta_B), width = 0.2) +
  scale_fill_manual(values = c("lightblue", "darkgreen"))+
  facet_wrap(~Bioassay, scales = "free")+
  xlab("Treatment") +
  ylab(expression("Change in Total Biomass (μg Chl " *italic(a)* " L"^-1*") over 72h")) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### bar graph based on change in RFU over 72 h:

RFU_delta_mean <- abs_data %>%
  group_by(Bioassay, Treatment, Nutrients) %>%
  summarize(mean_delta_RFU = mean(RFU_delta),
            sd_delta_RFU = sd(RFU_delta))


delta_RFU = ggplot(aes(x = Treatment, 
                           y = mean_delta_RFU,
                           fill = Nutrients),
                       data = RFU_delta_mean) +
  geom_col(aes(x = Treatment, 
               y = mean_delta_RFU,
               fill = Nutrients),
           data = RFU_delta_mean) +
  geom_errorbar(aes(ymin = mean_delta_RFU - sd_delta_RFU, 
                    ymax = mean_delta_RFU + sd_delta_RFU), width = 0.2) +
  scale_fill_manual(values = c("lightblue", "darkgreen"))+
  facet_wrap(~Bioassay, scales = "free")+
  xlab("Treatment") +
  ylab(expression("Change in Chl " *italic(a)* " Fluorescence (RFU)")) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








### bar graph based on percent difference relevant to solvent control, biomass:


B_pd_mean_sc <- pd_data %>%
  group_by(Bioassay, Treatment, Nutrients) %>%
  summarize(mean_pd_B_sc = mean(B_pd_sc),
            sd_pd_B_sc = sd(B_pd_sc))


pd_biomass = ggplot(aes(x = Treatment, 
                           y = mean_pd_B_sc,
                           fill = Nutrients),
                       data = B_pd_mean_sc) +
  geom_col(aes(x = Treatment, 
               y = mean_pd_B_sc,
               fill = Nutrients),
           data = B_pd_mean_sc) +
  geom_errorbar(aes(ymin = mean_pd_B_sc - sd_pd_B_sc, 
                    ymax = mean_pd_B_sc + sd_pd_B_sc), width = 0.2) +
  scale_fill_manual(values = c("lightblue", "darkgreen"))+
  facet_wrap(~Bioassay, scales = "free")+
  xlab("Treatment") +
  ylab(expression("Percent Difference in Total Biomass (μg Chl " *italic(a)* " L"^-1*") Relative to Solvent Control")) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



### bar graph based on percent difference relevant to solvent control, RFU:

RFU_pd_mean_sc <- pd_data %>%
  group_by(Bioassay, Treatment, Nutrients) %>%
  summarize(mean_pd_RFU_sc = mean(RFU_pd_sc),
            sd_pd_RFU_sc = sd(RFU_pd_sc))


pd_RFU = ggplot(aes(x = Treatment, 
                        y = mean_pd_RFU_sc,
                        fill = Nutrients),
                    data = RFU_pd_mean_sc) +
  geom_col(aes(x = Treatment, 
               y = mean_pd_RFU_sc,
               fill = Nutrients),
           data = RFU_pd_mean_sc) +
  geom_errorbar(aes(ymin = mean_pd_RFU_sc - sd_pd_RFU_sc, 
                    ymax = mean_pd_RFU_sc + sd_pd_RFU_sc), width = 0.2) +
  scale_fill_manual(values = c("lightblue", "darkgreen"))+
  facet_wrap(~Bioassay, scales = "free")+
  xlab("Treatment") +
  ylab(expression("Percent Difference in Chl " *italic(a)* " Fluorescence (RFU) Relative to Solvent Control")) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





### bar graph based on percent difference relevant to control, biomass:



B_pd_mean_c <- pd_data %>%
  group_by(Bioassay, Treatment, Nutrients) %>%
  summarize(mean_pd_B_c = mean(B_pd_c),
            sd_pd_B_c = sd(B_pd_c))


pd_biomass_c = ggplot(aes(x = Treatment, 
                        y = mean_pd_B_c,
                        fill = Nutrients),
                    data = B_pd_mean_c) +
  geom_col(aes(x = Treatment, 
               y = mean_pd_B_c,
               fill = Nutrients),
           data = B_pd_mean_c) +
  geom_errorbar(aes(ymin = mean_pd_B_c - sd_pd_B_c, 
                    ymax = mean_pd_B_c + sd_pd_B_c), width = 0.2) +
  scale_fill_manual(values = c("lightblue", "darkgreen"))+
  facet_wrap(~Bioassay, scales = "free")+
  xlab("Treatment") +
  ylab(expression("Percent Difference in Total Biomass (μg Chl " *italic(a)* " L"^-1*") Relative to  Control")) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



### bar graph based on percent difference relevant to control, RFU:

RFU_pd_mean_c <- pd_data %>%
  group_by(Bioassay, Treatment, Nutrients) %>%
  summarize(mean_pd_RFU_c = mean(RFU_pd_c),
            sd_pd_RFU_c = sd(RFU_pd_c))


pd_RFU_c = ggplot(aes(x = Treatment, 
                    y = mean_pd_RFU_c,
                    fill = Nutrients),
                data = RFU_pd_mean_c) +
  geom_col(aes(x = Treatment, 
               y = mean_pd_RFU_c,
               fill = Nutrients),
           data = RFU_pd_mean_c) +
  geom_errorbar(aes(ymin = mean_pd_RFU_c - sd_pd_RFU_c, 
                    ymax = mean_pd_RFU_c + sd_pd_RFU_c), width = 0.2) +
  scale_fill_manual(values = c("lightblue", "darkgreen"))+
  facet_wrap(~Bioassay, scales = "free")+
  xlab("Treatment") +
  ylab(expression("Percent Difference in Chl " *italic(a)* " Fluorescence (RFU) Relative to Control")) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




####################################################################### Statistics

#################################################### Biomass @ 72h



############################ Assumptions, based on raw data

### Normality test

shapiro = abs_data %>%
  group_by(Bioassay, Treatment) %>%
  filter(sd(B_72, na.rm = TRUE) > 0) %>%
  shapiro_test(B_72)

print(shapiro, n = 60)

capture.output(print(shapiro, n=60), file = "shapiro_table.doc")

### Homogeneity of variance

levene = abs_data %>% 
  group_by(Bioassay, Nutrients) %>%
  filter(sd(B_72, na.rm = TRUE) > 0) %>%
  levene_test(B_72 ~ Treatment) 

print(levene, n = 60)

capture.output(print(levene), file = "levene_table.doc")



############################ ANOVAs

##### regroup data

carb_no_nut = abs_data %>%
  filter(Bioassay == "Carbamazepine",
         Nutrients == "no")
carb_nut = abs_data %>%
  filter(Bioassay == "Carbamazepine",
         Nutrients == "yes")

dic_no_nut = abs_data %>%
  filter(Bioassay == "Diclofenac",
         Nutrients == "no")
dic_nut = abs_data %>%
  filter(Bioassay == "Diclofenac",
         Nutrients == "yes")

pfos_no_nut = abs_data %>%
  filter(Bioassay == "PFOS",
         Nutrients == "no")
pfos_nut = abs_data %>%
  filter(Bioassay == "PFOS",
         Nutrients == "yes")

quin_no_nut = abs_data %>%
  filter(Bioassay == "6ppd-q",
         Nutrients == "no")
quin_nut = abs_data %>%
  filter(Bioassay == "6ppd-q",
         Nutrients == "yes")

## Carbamazepine ##

# ANOVAs 

carb_no_nut_ANOVA <- aov(B_72 ~ Treatment, data = carb_no_nut)
summary(carb_no_nut_ANOVA)

carb_nut_ANOVA <- aov(B_72 ~ Treatment, data = carb_nut)
summary(carb_nut_ANOVA)

# Tukey test

carb_no_nut_Tukey = TukeyHSD(carb_no_nut_ANOVA, "Treatment")
print(carb_no_nut_Tukey)

carb_nut_Tukey = TukeyHSD(carb_nut_ANOVA, "Treatment")
print(carb_nut_Tukey)

# Assumptions based on residuals 

res <- residuals(carb_no_nut_ANOVA)
shapiro.test(res)
plot(carb_no_nut_ANOVA, which = 2) ########### not normal

levene1 = leveneTest(carb_no_nut_ANOVA)


res2 <- residuals(carb_nut_ANOVA)
shapiro.test(res2)
plot(carb_nut_ANOVA, which = 2) ############ normal

levene2 = leveneTest(carb_nut_ANOVA)


#
dic_no_nut_ANOVA <- aov(B_72 ~ Treatment, data = dic_no_nut)
summary(dic_no_nut_ANOVA)
dic_no_nut_Tukey = TukeyHSD(dic_no_nut_ANOVA, "Treatment")
print(dic_no_nut_Tukey)

#
dic_nut_ANOVA <- aov(B_72 ~ Treatment, data = dic_nut)
summary(dic_nut_ANOVA)
dic_nut_Tukey = TukeyHSD(dic_nut_ANOVA, "Treatment")
print(dic_nut_Tukey)


## PFOS ##

# ANOVAs

pfos_no_nut_ANOVA <- aov(B_72 ~ Treatment, data = pfos_no_nut)
summary(pfos_no_nut_ANOVA)

pfos_nut_ANOVA <- aov(B_72 ~ Treatment, data = pfos_nut)
summary(pfos_nut_ANOVA)

# Tukey Tests

pfos_no_nut_Tukey = TukeyHSD(pfos_no_nut_ANOVA, "Treatment")
print(pfos_no_nut_Tukey)

pfos_nut_Tukey = TukeyHSD(pfos_nut_ANOVA, "Treatment")
print(pfos_nut_Tukey)

# Assumptions based on residuals

res3 <- residuals(pfos_no_nut_ANOVA)
shapiro.test(res3)
plot(pfos_no_nut_ANOVA, which = 2) ########### not normal

levene3 = leveneTest(pfos_no_nut_ANOVA)


res4 <- residuals(pfos_nut_ANOVA)
shapiro.test(res4)
plot(pfos_nut_ANOVA, which = 2) ########### normal

levene4 = leveneTest(pfos_nut_ANOVA)



## 6ppd-q ##

# ANOVAs

quin_no_nut_ANOVA <- aov(B_72 ~ Treatment, data = quin_no_nut)
summary(quin_no_nut_ANOVA)

quin_nut_ANOVA <- aov(B_72 ~ Treatment, data = quin_nut)
summary(quin_nut_ANOVA)

# Tukey Tests

quin_no_nut_Tukey = TukeyHSD(quin_no_nut_ANOVA, "Treatment")
print(quin_no_nut_Tukey)

quin_nut_Tukey = TukeyHSD(quin_nut_ANOVA, "Treatment")
print(quin_nut_Tukey)

# Assumptions based on residuals 

res5 <- residuals(quin_no_nut_ANOVA)
shapiro.test(res5)
plot(quin_no_nut_ANOVA, which = 2) ###########  normal

levene5 = leveneTest(quin_no_nut_ANOVA)


res6 <- residuals(quin_nut_ANOVA)
shapiro.test(res6)
plot(quin_nut_ANOVA, which = 2) ###########  normal

levene6 = leveneTest(quin_nut_ANOVA)

## Print output to word doc:

# ANOVAs
capture.output(summary(carb_no_nut_ANOVA), print(carb_no_nut_Tukey), summary(carb_nut_ANOVA), print(carb_nut_Tukey), summary(pfos_no_nut_ANOVA), print(pfos_no_nut_Tukey), summary(pfos_nut_ANOVA), print(pfos_nut_Tukey),  summary(quin_no_nut_ANOVA), print(quin_no_nut_Tukey), summary(quin_nut_ANOVA), print(quin_nut_Tukey),  file = "anova_table.doc")

# Assumptions based on residuals
capture.output(print(shapiro.test(res)), print(shapiro.test(res2)), print(shapiro.test(res3)), print(shapiro.test(res4)), print(shapiro.test(res5)), print(shapiro.test(res6)), print(levene1), print(levene2), print(levene3), print(levene4), print(levene5), print(levene6),  file = "res_assumptions.doc")



############################ two way ANOVAs

# filter data with correctly grouped treatments 

carb = abs_data %>%
  filter(Bioassay == "Carbamazepine")
glimpse(carb)

dic = abs_data %>%
  filter(Bioassay == "Diclofenac")
glimpse(dic)

pfos = abs_data %>%
  filter(Bioassay == "PFOS")
glimpse(pfos)

quin = abs_data %>%
  filter(Bioassay == "Quinone")
glimpse(quin)


## Carbamazepine ##

# ANOVA

carb_2way = aov(B_72 ~ Treatment2 * Nutrients, data = carb)
summary(carb_2way) #no interaction, so on to additive

carb_2wayb = aov(B_72 ~ Treatment2 + Nutrients, data = carb)
summary(carb_2wayb)

# Interaction plot
interaction.plot(x.factor = carb$Treatment2, 
                 trace.factor = carb$Nutrients, 
                 response = carb$B_72, 
                 type = "b", 
                 col = c("red", "blue"), 
                 pch = c(19, 17), 
                 fixed = TRUE,
                 xlab = "Treatment", 
                 ylab = "Mean B_72", 
                 main = "Interaction Plot")


# Assumptions based on residuals
levene_carb_2way = leveneTest(B_72 ~ Treatment2 * Nutrients, data = carb)

resids <- resid(carb_2way)
shapiro.test(resids)
qqnorm(resids)
qqline(resids, col = "red")
shapiro.test(resid(carb_2way))


## PFOS ##

# ANOVA

pfos_2way = aov(B_72 ~ Treatment2 * Nutrients, data = pfos)
summary(pfos_2way) # significant interaction, so on to additive


# Interaction plot
interaction.plot(x.factor = pfos$Treatment2, 
                 trace.factor = pfos$Nutrients, 
                 response = pfos$B_72, 
                 type = "b", 
                 col = c("red", "blue"), 
                 pch = c(19, 17), 
                 fixed = TRUE,
                 xlab = "Treatment", 
                 ylab = "Mean B_72", 
                 main = "Interaction Plot")


# Assumptions based on residuals
levene_pfos_2way =leveneTest(B_72 ~ Treatment2 * Nutrients, data = pfos)

resids2 <- resid(pfos_2way)
shapiro.test(resids2)
qqnorm(resids2)
qqline(resids2, col = "red")
shapiro.test(resid(pfos_2way))


## 6ppd-q ##

# ANOVA

quin_2way = aov(B_72 ~ Treatment2 * Nutrients, data = quin)
summary(quin_2way) #interaction


# Interaction plot
interaction.plot(x.factor = quin$Treatment2, 
                 trace.factor = quin$Nutrients, 
                 response = quin$B_72, 
                 type = "b", 
                 col = c("red", "blue"), 
                 pch = c(19, 17), 
                 fixed = TRUE,
                 xlab = "Treatment", 
                 ylab = "Mean B_72", 
                 main = "Interaction Plot")


# Assumptions based on residuals
levene_quin_2way = leveneTest(B_72 ~ Treatment2 * Nutrients, data = quin)

resids3 <- resid(quin_2way)
shapiro.test(resids3)
qqnorm(resids3)
qqline(resids3, col = "red")
shapiro.test(resid(quin_2way))






capture.output(summary(carb_2way), print(levene_carb_2way), print(shapiro.test(resid(carb_2way))), summary(pfos_2way), print(levene_pfos_2way), print(shapiro.test(resid(pfos_2way))), summary(quin_2way), print(levene_quin_2way), print(shapiro.test(resid(quin_2way))),  file = "two-way anovas.doc")



















#################################################### Change over 72 h



############################ Assumptions

### Normality test

shapiro_delta = abs_data %>%
  group_by(Bioassay, Treatment) %>%
  filter(sd(B_delta, na.rm = TRUE) > 0) %>%
  shapiro_test(B_delta)

print(shapiro_delta, n = 60)

capture.output(print(shapiro_delta, n=60), file = "shapiro_table.doc")

### Homogeneity of variance

levene_delta = abs_data %>% 
  group_by(Bioassay, Nutrients) %>%
  filter(sd(B_delta, na.rm = TRUE) > 0) %>%
  levene_test(B_delta ~ Treatment) 

print(levene_delta, n = 60)

capture.output(print(shapiro_delta, n=60), print(levene_delta), file = "delta_assumptions_table.doc")



############################ ANOVAs

carb_no_nut_ANOVA_d <- aov(B_delta ~ Treatment, data = carb_no_nut)
summary(carb_no_nut_ANOVA_d)
carb_no_nut_Tukey_d = TukeyHSD(carb_no_nut_ANOVA_d, "Treatment")
print(carb_no_nut_Tukey_d)

carb_nut_ANOVA_d <- aov(B_delta ~ Treatment, data = carb_nut)
summary(carb_nut_ANOVA_d)
carb_nut_Tukey_d = TukeyHSD(carb_nut_ANOVA_d, "Treatment")
print(carb_nut_Tukey_d)



dic_no_nut_ANOVA_d <- aov(B_delta ~ Treatment, data = dic_no_nut)
summary(dic_no_nut_ANOVA_d)
dic_no_nut_Tukey_d = TukeyHSD(dic_no_nut_ANOVA_d, "Treatment")
print(dic_no_nut_Tukey_d)

dic_nut_ANOVA_d <- aov(B_delta ~ Treatment, data = dic_nut)
summary(dic_nut_ANOVA_d)
dic_nut_Tukey_d = TukeyHSD(dic_nut_ANOVA_d, "Treatment")
print(dic_nut_Tukey_d)



pfos_no_nut_ANOVA_d <- aov(B_delta ~ Treatment, data = pfos_no_nut)
summary(pfos_no_nut_ANOVA_d)
pfos_no_nut_Tukey_d = TukeyHSD(pfos_no_nut_ANOVA_d, "Treatment")
print(pfos_no_nut_Tukey_d)

pfos_nut_ANOVA_d <- aov(B_delta ~ Treatment, data = pfos_nut)
summary(pfos_nut_ANOVA_d)
pfos_nut_Tukey_d = TukeyHSD(pfos_nut_ANOVA_d, "Treatment")
print(pfos_nut_Tukey_d)



quin_no_nut_ANOVA_d <- aov(B_delta ~ Treatment, data = quin_no_nut)
summary(quin_no_nut_ANOVA_d)
quin_no_nut_Tukey_d = TukeyHSD(quin_no_nut_ANOVA_d, "Treatment")
print(quin_no_nut_Tukey_d)

quin_nut_ANOVA_d <- aov(B_delta ~ Treatment, data = quin_nut)
summary(quin_nut_ANOVA_d)
quin_nut_Tukey_d = TukeyHSD(quin_nut_ANOVA_d, "Treatment")
print(quin_nut_Tukey_d)


capture.output(summary(carb_no_nut_ANOVA_d), print(carb_no_nut_Tukey_d), summary(carb_nut_ANOVA_d), print(carb_nut_Tukey_d), summary(pfos_no_nut_ANOVA_d), print(pfos_no_nut_Tukey_d), summary(pfos_nut_ANOVA_d), print(pfos_nut_Tukey_d),  summary(quin_no_nut_ANOVA_d), print(quin_no_nut_Tukey_d), summary(quin_nut_ANOVA_d), print(quin_nut_Tukey_d),  file = "delta_anova_table.doc")
















#################################################### percent difference biomass



############################ Assumptions

### Normality test

shapiro_pd = pd_data %>%
  group_by(Bioassay, Treatment) %>%
  filter(sd(B_pd_sc, na.rm = TRUE) > 0) %>%
  shapiro_test(B_pd_sc)

print(shapiro_pd, n = 60)

capture.output(print(shapiro_delta, n=60), file = "shapiro_table.doc")

### Homogeneity of variance

levene_pd = pd_data %>% 
  group_by(Bioassay, Nutrients) %>%
  filter(sd(B_pd_sc, na.rm = TRUE) > 0) %>%
  levene_test(B_pd_sc ~ Treatment) 

print(levene_pd, n = 60)

capture.output(print(shapiro_pd, n=60), print(levene_pd), file = "pd_assumptions_table.doc")



############################ ANOVAs


carb_no_nut_pd = pd_data %>%
  filter(Bioassay == "Carbamazepine",
         Nutrients == "no")
carb_nut_pd = pd_data %>%
  filter(Bioassay == "Carbamazepine",
         Nutrients == "yes")

dic_no_nut_pd = pd_data %>%
  filter(Bioassay == "Diclofenac",
         Nutrients == "no")
dic_nut_pd = pd_data %>%
  filter(Bioassay == "Diclofenac",
         Nutrients == "yes")

pfos_no_nut_pd = pd_data %>%
  filter(Bioassay == "PFOS",
         Nutrients == "no")
pfos_nut_pd = pd_data %>%
  filter(Bioassay == "PFOS",
         Nutrients == "yes")

quin_no_nut_pd = pd_data %>%
  filter(Bioassay == "6ppd-q",
         Nutrients == "no")
quin_nut_pd = pd_data %>%
  filter(Bioassay == "6ppd-q",
         Nutrients == "yes")


carb_no_nut_ANOVA_pd <- aov(B_pd_sc ~ Treatment, data = carb_no_nut_pd)
summary(carb_no_nut_ANOVA_pd)
carb_no_nut_Tukey_pd = TukeyHSD(carb_no_nut_ANOVA_pd, "Treatment")
print(carb_no_nut_Tukey_pd)

carb_nut_ANOVA_pd <- aov(B_pd_sc ~ Treatment, data = carb_nut_pd)
summary(carb_nut_ANOVA_pd)
carb_nut_Tukey_pd = TukeyHSD(carb_nut_ANOVA_pd, "Treatment")
print(carb_nut_Tukey_pd)



dic_no_nut_ANOVA_pd <- aov(B_pd_sc ~ Treatment, data = dic_no_nut_pd)
summary(dic_no_nut_ANOVA_pd)
dic_no_nut_Tukey_pd = TukeyHSD(dic_no_nut_ANOVA_pd, "Treatment")
print(dic_no_nut_Tukey_pd)

dic_nut_ANOVA_pd <- aov(B_pd_sc ~ Treatment, data = dic_nut_pd)
summary(dic_nut_ANOVA_pd)
dic_nut_Tukey_pd = TukeyHSD(dic_nut_ANOVA_pd, "Treatment")
print(dic_nut_Tukey_pd)



pfos_no_nut_ANOVA_pd <- aov(B_pd_sc ~ Treatment, data = pfos_no_nut_pd)
summary(pfos_no_nut_ANOVA_pd)
pfos_no_nut_Tukey_pd = TukeyHSD(pfos_no_nut_ANOVA_pd, "Treatment")
print(pfos_no_nut_Tukey_pd)

pfos_nut_ANOVA_pd <- aov(B_pd_sc ~ Treatment, data = pfos_nut_pd)
summary(pfos_nut_ANOVA_pd)
pfos_nut_Tukey_pd = TukeyHSD(pfos_nut_ANOVA_pd, "Treatment")
print(pfos_nut_Tukey_pd)



quin_no_nut_ANOVA_pd <- aov(B_pd_sc ~ Treatment, data = quin_no_nut_pd)
summary(quin_no_nut_ANOVA_pd)
quin_no_nut_Tukey_pd = TukeyHSD(quin_no_nut_ANOVA_pd, "Treatment")
print(quin_no_nut_Tukey_pd)

quin_nut_ANOVA_pd <- aov(B_pd_sc ~ Treatment, data = quin_nut_pd)
summary(quin_nut_ANOVA_pd)
quin_nut_Tukey_pd = TukeyHSD(quin_nut_ANOVA_pd, "Treatment")
print(quin_nut_Tukey_pd)


capture.output(summary(carb_no_nut_ANOVA_pd), print(carb_no_nut_Tukey_pd), summary(carb_nut_ANOVA_pd), print(carb_nut_Tukey_pd), summary(pfos_no_nut_ANOVA_pd), print(pfos_no_nut_Tukey_pd), summary(pfos_nut_ANOVA_pd), print(pfos_nut_Tukey_pd),  summary(quin_no_nut_ANOVA_pd), print(quin_no_nut_Tukey_pd), summary(quin_nut_ANOVA_pd), print(quin_nut_Tukey_pd),  file = "pd_anova_table.doc")




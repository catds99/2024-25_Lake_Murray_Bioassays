# 2025-05-20
# non parametric statistics for contaminant bioassays
# re-testing woth continuous no high outlier data, previsouly used 2024 bioassay pigment data

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)
library(agricolae)


############################################## import data

all_data = read_excel("All_Bioassays/continuous_no_high_outliers.xlsx", sheet = "Biomass_total_group")

carb = all_data %>%
  filter(Other == "Carbamazepine")
dic = all_data %>%
  filter(Other == "Diclofenac")
pfos = all_data %>%
  filter(Other == "PFOS")
quin = all_data %>%
  filter(Other == "6ppd-q")


  ############################################## Welch's ANOVAs based on biomass
  
  ################################# Carbamazepine
  
  carb_outliers = carb %>%
  group_by(Treatment) %>%
  mutate(
    Q1 = quantile(ALL_Chl_a, 0.25),
    Q3 = quantile(ALL_Chl_a, 0.75),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR,
    Outlier = ifelse(ALL_Chl_a < Lower_Bound | ALL_Chl_a > Upper_Bound, "Yes", "No")
  )
  
  print(carb_outliers)

oneway.test(ALL_Chl_a ~ Treatment,
            data = carb,
            var.equal = FALSE)

# One-way analysis of means (not assuming equal variances)
# data:  ALL_Chl_a and Treatment
# F = 329.98, num df = 8.000, denom df = 14.791, p-value = 2.881e-15

carb_posthoc = games_howell_test(formula = ALL_Chl_a ~ Treatment,
                                 data = carb, 
                                 conf.level = 0.95, 
                                 detailed = FALSE)
print(carb_posthoc, n = 36)

# sig differences:
# 13 ALL_Chl_a B               F          -1.59      -2.80     -0.386 0.011      *   
# 15 ALL_Chl_a B               T0         -11.7      -12.7     -10.7  0       ****  
# 21 ALL_Chl_a C               T0         -11.1      -12.5     -9.78  4.06e-7 **** 
# 30 ALL_Chl_a D               T0         -10.5      -11.8     -9.11  6.41e-7 ****
# 33 ALL_Chl_a E               T0         -10.5      -12.2     -8.87  4.87e-6 ****
# 35 ALL_Chl_a F               T0         -10.1      -11.3     -8.96  8.2 e-8 ****
# Most treatment groups biomass are significantly greater than T0 but not the control or solvent control

################################# Diclofenac

oneway.test(ALL_Chl_a ~ Treatment,
            data = dic,
            var.equal = FALSE)

# One-way analysis of means (not assuming equal variances)
# data:  ALL_Chl_a and Treatment
# F = 1057.8, num df = 8.000, denom df = 13.179, p-value < 2.2e-16

dic_posthoc = games_howell_test(formula = ALL_Chl_a ~ Treatment,
                                data = dic, 
                                conf.level = 0.95, 
                                detailed = FALSE)
print(dic_posthoc, n = 36)

# sig differences:
#  8 ALL_Chl_a A               T0         -13.4     -17.5      -9.37  4.98e-4 *** 
# 15 ALL_Chl_a B               T0         -13.1     -15.1     -11.1   5.12e-5 **** 
# 21 ALL_Chl_a C               T0         -13.1     -14.8     -11.4   1.96e-5 ****
# 26 ALL_Chl_a Control         T0         -10.9     -13.9      -7.92  3.28e-4 ***
# 30 ALL_Chl_a D               T0         -12.8     -13.9     -11.7   1.10e-6 ****
# 33 ALL_Chl_a E               T0         -12.6     -13.6     -11.6   1.08e-6 ****
# 35 ALL_Chl_a F               T0         -12.3     -14.0     -10.6   2.62e-5 ****
# 36 ALL_Chl_a Solvent_Control T0         -11.2     -19.9      -2.44  2.5 e-2 *
# All treatment groups and both controls biomass are significantly greater than T0, but not different from each other

################################# PFOS

pfos_outliers = pfos %>%
  group_by(Treatment) %>%
  mutate(
    Q1 = quantile(ALL_Chl_a, 0.25),
    Q3 = quantile(ALL_Chl_a, 0.75),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR,
    Outlier = ifelse(ALL_Chl_a < Lower_Bound | ALL_Chl_a > Upper_Bound, "Yes", "No")
  )


aggregate(ALL_Chl_a ~ Treatment, data = pfos, FUN = mean)

# Treatment ALL_Chl_a
# 1               A 12.432630
# 2               B 12.748978
# 3               C 13.212233
# 4         Control 16.221472
# 5               D 12.762357
# 6               E 13.203647
# 7               F 11.252106
# 8 Solvent_Control 19.462798
# 9              T0  5.986621

oneway.test(ALL_Chl_a ~ Treatment,
            data = pfos,
            var.equal = FALSE)

# One-way analysis of means (not assuming equal variances)
# data:  ALL_Chl_a and Treatment
# F = 67.306, num df = 8.000, denom df = 13.851, p-value = 8.989e-10

pfos_posthoc = games_howell_test(formula = ALL_Chl_a ~ Treatment,
                                 data = pfos, 
                                 conf.level = 0.95, 
                                 detailed = FALSE)
print(pfos_posthoc, n = 36)

# sig differences:
#  3 ALL_Chl_a A               Control     3.79e+0   0.817     6.76   1.6 e-2 * 
#  8 ALL_Chl_a A               T0         -6.45e+0  -8.03     -4.86   4.46e-6 ****  
# 10 ALL_Chl_a B               Control     3.47e+0   0.0964    6.85   4.3 e-2 *     
# 15 ALL_Chl_a B               T0         -6.76e+0  -9.69     -3.83   9.55e-4 ***
# 21 ALL_Chl_a C               T0         -7.23e+0  -8.48     -5.97   2.09e-6 ****
# 24 ALL_Chl_a Control         F          -4.97e+0  -9.01     -0.924  1.7 e-2 *
# 26 ALL_Chl_a Control         T0         -1.02e+1 -13.2      -7.25   1.25e-4 ***
# 30 ALL_Chl_a D               T0         -6.78e+0 -10.0      -3.53   2   e-3 **
# 33 ALL_Chl_a E               T0         -7.22e+0 -10.6      -3.82   2   e-3 **
# 35 ALL_Chl_a F               T0         -5.27e+0  -9.24     -1.29   1.6 e-2 *

# All treatment groups and control but not solvent control biomass are significantly greater than T0
#A and B greater than control, (control no different from C, D, E, treatments no different from each other), but control greater than F
# but this doesn't add up because the control mean is greater than A and B


################################# 6ppd-quinone

aggregate(ALL_Chl_a ~ Treatment, data = quin, FUN = mean)

# Treatment ALL_Chl_a
# 1               A 13.413281
# 2               B 10.984801
# 3               C 10.295122
# 4         Control 12.659905
# 5               D 12.257672
# 6               E 11.599343
# 7               F  9.550015
# 8 Solvent_Control 18.044281
# 9              T0  5.882495

oneway.test(ALL_Chl_a ~ Treatment,
            data = quin,
            var.equal = FALSE)

# One-way analysis of means (not assuming equal variances)
# data:  ALL_Chl_a and Treatment
# F = 11.642, num df = 8.000, denom df = 14.143, p-value = 5.097e-05

quin_posthoc = games_howell_test(formula = ALL_Chl_a ~ Treatment,
                                 data = quin, 
                                 conf.level = 0.95, 
                                 detailed = FALSE)
print(quin_posthoc, n = 36)



# sig differences:
#  8 ALL_Chl_a A               T0           -7.53     -14.9   -0.155  4.6 e-2 *
# 15 ALL_Chl_a B               T0           -5.10     -7.73    -2.48  2   e-3 **
# 21 ALL_Chl_a C               T0           -4.41     -7.60    -1.23  8   e-3 **
# 30 ALL_Chl_a D               T0           -6.38     -9.59    -3.17  7.38e-4 ***
# 33 ALL_Chl_a E               T0           -5.72     -8.43    -3.00  5.92e-4 ***
# 36 ALL_Chl_a Solvent_Control T0           -9.28     -15.1    -3.46  0.008    **          

# Most treatment groups biomass are significantly greater than T0, but controls are not
# not significantly greater than control

kruskal.test(ALL_Chl_a ~ Treatment, data = quin)

pairwise.wilcox.test(quin$ALL_Chl_a, quin$Treatment,
                     p.adjust.method = "BH")

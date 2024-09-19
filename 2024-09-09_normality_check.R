#2024-09-09
#CDS
#PAM data from 6ppd-q Bioassay 1 (2024/08/21-24)

library(tidyverse)
library(ggpubr)
library(rstatix)
library(readxl)
library(car)


#import data:

Q_B1_TCA = read_excel("6ppd-q/Data/HPLC_6.xlsx", sheet = 1)
PFOS_B1_TCA = read_excel("PFOS/Data/HPLC_P.xlsx", sheet = 1)
Dic_B1_TCA = read_excel("Diclofenac/Data/HPLC_D.xlsx", sheet = 1)
Carb_B1_TCA = read_excel("Carbamazepine/Data/HPLC_C.xlsx", sheet = 1)


####################################################################### Normality - Shapiro Wilks test by group

###################### 6ppd Q

Q_B1_TCA %>%
  group_by(Sample) %>%
  shapiro_test(Total_Chl_a)

#Sample            variable    statistic      p
#<chr>             <chr>           <dbl>  <dbl>
#1 Control           Total_Chl_a     0.770 0.0453     not normal
#2 Methanol          Total_Chl_a     0.735 0.0215     not normal
#3 T0                Total_Chl_a     0.723 0.0164     not normal
#4 fifty             Total_Chl_a     0.932 0.608      normal 
#5 hundred           Total_Chl_a     0.969 0.867      normal
#6 hundredtwentyfive Total_Chl_a     0.974 0.901      normal
#7 seventyfive       Total_Chl_a     0.855 0.210      normal
#8 ten               Total_Chl_a     0.850 0.195      normal
#9 twentyfive        Total_Chl_a     0.812 0.100      normal

#p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. 
#In other words, we can assume the normality.

###################### PFOS

PFOS_B1_TCA %>%
  group_by(Sample) %>%
  shapiro_test(Total_Chl_a)

#Sample            variable    statistic      p
#<chr>             <chr>           <dbl>  <dbl>
#1 Control           Total_Chl_a     0.977 0.915      normal
#2 Methanol          Total_Chl_a     0.749 0.0291     not normal
#3 T0                Total_Chl_a     0.966 0.851      normal
#4 fifty             Total_Chl_a     0.898 0.397      normal
#5 hundred           Total_Chl_a     0.794 0.0730     normal
#6 hundredtwentyfive Total_Chl_a     0.804 0.0877     normal
#7 seventyfive       Total_Chl_a     0.933 0.617      normal
#8 ten               Total_Chl_a     0.916 0.506      normal
#9 twentyfive        Total_Chl_a     0.838 0.160      normal

#p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. 
#In other words, we can assume the normality.

###################### Diclofenac

Dic_B1_TCA %>%
  group_by(Sample) %>%
  shapiro_test(Total_Chl_a)

#  Sample            variable    statistic       p
#<chr>             <chr>           <dbl>   <dbl>
#1 Acetone           Total_Chl_a     0.620 0.00113    not normal
#2 Control           Total_Chl_a     0.908 0.454      normal
#3 T0                Total_Chl_a     0.916 0.504      normal
#4 fifty             Total_Chl_a     0.865 0.246      normal
#5 hundred           Total_Chl_a     0.846 0.183      normal
#6 hundredtwentyfive Total_Chl_a     0.949 0.732      normal
#7 seventyfive       Total_Chl_a     0.978 0.924      normal
#8 ten               Total_Chl_a     0.928 0.580      normal 
#9 twentyfive        Total_Chl_a     0.765 0.0406     not normal

#p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. 
#In other words, we can assume the normality.

###################### Carbamazepine

Carb_B1_TCA %>%
  group_by(Sample) %>%
  shapiro_test(Total_Chl_a)

#  Sample            variable    statistic      p
#<chr>             <chr>           <dbl>  <dbl>
#1 Acetone           Total_Chl_a     0.843 0.174      normal
#2 Control           Total_Chl_a     0.973 0.892      normal
#3 T0                Total_Chl_a     0.872 0.277      normal
#4 fifty             Total_Chl_a     0.836 0.153      normal
#5 hundred           Total_Chl_a     0.724 0.0169     not normal
#6 hundredtwentyfive Total_Chl_a     0.815 0.106      normal
#7 seventyfive       Total_Chl_a     0.919 0.523      normal
#8 ten               Total_Chl_a     0.885 0.332      normal
#9 twentyfive        Total_Chl_a     0.909 0.460      normal

#p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. 
#In other words, we can assume the normality.




















####################################################################### Homogeneity of Variances - Levenes test

#p-values less than 0.05 suggest variances are significantly different 
#and the homogeneity of variance assumption has been violated.

###################### 6ppd-q

leveneTest(Total_Chl_a ~ Sample, data = Q_B1_TCA)

# F = 0.9445, p = 0.492 -> homogeneity of variances - passes

###################### PFOS

leveneTest(Total_Chl_a ~ Sample, data = PFOS_B1_TCA)

# F = 1.5393, p = 0.1783 -> homogeneity of variances - passes

###################### Diclofenac

leveneTest(Total_Chl_a ~ Sample, data = Dic_B1_TCA)

# F = 1.1555, p = 0.352 -> homogeneity of variances - passes

###################### Carbamazepine

leveneTest(Total_Chl_a ~ Sample, data = Carb_B1_TCA)

# F = 2.2349, p = 0.04752 -> no homogeneity of variances - doesn't pass



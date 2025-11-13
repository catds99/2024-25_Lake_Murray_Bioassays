# 2025-03-20 
# non parametric statistics for contaminant bioassays
# kruskal wallis
# permanova
# nmds

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)
library(agricolae)
library(vegan)
library(grid)
library(gtable)
library(patchwork)

############################################## import data

no_outliers = read_excel("All_Bioassays/continuous_no_high_outliers.xlsx", sheet = 2)

carb_x = no_outliers %>%
  filter(Other == "Carbamazepine")
dic_x = no_outliers %>%
  filter(Other == "Diclofenac")
pfos_x = no_outliers %>%
  filter(Other == "PFOS")
quin_x = no_outliers %>%
  filter(Other == '6ppd-Quinone')

no_outliers$Treatment <- factor(no_outliers$Treatment,
                           levels = c("T0", "Control", "Solvent_Control", "A", "B", "C", "D", "E", "F"),
                           labels = c("Time Zero", "Control", "Solvent Control", "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))

no_outliers$Other <- factor(no_outliers$Other,
                          levels = c("Carbamazepine", "Diclofenac", "PFOS", "6ppd-q"),
                          labels = c("Carbamazepine", "Diclofenac", "PFOS", "6ppd-Quinone"))


############################################## combined boxplot


total_chla_boxplot_square = ggplot(data = no_outliers, aes(x = Treatment, y = ALL_Chl_a)) +
  geom_boxplot(fill = "lightcyan3") +
  facet_wrap(~Other, ncol = 2) +
  xlab("Treatment") +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) +
  theme_classic(base_size = 35) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1.07))

ggsave(total_chla_boxplot_square, filename = "All_Bioassays/Figures/total_chla_boxplot_square.png",
       device = "png", height = 18, width = 24)


############################################## 6ppd quinone boxplot


quin_boxplot = ggplot(data = quin_x, aes(x = Treatment, y = ALL_Chl_a)) +
  geom_boxplot(fill = "lightgrey") +
  xlab("Treatment") +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) +
  theme_classic(base_size = 35) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1.07))

ggsave(quin_boxplot, filename = "6ppd-q/Figures/quin_boxplot.png",
       device = "png", height = 18, width = 24)


############################################## scatter plots with trendline

ggplot(carb_x, aes(x=concentration, y=ALL_Chl_a)) + 
  geom_point() +
  geom_smooth() +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab(expression(paste("Carbamezepine \u03BCg", l^-1))) + 
  theme_classic()

ggplot(dic_x, aes(x=concentration, y=ALL_Chl_a)) + 
  geom_point() +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab(expression(paste("Diclofenac \u03BCg", l^-1))) + 
  geom_smooth() +
  theme_classic()

ggplot(pfos_x, aes(x=concentration, y=ALL_Chl_a)) + 
  geom_point() +ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab(expression(paste("PFOS \u03BCg", l^-1))) + 
  geom_smooth() +
  theme_classic()

ggplot(quin_x, aes(x=concentration, y=ALL_Chl_a)) + 
  geom_point() +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab(expression(paste("6ppd-q ng", l^-1))) + 
  geom_smooth() +
  theme_classic()


############################################## Kruskal Wallis based on biomass

################################# Carbamazepine

kruskal.test(ALL_Chl_a ~ Treatment, data = carb_x)

# Kruskal-Wallis rank sum test

# data:  ALL_Chl_a by Treatment
# Kruskal-Wallis chi-squared = 26.725, df = 8, p-value = 0.0007887

pairwise.wilcox.test(carb_x$ALL_Chl_a, carb_x$Treatment,
                     p.adjust.method = "BH")

# Pairwise comparisons using Wilcoxon rank sum exact test 

# data:  carb_x$ALL_Chl_a and carb_x$Treatment 

#                  Time Zero Control Solvent Control 10% Max 25% Max 50% Max 75% Max 100% Max
#  Control         0.026     -       -               -       -       -       -       -       
#  Solvent Control 0.125     0.616   -               -       -       -       -       -       
#  10% Max         0.026     0.429   0.616           -       -       -       -       -       
#  25% Max         0.026     0.026   0.171           0.333   -       -       -       -       
#  50% Max         0.026     0.044   0.333           0.616   0.259   -       -       -       
#  75% Max         0.026     0.125   0.541           1.000   0.026   0.171   -       -       
#  100% Max        0.026     0.044   0.333           0.891   0.026   0.171   1.000   -       
#  125% Max        0.026     0.171   0.616           0.891   0.026   0.082   0.429   0.541   

#  P value adjustment method: BH 

# nothing is different from the solvent control, a few are higher than the control, most are higher than time zero

################################# Diclofenac

kruskal.test(ALL_Chl_a ~ Treatment, data = dic_x)

# Kruskal-Wallis rank sum test

# data:  ALL_Chl_a by Treatment
# Kruskal-Wallis chi-squared = 23.444, df = 8, p-value = 0.002839

# P value adjustment method: BH 

pairwise.wilcox.test(dic_x$ALL_Chl_a, dic_x$Treatment,
                     p.adjust.method = "BH")

# Pairwise comparisons using Wilcoxon rank sum exact test 

# data:  dic_x$ALL_Chl_a and dic_x$Treatment 

#                  Time Zero Control Solvent Control 10% Max 25% Max 50% Max 75% Max 100% Max
#  Control         0.032     -       -               -       -       -       -       -       
#  Solvent Control 0.048     0.987   -               -       -       -       -       -       
#  10% Max         0.032     0.154   0.464           -       -       -       -       -       
#  25% Max         0.032     0.032   0.429           1.000   -       -       -       -       
#  50% Max         0.032     0.032   0.429           1.000   0.946   -       -       -       
#  75% Max         0.032     0.048   0.464           0.857   1.000   0.857   -       -       
#  100% Max        0.032     0.048   0.464           0.857   0.769   0.464   0.946   -       
#  125% Max        0.032     0.245   0.769           0.464   0.444   0.464   0.444   0.946   

# P value adjustment method: BH 

################################# PFOS

kruskal.test(ALL_Chl_a ~ Treatment, data = pfos_x)

# Kruskal-Wallis rank sum test

# data:  ALL_Chl_a by Treatment
# Kruskal-Wallis chi-squared = 25.96, df = 8, p-value = 0.001067

pairwise.wilcox.test(pfos_x$ALL_Chl_a, pfos_x$Treatment,
                     p.adjust.method = "BH")

# Pairwise comparisons using Wilcoxon rank sum exact test 

# data:  pfos_x$ALL_Chl_a and pfos_x$Treatment 

#                  Time Zero Control Solvent Control 10% Max 25% Max 50% Max 75% Max 100% Max
#  Control         0.022     -       -               -       -       -       -       -       
#  Solvent Control 0.038     0.468   -               -       -       -       -       -       
#  10% Max         0.022     0.022   0.906           -       -       -       -       -       
#  25% Max         0.022     0.022   1.000           0.741   -       -       -       -       
#  50% Max         0.022     0.022   0.468           0.071   1.000   -       -       -       
#  75% Max         0.022     0.022   1.000           0.888   1.000   1.000   -       -       
#  100% Max        0.022     0.038   0.741           0.741   1.000   0.484   1.000   -       
#  125% Max        0.022     0.022   0.741           0.302   0.400   0.022   0.400   0.202   

# P value adjustment method: BH


################################# 6ppd-q

kruskal.test(ALL_Chl_a ~ Treatment, data = quin_x)

# Kruskal-Wallis rank sum test

# data:  ALL_Chl_a by Treatment
# Kruskal-Wallis chi-squared = 25.96, df = 8, p-value = 0.001067

pairwise.wilcox.test(quin_x$ALL_Chl_a, quin_x$Treatment,
                     p.adjust.method = "BH")

# Pairwise comparisons using Wilcoxon rank sum exact test 

# data:  pfos_x$ALL_Chl_a and pfos_x$Treatment 

#                  Time Zero Control Solvent Control 10% Max 25% Max 50% Max 75% Max 100% Max
#  Control         0.022     -       -               -       -       -       -       -       
#  Solvent Control 0.038     0.468   -               -       -       -       -       -       
#  10% Max         0.022     0.022   0.906           -       -       -       -       -       
#  25% Max         0.022     0.022   1.000           0.741   -       -       -       -       
#  50% Max         0.022     0.022   0.468           0.071   1.000   -       -       -       
#  75% Max         0.022     0.022   1.000           0.888   1.000   1.000   -       -       
#  100% Max        0.022     0.038   0.741           0.741   1.000   0.484   1.000   -       
#  125% Max        0.022     0.022   0.741           0.302   0.400   0.022   0.400   0.202   

# P value adjustment method: BH


############################################## permanovas based on biomass


carb_permanova = adonis2(carb_x[ , c("Cyanobacteria", "GreenAlgae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes")] ~ Treatment,
                         data = carb_x,
                         method = "euc")

print(carb_permanova)

carb_permanova_b = adonis2(carb_x[ , c("Cyanobacteria", "GreenAlgae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes")] ~ Treatment,
                         data = carb_x,
                         method = "bray")

print(carb_permanova_b)


dic_permanova = adonis2(dic_x[ , c("Cyanobacteria", "GreenAlgae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes")] ~ Treatment,
                        data = dic_x,
                        method = "euc")

print(dic_permanova)

dic_permanova_b = adonis2(dic_x[ , c("Cyanobacteria", "GreenAlgae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes")] ~ Treatment,
                        data = dic_x,
                        method = "bray")

print(dic_permanova_b)

pfos_permanova = adonis2(pfos_x[ , c("Cyanobacteria", "GreenAlgae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes")] ~ Treatment,
                         data = pfos_x,
                         method = "euc")

print(pfos_permanova)

pfos_permanova_b = adonis2(pfos_x[ , c("Cyanobacteria", "GreenAlgae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes")] ~ Treatment,
                         data = pfos_x,
                         method = "bray")

print(pfos_permanova_b)

quin_permanova = adonis2(quin_x[ , c("Cyanobacteria", "GreenAlgae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes")] ~ Treatment,
                         data = quin_x,
                         method = "euc")

print(quin_permanova)

quin_permanova_b = adonis2(quin_x[ , c("Cyanobacteria", "GreenAlgae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes")] ~ Treatment,
                         data = quin_x,
                         method = "bray")

print(quin_permanova_b)


############################################## NMDS based on biomass

################################# carbamazepine

head(carb_x)
carb_x_2 = carb_x[,6:11]
head(carb_x_2)
m_carb_x_2 = as.matrix(carb_x_2)


set.seed(3)
carb_nmds_2 = metaMDS(m_carb_x_2, distance = "bray")
carb_nmds_2

carb_data_scores = as.data.frame(scores(carb_nmds_2)$sites)
carb_data_scores$Treatment = carb_x$Treatment
head(carb_data_scores)

carb_data_scores$Bioassay <- 'Carbamazepine'


xx = ggplot(carb_data_scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Treatment)) + 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), 
        legend.key=element_blank()) + 
  annotate("text", x = Inf, y = -0.35, label = "Stress = 0.08", hjust = -.5, size = 5) +
  labs(x = "NMDS1", color = "Treatment", y = "NMDS2")  + 
  scale_colour_manual(values = c("black", "lightgrey", "darkgrey", "firebrick2", "darkorange", "gold", "forestgreen", "deepskyblue2", "orchid2")) +
  theme_classic(base_size = 20)

g = ggplotGrob(xx)
g$layout$clip[g$layout$name == "panel"] = "off"
grid.draw(g)





set.seed(34)
carb_nmds_2b = metaMDS(m_carb_x_2, distance = "euc")
carb_nmds_2b

carb_data_scores_b = as.data.frame(scores(carb_nmds_2b)$sites)
carb_data_scores_b$Treatment = carb_x$Treatment
head(carb_data_scores_b)

carb_data_scores_b$Bioassay <- 'Carbamazepine'


xx_c_b = ggplot(carb_data_scores_b, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Treatment)) + 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), 
        legend.key=element_blank()) + 
  annotate("text", x = Inf, y = -0.35, label = "Stress = 0.08", hjust = -.5, size = 5) +
  labs(x = "NMDS1", color = "Treatment", y = "NMDS2")  + 
  scale_colour_manual(values = c("black", "lightgrey", "darkgrey", "firebrick2", "darkorange", "gold", "forestgreen", "deepskyblue2", "orchid2")) +
  theme_classic(base_size = 20)

g2 = ggplotGrob(xx_c_b)
g2$layout$clip[g2$layout$name == "panel"] = "off"
grid.draw(g2)

stress_c = stressplot(carb_nmds_2b)



################################# diclofenac



head(dic_x)
dic_x_2 = dic_x[,6:ncol(dic_x)]
head(dic_x_2)
m_dic_x_2 = as.matrix(dic_x_2)


set.seed(4)
dic_nmds_2 = metaMDS(m_dic_x_2, distance = "bray")
dic_nmds_2

dic_data_scores = as.data.frame(scores(dic_nmds_2)$sites)
dic_data_scores$Treatment = dic_x$Treatment
head(dic_data_scores)

dic_data_scores$Bioassay <- 'Diclofenac'

xx_d = ggplot(dic_data_scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Treatment)) + 
  labs(x = "NMDS1", color = "Treatment", y = "NMDS2")  + 
  scale_colour_manual(values = c("black", "lightgrey", "darkgrey", "firebrick2", "darkorange", "gold", "forestgreen", "deepskyblue2", "orchid2")) +
  theme_classic(base_size = 20) +
  annotate("text", x = Inf, y = -1.8e-4, label = "Stress = near zero", hjust = -.25, vjust = -.25, size = 5) 
  
gd = ggplotGrob(xx_d)
gd$layout$clip[gd$layout$name == "panel"] = "off"
grid.draw(gd)

set.seed(67)
dic_nmds_2b = metaMDS(m_dic_x_2, distance = "euc")
dic_nmds_2b

dic_data_scores_b = as.data.frame(scores(dic_nmds_2b)$sites)
dic_data_scores_b$Treatment = dic_x$Treatment
head(dic_data_scores_b)

dic_data_scores_b$Bioassay <- 'Diclofenac'

xx_d_b = ggplot(dic_data_scores_b, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Treatment)) + 
  labs(x = "NMDS1", color = "Treatment", y = "NMDS2")  + 
  scale_colour_manual(values = c("black", "lightgrey", "darkgrey", "firebrick2", "darkorange", "gold", "forestgreen", "deepskyblue2", "orchid2")) +
  theme_classic(base_size = 20) +
  annotate("text", x = Inf, y = -1.8e-4, label = "Stress = near zero", hjust = -.25, vjust = -.25, size = 5) 

gd2 = ggplotGrob(xx_d_b)
gd2$layout$clip[gd2$layout$name == "panel"] = "off"
grid.draw(gd2)

stress_d = stressplot(dic_nmds_2b)


stress_vals_d <- data.frame(
  k = 1:6,
  stress = sapply(1:6, function(k) {
    m <- metaMDS(m_dic_x_2, distance = "euc", k = k, trymax = 50, trace = FALSE)
    m$stress
  })
)

# Plot it
library(ggplot2)
ggplot(stress_vals_d, aes(x = k, y = stress)) +
  geom_point(size = 3) +
  geom_line() +
  labs(x = "Number of Dimensions (k)",
       y = "Stress",
       title = "Stress vs. Dimensions (NMDS Scree Plot)") +
  theme_classic(base_size = 16)


################################# pfos




head(pfos_x)
pfos_x_2 = pfos_x[,6:ncol(pfos_x)]
head(pfos_x_2)
m_pfos_x_2 = as.matrix(pfos_x_2)


set.seed(5)
pfos_nmds_2 = metaMDS(m_pfos_x_2, distance = "bray")
pfos_nmds_2

pfos_data_scores = as.data.frame(scores(pfos_nmds_2)$sites)
pfos_data_scores$Treatment = pfos_x$Treatment
head(pfos_data_scores)

pfos_data_scores$Bioassay <- 'PFOS'

xx_p = ggplot(pfos_data_scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Treatment)) + 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), 
        legend.key=element_blank()) + 
  annotate("text", x = Inf, y = -0.125, label = "Stress = 0.1", hjust = -.6, size = 5) +
  labs(x = "NMDS1", color = "Treatment", y = "NMDS2")  + 
  scale_colour_manual(values = c("black", "lightgrey", "darkgrey", "firebrick2", "darkorange", "gold", "forestgreen", "deepskyblue2", "orchid2")) +
  theme_classic(base_size = 20)

gp = ggplotGrob(xx_p)
gp$layout$clip[gp$layout$name == "panel"] = "off"
grid.draw(gp)


set.seed(58)
pfos_nmds_2b = metaMDS(m_pfos_x_2, distance = "euc")
pfos_nmds_2b

pfos_data_scores_b = as.data.frame(scores(pfos_nmds_2b)$sites)
pfos_data_scores_b$Treatment = pfos_x$Treatment
head(pfos_data_scores_b)

pfos_data_scores_b$Bioassay <- 'PFOS'

xx_p_b = ggplot(pfos_data_scores_b, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Treatment)) + 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), 
        legend.key=element_blank()) + 
  annotate("text", x = Inf, y = -0.125, label = "Stress = 0.1", hjust = -.6, size = 5) +
  labs(x = "NMDS1", color = "Treatment", y = "NMDS2")  + 
  scale_colour_manual(values = c("black", "lightgrey", "darkgrey", "firebrick2", "darkorange", "gold", "forestgreen", "deepskyblue2", "orchid2")) +
  theme_classic(base_size = 20)

gp2 = ggplotGrob(xx_p_b)
gp2$layout$clip[gp2$layout$name == "panel"] = "off"
grid.draw(gp2)

stress_p = stressplot(pfos_nmds_2b)


################################# 6ppd-q



head(quin_x)
quin_x_2 = quin_x[,6:ncol(quin_x)]
head(quin_x_2)
m_quin_x_2 = as.matrix(quin_x_2)


set.seed(6)
quin_nmds_2 = metaMDS(m_quin_x_2, distance = "bray")
quin_nmds_2

quin_data_scores = as.data.frame(scores(quin_nmds_2)$sites)
quin_data_scores$Treatment = quin_x$Treatment
head(quin_data_scores)

quin_data_scores$Bioassay <- '6ppd-q'

xx_q = ggplot(quin_data_scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Treatment)) + 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), 
        legend.key=element_blank()) + 
  annotate("text", x = Inf, y = -0.14, label = "Stress = 0.03", hjust = -.6, size = 5) +
  labs(x = "NMDS1", color = "Treatment", y = "NMDS2")  + 
  scale_colour_manual(values = c("black", "lightgrey", "darkgrey", "firebrick2", "darkorange", "gold", "forestgreen", "deepskyblue2", "orchid2")) +
  theme_classic(base_size = 20)

gq = ggplotGrob(xx_q)
gq$layout$clip[gq$layout$name == "panel"] = "off"
grid.draw(gq)





set.seed(69)
quin_nmds_2b = metaMDS(m_quin_x_2, distance = "bray")
quin_nmds_2b

quin_data_scores_b = as.data.frame(scores(quin_nmds_2b)$sites)
quin_data_scores_b$Treatment = quin_x$Treatment
head(quin_data_scores_b)

quin_data_scores_b$Bioassay <- '6ppd-q'

xx_q_b = ggplot(quin_data_scores_b, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Treatment)) + 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), 
        legend.key=element_blank()) + 
  annotate("text", x = Inf, y = -0.14, label = "Stress = 0.03", hjust = -.6, size = 5) +
  labs(x = "NMDS1", color = "Treatment", y = "NMDS2")  + 
  scale_colour_manual(values = c("black", "lightgrey", "darkgrey", "firebrick2", "darkorange", "gold", "forestgreen", "deepskyblue2", "orchid2")) +
  theme_classic(base_size = 20)

gq2 = ggplotGrob(xx_q_b)
gq2$layout$clip[gq2$layout$name == "panel"] = "off"
grid.draw(gq2)

stress_q = stressplot(quin_nmds_2b)





combined = rbind(carb_data_scores, dic_data_scores, pfos_data_scores, quin_data_scores)

combined$Bioassay <- factor(combined$Bioassay,
                                    levels = c("Carbamazepine", "Diclofenac", "PFOS", "6ppd-q"))

all = ggplot(combined, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Treatment)) + 
  facet_wrap(~ Bioassay) +
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), 
        legend.key=element_blank()) + 
  annotate("text", x = Inf, y = -0.14, label = "Stress = 0.03", hjust = -.6, size = 5) +
  labs(x = "NMDS1", color = "Treatment", y = "NMDS2")  + 
  scale_colour_manual(values = c("black", "lightgrey", "darkgrey", "firebrick2", "darkorange", "gold", "forestgreen", "deepskyblue2", "orchid2")) +
  theme_classic(base_size = 30)
  
ggsave(all, filename = "All_Bioassays/Figures/biomass_nmds_combined.png",
       device = "png", height = 12, width = 17)






combined_2 = rbind(carb_data_scores_b, dic_data_scores_b, pfos_data_scores_b, quin_data_scores_b)

combined_2$Bioassay <- factor(combined_2$Bioassay,
                            levels = c("Carbamazepine", "Diclofenac", "PFOS", "6ppd-q"))

all_b = ggplot(combined_2, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Treatment)) + 
  facet_wrap(~ Bioassay) +
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), 
        legend.key=element_blank()) + 
  annotate("text", x = Inf, y = -0.14, label = "Stress = 0.03", hjust = -.6, size = 5) +
  labs(x = "NMDS1", color = "Treatment", y = "NMDS2")  + 
  scale_colour_manual(values = c("black", "lightgrey", "darkgrey", "firebrick2", "darkorange", "gold", "forestgreen", "deepskyblue2", "orchid2")) +
  theme_classic(base_size = 30)

ggsave(all_b, filename = "All_Bioassays/Figures/biomass_nmds_combined_b.png",
       device = "png", height = 12, width = 17)


(stress_c | stress_d) / (stress_p | stress_q) 


############################################## Kruskal Wallis based on percent change

################################# Carbamazepine

kruskal.test(chl_a_pc ~ Treatment, data = carb_pc)

# Kruskal-Wallis rank sum test

# data:  chl_a_pc by Treatment
# Kruskal-Wallis chi-squared = 12.819, df = 5, p-value = 0.02513

pairwise.wilcox.test(carb_pc$chl_a_pc, carb_pc$Treatment,
                     p.adjust.method = "BH")

# Pairwise comparisons using Wilcoxon rank sum exact test 

# data:  carb_pc$chl_a_pc and carb_pc$Treatment 

#           10% Max 25% Max 50% Max 75% Max 100% Max
#  25% Max  0.42    -       -       -       -       
#  50% Max  0.75    0.32    -       -       -       
#  75% Max  1.00    0.04    0.24    -       -       
#  100% Max 0.97    0.04    0.24    1.00    -       
#  125% Max 0.97    0.04    0.12    0.52    0.63    

# P value adjustment method: BH 

# 25% greater than 75, 100, 125

################################# Diclofenac

kruskal.test(chl_a_pc ~ Treatment, data = dic_pc)

# Kruskal-Wallis rank sum test

# data:  chl_a_pc by Treatment
# Kruskal-Wallis chi-squared = 3.1058, df = 5, p-value = 0.6837

pairwise.wilcox.test(dic_pc$chl_a_pc, dic_pc$Treatment,
                     p.adjust.method = "BH")



################################# PFOS

kruskal.test(chl_a_pc ~ Treatment, data = pfos_pc)

# Kruskal-Wallis rank sum test

# data:  chl_a_pc by Treatment
# Kruskal-Wallis chi-squared = 7.5239, df = 5, p-value = 0.1845

pairwise.wilcox.test(pfos_pc$chl_a_pc, pfos_pc$Treatment,
                     p.adjust.method = "BH")



################################# 6ppd-q

kruskal.test(chl_a_pc ~ Treatment, data = quin_pc)

#Kruskal-Wallis rank sum test

# data:  chl_a_pc by Treatment
# Kruskal-Wallis chi-squared = 9.1755, df = 5, p-value = 0.1023

pairwise.wilcox.test(quin_pc$chl_a_pc, quin_pc$Treatment,
                     p.adjust.method = "BH")

# Pairwise comparisons using Wilcoxon rank sum exact test 

# data:  pfos_x$ALL_Chl_a and pfos_x$Treatment 

#                  Time Zero Control Solvent Control 10% Max 25% Max 50% Max 75% Max 100% Max
#  Control         0.022     -       -               -       -       -       -       -       
#  Solvent Control 0.038     0.468   -               -       -       -       -       -       
#  10% Max         0.022     0.022   0.906           -       -       -       -       -       
#  25% Max         0.022     0.022   1.000           0.741   -       -       -       -       
#  50% Max         0.022     0.022   0.468           0.071   1.000   -       -       -       
#  75% Max         0.022     0.022   1.000           0.888   1.000   1.000   -       -       
#  100% Max        0.022     0.038   0.741           0.741   1.000   0.484   1.000   -       
#  125% Max        0.022     0.022   0.741           0.302   0.400   0.022   0.400   0.202   

# P value adjustment method: BH

############################################## permanovas based on percent change

install.packages("RVAideMemoire")
library(RVAideMemoire)

no_outliers_pc = read_excel("All_Bioassays/continuous_no_high_outliers.xlsx", sheet = 3)

carb_pc = no_outliers_pc %>%
  filter(Other == "Carbamazepine")
dic_pc = no_outliers_pc %>%
  filter(Other == "Diclofenac")
pfos_pc = no_outliers_pc %>%
  filter(Other == "PFOS")
quin_pc = no_outliers_pc %>%
  filter(Other == "6ppd-Quinone")

no_outliers_pc$Treatment <- factor(no_outliers_pc$Treatment,
                                levels = c("A", "B", "C", "D", "E", "F"),
                                labels = c("10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))

no_outliers_pc$Other <- factor(no_outliers_pc$Other,
                            levels = c("Carbamazepine", "Diclofenac", "PFOS", "6ppd-q"),
                            labels = c("Carbamazepine", "Diclofenac", "PFOS", "6ppd-Quinone"))

carb_pc_permanova = adonis2(carb_pc[ , c("Cyanobacteria_pc", "GreenAlgae_pc", "Cryptophytes_pc", "Diatoms_pc", "Dinoflagellates_pc", "Haptophytes_pc")] ~ Treatment,
                         data = carb_pc,
                         method = "euc")

print(carb_pc_permanova)


dic_pc_permanova = adonis2(dic_pc[ , c("Cyanobacteria_pc", "GreenAlgae_pc", "Cryptophytes_pc", "Diatoms_pc", "Dinoflagellates_pc", "Haptophytes_pc")] ~ Treatment,
                        data = dic_pc,
                        method = "euc")

print(dic_pc_permanova)

pfos_pc_permanova = adonis2(pfos_pc[ , c("Cyanobacteria_pc", "GreenAlgae_pc", "Cryptophytes_pc", "Diatoms_pc", "Dinoflagellates_pc", "Haptophytes_pc")] ~ Treatment,
                         data = pfos_pc,
                         method = "euc")

print(pfos_pc_permanova)

quin_pc_permanova = adonis2(quin_pc[ , c("Cyanobacteria_pc", "GreenAlgae_pc", "Cryptophytes_pc", "Diatoms_pc", "Dinoflagellates_pc", "Haptophytes_pc")] ~ Treatment,
                         data = quin_pc,
                         method = "euc")

print(quin_pc_permanova)





################################# carbamazepine 

head()
carb_pc_2 = carb_pc[,6:ncol(carb_pc)]
head(carb_pc_2)
m_carb_pc_2 = as.matrix(carb_pc_2)


set.seed(7)
carb_nmds_pc = metaMDS(m_carb_pc_2, distance = "euclidean")
carb_nmds_pc

carb_data_scores_pc = as.data.frame(scores(quin_nmds_pc)$sites)
carb_data_scores_pc$Treatment = carb_pc$Treatment
head(carb_data_scores_pc)

carb_data_scores_pc = as.data.frame(scores(carb_nmds_pc, display = "sites"))
carb_data_scores_pc$Treatment = carb_pc$Treatment  # Add Treatment info
head(carb_data_scores_pc)

carb_data_scores_pc$Bioassay <- 'Carbamazepine'


xx_pc = ggplot(carb_data_scores_pc, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Treatment)) + 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), 
        legend.key=element_blank()) + 
  annotate("text", x = Inf, y = -0.14, label = "Stress = 0.03", hjust = -.6, size = 5) +
  labs(x = "NMDS1", color = "Treatment", y = "NMDS2")  + 
  scale_colour_manual(values = c("firebrick2", "darkorange", "gold", "forestgreen", "deepskyblue2", "orchid2")) +
  theme_classic(base_size = 20)

gc_pc = ggplotGrob(xx_pc)
gc_pc$layout$clip[gc_pc$layout$name == "panel"] = "off"
grid.draw(gc_pc)


cpc_dist_matrix <- dist(m_carb_pc_2, method = "euclidean")
cpc_pairwise_results <- pairwise.perm.manova(cpc_dist_matrix, carb_pc$Treatment, nperm = 999)
print(cpc_pairwise_results)
cpc_pairwise_results$p.adjusted <- p.adjust(cpc_pairwise_results$p.value, method = "fdr")  # Adjust p-values
print(cpc_pairwise_results)



################################# diclofenac


head(dic_pc)
dic_pc_2 = dic_pc[,6:ncol(dic_pc)]
head(dic_pc_2)
m_dic_pc_2 = as.matrix(dic_pc_2)


set.seed(8)
dic_nmds_pc = metaMDS(m_dic_pc_2, distance = "euclidean")
dic_nmds_pc

dic_data_scores_pc = as.data.frame(scores(dic_nmds_pc)$sites)
dic_data_scores_pc$Treatment = dic_pc$Treatment
head(dic_data_scores_pc)

dic_data_scores_pc = as.data.frame(scores(dic_nmds_pc, display = "sites"))
dic_data_scores_pc$Treatment = dic_pc$Treatment  # Add Treatment info
head(dic_data_scores_pc)

dic_data_scores_pc$Bioassay <- 'Diclofenac'


xx_d_pc = ggplot(dic_data_scores_pc, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Treatment)) + 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), 
        legend.key=element_blank()) + 
  annotate("text", x = Inf, y = -0.14, label = "Stress = 0.03", hjust = -.6, size = 5) +
  labs(x = "NMDS1", color = "Treatment", y = "NMDS2")  + 
  scale_colour_manual(values = c("firebrick2", "darkorange", "gold", "forestgreen", "deepskyblue2", "orchid2")) +
  theme_classic(base_size = 20)

gd_pc = ggplotGrob(xx_d_pc)
gd_pc$layout$clip[gd_pc$layout$name == "panel"] = "off"
grid.draw(gd_pc)


dpc_dist_matrix <- dist(m_dic_pc_2, method = "euclidean")
dpc_pairwise_results <- pairwise.perm.manova(dpc_dist_matrix, dic_pc$Treatment, nperm = 999)
print(dpc_pairwise_results)
dpc_pairwise_results$p.adjusted <- p.adjust(dpc_pairwise_results$p.value, method = "fdr")  # Adjust p-values
print(dpc_pairwise_results)




################################# pfos



head(pfos_pc)
pfos_pc_2 = pfos_pc[,6:ncol(pfos_pc)]
head(pfos_pc_2)
m_pfos_pc_2 = as.matrix(pfos_pc_2)


set.seed(9)
pfos_nmds_pc = metaMDS(m_pfos_pc_2, distance = "euclidean")
pfos_nmds_pc

pfos_data_scores_pc = as.data.frame(scores(pfos_nmds_pc)$sites)
quin_data_scores_pc$Treatment = quin_pc$Treatment
head(quin_data_scores_pc)

pfos_data_scores_pc = as.data.frame(scores(pfos_nmds_pc, display = "sites"))
pfos_data_scores_pc$Treatment = pfos_pc$Treatment  # Add Treatment info
head(pfos_data_scores_pc)

pfos_data_scores_pc$Bioassay <- 'PFOS'


xx_p_pc = ggplot(pfos_data_scores_pc, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Treatment)) + 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), 
        legend.key=element_blank()) + 
  annotate("text", x = Inf, y = -0.14, label = "Stress = 0.03", hjust = -.6, size = 5) +
  labs(x = "NMDS1", color = "Treatment", y = "NMDS2")  + 
  scale_colour_manual(values = c("firebrick2", "darkorange", "gold", "forestgreen", "deepskyblue2", "orchid2")) +
  theme_classic(base_size = 20)

gp_pc = ggplotGrob(xx_p_pc)
gp_pc$layout$clip[gp_pc$layout$name == "panel"] = "off"
grid.draw(gp_pc)


qpc_dist_matrix <- dist(m_quin_pc_2, method = "euclidean")
qpc_pairwise_results <- pairwise.perm.manova(qpc_dist_matrix, quin_pc$Treatment, nperm = 999)
print(qpc_pairwise_results)
qpc_pairwise_results$p.adjusted <- p.adjust(qpc_pairwise_results$p.value, method = "fdr")  # Adjust p-values
print(qpc_pairwise_results)








################################# 6ppd-q



head(quin_pc)
quin_pc_2 = quin_pc[,6:ncol(quin_pc)]
head(quin_pc_2)
m_quin_pc_2 = as.matrix(quin_pc_2)


set.seed(10)
quin_nmds_pc = metaMDS(m_quin_pc_2, distance = "euclidean")
quin_nmds_pc

quin_data_scores_pc = as.data.frame(scores(quin_nmds_pc)$sites)
quin_data_scores_pc$Treatment = quin_pc$Treatment
head(quin_data_scores_pc)

quin_data_scores_pc = as.data.frame(scores(quin_nmds_pc, display = "sites"))
quin_data_scores_pc$Treatment = quin_pc$Treatment  # Add Treatment info
head(quin_data_scores_pc)

quin_data_scores_pc$Bioassay <- '6ppd-q'


xx_q_pc = ggplot(quin_data_scores_pc, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Treatment)) + 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), 
        legend.key=element_blank()) + 
  annotate("text", x = Inf, y = -0.14, label = "Stress = 0.03", hjust = -.6, size = 5) +
  labs(x = "NMDS1", color = "Treatment", y = "NMDS2")  + 
  scale_colour_manual(values = c("firebrick2", "darkorange", "gold", "forestgreen", "deepskyblue2", "orchid2")) +
  theme_classic(base_size = 20)

gq_pc = ggplotGrob(xx_q_pc)
gq_pc$layout$clip[gq_pc$layout$name == "panel"] = "off"
grid.draw(gq_pc)


qpc_dist_matrix <- dist(m_quin_pc_2, method = "euclidean")
qpc_pairwise_results <- pairwise.perm.manova(qpc_dist_matrix, quin_pc$Treatment, nperm = 999)
print(qpc_pairwise_results)
qpc_pairwise_results$p.adjusted <- p.adjust(qpc_pairwise_results$p.value, method = "fdr")  # Adjust p-values
print(qpc_pairwise_results)




combined_pc = rbind(carb_data_scores_pc, dic_data_scores_pc, pfos_data_scores_pc, quin_data_scores_pc)

combined_pc$Bioassay <- factor(combined_pc$Bioassay,
                            levels = c("Carbamazepine", "Diclofenac", "PFOS", "6ppd-q"))


all_pc = ggplot(combined_pc, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(colour = Treatment)) + 
  facet_wrap(~ Bioassay) +
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), 
        legend.key=element_blank()) + 
  annotate("text", x = Inf, y = -0.14, label = "Stress = 0.03", hjust = -.6, size = 5) +
  labs(x = "NMDS1", color = "Treatment", y = "NMDS2")  + 
  scale_colour_manual(values = c("firebrick2", "darkorange", "gold", "forestgreen", "deepskyblue2", "orchid2")) +
  theme_classic(base_size = 30)

ggsave(all_pc, filename = "All_Bioassays/Figures/percent_dif_nmds_combined.png",
       device = "png", height = 12, width = 17)


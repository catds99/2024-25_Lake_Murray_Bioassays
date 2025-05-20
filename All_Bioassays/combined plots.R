# from early 2025
# makes combined biomass and community composition ploits with outliers removed

library(ggplot2)
library(ggpubr)

# import data:

no_outliers_long = read_excel("continuous_no_high_outliers.xlsx", sheet = 5)

# reformat data:

no_outliers_long$Group <- factor(no_outliers_long$Group,
                                 levels = c("ALLChla", "Cyanobacteria", "GreenAlgae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes"),
                                 labels = c("Total Chl a", "Cyanobacteria", "Green Algae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes"))

group_colors = c("Cyanobacteria" = "aquamarine3", "Cryptophytes" = "lightpink3", "Diatoms" = "lightgoldenrod", "Dinoflagellates" = "darkgoldenrod", "Green Algae" = "chartreuse4", "Haptophytes" = "burlywood4")

bioassay_labels <- c(
  "Carbamazepine" = "Carbamazepine",
  "Diclofenac" = "Diclofenac",
  "PFOS" = "PFOS",
  "6ppd-q" = "6ppd-q"
)

no_outliers_long$Bioassay <- factor(no_outliers_long$Bioassay,
                                    levels = c("Carbamazepine", "Diclofenac", "PFOS", "6ppd-q"))

# plot biomass of all contaminant bioassays all together:

no_outliers_biomass_all = ggplot(no_outliers_long, aes(x = fct_relevel(Treatment, "T0" , "Control", "Solvent", "A", "B", "C", "D", "E", "F"), y = Biomass, fill = Group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = group_colors) +
  facet_wrap(~ Bioassay, labeller = labeller(Bioassay = bioassay_labels)) +
  theme_classic(base_size = 30) +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "Solvent" =   "Solvent Control", "A" = "10% Max", "B" = "25% Max", "C" = "50% Max", "D" = "75% Max", "E" = "100% Max", "F" = "125% Max")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(no_outliers_biomass_all, filename = "no_outliers_biomass_all.png",
       device = "png", height = 12, width = 17)













































carb_pc_sd <- carb_pc %>%
  group_by(Treatment) %>%
  summarise(across(where(is.numeric), sd, na.rm = TRUE))

head(carb_pc_sd)


colnames(carb_pc_sd)[-1] <- paste0(colnames(carb_pc_sd)[-1], "_SD")

# Merge with original data
carb_pc <- left_join(carb_pc, carb_pc_sd, by = "Treatment")

# View updated dataset
head(carb_pc)
glimpse(carb_pc)



no_outliers_comm = read_excel("continuous_no_high_outliers.xlsx", sheet = 4)

no_outliers_comm$Group <- factor(no_outliers_comm$Group,
                        levels = c("Cyanobacteria", "GreenAlgae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes"),
                        labels = c("Cyanobacteria", "Green Algae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes"))

group_colors = c("Cyanobacteria" = "aquamarine3", "Cryptophytes" = "lightpink3", "Diatoms" = "lightgoldenrod", "Dinoflagellates" = "darkgoldenrod", "Green Algae" = "chartreuse4", "Haptophytes" = "burlywood4")


carb1 = ggplot(no_outliers_comm, aes(fill = Group, y = Carbamazepine, x = fct_relevel(Treatment, "T0" , "Control", "Solvent", "A", "B", "C", "D", "E", "F"))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = group_colors) +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "Solvent" = "Solvent Control", "A" = "10%", "B" = "25%", "C" = "50%", "D" = "75%", "E" = "100%", "F" = "125%")) +
  theme_classic(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

carb_percent_change_sd = ggplot(carb_pc_sd, aes(fill = Group, y = Haptophytes_pc, x = fct_relevel(Treatment, "T0" , "Control", "Solvent", "A", "B", "C", "D", "E", "F"))) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin=Carbamazepine-carb_pc_sd, ymax=Carbamazepine+carb_pc_sd), 
                width = 0.25,
                position=position_dodge(0.9)) +
  scale_fill_manual(values = group_colors) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", linewidth = 1) +
  geom_vline(xintercept=c(1.5, 2.5, 3.5, 4.5), linetype="solid", color = "black", linewidth = 0.5) +
  ylab("Percent Change") + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 30)











group_colors = c("Cyanobacteria" = "aquamarine3", "Cryptophytes" = "lightpink3", "Diatoms" = "lightgoldenrod", "Dinoflagellates" = "darkgoldenrod", "Green Algae" = "chartreuse4", "Haptophytes" = "burlywood4")



carbamazepine = ggplot(no_outliers_comm, aes(fill = Group, y = Carbamazepine, x = fct_relevel(Treatment, "T0" , "Control", "Solvent", "A", "B", "C", "D", "E", "F"))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = group_colors) +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "Solvent" = "Solvent Control", "A" = "10%", "B" = "25%", "C" = "50%", "D" = "75%", "E" = "100%", "F" = "125%")) +
  theme_classic(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

Diclofenac = ggplot(no_outliers_comm, aes(fill = Group, y = Diclofenac, x = fct_relevel(Treatment, "T0" , "Control", "Solvent", "A", "B", "C", "D", "E", "F"))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = group_colors) +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "Solvent" = "Solvent Control", "A" = "10%", "B" = "25%", "C" = "50%", "D" = "75%", "E" = "100%", "F" = "125%")) +
  theme_classic(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

PFOS = ggplot(no_outliers_comm, aes(fill = Group, y = PFOS, x = fct_relevel(Treatment, "T0" , "Control", "Solvent", "A", "B", "C", "D", "E", "F"))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = group_colors) +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "Solvent" = "Solvent Control", "A" = "10%", "B" = "25%", "C" = "50%", "D" = "75%", "E" = "100%", "F" = "125%")) +
  theme_classic(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

Quinone = ggplot(no_outliers_comm, aes(fill = Group, y = Quinone, x = fct_relevel(Treatment, "T0" , "Control", "Solvent", "A", "B", "C", "D", "E", "F"))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = group_colors) +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "Solvent" = "Solvent Control", "A" = "10%", "B" = "25%", "C" = "50%", "D" = "75%", "E" = "100%", "F" = "125%")) +
  theme_classic(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave(Quinone, filename = "6ppdquinonecommunitybarchart.png",
       device = "png", height = 12, width = 17)

#all plots together

all = ggarrange(carbamazepine, Diclofenac, PFOS, Quinone, 
                labels = c("Carbamazepine", "Diclofenac", "PFOS", "6ppd-q"),
                ncol = 2, nrow = 2)
ggsave(all, filename = "allbioassaycommunities.png",
       device = "png", height = 12, width = 17)



no_outliers_long = read_excel("continuous_no_high_outliers.xlsx", sheet = 5)

no_outliers_long$Group <- factor(no_outliers_long$Group,
                        levels = c("ALLChla", "Cyanobacteria", "GreenAlgae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes"),
                        labels = c("Total Chl a", "Cyanobacteria", "Green Algae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes"))

group_colors = c("Cyanobacteria" = "aquamarine3", "Cryptophytes" = "lightpink3", "Diatoms" = "lightgoldenrod", "Dinoflagellates" = "darkgoldenrod", "Green Algae" = "chartreuse4", "Haptophytes" = "burlywood4")

bioassay_labels <- c(
  "Carbamazepine" = "Carbamazepine",
  "Diclofenac" = "Diclofenac",
  "PFOS" = "PFOS",
  "6ppd-q" = "6ppd-q"
)

no_outliers_long$Bioassay <- factor(no_outliers_long$Bioassay,
                                 levels = c("Carbamazepine", "Diclofenac", "PFOS", "6ppd-q"))

no_outliers_biomass_all = ggplot(no_outliers_long, aes(x = fct_relevel(Treatment, "T0" , "Control", "Solvent", "A", "B", "C", "D", "E", "F"), y = Biomass, fill = Group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = group_colors) +
  facet_wrap(~ Bioassay, labeller = labeller(Bioassay = bioassay_labels)) +
  theme_classic(base_size = 30) +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "Solvent" =   "Solvent Control", "A" = "10% Max", "B" = "25% Max", "C" = "50% Max", "D" = "75% Max", "E" = "100% Max", "F" = "125% Max")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(no_outliers_biomass_all, filename = "no_outliers_biomass_all.png",
       device = "png", height = 12, width = 17)










chemtax_long <- chemtax %>%
  pivot_longer(cols = c(Carbamazepine, Diclofenac, PFOS, Quinone), 
               names_to = "Substance", 
               values_to = "Value")
substance_labels <- c(
  "Carbamazepine" = "Carbamazepine",
  "Diclofenac" = "Diclofenac",
  "PFOS" = "PFOS",
  "Quinone" = "6ppd-q"
)











summary_data = no_outliers_pc %>%
  group_by(Other, Treatment) %>%
  summarise(
    mean = mean(chl_a_pc),
    sd = sd(chl_a_pc, na.rm = TRUE),
  )

summary_data


summary_data$Other = factor(summary_data$Other, levels=c("Carbamazepine", "Diclofenac", "PFOS", "6ppd-Quinone")) 

all_bioassay_total_chl_a_pc = ggplot(data = summary_data, 
                                  aes(x = mean, y = fct_relevel(Treatment, "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))) + 
  geom_col(aes(x = mean,
               y = fct_relevel(Treatment, "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max")),
           fill = "lightcyan3",
           data = summary_data) +  
  geom_errorbar(aes(x = mean,
                    y = fct_relevel(Treatment, "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"), 
                    xmin = mean-sd, 
                    xmax = mean+sd,
                    width = 0.25),
                data = summary_data) +
  xlab("Percent Difference from Solvent Control") +
  ylab("Treatment") +
  theme_classic(base_size = 14) +
  facet_wrap(~Other, nrow = 1)

ggsave(all_bioassay_total_chl_a, filename = "figures/Biomass_horizontal/all_bioassay_total_chl_a.png",
       device = "png", height = 12, width = 15)









group_summary = no_outliers_pc %>%
  group_by(Other, Treatment) %>%
  summarise(
    tca = mean(chl_a_pc),
    cyano = mean(Cyanobacteria_pc),
    ga = mean(GreenAlgae_pc),
    crypto = mean(Cryptophytes_pc),
    dia = mean(Diatoms_pc),
    dino = mean(Dinoflagellates_pc),
    hapto = mean(Haptophytes_pc),
    tca_sd = sd(chl_a_pc, na.rm = TRUE),
    cyano_sd = sd(Cyanobacteria_pc, na.rm = TRUE),
    ga_sd = sd(GreenAlgae_pc, na.rm = TRUE),
    crypto_sd = sd(Cryptophytes_pc, na.rm = TRUE),
    dia_sd = sd(Diatoms_pc, na.rm = TRUE),
    dino_sd = sd(Dinoflagellates_pc, na.rm = TRUE),
    hapto_sd = sd(Haptophytes_pc, na.rm = TRUE)
  )

glimpse(group_summary)


group_summary$Other = factor(group_summary$Other, levels=c("Carbamazepine", "Diclofenac", "PFOS", "6ppd-Quinone")) 


groups= reshape(data = group_summary,
               idvar= c("Other", "Treatment"), 
               varying = 3:16, #We need to specify here the columns to be reshaped
               sep= "",
               direction = "long",
               v.names = "Value")

carb_haptos = group_summary %>%
  filter(Other == "Carbamazepine")

dic_comm = group_summary %>%
  filter(Other == "Diclofenac")

carb_haptos_plot = ggplot(data = carb_haptos) + 
  geom_col(aes(y = hapto,
               x = fct_relevel(Treatment, "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max")),
           fill = "lightcyan3",
           data = carb_haptos) +  
  geom_errorbar(aes(y = hapto,
                    x = fct_relevel(Treatment, "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"), 
                    ymin = hapto-hapto_sd, 
                    ymax = hapto+hapto_sd,
                    width = 0.25),
                data = carb_haptos) +
  ylab("Haptophyte Percent Difference") +
  xlab("Treatment") +
  theme_classic(base_size = 40) 

ggsave(carb_haptos_plot, filename = "carb_haptos_plot.png",
       device = "png", height = 18, width = 24)


dic_comm = group_summary %>%
  filter(Other == "Diclofenac")

dic_haptos_plot = ggplot(data = dic_comm) + 
  geom_col(aes(y = hapto,
               x = fct_relevel(Treatment, "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max")),
           fill = "lightcyan3",
           data = dic_comm) +  
  geom_errorbar(aes(y = hapto,
                    x = fct_relevel(Treatment, "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"), 
                    ymin = hapto-hapto_sd, 
                    ymax = hapto+hapto_sd,
                    width = 0.25),
                data = dic_comm) +
  ylab("Haptophyte Percent Difference") +
  xlab("Treatment") +
  theme_classic(base_size = 40) 

ggsave(dic_haptos_plot, filename = "dic_haptos_plot.png",
       device = "png", height = 18, width = 24)



dic_dinos_plot = ggplot(data = dic_comm) + 
  geom_col(aes(y = dino,
               x = fct_relevel(Treatment, "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max")),
           fill = "lightcyan3",
           data = dic_comm) +  
  geom_errorbar(aes(y = dino,
                    x = fct_relevel(Treatment, "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"), 
                    ymin = dino-dino_sd, 
                    ymax = dino+dino_sd,
                    width = 0.25),
                data = dic_comm) +
  ylab("Dinoflagellate Percent Difference") +
  xlab("Treatment") +
  theme_classic(base_size = 40) 

ggsave(dic_dinos_plot, filename = "dic_dinos_plot.png",
       device = "png", height = 18, width = 24)


haptos = ggplot(data = group_summary, 
                            aes(x = Other, y = fct_relevel(Treatment, "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))) + 
  geom_col(aes(x = hapto,
               y = fct_relevel(Treatment, "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max")),
           fill = "darkgrey",
           data = group_summary) +  
  geom_errorbar(aes(x = hapto,
                    y = fct_relevel(Treatment, "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"), 
                    xmin = hapto-hapto_sd, 
                    xmax = hapto+hapto_sd,
                    width = 0.25),
                data = group_summary) +
  xlab("Haptophyte Percent Change") +
  ylab("Treatment") +
  theme_classic(base_size = 14) +
  facet_wrap(~Other, nrow = 1)

ggsave(all_bioassay_cyano, filename = "figures/Biomass_horizontal/all_bioassay_cyano.png",
       device = "png", height = 12, width = 15)


dino = ggplot(data = group_summary, 
                aes(x = Ot, y = fct_relevel(Treatment, "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"))) + 
  geom_col(aes(x = dino,
               y = fct_relevel(Treatment, "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max")),
           fill = "darkgrey",
           data = group_summary) +  
  geom_errorbar(aes(x = dino,
                    y = fct_relevel(Treatment, "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"), 
                    xmin = dino-dino_sd, 
                    xmax = dino+dino_sd,
                    width = 0.25),
                data = group_summary) +
  xlab("Dinoflagellate Percent Change") +
  ylab("Treatment") +
  theme_classic(base_size = 14) +
  facet_wrap(~Other, nrow = 1)


carb_dinos_plot = ggplot(data = carb_haptos) + 
  geom_col(aes(y = dino,
               x = fct_relevel(Treatment, "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max")),
           fill = "lightcyan3",
           data = carb_haptos) +  
  geom_errorbar(aes(y = dino,
                    x = fct_relevel(Treatment, "10% Max", "25% Max", "50% Max", "75% Max", "100% Max", "125% Max"), 
                    ymin = dino-dino_sd, 
                    ymax = dino+dino_sd,
                    width = 0.25),
                data = carb_haptos) +
  ylab("Dinoflagellate Percent Difference") +
  xlab("Treatment") +
  theme_classic(base_size = 30) 

ggsave(carb_dinos_plot, filename = "carb_dinos_plot.png",
       device = "png", height = 18, width = 24)

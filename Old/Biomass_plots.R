# This is from sometime in 2024, with data before outliers were removed
# it creates plots that show biomass

library(tidyverse)
library(ggpubr)
library(rstatix)
library(readxl)
library(car)


#################################### import data:

chemtax = read_excel("2024_Bioassay_pigment_data.xlsx", sheet = 4)

glimpse(chemtax)

######################################### Relabel and Reorder

chemtax$Group <- factor(chemtax$Group,
                           levels = c("ALLChla", "Cyanobacteria", "GreenAlgae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes"),
                           labels = c("Total Chl a", "Cyanobacteria", "Green Algae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes"))

group_colors = c("Cyanobacteria" = "aquamarine3", "Cryptophytes" = "lightpink3", "Diatoms" = "lightgoldenrod", "Dinoflagellates" = "darkgoldenrod", "Green Algae" = "chartreuse4", "Haptophytes" = "burlywood4")

################################# Biomass Bioassay Plots

carbamazepine = ggplot(chemtax, aes(fill = Group, y = Carbamazepine, x = fct_relevel(Treatment, "T0" , "Control", "Solvent", "A", "B", "C", "D", "E", "F"))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = group_colors) +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "Solvent" = "Solvent Control", "A" = "10%", "B" = "25%", "C" = "50%", "D" = "75%", "E" = "100%", "F" = "125%")) +
  theme_classic(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

Diclofenac = ggplot(chemtax, aes(fill = Group, y = Diclofenac, x = fct_relevel(Treatment, "T0" , "Control", "Solvent", "A", "B", "C", "D", "E", "F"))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = group_colors) +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "Solvent" = "Solvent Control", "A" = "10%", "B" = "25%", "C" = "50%", "D" = "75%", "E" = "100%", "F" = "125%")) +
  theme_classic(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

PFOS = ggplot(chemtax, aes(fill = Group, y = PFOS, x = fct_relevel(Treatment, "T0" , "Control", "Solvent", "A", "B", "C", "D", "E", "F"))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = group_colors) +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "Solvent" = "Solvent Control", "A" = "10%", "B" = "25%", "C" = "50%", "D" = "75%", "E" = "100%", "F" = "125%")) +
  theme_classic(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

Quinone = ggplot(chemtax, aes(fill = Group, y = Quinone, x = fct_relevel(Treatment, "T0" , "Control", "Solvent", "A", "B", "C", "D", "E", "F"))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = group_colors) +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "Solvent" = "Solvent Control", "A" = "10%", "B" = "25%", "C" = "50%", "D" = "75%", "E" = "100%", "F" = "125%")) +
  theme_classic(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


#all plots together

all = ggarrange(carbamazepine, Diclofenac, PFOS, Quinone, 
          labels = c("Carbamazepine", "Diclofenac", "PFOS", "6ppd-q"),
          ncol = 2, nrow = 2)
ggsave(all, filename = "allbioassaycommunities.png",
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

chemtax_biomass = ggplot(chemtax_long, aes(x = fct_relevel(Treatment, "T0" , "Control", "Solvent", "A", "B", "C", "D", "E", "F"), y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = group_colors) +
  facet_wrap(~ Substance, labeller = labeller(Substance = substance_labels)) +
  theme_classic(base_size = 30) +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(chemtax_biomass, filename = "chemtax_biomass.png",
       device = "png", height = 12, width = 17)
